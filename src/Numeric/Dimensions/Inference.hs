-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Inference
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This is a supplementary module that provides a GHC type-checker plugin.
-- The plugin infers some equalities that overwise would be impossible to infer
--  using GHC typechecker only.
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Numeric.Dimensions.Inference (plugin, inferListOpsPlugin) where

import           Control.Applicative                (Alternative (..))
import           Control.Arrow
import           Control.Monad                      ((>=>))
import           Data.Coerce
import           Data.Maybe                         (catMaybes, fromMaybe,
                                                     mapMaybe)
import           Data.Semigroup

import           Class
import           Coercion
import           FamInst
import           FastString
import           Id
import           InstEnv                            (DFunId)
import           Name
import           Outputable                         (Outputable (..),
                                                     showSDocUnsafe)
import           Plugins                            (Plugin (..), defaultPlugin)
import           TcEvidence
import           TcPluginM
import           TcRnMonad
import           TcType
import           TyCon
import           TyCoRep
import           Type
import           TysWiredIn                         (listTyCon)

import           Numeric.Dimensions.Inference.Types


-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just inferNatsPlugin }


inferNatsPlugin :: TcPlugin
inferNatsPlugin = TcPlugin
  { tcPluginInit  = initInferenceTypes
  , tcPluginSolve = const decideNatOps
  , tcPluginStop  = const (return ())
  }

decideNatOps ::  [Ct] -- ^ Givens
              -> [Ct] -- ^ Deriveds
              -> [Ct] -- ^ Wanteds
              -> TcPluginM TcPluginResult
decideNatOps _ _ []      = return (TcPluginOk [] [])
decideNatOps _ _ wanteds =
  uncurry TcPluginOk . second concat . unzip . catMaybes <$> mapM checkNats wanteds

checkNats :: Ct -> TcPluginM (Maybe ((EvTerm, Ct), [Ct]))
checkNats ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})
  = case classifyPredType t of
    ClassPred cl types ->
      case ( getOccName cl == mkOccName tcClsName "KnownNat"
           , splitTyConApp_maybe <$> types
           ) of
        (True, [Just (constr, apps)])
          | getOccName constr == mkOccName tcName "Length"
          , [_, innerType] <- apps -> do
              tcPluginIO . putStrLn $ "Inside a list: " ++ show innerType
              return $ Just
                  ( ( EvLit (EvNum 23), ct) -- (EvStr $ mkFastString "Length of a list is always known, because we do not allow infinite types." ), ct )
                  , []
                  )
          | any ((getOccName constr ==) . mkOccName tcName ) ["+", "-", "*", "^"] -> do
              newWanteds <- traverse (fmap CNonCanonical . newWanted myLoc . mkClassPred cl . (:[])) apps
              return $ Just
                  ( ( EvLit (EvNum 17), ct)  -- (EvStr $ mkFastString "It does not work this way :("), ct )
                  , newWanteds
                  )
          | otherwise -> return Nothing
        _ -> return Nothing
    _ -> return Nothing
checkNats _ = return Nothing


-- | Initialize with "Numeric.Dimensions" module necessary types
inferListOpsPlugin :: TcPlugin
inferListOpsPlugin = TcPlugin
  { tcPluginInit  = initInferenceTypes
  , tcPluginSolve = decideListOps
  , tcPluginStop  = const (return ())
  }

-- | Go over all wanteds and try to simplify or solve them
decideListOps :: Maybe InferenceTypes
              -> [Ct] -- ^ Givens
              -> [Ct] -- ^ Deriveds
              -> [Ct] -- ^ Wanteds
              -> TcPluginM TcPluginResult
decideListOps Nothing _ _ _         = return (TcPluginOk [] [])
decideListOps (Just it0) givens _ []         = do
    resetPhase it0
    (newGivens, _)<- runPrepInference (makeAllPossibleGivens givens) it0
    return (TcPluginOk (fmap (first ctEvTerm) newGivens) [])
decideListOps (Just it0) givens _deriveds wanteds = incrementPhase it0 >>= \i ->
  if i >= 3
  then return (TcPluginOk [] [])
  else do
    -- -- tcPluginIO . putStrLn . ("SKOLEMS: " ++ ) . show . catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev <$> givens
    -- -- tcPluginIO . print $  (varSetElems hsListVars, varSetElems listVars)
    -- -- tcPluginIO . putStrLn $ "ToLists: " ++ show (eltsUFM toListRelations)
    -- -- tcPluginIO . putStrLn $ "EvalConses: " ++ show (eltsUFM evalConsRelations)
    -- tcPluginIO . putStrLn $ "Found wanteds: "
    -- tcPluginIO $ mapM_  print wanteds
    -- tcPluginIO . putStrLn $ "Found givens:"
    -- tcPluginIO $ mapM_  print ((\g -> (onlySkolemOrigin . ctl_origin . ctev_loc $ cc_ev g, g)) <$> givens)
    -- let givensLLL = filter (not . null . snd)
    --           $ second (filter (\x -> occNameString (getOccName x) == "asLLLL"))
    --          <$> map (\g -> (g, elems $ findAllCtTyVars g)) givens
    (_newGivens0, it1)<- runPrepInference (makeAllPossibleGivens givens) it0
    -- tcPluginIO . putStrLn $ "All possible givens: "
    -- tcPluginIO $ mapM_ print newGivens
    -- unless (null givensLLL) $
      -- tcPluginIO . putStrLn . ("SKOLEMS: " ++ ) . show . catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev . fst <$> givensLLL
    (_newGivens_, it) <- flip runPrepInference it1 $ do
        toListRelations <- foldMapMon getCtToListRelationMap givens
        evalConsRelations <- foldMapMon getCtEvalConsRelationMap givens
        -- lift . tcPluginIO . putStrLn $ "Found given ToList rels:" ++ show (elems toListRelations)
        -- lift . tcPluginIO . putStrLn $ "Found given EvalCons rels:" ++ show (elems evalConsRelations)
        createListListRelations hsListVars listOpVars toListRelations evalConsRelations
        -- replaceVarsInGivens givens
        --return r
    -- tcPluginIO . putStrLn $ "New givens: "
    -- tcPluginIO $ mapM_ print newGivens
    -- tcPluginIO . putStrLn $ "Found vars: "  ++ show (map (\v -> (v, tyVarKind v)) $ elems allVars)
    -- tcPluginIO . print $ map (\v -> (v, tyVarKind v, getTyVarKindParam v)). varSetElems $ findAllTyVars lhs <> findAllTyVars rhs <> tyVarsSet
    -- EvBindsVar evBindMapRef _ <- getEvBindsTcPluginM
    -- evBinds <- unsafeTcPluginTcM $ bagToList . evBindMapBinds <$> readMutVar evBindMapRef
    -- tcPluginIO . putStrLn $ "Evidence binds: " ++ show (eb_rhs <$> evBinds)

    -- tcPluginIO . putStrLn $ "Computed ToList rels:" ++ show (elems . toListRels $ tyVarRelations it)
    -- tcPluginIO . putStrLn $ "Computed EvalCons rels:" ++ show (elems . evalConsRels $ tyVarRelations it)
    uncurry TcPluginOk . second catMaybes . unzip . catMaybes <$> mapM (check it) wanteds
      -- . first (fmap (first ctEvTerm) newGivens ++)
      -- first (fmap newEvidence newGivens ++) .
  where
    allVars = foldMap findAllCtTyVars givens
          --  <> foldMap findAllCtTyVars deriveds
           <> foldMap findAllCtTyVars wanteds
    hsListVars = coerce $ Tagged <$> filterMap isHsListKind allVars :: Set HsListVar
    listOpVars = coerce $ Tagged <$> filterMap isListKind allVars :: Set ListOpVar
    -- onlySkolemOrigin :: CtOrigin -> Maybe SkolemInfo
    -- onlySkolemOrigin (GivenOrigin so) = Just so
    -- onlySkolemOrigin _                = Nothing




-- | Wrapper function for my type inference
check :: InferenceTypes -> Ct -> TcPluginM (Maybe ((EvTerm, Ct), Maybe Ct))
check it ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})
  = case classifyPredType t of
    EqPred NomEq lhs rhs -> runInference (liftToPlugin . solve $ InferEq lhs rhs) it
      >>= \r -> case r of
        (_ , False) -> do
          tcPluginIO . putStrLn $ "NOT APPLICABLE: " ++ show lhs ++ " ~ " ++ show rhs
          return Nothing
        (Eliminated, True) -> do
          tcPluginIO . putStrLn $ "ELIMINATED!: " ++ show lhs ++ " ~ " ++ show rhs
          return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal lhs rhs),ct), Nothing)
        (InferEq newLhs newRhs, True) -> do
          let newEqPred = mkPrimEqPredRole Nominal newLhs newRhs
          newWantedEq <- newWanted myLoc newEqPred
          tcLclEnv <- snd <$> TcPluginM.getEnvs
          let tyVarsRef = tcl_tyvars tcLclEnv
          _tyVarsSet <- unsafeTcPluginTcM $ readMutVar tyVarsRef
          -- tcPluginIO . print $ map (\v -> (v, tyVarKind v, getTyVarKindParam v)). varSetElems $ findAllTyVars lhs <> findAllTyVars rhs <> tyVarsSet
          tcPluginIO . putStrLn $ "WAS: " ++ show t
          tcPluginIO . putStrLn $ "NOW: " ++ show newEqPred
          return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal lhs rhs),ct), Just $ CNonCanonical newWantedEq)
    _ -> return Nothing
check _ _ = return Nothing





-- | Given two types, try to infer their equality or simplify them
solve :: InferEq -> Inference InferEq
solve = solve' >=> normaliseOutEvalCons >=> solve'
  where
    solve' Eliminated = return Eliminated
    solve' (InferEq lhs rhs) = do
      newLhs <- simplify lhs
      newRhs <- simplify rhs
      simplified <- isModified
      return $
        if simplified && eqType newLhs newRhs
        then Eliminated
        else InferEq newLhs newRhs





findAllTyVars :: Type -> Set Var
findAllTyVars t = case (splitTyConApp_maybe t, getTyVar_maybe t) of
    (Nothing, Nothing)        -> mempty
    (Nothing, Just v)         -> singleton v v
    (Just (_, apps), Nothing) -> foldMap findAllTyVars apps
    (Just (_, apps), Just v)  -> insert (foldMap findAllTyVars apps) v v



findAllPredTreeTyVars :: PredTree -> Set Var
findAllPredTreeTyVars (ClassPred _ ts) = foldMap findAllTyVars ts
findAllPredTreeTyVars (EqPred _ lhs rhs) = findAllTyVars lhs <> findAllTyVars rhs
findAllPredTreeTyVars (IrredPred _) = mempty

findAllCtTyVars :: Ct -> Set Var
findAllCtTyVars = findAllPredTreeTyVars . classifyPredType . ctev_pred . cc_ev


-- | Checks if this type is "ToList ds"; if so, gives type variable and "ToList ds"
tcToListMaybe :: Monad m => Type -> InferenceT m (Maybe (HsListVar, ListOpType))
tcToListMaybe t = do
  InferenceTypes {..} <- getState
  return $ case splitTyConApp_maybe t of
    Nothing -> Nothing
    Just (constr, apps) ->
      if constr == tcToListNat || constr == tcToList
      then case apps of
        []    -> Nothing
        [x]   -> getToList tcToList $ Tagged x
        _:x:_ -> getToList tcToList $ Tagged x
      else Nothing
  where
    getToList :: TyCon -> HsListType -> Maybe (HsListVar, ListOpType)
    getToList tc x = case getTyVar_maybe (unTag x) of
      Nothing -> Nothing
      Just xv -> Just (Tagged xv, Tagged $ mkTyConApp tc [getTyVarKindParam xv, unTag x])

-- | Checks if this type is "EvalCons dsL"; if so, "EvalCons dsL"
tcEvalConsMaybe :: Monad m => Type -> InferenceT m (Maybe (ListOpVar, HsListType))
tcEvalConsMaybe t = do
  InferenceTypes {..} <- getState
  return $ case splitTyConApp_maybe t of
    Nothing -> Nothing
    Just (constr, apps) ->
      if constr == tcEvalConsNat || constr == tcEvalCons
      then case apps of
        []    -> Nothing
        [x]   -> getEvalCons tcEvalCons $ Tagged x
        _:x:_ -> getEvalCons tcEvalCons $ Tagged x
      else Nothing
  where
    getEvalCons :: TyCon -> ListOpType -> Maybe (ListOpVar, HsListType)
    getEvalCons tc x = case getTyVar_maybe (unTag x) of
      Nothing -> Nothing
      Just xv -> Just (Tagged xv, Tagged $ mkTyConApp tc [getTyVarKindParam xv, unTag x])

-- NOT USED AT THE MOMENT, BUT USABLE
-- findAllTyToLists :: Monad m => Type -> InferenceT m (Map HsListVar (HsListVar, ListOpType))
-- findAllTyToLists t = tcToListMaybe t >>= f
--   where
--     f (Just (xv, xt)) = pure $ singleton xv (xv, xt)
--     f Nothing = withTypeConstr t mempty $ foldM (\m x -> mappend m <$> findAllTyToLists x) mempty . snd


replaceAllToLists :: Monad m => Type -> InferenceT m Type
replaceAllToLists t0 = trySubstituteToList (Tagged t0) >>= \(Tagged t1, replaced) ->
    if replaced
    then modified t1
    else withTypeConstr t0 t0 $
      \(constr, apps) -> mkTyConApp constr <$> traverse replaceAllToLists apps



-- NOT USED AT THE MOMENT, BUT USABLE
-- findAllTyEvalCons :: Monad m => Type -> InferenceT m  (Map ListOpVar (ListOpVar, HsListType))
-- findAllTyEvalCons t = tcEvalConsMaybe t >>= f
--   where
--     f (Just (xv, xt)) = pure $ singleton xv (xv, xt)
--     f Nothing = withTypeConstr t mempty $ foldM (\m x -> mappend m <$> findAllTyEvalCons x) mempty . snd


getEvalConsRelationMaybe :: Monad m => PredTree -> InferenceT m  (Maybe (HsListVar, ListOpVar))
getEvalConsRelationMaybe (EqPred _ lhs rhs) = do
    lEvalCons <- tcEvalConsMaybe lhs
    rEvalCons <- tcEvalConsMaybe rhs
    pure $ case ( getTyVar_maybe lhs, lEvalCons
                , getTyVar_maybe rhs, rEvalCons) of
      (Nothing, _, Nothing, _)    -> Nothing
      (_, Nothing, _, Nothing)    -> Nothing
      (Just v , _, _, Just (w,_)) -> Just (Tagged v,w)
      (_, Just (w,_), Just v , _) -> Just (Tagged v,w)
      (_,_,_,_)                   -> Nothing
getEvalConsRelationMaybe _ = pure Nothing

getToListRelationMaybe :: Monad m => PredTree -> InferenceT m (Maybe (ListOpVar, HsListVar))
getToListRelationMaybe (EqPred _ lhs rhs) = do
    lToList <- tcToListMaybe lhs
    rToList <- tcToListMaybe rhs
    pure $ case ( getTyVar_maybe lhs, lToList
                , getTyVar_maybe rhs, rToList ) of
      (Nothing, _, Nothing, _)    -> Nothing
      (_, Nothing, _, Nothing)    -> Nothing
      (Just v , _, _, Just (w,_)) -> Just (Tagged v,w)
      (_, Just (w,_), Just v , _) -> Just (Tagged v,w)
      (_,_,_,_)                   -> Nothing
getToListRelationMaybe _ = pure Nothing


-- List k var key ([k] var, EvalCons List k var)
getCtEvalConsRelationMap :: Monad m => Ct -> InferenceT m (Map ListOpVar (HsListVar, ListOpVar))
getCtEvalConsRelationMap = fmap fm . getEvalConsRelationMaybe . classifyPredType . ctev_pred . cc_ev
  where
    fm Nothing      = mempty
    fm (Just (v,w)) = singleton w (v,w)

-- [k] var key ( List k var, ToList [k] var)
getCtToListRelationMap ::  Monad m => Ct -> InferenceT m (Map HsListVar (ListOpVar, HsListVar))
getCtToListRelationMap = fmap fm . getToListRelationMaybe . classifyPredType . ctev_pred . cc_ev
  where
    fm Nothing      = mempty
    fm (Just (v,w)) = singleton w (v,w)



-- | Get all possible one-to-one relations of variables.
--   This includes type equalities xs~ys, as well as ToList and EvalCons relations.
--   `AllVarRelations` type has an overloaded Monoid instance, allowing to combine all relation graphs together.
getCtVarRelations :: Monad m => Ct -> InferenceT m AllVarRelations
getCtVarRelations = go . classifyPredType . ctev_pred . cc_ev
  where
    go (EqPred _ lhs rhs) = do
        InferenceTypes {..} <- getState
        lToList   <- fmap toMapping <$> tcToListMaybe lhs
        rToList   <- fmap toMapping <$> tcToListMaybe rhs
        lEvalCons <- fmap toMapping <$> tcEvalConsMaybe lhs
        rEvalCons <- fmap toMapping <$> tcEvalConsMaybe rhs
        let lpure = toMapping . (id &&& mkTyVarTy) <$> getTyVar_maybe lhs
            rpure = toMapping . (id &&& mkTyVarTy) <$> getTyVar_maybe rhs
            newLhs = lToList <|> lEvalCons <|> lpure
            newRhs = rToList <|> rEvalCons <|> rpure
            allDirectRelations = case (,) <$> newLhs <*> newRhs of
              Nothing -> mempty
              -- Identity between two variables
              Just (IsSame lv lt, IsSame rv rt)
                -> AVR ( mkMap [ (lv, (lv, singleton rv (IsSame rv rt)))
                               , (rv, (rv, singleton lv (IsSame lv lt)))
                               ] )
              Just (IsToList (Tagged lv) _, IsToList (Tagged rv) _)
                -> AVR ( mkMap [ (lv, (lv, singleton rv (IsSame rv $ mkTyVarTy rv)))
                               , (rv, (rv, singleton lv (IsSame lv $ mkTyVarTy lv)))
                               ] )
              Just (IsEvalCons (Tagged lv) _, IsEvalCons (Tagged rv) _)
                -> AVR ( mkMap [ (lv, (lv, singleton rv (IsSame rv $ mkTyVarTy rv)))
                               , (rv, (rv, singleton lv (IsSame lv $ mkTyVarTy lv)))
                               ] )
              Just (IsSame lv _, IsToList rv rt)
                -> AVR ( mkMap [ (lv, (lv, singleton (unTag rv) (IsToList rv rt)))
                               , (unTag rv, (unTag rv, singleton lv (IsEvalCons (Tagged lv)
                                                     . Tagged $ mkTyConApp tcEvalCons [getTyVarKindParam lv, mkTyVarTy lv])))
                               ] )
              Just (IsToList rv rt, IsSame lv _)
                -> AVR ( mkMap [ (lv, (lv, singleton (unTag rv) (IsToList rv rt)))
                               , (unTag rv, (unTag rv, singleton lv (IsEvalCons (Tagged lv)
                                                     . Tagged $ mkTyConApp tcEvalCons [getTyVarKindParam lv, mkTyVarTy lv])))
                               ] )
              Just (IsSame lv _, IsEvalCons rv rt)
                -> AVR ( mkMap [ (lv, (lv, singleton (unTag rv) (IsEvalCons rv rt)))
                               , (unTag rv, (unTag rv, singleton lv (IsToList (Tagged lv)
                                                     . Tagged $ mkTyConApp tcToList [getTyVarKindParam lv, mkTyVarTy lv])))
                               ] )
              Just (IsEvalCons rv rt, IsSame lv _)
                -> AVR ( mkMap [ (lv, (lv, singleton (unTag rv) (IsEvalCons rv rt)))
                               , (unTag rv, (unTag rv, singleton lv (IsToList (Tagged lv)
                                                     . Tagged $ mkTyConApp tcToList [getTyVarKindParam lv, mkTyVarTy lv])))
                               ] )
              Just (IsEvalCons {}, IsToList {}) -> mempty
              Just (IsToList {}, IsEvalCons {}) -> mempty

        allReachableRelations allDirectRelations
    go _ = return mempty


-- | Get list of givens and extract all variables from there.
--   Next, construct relations map and create a given relation for each single possible relation.
makeAllPossibleGivens :: [Ct] -> InferenceT TcPluginM [(CtEvidence, Ct)]
makeAllPossibleGivens cts = do
    -- lift . tcPluginIO . print $ skolems
    InferenceTypes {..} <- getState
    relationGraph <- foldMapMon getCtVarRelations cts
    let makeEvidence :: (TyVar, Mapping) -> InferenceT TcPluginM (CtEvidence, Ct)
        makeEvidence (lhs, rhs) = lift $ do
            loc <- mkGivenLoc topTcLevel ( bestDefaultSkolem (Check eqPredType)
                                           (SigSkol InstDeclCtxt (Check eqPredType))
                                           skolems
                                         ) . snd <$> TcPluginM.getEnvs
            ctEv <- newGiven loc eqPredType evterm
            pure (ctEv, makeCt lhs rt ctEv)
          where
            makeCt :: TyVar -> Type -> CtEvidence -> Ct
            makeCt lhsv rhsty ctEv = case splitTyConApp_maybe rhsty of
              Just (c, apps) -> CFunEqCan ctEv c apps lhsv
              Nothing        -> CTyEqCan ctEv lhsv rhsty NomEq
            lt = mkTyVarTy lhs
            rt = mappingType rhs
            eqPredType = mkPrimEqPredRole Nominal lt rt
            evterm = EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference-mkEqs") Nominal lt rt)
    fmap concat $ traverse (\(v, m) -> traverse (makeEvidence . (,) v) $ elems m) . elems $ _getAVR relationGraph
  where
    skolems = catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev <$> cts
    onlySkolemOrigin :: CtOrigin -> Maybe SkolemInfo
    onlySkolemOrigin (GivenOrigin so) = Just so
    onlySkolemOrigin _                = Nothing
    bestDefaultSkolem :: ExpType -> SkolemInfo -> [SkolemInfo] -> SkolemInfo
    bestDefaultSkolem _ si []               = si
    bestDefaultSkolem et _ (SigSkol c _: _) = SigSkol c et
    bestDefaultSkolem et si (_ : xs)        = bestDefaultSkolem et si xs


-- replaceVarsInGivens :: [Ct]
--                     -> InferenceT TcPluginM ()
-- replaceVarsInGivens = mapM_ (replaceG . cc_ev)
--   where
--     replaceG ctEv = case classifyPredType (ctev_pred ctEv) of
--       EqPred NomEq lhs rhs -> do
--         InferenceTypes {..} <- getState
--         (ln, lUseful)<- subInference $ replaceOne lhs
--         (rn, rUseful) <- subInference $ replaceOne rhs
--         when (lUseful || rUseful)
--           . void . lift $ newGiven (ctev_loc ctEv) (mkPrimEqPredRole Nominal ln rn) (EvId $ ctev_evar ctEv)
--       _ -> return ()
--     replaceOne t = withTypeConstr t t $ \(constr, apps) -> do
--         InferenceTypes {..} <- getState
--         if constr == tcToList || constr == tcToListNat
--         then pure t
--         else mkTyConApp constr <$> traverse replaceAllToLists apps



      -- check it ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})

createListListRelations :: Set HsListVar -- ^ Bare haskell list variables
                        -> Set ListOpVar
                           -- [k] var key ( List k var, ToList [k] var)
                        -> Map HsListVar (ListOpVar, HsListVar) -- ^ ToLists
                           -- List k var key ([k] var, EvalCons List k var)
                        -> Map ListOpVar (HsListVar, ListOpVar) -- ^ EvalConses
                        -> InferenceT TcPluginM [CtEvidence]
createListListRelations hsListVars listOpVars givenToListRels givenEvalConsRels = do
    it@InferenceTypes {..} <- getState
    let -- create List k variable given [k] variable
        makeListOpVar :: HsListVar -- ^ [k]
                      -> InferenceT TcPluginM ListOpVar -- ^ List k
        makeListOpVar v = lift $ Tagged <$> newFlexiTyVar (mkTyConApp tcList [getTyVarKindParam $ unTag v])
        -- create [k] variable given List k variable
        makeHsListVar :: ListOpVar -- ^ List k
                      -> InferenceT TcPluginM HsListVar -- ^ [k]
        makeHsListVar v = lift $ Tagged <$> newFlexiTyVar (mkTyConApp listTyCon [getTyVarKindParam $ unTag v])
        -- List of all found [k] variables
        allHsListVars :: Set HsListVar
        allHsListVars = hsListVars
                     <> mkSet (snd <$> elems givenToListRels)
                     <> mkSet (fst <$> elems givenEvalConsRels)
        -- List of all found List k variables
        allListOpVars :: Set ListOpVar
        allListOpVars = listOpVars
                     <> mkSet (fst <$> elems givenToListRels)
                     <> mkSet (snd <$> elems givenEvalConsRels)

        -- all given relations [k] -> List k
        allExistingToLists :: Map HsListVar (ListOpVar, HsListVar)
        allExistingToLists = givenToListRels <> mkMap (map (\(a,b) -> (a,(b,a))) (elems givenEvalConsRels))
        -- all given relations List k -> [k]
        allExistingEvalConses :: Map ListOpVar (HsListVar, ListOpVar)
        allExistingEvalConses = givenEvalConsRels <> mkMap (map (\(a,b) -> (a,(b,a))) (elems givenToListRels))
        -- those relations that are known, but missing one of the equations (either ToList or EvalCons)
        missingToLists    = allExistingToLists `difference` givenToListRels
        missingEvalConses = allExistingEvalConses `difference`  givenEvalConsRels
        -- a function to write in an evidence about ToList relation
        makeToListEvidence :: (ListOpVar, HsListVar) -> InferenceT TcPluginM CtEvidence
        makeToListEvidence (lhs, rhs)
          = let lt = mkTyVarTy <$> lhs
                rt = Tagged $ mkTyConApp tcToList [getTyVarKindParam $ unTag rhs, mkTyVarTy $ unTag rhs] :: ListOpType
                eqPredType = mkPrimEqPredRole Nominal (unTag lt) (unTag rt)
                evterm = EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference-newToList") Nominal (unTag lt) (unTag rt))
            in lift $ do
              loc <- mkGivenLoc topTcLevel (SigSkol InstDeclCtxt $ Check eqPredType) . snd <$> TcPluginM.getEnvs
              newGiven loc eqPredType evterm
        -- a function to write in an evidence about EvalCons relation
        makeEvalConsEvidence :: (HsListVar, ListOpVar) -> InferenceT TcPluginM CtEvidence
        makeEvalConsEvidence (lhs, rhs)
          = let lt = mkTyVarTy <$> lhs
                rt = Tagged $ mkTyConApp tcEvalCons [getTyVarKindParam $ unTag rhs, mkTyVarTy $ unTag rhs] :: HsListType
                eqPredType = mkPrimEqPredRole Nominal (unTag lt) (unTag rt)
                evterm = EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference-newEvalCons") Nominal (unTag lt) (unTag rt))
            in lift $ do
              loc <- mkGivenLoc topTcLevel (SigSkol InstDeclCtxt $ Check eqPredType) . snd <$> TcPluginM.getEnvs
                 -- (InferSkol [(tyVarName lhs, lt)])
              newGiven loc eqPredType evterm

    -- Add known missing equations to the evidence base
    newToListEvs <- traverse makeToListEvidence . elems $ missingToLists
    newEvalConsEvs <- traverse makeEvalConsEvidence . elems $ missingEvalConses
    newToListEvs2 <- traverse (\(x,y) -> makeToListEvidence (y,x)) . elems $ missingEvalConses
    newEvalConsEvs2 <- traverse (\(x,y) -> makeEvalConsEvidence (y,x)) . elems $ missingToLists

    -- By this time there are variables that have no pairs in List k <-> [k]
    let notCoveredHsListVars = allHsListVars `difference` allExistingToLists
        notCoveredListOpVars = allListOpVars `difference` allExistingEvalConses
    -- Create them
    newListOpVars <- traverse (\v -> (,) v <$> makeListOpVar v)  $ elems notCoveredHsListVars
    newHsListVars <- traverse (\v -> (,) v <$> makeHsListVar v)  $ elems notCoveredListOpVars

    createdToListEvs <- traverse makeToListEvidence newHsListVars
    createdEvalConsEvs <- traverse makeEvalConsEvidence newListOpVars
    createdToListEvs2 <- traverse (\(x,y) -> makeToListEvidence (y,x)) newListOpVars
    createdEvalConsEvs2 <- traverse (\(x,y) -> makeEvalConsEvidence (y,x)) newHsListVars

    -- let newListVars = mempty
    -- unless (null newListVars) $
    --   tcPluginIO . putStrLn $ "newListVars: " ++ show newListVars
    -- let newToList'   = mkMap $ map (\(a,b) -> (a,(b,a))) newListOpVars ++ map (\(a,b) -> (b,(a,b))) newHsListVars
    --     newEvalCons' = mkMap $ map (\(a,b) -> (a,(b,a))) newHsListVars ++ map (\(a,b) -> (b,(a,b))) newListOpVars


    let createdTyVarRels =
              TyVarRelations (mkMap newListOpVars <> fmap fst allExistingToLists)
                             (mkMap newHsListVars <> fmap fst allExistingEvalConses)
    -- lift . tcPluginIO . putStrLn $ "Constructed ToList rels: " ++ show (elems $ toListRels createdTyVarRels)
    -- lift . tcPluginIO . putStrLn $ "Constructed EvalCons rels: " ++ show (elems $ evalConsRels createdTyVarRels)
    setState it{tyVarRelations = createdTyVarRels}
    return ( newToListEvs ++ createdToListEvs ++ newEvalConsEvs ++ createdEvalConsEvs ++ newToListEvs2 ++ newEvalConsEvs2
           ++ createdToListEvs2 ++ createdEvalConsEvs2)

    -- mkPrimEqPredRole Nominal newLhs newRhs
    --  makeListVarAndRels :: TcPluginM TyVar -- ^ [] variable
    --                     -> ( [(TyVar, TyVar)]
    --                        , TcTyVar )







--------------------------------------------------------------------------------
-- Simplification
--------------------------------------------------------------------------------



simplify :: Type -> Inference Type
simplify = simplify0 >=> replaceAllToLists

-- | Try various simplification rules on a type family contructor
simplify0 :: Type -> Inference Type
simplify0 t0 = do
    t1 <- withListOpType t0 t0 (fmap unTag . simplifyListToList)
    withTypeConstr t1 t1 removeAllDescSimplifyLists -- TODO: very inefficient, need to refink
    -- return t1
  where
    removeAllDescSimplifyLists (constr, apps) = do
      InferenceTypes {..} <- getState
      mkTyConApp constr <$>
        if constr == tcSimplifyList
        then traverse (fmap unTag . removeAllSimplifyList . Tagged) apps
        else traverse simplify0 apps



-- | Transform (SimplifyList (ToList xs)) -> (ToList xs).
--   SimplifyList guaranteed to return Cons-Empty representation,
--   the same is true for ToList function.
--   Thus, SimplifyList applied directly on ToList makes no sense at all.
--   This functions removes such applications.
simplifyListToList :: ListOpType
                   -> Inference ListOpType
simplifyListToList t = withTypeConstr (unTag t) t $ \(constr, apps) -> do
    InferenceTypes {..} <- getState
    if constr == tcSimplifyList
    then case apps of
      _ : innerType : _ -> fromMaybe t <$> withListOpType innerType Nothing clearToList
      [innerType]       -> fromMaybe t <$> withListOpType innerType Nothing clearToList
      _                 -> pure t
    else pure t
  where
    clearToList s =  withTypeConstr (unTag s) Nothing $ \(constr, _) ->
      if   getOccName constr == mkOccName tcName "ToList"
        || getOccName constr == mkOccName tcName "ToListNat"
      then modified $ Just s
      else pure Nothing




-- | If current type constructor is Numeric.Dimensions.SimplifyList,
--   then remove it. Do it recursively, unless type variable or ToList is faced.
--   We should run it only if we know that there is another SimplifyList outside.
removeAllSimplifyList :: ListOpType -> Inference ListOpType
removeAllSimplifyList t = withTypeConstr (unTag t) t $ \(constr, apps) -> do
    InferenceTypes {..} <- getState
    if constr == tcSimplifyList
    -- SimplifyList is of type List k -> List k,
    -- so I can safely Tag an inner type.
    then case apps of
      _:inner:_ -> removeAllSimplifyList (Tagged inner) >>= modified
      [inner]   -> removeAllSimplifyList (Tagged inner) >>= modified
      []        -> pure t
    -- if this is not simplify list, there can be anything inside,
    -- but I am only interested in List k type operands.
    else if constr == tcToList || constr == tcToListNat
         -- do not touch ToList!
         then pure t
         -- go inside of (presumably) List k constructors
         else Tagged . mkTyConApp constr <$> traverse recurse apps
  where
    -- This function should go recursively into List k types
    recurse x = withListOpType x x (fmap unTag . removeAllSimplifyList)




normaliseOutEvalCons :: InferEq -> Inference InferEq
normaliseOutEvalCons Eliminated = return Eliminated
normaliseOutEvalCons ie@(InferEq lhs rhs) = do
    mlEvalCons <- getTyConAppSafe lhs
    mrEvalCons <- getTyConAppSafe rhs
    InferenceTypes {..} <- getState
    case (mlEvalCons, mrEvalCons) of
      (Just lApps , Just rApps ) -> case InferEq <$> getInner lApps <*> getInner rApps of
                                      Nothing -> pure ie
                                      Just i  -> modified i
      (Nothing, Nothing)         -> pure ie
      (Just lApps , Nothing)     -> case lApps of
          k : t : _ -> modified $ InferEq (mkTyConApp tcToList [k, rhs]) t
          [t]       -> modified $ InferEq (mkTyConApp tcToList [typeKind rhs, rhs]) t
          []        -> pure ie
      (Nothing, Just rApps ) -> case rApps of
          k : t : _ -> modified $ InferEq (mkTyConApp tcToList [k, lhs]) t
          [t]       -> modified $ InferEq (mkTyConApp tcToList [typeKind lhs, lhs]) t
          []        -> pure ie
  where
    getInner :: [Type] -> Maybe Type
    getInner (_ : t : _) = Just t
    getInner [t]         = Just t
    getInner []          = Nothing
    getTyConAppSafe t = case splitTyConApp_maybe t of
        Nothing -> return Nothing
        Just (c, apps) -> do
          InferenceTypes {..} <- getState
          pure $ if c == tcEvalCons || c == tcEvalConsNat
                 then Just apps
                 else Nothing








-- | THIS CODE IS COPIED FROM:
-- https://github.com/clash-lang/ghc-typelits-knownnat/blob/40a5e1bf3309e0d16bab894d7b065a157c03f997/src/GHC/TypeLits/KnownNat/Solver.hs

-- | Classes and instances from "GHC.TypeLits.KnownNat"
type KnownNatDefs = Int -> Maybe Class -- ^ KnownNatN class

-- | KnownNat constraints
type KnConstraint = (Ct    -- The constraint
                    ,Class -- KnownNat class
                    ,Type  -- The argument to KnownNat
                    )

-- normalisePlugin :: TcPlugin
-- normalisePlugin =
--   TcPlugin { tcPluginInit  = pure ()
--            , tcPluginSolve = const solveKnownNat
--            , tcPluginStop  = const (return ())
--            }
--
-- solveKnownNat :: [Ct] -> [Ct] -> [Ct]
--               -> TcPluginM TcPluginResult
-- solveKnownNat _givens _deriveds []      = return (TcPluginOk [] [])
-- solveKnownNat  givens  _deriveds wanteds = do
--   let wanteds'   = filter (isWanted . ctEvidence) wanteds
--       kn_wanteds = mapMaybe toKnConstraint wanteds'
--   case kn_wanteds of
--     [] -> return (TcPluginOk [] [])
--     _  -> do
--       -- Make a lookup table for all the [G]iven constraints
--       given_map <- mapM (fmap toGivenEntry . zonkCt) givens
--       -- Try to solve the wanted KnownNat constraints given the [G]iven
--       -- KnownNat constraints
--       (solved,new) <- (unzip . catMaybes) <$> (mapM (constraintToEvTerm defs given_map) kn_wanteds)
--       return (TcPluginOk solved (concat new))
--
-- -- | Get the KnownNat constraints
-- toKnConstraint :: Ct -> Maybe KnConstraint
-- toKnConstraint ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
--   ClassPred cls [ty]
--     |  className cls == knownNatClassName
--     -> Just (ct,cls,ty)
--   _ -> Nothing
--
-- -- | Create a look-up entry for a [G]iven constraint.
-- toGivenEntry :: Ct -> (Tagged "Nat" Type,EvTerm)
-- toGivenEntry ct = let ct_ev = ctEvidence ct
--                       c_ty  = ctEvPred   ct_ev
--                       ev    = ctEvTerm   ct_ev
--                   in  (CType c_ty,ev)
--
-- -- | Normalise a type to Sum-of-Product type form as defined in the
-- -- `ghc-typelits-natnormalise` package.
-- normaliseSOP :: Type -> Type
-- normaliseSOP = undefined -- reifySOP . normaliseNat
--
--
-- -- | Try to create evidence for a wanted constraint
-- constraintToEvTerm :: KnownNatDefs     -- ^ The "magic" KnownNatN classes
--                    -> [(Tagged "Nat" Type,EvTerm)] -- All the [G]iven constraints
--                    -> KnConstraint
--                    -> TcPluginM (Maybe ((EvTerm,Ct),[Ct]))
-- constraintToEvTerm defs givens (ct,cls,op) = do
--     -- 1. Normalise to SOP normal form
--     let ty = normaliseSOP op
--     -- 2. Determine if we are an offset apart from a [G]iven constraint
--     offsetM <- offset ty
--     evM     <- case offsetM of
--                  -- 3.a If so, we are done
--                  found@Just {} -> return found
--                  -- 3.b If not, we check if the outer type-level operation
--                  -- has a corresponding KnownNat<N> instance.
--                  _             -> go ty
--     return (first (,ct) <$> evM)
--   where
--     -- Determine whether the outer type-level operation has a corresponding
--     -- KnownNat<N> instance, where /N/ corresponds to the arity of the
--     -- type-level operation
--     go :: Type -> TcPluginM (Maybe (EvTerm,[Ct]))
--     go (go_other -> Just ev) = return (Just (ev,[]))
--     go ty@(TyConApp tc args)
--       | let tcNm = tyConName tc
--       , Just m <- nameModule_maybe tcNm
--       , Just knN_cls <- defs (length args)
--       = do let mS    = moduleNameString (moduleName m)
--                tcS   = occNameString (nameOccName tcNm)
--                fn    = mkStrLitTy (fsLit (mS ++ "." ++ tcS))
--                args' = fn:args
--            ienv <- getInstEnvs
--            case lookupUniqueInstEnv ienv knN_cls args' of
--              Right (inst, _) -> do
--                let df_id   = instanceDFunId inst
--                    df      = (knN_cls,df_id)
--                    df_args = fst                  -- [KnownNat x, KnownNat y]
--                            . splitFunTys          -- ([KnownNat x, KnowNat y], DKnownNat2 "+" x y)
--                            . (`piResultTys` args) -- (KnowNat x, KnownNat y) => DKnownNat2 "+" x y
--                            $ idType df_id         -- forall a b . (KnownNat a, KnownNat b) => DKnownNat2 "+" a b
--                (evs,new) <- unzip <$> mapM go_arg df_args
--                return ((,concat new) <$> makeOpDict df cls args' op evs)
--              _ -> return ((,[]) <$> go_other ty)
--     go (LitTy (NumTyLit i))
--       -- Let GHC solve simple Literal constraints
--       | LitTy _ <- op
--       = return Nothing
--       -- This plugin only solves Literal KnownNat's that needed to be normalised
--       -- first
--       | otherwise
--       = return ((,[]) <$> makeLitDict cls op i)
--     go _ = return Nothing
--
--     -- Get EvTerm arguments for type-level operations. If they do not exist
--     -- as [G]iven constraints, then generate new [W]anted constraints
--     go_arg :: PredType -> TcPluginM (EvTerm,[Ct])
--     go_arg ty = case lookup (CType ty) givens of
--       Just ev -> return (ev,[])
--       _ -> do
--         -- Create a new wanted constraint
--         wantedCtEv <- newWanted (ctLoc ct) ty
--         let ev      = ctEvTerm wantedCtEv
--             wanted  = mkNonCanonical wantedCtEv
--         -- Set the source-location of the new wanted constraint to the source
--         -- location of the [W]anted constraint we are currently trying to solve
--         let ct_ls   = ctLocSpan (ctLoc ct)
--             ctl     = ctEvLoc  wantedCtEv
--             wanted' = setCtLoc wanted (setCtLocSpan ctl ct_ls)
--         return (ev,[wanted'])
--
--     -- Fall through case: look up the normalised [W]anted constraint in the list
--     -- of [G]iven constraints.
--     go_other :: Type -> Maybe EvTerm
--     go_other ty =
--       let knClsTc = classTyCon cls
--           kn      = mkTyConApp knClsTc [ty]
--           cast    = if CType ty == CType op
--                        then Just
--                        else makeKnCoercion cls ty op
--       in  cast =<< lookup (CType kn) givens
--
--     -- Find a known constraint for a wanted, so that (modulo normalization)
--     -- the two are a constant offset apart.
--     offset :: Type -> TcPluginM (Maybe (EvTerm,[Ct]))
--     offset want = runMaybeT $ do
--       let unKn ty' = case classifyPredType ty' of
--                        ClassPred cls' [ty'']
--                          | className cls' == knownNatClassName
--                          -> Just ty''
--                        _ -> Nothing
--           -- Get only the [G]iven KnownNat constraints
--           knowns   = mapMaybe (unKn . unCType . fst) givens
--           -- pair up the sum-of-products KnownNat constraints
--           -- with the original Nat operation
--           subWant  = mkTyConApp typeNatSubTyCon . (:[want])
--           exploded = map (normaliseNat . subWant &&& id) knowns
--           -- interesting cases for us are those where
--           -- wanted and given only differ by a constant
--           -- examineDiff (S [P [I n]]) entire = Just (entire,I n)
--           -- examineDiff (S [P [V v]]) entire = Just (entire,V v)
--           examineDiff _ _                  = Nothing
--           interesting = mapMaybe (uncurry examineDiff) exploded
--       -- convert the first suitable evidence
--       ((h,corr):_) <- pure interesting
--       let x = case corr of
--                 -- I 0 -> h
--                 -- I i | i < 0     -> mkTyConApp typeNatAddTyCon [h,mkNumLitTy (negate i)]
--                     -- | otherwise -> mkTyConApp typeNatSubTyCon [h,mkNumLitTy i]
--                 _ -> mkTyConApp typeNatSubTyCon [h,reifySOP (S [P [corr]])]
--       MaybeT (go x)

{- |
Given:

* A "magic" class, and corresponding instance dictionary function, for a
  type-level arithmetic operation
* Two KnownNat dictionaries

makeOpDict instantiates the dictionary function with the KnownNat dictionaries,
and coerces it to a KnownNat dictionary. i.e. for KnownNat2, the "magic"
dictionary for binary functions, the coercion happens in the following steps:

1. KnownNat2 "+" a b           -> SNatKn (KnownNatF2 "+" a b)
2. SNatKn (KnownNatF2 "+" a b) -> Integer
3. Integer                     -> SNat (a + b)
4. SNat (a + b)                -> KnownNat (a + b)

this process is mirrored for the dictionary functions of a higher arity
-}
makeOpDict :: (Class,DFunId) -- ^ "magic" class function and dictionary function id
           -> Class          -- ^ KnownNat class
           -> [Type]         -- ^ Argument types
           -> Type           -- ^ Type of the result
           -> [EvTerm]       -- ^ Evidence arguments
           -> Maybe EvTerm
makeOpDict (opCls,dfid) knCls tyArgs z evArgs
  | Just (_, kn_co_dict) <- tcInstNewTyCon_maybe (classTyCon knCls) [z]
    -- KnownNat n ~ SNat n
  , [ kn_meth ] <- classMethods knCls
  , Just kn_tcRep <- tyConAppTyCon_maybe -- SNat
                      $ funResultTy      -- SNat n
                      $ dropForAlls      -- KnownNat n => SNat n
                      $ idType kn_meth   -- forall n. KnownNat n => SNat n
  , Just (_, kn_co_rep) <- tcInstNewTyCon_maybe kn_tcRep [z]
    -- SNat n ~ Integer
  , Just (_, op_co_dict) <- tcInstNewTyCon_maybe (classTyCon opCls) tyArgs
    -- KnownNatAdd a b ~ SNatKn (a+b)
  , [ op_meth ] <- classMethods opCls
  , Just (op_tcRep,op_args) <- splitTyConApp_maybe        -- (SNatKn, [KnownNatF2 f x y])
                                 $ funResultTy            -- SNatKn (KnownNatF2 f x y)
                                 $ (`piResultTys` tyArgs) -- KnownNatAdd f x y => SNatKn (KnownNatF2 f x y)
                                 $ idType op_meth         -- forall f a b . KnownNat2 f a b => SNatKn (KnownNatF2 f a b)
  , Just (_, op_co_rep) <- tcInstNewTyCon_maybe op_tcRep op_args
    -- SNatKn (a+b) ~ Integer
  , let dfun_inst = EvDFunApp dfid (tail tyArgs) evArgs
        -- KnownNatAdd a b
        op_to_kn  = mkTcTransCo (mkTcTransCo op_co_dict op_co_rep)
                                (mkTcSymCo (mkTcTransCo kn_co_dict kn_co_rep))
        -- KnownNatAdd a b ~ KnownNat (a+b)
        ev_tm     = mkEvCast dfun_inst op_to_kn
  = Just ev_tm
  | otherwise
  = Nothing


{-
Given:
* A KnownNat dictionary evidence over a type x
* a desired type z
makeKnCoercion assembles a coercion from a KnownNat x
dictionary to a KnownNat z dictionary and applies it
to the passed-in evidence.
The coercion happens in the following steps:
1. KnownNat x -> SNat x
2. SNat x     -> Integer
3. Integer    -> SNat z
4. SNat z     -> KnownNat z
-}
makeKnCoercion :: Class          -- ^ KnownNat class
               -> Type           -- ^ Type of the argument
               -> Type           -- ^ Type of the result
               -> EvTerm         -- ^ KnownNat dictionary for the argument
               -> Maybe EvTerm
makeKnCoercion knCls x z xEv
  | Just (_, kn_co_dict_z) <- tcInstNewTyCon_maybe (classTyCon knCls) [z]
    -- KnownNat z ~ SNat z
  , [ kn_meth ] <- classMethods knCls
  , Just kn_tcRep <- tyConAppTyCon_maybe -- SNat
                      $ funResultTy      -- SNat n
                      $ dropForAlls      -- KnownNat n => SNat n
                      $ idType kn_meth   -- forall n. KnownNat n => SNat n
  , Just (_, kn_co_rep_z) <- tcInstNewTyCon_maybe kn_tcRep [z]
    -- SNat z ~ Integer
  , Just (_, kn_co_rep_x) <- tcInstNewTyCon_maybe kn_tcRep [x]
    -- Integer ~ SNat x
  , Just (_, kn_co_dict_x) <- tcInstNewTyCon_maybe (classTyCon knCls) [x]
    -- SNat x ~ KnownNat x
  = Just . mkEvCast xEv $ (kn_co_dict_x `mkTcTransCo` kn_co_rep_x) `mkTcTransCo` mkTcSymCo (kn_co_dict_z `mkTcTransCo` kn_co_rep_z)
  | otherwise = Nothing

-- | THIS CODE IS COPIED FROM:
-- https://github.com/ghc/ghc/blob/8035d1a5dc7290e8d3d61446ee4861e0b460214e/compiler/typecheck/TcInteract.hs#L1973
--
-- makeLitDict adds a coercion that will convert the literal into a dictionary
-- of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit]
-- in TcEvidence.  The coercion happens in 2 steps:
--
--     Integer -> SNat n     -- representation of literal to singleton
--     SNat n  -> KnownNat n -- singleton to dictionary
makeLitDict :: Class -> Type -> Integer -> Maybe EvTerm
makeLitDict clas ty i
  | Just (_, co_dict) <- tcInstNewTyCon_maybe (classTyCon clas) [ty]
    -- co_dict :: KnownNat n ~ SNat n
  , [ meth ]   <- classMethods clas
  , Just tcRep <- tyConAppTyCon_maybe -- SNat
                    $ funResultTy     -- SNat n
                    $ dropForAlls     -- KnownNat n => SNat n
                    $ idType meth     -- forall n. KnownNat n => SNat n
  , Just (_, co_rep) <- tcInstNewTyCon_maybe tcRep [ty]
        -- SNat n ~ Integer
  , let ev_tm = mkEvCast (EvLit (EvNum i)) (mkTcSymCo (mkTcTransCo co_dict co_rep))
  = Just ev_tm
  | otherwise
  = Nothing




--------------------------------------------------------------------------------







instance Show TcPredType where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
instance Show Var where
  show = showSDocUnsafe . ppr
instance Show TcTyThing where
  show = showSDocUnsafe . ppr
instance Show Ct where
  show = showSDocUnsafe . ppr
instance Show Name where
  show = showSDocUnsafe . ppr
instance Show EvBind where
  show = showSDocUnsafe . ppr
instance Show EvTerm where
  show = showSDocUnsafe . ppr
instance Show CtEvidence where
  show = showSDocUnsafe . ppr
instance Show Class where
  show = showSDocUnsafe . ppr
instance Show SkolemInfo where
  show (SigSkol utc ep) = "SigSkol {" ++ show utc ++ "} {" ++ showSDocUnsafe (ppr ep) ++ "} "
  show (PatSynSigSkol n) = "PatSynSigSkol " ++ showSDocUnsafe (ppr n)
  show (ClsSkol n) = "ClsSkol " ++ showSDocUnsafe (ppr n)
  show (DerivSkol n) = "DerivSkol " ++ showSDocUnsafe (ppr n)
  show InstSkol = "InstSkol"
  show (InstSC n) = "InstSC " ++ showSDocUnsafe (ppr n)
  show DataSkol = "DataSkol"
  show FamInstSkol = "FamInstSkol"
  show (PatSkol a _) = "PatSkol " ++ showSDocUnsafe (ppr a) ++ " HsMatchContext"
  show ArrowSkol = "ArrowSkol"
  show (IPSkol n) = "IPSkol " ++ showSDocUnsafe (ppr n)
  show (RuleSkol n) = "RuleSkol " ++ showSDocUnsafe (ppr n)
  show (InferSkol n) = "InferSkol " ++ showSDocUnsafe (ppr n)
  show BracketSkol = "BracketSkol"
  show (UnifyForAllSkol n) = "UnifyForAllSkol " ++ showSDocUnsafe (ppr n)
  show UnkSkol = "UnkSkol"


deriving instance Show UserTypeCtxt
