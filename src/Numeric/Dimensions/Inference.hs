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
{-# LANGUAGE RecordWildCards      #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Numeric.Dimensions.Inference (plugin) where

import           Control.Arrow
import           Control.Monad                      (foldM, unless, void, when,
                                                     (>=>))
import           Data.Coerce
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Semigroup

import           Coercion
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
plugin = defaultPlugin { tcPlugin = const $ Just inferListOpsPlugin }

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
    -- tcPluginIO . putStrLn $ "STARTING WITH GIVENS:" ++ show ((\g -> (onlySkolemOrigin . ctl_origin . ctev_loc $ cc_ev g, g)) <$> givens)
    return (TcPluginOk [] [])
  where
    onlySkolemOrigin :: CtOrigin -> Maybe SkolemInfo
    onlySkolemOrigin (GivenOrigin so) = Just so
    onlySkolemOrigin _                = Nothing
decideListOps (Just it0) givens _deriveds wanteds = incrementPhase it0 >>= \i ->
  if i >= 3
  then return (TcPluginOk [] [])
  else do
    -- tcPluginIO . putStrLn . ("SKOLEMS: " ++ ) . show . catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev <$> givens
    -- tcPluginIO . print $  (varSetElems hsListVars, varSetElems listVars)
    -- tcPluginIO . putStrLn $ "ToLists: " ++ show (eltsUFM toListRelations)
    -- tcPluginIO . putStrLn $ "EvalConses: " ++ show (eltsUFM evalConsRelations)
    -- tcPluginIO . putStrLn $ "Found wanteds: " ++ show wanteds
    -- tcPluginIO . putStrLn $ "Found givens:" ++ show ((\g -> (onlySkolemOrigin . ctl_origin . ctev_loc $ cc_ev g, g)) <$> givens)
    let givensLLL = filter (not . null . snd)
              $ second (filter (\x -> occNameString (getOccName x) == "asLLLL"))
             <$> map (\g -> (g, elems $ findAllCtTyVars g)) givens
    -- unless (null givensLLL) $
      -- tcPluginIO . putStrLn . ("SKOLEMS: " ++ ) . show . catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev . fst <$> givensLLL
    (newGivens, it) <- flip runPrepInference it0 $ do
        toListRelations <- foldMapMon getCtToListRelationMap givens
        evalConsRelations <- foldMapMon getCtEvalConsRelationMap givens
        -- lift . tcPluginIO . putStrLn $ "Found given ToList rels:" ++ show (elems toListRelations)
        -- lift . tcPluginIO . putStrLn $ "Found given EvalCons rels:" ++ show (elems evalConsRelations)
        r <- createListListRelations hsListVars listOpVars toListRelations evalConsRelations
        replaceVarsInGivens givens
        return r
    -- tcPluginIO . putStrLn $ "New givens: " ++ show newGivens
    -- tcPluginIO . putStrLn $ "Found vars: "  ++ show (map (\v -> (v, tyVarKind v)) $ elems allVars)
    -- tcPluginIO . print $ map (\v -> (v, tyVarKind v, getTyVarKindParam v)). varSetElems $ findAllTyVars lhs <> findAllTyVars rhs <> tyVarsSet
    -- EvBindsVar evBindMapRef _ <- getEvBindsTcPluginM
    -- evBinds <- unsafeTcPluginTcM $ bagToList . evBindMapBinds <$> readMutVar evBindMapRef
    -- tcPluginIO . putStrLn $ "Evidence binds: " ++ show (eb_rhs <$> evBinds)

    -- tcPluginIO . putStrLn $ "Computed ToList rels:" ++ show (elems . toListRels $ tyVarRelations it)
    -- tcPluginIO . putStrLn $ "Computed EvalCons rels:" ++ show (elems . evalConsRels $ tyVarRelations it)
    uncurry TcPluginOk . second catMaybes . unzip . catMaybes <$> mapM (check it) wanteds
      -- first (fmap newEvidence newGivens ++) .
  where
    newEvidence :: CtEvidence -> (EvTerm, Ct)
    newEvidence ctEv = (EvId $ ctev_evar ctEv  , CNonCanonical ctEv)

    allVars = foldMap findAllCtTyVars givens
          --  <> foldMap findAllCtTyVars deriveds
           <> foldMap findAllCtTyVars wanteds
    hsListVars = coerce $ Tagged <$> filterMap isHsListKind allVars :: Set HsListVar
    listOpVars = coerce $ Tagged <$> filterMap isListKind allVars :: Set ListOpVar
    onlySkolemOrigin :: CtOrigin -> Maybe SkolemInfo
    onlySkolemOrigin (GivenOrigin so) = Just so
    onlySkolemOrigin _                = Nothing




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



replaceVarsInGivens :: [Ct]
                    -> InferenceT TcPluginM ()
replaceVarsInGivens = mapM_ (replaceG . cc_ev)
  where
    replaceG ctEv = case classifyPredType (ctev_pred ctEv) of
      EqPred NomEq lhs rhs -> do
        InferenceTypes {..} <- getState
        (ln, lUseful)<- subInference $ replaceOne lhs
        (rn, rUseful) <- subInference $ replaceOne rhs
        when (lUseful || rUseful)
          . void . lift $ newGiven (ctev_loc ctEv) (mkPrimEqPredRole Nominal ln rn) (EvId $ ctev_evar ctEv)
      _ -> return ()
    replaceOne t = withTypeConstr t t $ \(constr, apps) -> do
        InferenceTypes {..} <- getState
        if constr == tcToList || constr == tcToListNat
        then pure t
        else mkTyConApp constr <$> traverse replaceAllToLists apps



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
    createdToListEvs2 <- traverse (\(x,y) -> makeToListEvidence (y,x)) $ newListOpVars
    createdEvalConsEvs2 <- traverse (\(x,y) -> makeEvalConsEvidence (y,x)) $ newHsListVars

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


getTyVarKindParam :: TyVar -> Kind
getTyVarKindParam v = case splitTyConApp_maybe (tyVarKind v) of
   Just (_, k:_) -> k
   _             -> tyVarKind v


isListKind :: TyVar -> Bool
isListKind = isGood . splitTyConApp_maybe . tyVarKind
  where
    isGood Nothing            = False
    isGood (Just (constr, _)) = getOccName constr == mkOccName tcName "List"

isHsListKind :: TyVar -> Bool
isHsListKind = isGood . splitTyConApp_maybe . tyVarKind
  where
    isGood Nothing            = False
    isGood (Just (constr, _)) = constr == listTyCon









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



foldMapMon :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapMon f = foldM (\acc x -> mappend acc <$> f x) mempty


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
instance Show SkolemInfo where
  show (SigSkol utc ep) = "SigSkol {" ++ (show utc) ++ "} {"++ (showSDocUnsafe $ ppr ep) ++ "} "
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
