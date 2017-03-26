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

{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Numeric.Dimensions.Inference (plugin) where

import           Control.Arrow      (first, second, (***))
import           Control.Monad.Fail
import           Control.Monad (unless)
import           Data.Maybe         (catMaybes, fromMaybe)
import           Data.Semigroup

import           Outputable         (Outputable (..), showSDocUnsafe)
import           Plugins            (Plugin (..), defaultPlugin)
import           TcEvidence         (EvTerm (..))
-- import           TysWiredIn (typeNatKind)
import           Coercion
import           Module
import           Name
-- import           NameEnv
-- import           OccName
import           TcPluginM
import           TcRnMonad
-- import           TcRnTypes
import           TcType
import           TyCon
import           TyCoRep
import           Type
import           UniqFM
import           VarSet


import Numeric.Dimensions.Inference.Types
-- import           Var
-- import           FastString

-- import           Debug.Trace

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
decideListOps (Just it) givens _deriveds wanteds = do
    -- tcPluginIO . putStrLn . ("SKOLEMS: " ++ ) . show . catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev <$> givens
    -- tcPluginIO . print $  (varSetElems hsListVars, varSetElems listVars)
    -- tcPluginIO . putStrLn $ "ToLists: " ++ show (eltsUFM toListRelations)
    -- tcPluginIO . putStrLn $ "EvalConses: " ++ show (eltsUFM evalConsRelations)
    let givensLLL = filter (not . null . snd)
              $ second (filter (\x -> occNameString (getOccName x) == "asLLLL"))
             <$> map (\g -> (g, eltsUFM $ findAllCtTyVars g)) givens
    unless (null givensLLL) $
      tcPluginIO . putStrLn . ("SKOLEMS: " ++ ) . show . catMaybes $ onlySkolemOrigin . ctl_origin . ctev_loc . cc_ev . fst <$> givensLLL
    (_newGivens, llRels) <- createListListRelations it hsListVars toListRelations evalConsRelations
    uncurry TcPluginOk . second catMaybes . unzip . catMaybes <$> mapM (check it llRels) wanteds
  where
    allVars = foldMap findAllCtTyVars givens
          --  <> foldMap findAllCtTyVars deriveds
           <> foldMap findAllCtTyVars wanteds
    hsListVars = filterVarSet isHsListKind allVars
    listVars = filterVarSet isListKind allVars
    -- ( List k var, [k] var)
    toListRelations = foldMap (getCtToListRelationMap it) givens
                  --  <> foldMap (getCtToListRelationMap it) deriveds
    -- ( List k var, [k] var)
    evalConsRelations = foldMap (getCtEvalConsRelationMap it) givens
                    --  <> foldMap (getCtEvalConsRelationMap it) deriveds
    onlySkolemOrigin :: CtOrigin -> Maybe SkolemInfo
    onlySkolemOrigin (GivenOrigin so) = Just so
    onlySkolemOrigin _                = Nothing




-- | Wrapper function for my type inference
check :: InferenceTypes -> ListListRelations -> Ct -> TcPluginM (Maybe ((EvTerm, Ct), Maybe Ct))
check it llRels ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})
  = case classifyPredType t of
    EqPred NomEq lhs rhs -> case fromMaybe NotApplicable $ runInference (solve llRels lhs rhs) it of
        (_ , False) -> do
          tcPluginIO . putStrLn $ "NOT APPLICABLE: " ++ show lhs ++ " ~ " ++ show rhs
          return Nothing
        (Nothing , True) -> do
          tcPluginIO . putStrLn $ "ELIMINATED!: " ++ show lhs ++ " ~ " ++ show rhs
          return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal lhs rhs),ct), Nothing)
        (Just newLhs newRhs, True) -> do
          let newEqPred = mkPrimEqPredRole Nominal newLhs newRhs
          newWantedEq <- newWanted myLoc newEqPred
          tcLclEnv <- snd <$> TcPluginM.getEnvs
          let tyVarsRef = tcl_tyvars tcLclEnv
          _tyVarsSet <- unsafeTcPluginTcM $ readMutVar tyVarsRef
          -- tcPluginIO . print $ map (\v -> (v, tyVarKind v, getTyVarKindParam v)). varSetElems $ findAllTyVars lhs <> findAllTyVars rhs <> tyVarsSet
          tcPluginIO . putStrLn $ "WAS: " ++ show t
          tcPluginIO . putStrLn $ "NOW: " ++ show newEqPred
          return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal t newEqPred),ct), Just $ CNonCanonical newWantedEq)
    _ -> return Nothing
check _ _ _ = return Nothing





-- | Given two types, try to infer their equality or simplify them
solve :: InferEq -> Inference InferEq
solve = solve' >=> normaliseOutEvalCons' >=> solve'
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



simplify :: Type -> Inference Type
simplify = simplify0 >=> raplaceAllToLists

-- | Try various simplification rules on a type family contructor
simplify0 :: Type -> Inference Type
simplify0 t = withTypeConstr t f
  where
    f (constr, apps) = do
      InferenceTypes {..} <- getState
      if constr == tcSimplifyList
      then do
          afterSimplifyListToList <- simplifyListToList t apps
          withDefault afterSimplifyListToList $ case afterSimplifyListToList of
            -- Remove Current SimplifyList if it is applied directly on ToList
            innerType -> withDefault afterSimplifyListToList $ simplify0 innerType
            -- Only if the next one is not ToList I can lookup for all descendant SimplifyList
            _          -> constructBack t constr
                                      <$> traverse (\x -> withDefault (StaysSame x)
                                                         $ removeAllSimplifyList x) apps
       else mkTyConApp innerConstr <$> traverse simplify0 apps



-- | Transform (SimplifyList (ToList xs)) -> (ToList xs)
--   Guaranteed to return not Nothing
simplifyListToList :: Type -- ^ Assumed to be SimplifyList type
                   -> [Type] -- ^ SimplifyList constructor arguments
                   -> Inference Type
simplifyListToList t apps = case apps of
    _ : innerType : _ -> withDefault (StaysSame t) $ checkType innerType
    [innerType]       -> withDefault (StaysSame t) $ checkType innerType
    _                 -> return $ StaysSame t
  where
    checkType :: Type -> Inference Type
    checkType innerType = do
        (iConstr, _) <- getTyConApp innerType
        True <- isToList iConstr
        return $ Simplified innerType


findAllTyVars :: Type -> VarSet
findAllTyVars t = case (splitTyConApp_maybe t, getTyVar_maybe t) of
    (Nothing, Nothing)        -> emptyVarSet
    (Nothing, Just v)         -> unitVarSet v
    (Just (_, apps), Nothing) -> foldMap findAllTyVars apps
    (Just (_, apps), Just v)  -> extendVarSet (foldMap findAllTyVars apps) v



findAllPredTreeTyVars :: PredTree -> VarSet
findAllPredTreeTyVars (ClassPred _ ts) = foldMap findAllTyVars ts
findAllPredTreeTyVars (EqPred _ lhs rhs) = findAllTyVars lhs <> findAllTyVars rhs
findAllPredTreeTyVars (IrredPred _) = emptyVarSet

findAllCtTyVars :: Ct -> VarSet
findAllCtTyVars = findAllPredTreeTyVars . classifyPredType . ctev_pred . cc_ev


-- | Checks if this type is "ToList ds"; if so, gives type variable and "ToList ds"
tcToListMaybe :: Type -> Inference (Maybe (TyVar, Type))
tcToListMaybe InferenceTypes {..} t = case splitTyConApp_maybe t of
    Nothing -> Nothing
    Just (constr, apps) ->
      if constr == tcToListNat || constr == tcToList
      then case apps of
        []    -> Nothing
        [x]   -> getToList x
        _:x:_ -> getToList x
      else Nothing
  where
    getToList :: Type -> Maybe (TyVar, Type)
    getToList x = case getTyVar_maybe x of
      Nothing -> Nothing
      Just xv -> Just (xv, mkTyConApp tcToList [getTyVarKindParam xv, x])

-- | Checks if this type is "EvalCons dsL"; if so, "EvalCons dsL"
tcEvalConsMaybe :: Type -> Inference (Maybe (TyVar, Type))
tcEvalConsMaybe t = do
  InferenceTypes {..} <- getState
  return $ case splitTyConApp_maybe t of
    Nothing -> Nothing
    Just (constr, apps) ->
      if constr == tcEvalConsNat || constr == tcEvalCons
      then case apps of
        []    -> Nothing
        [x]   -> getEvalCons x
        _:x:_ -> getEvalCons x
      else Nothing
  where
    getEvalCons :: Type -> Maybe (TyVar, Type)
    getEvalCons x = case getTyVar_maybe x of
      Nothing -> Nothing
      Just xv -> Just (xv, mkTyConApp tcEvalCons [getTyVarKindParam xv, x])

findAllTyToLists :: InferenceTypes -> Type -> UniqFM (TyVar, Type)
findAllTyToLists it t = case tcToListMaybe it t of
  Just (xv, xt) -> unitUFM xv (xv, xt)
  Nothing -> case splitTyConApp_maybe t of
    Nothing        -> emptyUFM
    Just (_, apps) -> foldMap (findAllTyToLists it) apps

raplaceAllToLists :: ListListRelations -> Type -> Inference SimplificationResult
raplaceAllToLists llr t = do
  it <- getIT
  case tcToListMaybe it t of
    Just (xv, _xt) -> case lookupUFM (toListRels llr) xv of
      Nothing -> return $ StaysSame t
      Just yv -> return . Simplified $ mkTyVarTy yv
    Nothing -> case splitTyConApp_maybe t of
      Nothing        -> return $ StaysSame t
      Just (constr, apps) -> constructBack t constr
                                <$> traverse (\x -> withDefault (StaysSame x)
                                                  $ raplaceAllToLists llr x) apps


findAllTyEvalCons :: InferenceTypes -> Type -> UniqFM (TyVar, Type)
findAllTyEvalCons it t = case tcEvalConsMaybe it t of
  Just (xv, xt) -> unitUFM xv (xv, xt)
  Nothing -> case splitTyConApp_maybe t of
    Nothing        -> emptyUFM
    Just (_, apps) -> foldMap (findAllTyEvalCons it) apps

getEvalConsRelationMaybe :: InferenceTypes -> PredTree -> Maybe (TyVar, TyVar)
getEvalConsRelationMaybe it (EqPred _ lhs rhs)
  = case ( getTyVar_maybe lhs, tcEvalConsMaybe it lhs
         , getTyVar_maybe rhs, tcEvalConsMaybe it rhs) of
      (Nothing, _, Nothing, _)    -> Nothing
      (_, Nothing, _, Nothing)    -> Nothing
      (Just v , _, _, Just (w,_)) -> Just (v,w)
      (_, Just (w,_), Just v , _) -> Just (v,w)
      (_,_,_,_)                   -> Nothing
getEvalConsRelationMaybe _ _ = Nothing

getToListRelationMaybe :: InferenceTypes -> PredTree -> Maybe (TyVar, TyVar)
getToListRelationMaybe it (EqPred _ lhs rhs)
  = case ( getTyVar_maybe lhs, tcToListMaybe it lhs
         , getTyVar_maybe rhs, tcToListMaybe it rhs) of
      (Nothing, _, Nothing, _)    -> Nothing
      (_, Nothing, _, Nothing)    -> Nothing
      (Just v , _, _, Just (w,_)) -> Just (v,w)
      (_, Just (w,_), Just v , _) -> Just (v,w)
      (_,_,_,_)                   -> Nothing
getToListRelationMaybe _ _ = Nothing


-- List k var key ([k] var, EvalCons List k var)
getCtEvalConsRelationMap :: InferenceTypes -> Ct -> UniqFM (TyVar, TyVar)
getCtEvalConsRelationMap it = fm . getEvalConsRelationMaybe it . classifyPredType . ctev_pred . cc_ev
  where
    fm Nothing      = emptyUFM
    fm (Just (v,w)) = unitUFM w (v,w)

-- [k] var key ( List k var, ToList [k] var)
getCtToListRelationMap :: InferenceTypes -> Ct -> UniqFM (TyVar, TyVar)
getCtToListRelationMap it = fm . getToListRelationMaybe it . classifyPredType . ctev_pred . cc_ev
  where
    fm Nothing      = emptyUFM
    fm (Just (v,w)) = unitUFM w (v,w)


-- | Assume equations:
--     xsL ~ ToList xs
--     xs  ~ EvalCons xs
data ListListRelations = ListListRelations
  { toListRels   :: !(UniqFM TyVar)
    -- ^  Map xs -> xsL
  , evalConsRels :: !(UniqFM TyVar)
    -- ^ Map xsL -> xs
  }

-- instance Monoid ListListRelations


createListListRelations :: InferenceTypes
                        -> VarSet -- ^ Bare haskell list variables
                           -- [k] var key ( List k var, ToList [k] var)
                        -> UniqFM (TyVar, TyVar) -- ^ ToLists
                           -- List k var key ([k] var, EvalCons List k var)
                        -> UniqFM (TyVar, TyVar) -- ^ EvalConses
                        -> TcPluginM ([CtEvidence], ListListRelations)
createListListRelations InferenceTypes {..} hsListVars toListRels evalConsRels = do
    newListVars <- traverse (\v -> (,) v <$> makeListVar v)  $ varSetElems notCoveredHsListVars
    -- unless (null newListVars) $
    --   tcPluginIO . putStrLn $ "newListVars: " ++ show newListVars
    let newToList'   = listToUFM $ map (\(a,b) -> (a,(b,a))) newListVars
        newEvalCons' = listToUFM $ map (\(a,b) -> (b,(a,b))) newListVars
    newToListEvs <- traverse makeToListEvidence . eltsUFM $ newToList' <> missingToLists
    newEvalConsEvs <- traverse makeEvalConsEvidence . eltsUFM $ newEvalCons' <> missingEvalConses
    return ( newToListEvs ++ newEvalConsEvs
           , ListListRelations (mapUFM fst $ newToList' <> allExistingToLists)
                               (mapUFM fst $ newEvalCons' <> allExistingEvalConses)
           )
  where
    makeListVar :: TyVar -- ^ [k]
                -> TcPluginM TyVar -- ^ List k
    makeListVar v = newFlexiTyVar (mkTyConApp tcList [getTyVarKindParam v])

    allHsListVars :: VarSet
    allHsListVars = hsListVars
                 <> mkVarSet (snd <$> eltsUFM toListRels)
                 <> mkVarSet (fst <$> eltsUFM evalConsRels)
    allListVars :: VarSet
    allListVars = mkVarSet (fst <$> eltsUFM toListRels)
             <> mkVarSet (snd <$> eltsUFM evalConsRels)
    allExistingToLists :: UniqFM (TyVar, TyVar) -- ^ all List k <-> [k] matchings
    allExistingToLists = toListRels <> listToUFM (map (\(a,b) -> (a,(b,a))) (eltsUFM evalConsRels))
    allExistingEvalConses :: UniqFM (TyVar, TyVar) -- ^ all [k] <-> List k matchings
    allExistingEvalConses = evalConsRels <> listToUFM (map (\(a,b) -> (a,(b,a))) (eltsUFM toListRels))
    missingToLists    = allExistingToLists `minusUFM` toListRels
    missingEvalConses = allExistingEvalConses `minusUFM`  evalConsRels
    notCoveredHsListVars = allHsListVars `minusUFM` allExistingToLists
    -- makeNewListRels :: TyVar -- ^ [] variable
    --                 -> [((TyVar,))] --^
    makeToListEvidence :: (TyVar, TyVar) -> TcPluginM CtEvidence
    makeToListEvidence (lhs, rhs)
      = let lt = mkTyVarTy lhs
            rt = mkTyConApp tcToList [getTyVarKindParam rhs, mkTyVarTy rhs]
            eqPredType = mkPrimEqPredRole Nominal lt rt
            evterm = EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference-newToList") Nominal lt rt)
        in do
          loc <- mkGivenLoc (TcLevel 1) InstSkol . snd <$> TcPluginM.getEnvs
          newGiven loc eqPredType evterm

    makeEvalConsEvidence :: (TyVar, TyVar) -> TcPluginM CtEvidence
    makeEvalConsEvidence (lhs, rhs)
      = let lt = mkTyVarTy lhs
            rt = mkTyConApp tcEvalCons [getTyVarKindParam rhs, mkTyVarTy rhs]
            eqPredType = mkPrimEqPredRole Nominal lt rt
            evterm = EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference-newEvalCons") Nominal lt rt)
        in do
          loc <- mkGivenLoc (TcLevel 1) InstSkol . snd <$> TcPluginM.getEnvs
             -- (InferSkol [(tyVarName lhs, lt)])
          newGiven loc eqPredType evterm


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
    isGood (Just (constr, _)) = getOccName constr == mkOccName tcName "[]"



-- | If current type constructor is Numeric.Dimensions.SimplifyList,
--   then look into all descendants and remove SimplifyList occurrences
removeAllSimplifyList :: Type -> Inference Type
removeAllSimplifyList t = do
    (constr, apps) <- getTyConApp t
    shouldRemove <- isSimplifyList constr
    case (shouldRemove, apps) of
      (True, _ :inner:_) -> withDefault (Simplified inner) $ removeAllSimplifyList inner
      (True, [inner]) -> withDefault (Simplified inner) $ removeAllSimplifyList inner
      (_, []) -> return (StaysSame t)
      (False, xs) -> constructBack t constr
                  <$> traverse (\x -> withDefault (StaysSame x) $ removeAllSimplifyList x) xs





normaliseOutEvalCons :: InferEq -> Inference InferEq
normaliseOutEvalCons Eliminated = return Eliminated
normaliseOutEvalCons ie@(InferEq lhs rhs) = do
    mlEvalCons <- getTyConAppSafe lhs
    mrEvalCons <- getTyConAppSafe rhs
    InferenceTypes {..} <- getState
    case (mlEvalCons, mrEvalCons) of
      (Just lApps , Just rApps ) -> Reduced <$> getInner lApps <*> getInner rApps
      (Nothing, Nothing)                   -> return NotApplicable
      (Just lApps , Nothing) -> case lApps of
          k : t : _ -> modified $ InferEq (mkTyConApp tcToList [k, rhs]) t
          [t]       -> modified $ InferEq (mkTyConApp tcToList [typeKind rhs, rhs]) t
          []        -> return ie
      (Nothing, Just rApps ) -> case rApps of
          k : t : _ -> modified $ InferEq (mkTyConApp tcToList [k, lhs]) t
          [t]       -> modified $ InferEq (mkTyConApp tcToList [typeKind lhs, lhs]) t
          []        -> return ie
  where
    getInner :: [Type] -> Inference Type
    getInner (_ : t : _) = return t
    getInner [t]         = return t
    getInner []          = notApplicable
    getTyConAppSafe t = case splitTyConApp_maybe t of
        Nothing -> return Nothing
        Just (c, apps) -> isEvalCons c >>= \indeed -> if indeed then return (Just apps)
                                                                else return Nothing




--------------------------------------------------------------------------------






-- | Given a deconstructed type (Constructor + list of types to apply to)
--   construct it back to a type, preserving information whether it was simplified or not.
-- constructBack :: Type -- ^ Original Type
--               -> TyCon -- ^ Constructor to use
--               -> [SimplificationResult] -- ^ inner types to use
--               -> SimplificationResult
-- constructBack t constr apps = constructProperly constr $ anyIsModified apps
--   where
--      anyIsModified :: [SimplificationResult] -> ([Type], Bool)
--      anyIsModified (StaysSame x : xs) = first (x:) $ anyIsModified xs
--      anyIsModified (Simplified x: xs) = (x:) *** const True $ anyIsModified xs
--      anyIsModified []                 = ([], False)
--      constructProperly :: TyCon -> ([Type], Bool) -> SimplificationResult
--      constructProperly _ (_, False)      = StaysSame t
--      constructProperly innerConstr (xs, True) = Simplified $ mkTyConApp innerConstr xs




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
instance Show SkolemInfo where
  show (SigSkol _ _) = "SigSkol UserTypeCtxt ExpType"
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
