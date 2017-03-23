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
module Numeric.Dimensions.Inference (plugin) where

import           Control.Arrow      (first, second, (***))
import           Control.Monad.Fail
import           Data.Maybe         (catMaybes, fromMaybe)
import           Data.Semigroup

import           Outputable         (Outputable (..), showSDocUnsafe)
import           Plugins            (Plugin (..), defaultPlugin)
import           TcEvidence         (EvTerm (..))
-- import           TysWiredIn (typeNatKind)
import           Coercion
import           Module
import           OccName
import           TcPluginM
import           TcRnTypes
import           TcType
import           TyCon
import           TyCoRep
import           Type



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
decideListOps (Just it) _ _ wanteds =
  uncurry TcPluginOk . second catMaybes . unzip . catMaybes <$> mapM (check it) wanteds

-- | Wrapper function for my type inference
check :: InferenceTypes -> Ct -> TcPluginM (Maybe ((EvTerm, Ct), Maybe Ct))
check it ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})
  = case classifyPredType t of
    EqPred NomEq lhs rhs -> case fromMaybe NotApplicable $ runInference (solve lhs rhs) it of
        NotApplicable -> do
          tcPluginIO . putStrLn $ "NOT APPLICABLE: " ++ show lhs ++ " ~ " ++ show rhs
          return Nothing
        Eliminated -> do
          tcPluginIO . putStrLn $ "ELIMINATED!: " ++ show lhs ++ " ~ " ++ show rhs
          return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal lhs rhs),ct), Nothing)
        Reduced newLhs newRhs -> do
          let newEqPred = mkPrimEqPredRole Nominal newLhs newRhs
          newWantedEq <- newWanted myLoc newEqPred
          tcPluginIO . putStrLn $ "WAS: " ++ show t
          tcPluginIO . putStrLn $ "NOW: " ++ show newEqPred
          return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal t newEqPred),ct), Just $ CNonCanonical newWantedEq)
    _ -> return Nothing
check _ _ = return Nothing


-- | Given two types, try to infer their equality or simplify them
solve :: Type -> Type -> Inference InferenceResult
solve lhs rhs = solve' >>= normaliseOutEvalCons' lhs rhs
  where
    solve' = do
      newLhs <- withDefault (StaysSame lhs) $ simplify lhs
      newRhs <- withDefault (StaysSame rhs) $ simplify rhs
      case (newLhs, newRhs) of
        (StaysSame _, StaysSame _) -> return NotApplicable
        (_, _) -> return $ case (getSResult newLhs, getSResult newRhs) of
            (lt, rt) -> if eqType lt rt then Eliminated
                                        else Reduced lt rt



-- | Try various simplification rules on a type family contructor
simplify :: Type -> Inference SimplificationResult
simplify t = do
    (constr, apps) <- getTyConApp t
    indeedSimplifyList <- isSimplifyList constr
    if indeedSimplifyList
    -- If current constructor is SimplifyList, then
    --  I can try either to remove it (if the next one is ToList)
    --            or remove all descendant SimplifyLists
    then do
        afterSimplifyListToList <- simplifyListToList t apps
        withDefault afterSimplifyListToList $ case afterSimplifyListToList of
          -- Remove Current SimplifyList if it is applied directly on ToList
          Simplified innerType -> withDefault afterSimplifyListToList $ simplify innerType
          -- Only if the next one is not ToList I can lookup for all descendant SimplifyList
          StaysSame _          -> constructBack t constr
                                    <$> traverse (\x -> withDefault (StaysSame x)
                                     $ removeAllSimplifyList x) apps

    -- go recursively into child types
    else constructBack t constr
            <$> traverse (\x -> withDefault (StaysSame x) $ simplify x) apps



-- | Transform (SimplifyList (ToList xs)) -> (ToList xs)
--   Guaranteed to return not Nothing
simplifyListToList :: Type -- ^ Assumed to be SimplifyList type
                   -> [Type] -- ^ SimplifyList constructor arguments
                   -> Inference SimplificationResult
simplifyListToList t apps = case apps of
    _ : innerType : _ -> withDefault (StaysSame t) $ checkType innerType
    [innerType]       -> withDefault (StaysSame t) $ checkType innerType
    _                 -> return $ StaysSame t
  where
    checkType :: Type -> Inference SimplificationResult
    checkType innerType = do
        (iConstr, _) <- getTyConApp innerType
        True <- isToList iConstr
        return $ Simplified innerType



-- | If current type constructor is Numeric.Dimensions.SimplifyList,
--   then look into all descendants and remove SimplifyList occurrences
removeAllSimplifyList :: Type -> Inference SimplificationResult
removeAllSimplifyList t = do
    (constr, apps) <- getTyConApp t
    shouldRemove <- isSimplifyList constr
    case (shouldRemove, apps) of
      (True, _ :inner:_) -> withDefault (Simplified inner) $ removeAllSimplifyList inner
      (True, [inner]) -> withDefault (Simplified inner) $ removeAllSimplifyList inner
      (_, []) -> return (StaysSame t)
      (False, xs) -> constructBack t constr
                  <$> traverse (\x -> withDefault (StaysSame x) $ removeAllSimplifyList x) xs






normaliseOutEvalCons' :: Type -> Type -> InferenceResult -> Inference InferenceResult
normaliseOutEvalCons' _ _ Eliminated = return Eliminated
normaliseOutEvalCons' t1 t2 NotApplicable = withDefault NotApplicable (normaliseOutEvalCons t1 t2)
normaliseOutEvalCons' _ _ (Reduced t1 t2) = withDefault (Reduced t1 t2) (normaliseOutEvalCons t1 t2)



normaliseOutEvalCons :: Type -> Type -> Inference InferenceResult
normaliseOutEvalCons lhs rhs = do
    mlEvalCons <- getTyConAppSafe lhs
    mrEvalCons <- getTyConAppSafe rhs
    InferenceTypes {..} <- getIT
    case (mlEvalCons, mrEvalCons) of
      (Just lApps , Just rApps ) -> Reduced <$> getInner lApps <*> getInner rApps
      (Nothing, Nothing)                   -> return NotApplicable
      (Just lApps , Nothing) -> case lApps of
          k : t : _ -> return $ Reduced (mkTyConApp tcToList [k, rhs]) t
          [t]       -> return $ Reduced (mkTyConApp tcToList [typeKind rhs, rhs]) t
          []        -> return NotApplicable
      (Nothing, Just rApps ) -> case rApps of
          k : t : _ -> return $ Reduced (mkTyConApp tcToList [k, lhs]) t
          [t]       -> return $ Reduced (mkTyConApp tcToList [typeKind lhs, lhs]) t
          []        -> return NotApplicable
  where
    getInner :: [Type] -> Inference Type
    getInner (_ : t : _) = return t
    getInner [t]         = return t
    getInner []          = notApplicable
    getTyConAppSafe t = case splitTyConApp_maybe t of
        Nothing -> return Nothing
        Just (c, apps) -> isEvalCons c >>= \indeed -> if indeed then return (Just apps)
                                                                else return Nothing



-- removeEvalConsToList :: Type -> Inference InferenceResult
-- removeEvalConsToList t = withDefault (StaysSame lhs)



--------------------------------------------------------------------------------






-- | Given a deconstructed type (Constructor + list of types to apply to)
--   construct it back to a type, preserving information whether it was simplified or not.
constructBack :: Type -- ^ Original Type
              -> TyCon -- ^ Constructor to use
              -> [SimplificationResult] -- ^ inner types to use
              -> SimplificationResult
constructBack t constr apps = constructProperly constr $ anyIsModified apps
  where
     anyIsModified :: [SimplificationResult] -> ([Type], Bool)
     anyIsModified (StaysSame x : xs) = first (x:) $ anyIsModified xs
     anyIsModified (Simplified x: xs) = (x:) *** const True $ anyIsModified xs
     anyIsModified []                 = ([], False)
     constructProperly :: TyCon -> ([Type], Bool) -> SimplificationResult
     constructProperly _ (_, False)      = StaysSame t
     constructProperly innerConstr (xs, True) = Simplified $ mkTyConApp innerConstr xs






-- | Result of the plugin work
data InferenceResult
  = NotApplicable
  | Reduced !Type !Type
    -- ^ New equality
  | Eliminated
    -- ^ Best result: equality solved (reduced to trivial)
  deriving Show

instance Monoid InferenceResult where
  mempty = NotApplicable
  mappend NotApplicable x = x
  mappend x NotApplicable = x
  mappend Eliminated _    = Eliminated
  mappend _  Eliminated   = Eliminated
  mappend _ x             = x

instance Semigroup InferenceResult where
  (<>) = mappend

data SimplificationResult
  = StaysSame  { getSResult :: !Type }
  | Simplified { getSResult :: !Type }
  deriving Show

instance Semigroup SimplificationResult where
  StaysSame _ <> x = x
  x <> StaysSame _ = x
  Simplified _ <> Simplified x = Simplified x


newtype Inference a = Inference
  { runInference :: InferenceTypes -> Maybe a }

instance Functor Inference where
  fmap f i = Inference $ fmap f . runInference i

instance Applicative Inference where
  pure = Inference . const . Just
  mf <*> mx = Inference $ \it -> runInference mf it <*> runInference mx it

instance Monad Inference where
  return = pure
  mx >>= fm = Inference $ \it -> runInference mx it
                            >>= (\x -> runInference (fm x) it)
  fail = const notApplicable
  -- ma >> mb = Inference $ \it -> case (runInference ma it, runInference mb it) of
  --                                  (Left ar, Left br) -> Left $ ar <> br
  --                                  (_, x) -> x

instance MonadFail Inference where
  fail = const notApplicable


notApplicable :: Inference a
notApplicable = Inference $ const Nothing

isToList :: TyCon -> Inference Bool
isToList tc = (\InferenceTypes {..} -> tc == tcToList || tc == tcToListNat) <$> getIT

isEvalCons :: TyCon -> Inference Bool
isEvalCons tc = (\InferenceTypes {..} -> tc == tcEvalCons || tc == tcEvalConsNat) <$> getIT

isSimplifyList :: TyCon -> Inference Bool
isSimplifyList tc = (tc ==) .  tcSimplifyList <$> getIT

getTyConApp :: Type -> Inference (TyCon, [Type])
getTyConApp t = Inference . const $ splitTyConApp_maybe t

getIT :: Inference InferenceTypes
getIT = Inference Just

withDefault :: Semigroup a => a -> Inference a -> Inference a
withDefault a i =  Inference $ \it -> Just $ a <> fromMaybe a (runInference i it)

data InferenceTypes = InferenceTypes
  { tcToList       :: !TyCon
  , tcToListNat    :: !TyCon
  , tcEvalCons     :: !TyCon
  , tcEvalConsNat  :: !TyCon
  , tcSimplifyList :: !TyCon
  , mDimensions    :: !Module
  }

-- | Initialize important types from Numeric.Dimensions module
initInferenceTypes :: TcPluginM (Maybe InferenceTypes)
initInferenceTypes = do
  frModule <- findImportedModule (mkModuleName "Numeric.Dimensions.List") Nothing
  case frModule of
    Found _ mDimensions -> do
      tcSimplifyList <- lookupOrig mDimensions (mkOccName tcName "SimplifyList") >>= tcLookupTyCon
      tcToList <- lookupOrig mDimensions (mkOccName tcName "ToList") >>= tcLookupTyCon
      tcToListNat <- lookupOrig mDimensions (mkOccName tcName "ToListNat") >>= tcLookupTyCon
      tcEvalCons <- lookupOrig mDimensions (mkOccName tcName "EvalCons") >>= tcLookupTyCon
      tcEvalConsNat <- lookupOrig mDimensions (mkOccName tcName "EvalConsNat") >>= tcLookupTyCon
      return $ Just InferenceTypes {..}
    _ -> return Nothing





instance Show TcPredType where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
