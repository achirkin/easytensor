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

import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Monoid
import           Control.Monad.Fail

import           Outputable (Outputable (..), showSDocUnsafe)
import           Plugins    (Plugin (..), defaultPlugin)
import           TcEvidence (EvTerm (..))
-- import           TysWiredIn (typeNatKind)
import           Coercion
import           OccName
import           TcPluginM
import           TcRnTypes
import           TcType
import           TyCon
import           TyCoRep
import           Type
import           Module


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
  uncurry TcPluginOk . unzip . catMaybes <$> mapM (check' it) wanteds


check' :: InferenceTypes -> Ct -> TcPluginM (Maybe ((EvTerm, Ct), Ct))
check' InferenceTypes {..} ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})
  = case classifyPredType t of
    EqPred NomEq lhs rhs -> do
      let rSListToList :: Type -> Type
          rSListToList myType = fromMaybe myType $
            splitTyConApp_maybe myType >>= \(constr, apps) ->
              if constr == tcSimplifyList
              then case apps of
                    [_, innerType] -> splitTyConApp_maybe innerType >>= \(innerConstr, _) -> Just $
                      if innerConstr == tcSimplifyList || innerConstr == tcToList
                      then rSListToList innerType
                      else mkTyConApp constr (rSListToList <$> apps)
                    _ -> Nothing
              else Just $ mkTyConApp constr (rSListToList <$> apps)
      tcPluginIO . print $ tyConArity tcSimplifyList
      tcPluginIO . print $ tyConArity tcToList
      tcPluginIO . print $ tyConArity tcEvalCons
      tcPluginIO . putStrLn $ "LHS TYPE: " ++ show lhs
      tcPluginIO . putStrLn $ "LHS TYPE 2: " ++ show (rSListToList lhs)
      tcPluginIO . putStrLn $ "RHS TYPE: " ++ show rhs
      tcPluginIO . putStrLn $ "RHS TYPE 2: " ++ show (rSListToList rhs)
      let newLhs = rSListToList lhs
          newRhs = rSListToList rhs
          newEqPred = mkPrimEqPredRole Nominal newLhs newRhs

      newWantedEq <- newWanted myLoc newEqPred
      tcPluginIO . putStrLn $ "WAS: " ++ show t
      tcPluginIO . putStrLn $ "NOW: " ++ show newEqPred
      tcPluginIO . putStrLn $ "LHS EXPANSION: " ++ show (splitTyConApp_maybe lhs)
      tcPluginIO . putStrLn $ "RHS EXPANSION: " ++ show (splitTyConApp_maybe rhs)
      return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal t newEqPred),ct), CNonCanonical newWantedEq)
    _ -> return Nothing
check' _ _ = return Nothing


solve :: Type -> Type -> Inference InferenceResult
solve lhs rhs = undefined


removeAllSimplifyList :: Type -> Inference Type
removeAllSimplifyList t = do
    (constr, apps) <- getTyConApp t
    shouldRemove <- isSimplifyList constr
    case (shouldRemove, apps) of
      (True, [_, inner]) -> withDefault inner $ removeAllSimplifyList inner
      (True, [inner]) -> withDefault inner $ removeAllSimplifyList inner
      (_, []) -> return t
      (False, xs) -> mkTyConApp constr <$> (traverse (\x -> withDefault x $ removeAllSimplifyList x) apps)

-- simplify :: InferenceTypes -> Type -> InferenceResult
-- simplify it@InferenceTypes{..} t
--     | Just (constr, [_, innerType]) <- splitTyConApp_maybe t
--     , constr == tcSimplifyList
--        = case splitTyConApp_maybe innerType of
--            Nothing -> NotApplicable
--            Just (iConstr, apps) -> if iConstr == tcSimplifyList || iConstr == tcToList
--                                    then simplify it innerType
--                                    else mkTyConApp constr (rSListToList <$> apps)


-- | Result of the plugin work
data InferenceResult
  = Simplified !String !Type !Type
    -- ^ New equality
  | Eliminated !String
    -- ^ Best result: equality solved (reduced to trivial)

-- instance Monoid InferenceResult where
--   mempty = NotApplicable
--   mappend NotApplicable x   = x
--   mappend x NotApplicable   = x
--   mappend (Eliminated s) _  = Eliminated s
--   mappend _  (Eliminated s) = Eliminated s
--   mappend _ x               = x


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
  -- ma >> mb = Inference $ \it -> case (runInference ma it, runInference mb it) of
  --                                  (Left ar, Left br) -> Left $ ar <> br
  --                                  (_, x) -> x

instance MonadFail Inference where
  fail = const notApplicable

notApplicable :: Inference a
notApplicable = Inference $ const Nothing

isToList :: TyCon -> Inference Bool
isToList tc = (tc ==) .  tcToList <$> getIT

isEvalCons :: TyCon -> Inference Bool
isEvalCons tc = (tc ==) .  tcSimplifyList <$> getIT

isSimplifyList :: TyCon -> Inference Bool
isSimplifyList tc = (tc ==) .  tcSimplifyList <$> getIT

getTyConApp :: Type -> Inference (TyCon, [Type])
getTyConApp t = Inference . const $ splitTyConApp_maybe t

getIT :: Inference InferenceTypes
getIT = Inference Just

withDefault :: a -> Inference a -> Inference a
withDefault a i =  Inference $ \it -> Just $ fromMaybe a (runInference i it)

data InferenceTypes = InferenceTypes
  { tcToList       :: !TyCon
  , tcEvalCons     :: !TyCon
  , tcSimplifyList :: !TyCon
  , mDimensions    :: !Module
  }

-- | Initialize important types from Numeric.Dimensions module
initInferenceTypes :: TcPluginM (Maybe InferenceTypes)
initInferenceTypes = do
  frModule <- findImportedModule (mkModuleName "Numeric.Dimensions") Nothing
  case frModule of
    Found _ mDimensions -> do
      tcSimplifyList <- lookupOrig mDimensions (mkOccName tcName "SimplifyList") >>= tcLookupTyCon
      tcToList <- lookupOrig mDimensions (mkOccName tcName "ToList") >>= tcLookupTyCon
      tcEvalCons <- lookupOrig mDimensions (mkOccName tcName "EvalCons") >>= tcLookupTyCon
      return $ Just InferenceTypes {..}
    _ -> return Nothing





instance Show TcPredType where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
