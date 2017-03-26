-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Inference.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE DeriveTraversable      #-}
module Numeric.Dimensions.Inference.Types
  ( Tagged (..), Map (..)
  , HsList, ListOp
  , TyVarRelations (..)
  , InferenceTypes (..), initInferenceTypes
  , InferenceT (), Inference, InferencePlugin, runInference
  , liftToPlugin
  , getState, setState
  , isModified, whenModified, whenNotModified, modified
  , InferEq (..)
  , withTypeConstr, withTypeVar
  ) where

import           GHC.TypeLits
import           Control.Arrow
import           Control.Monad ((>=>))
import           Data.Semigroup
import           Data.Functor.Identity

import           Outputable         (Outputable ())
import           Module
import           Name
import           TcPluginM
import           TcType
import           TyCon
import           Var
import           UniqFM
import           Unique


-- | Tag GHC types with my own annotations
newtype Tagged (tag :: Symbol) a = Tagged { unTag :: a}
  deriving ( Eq, Ord, Show, Num, Fractional, Floating, Real, RealFrac, RealFloat
           , Functor, Foldable, Traversable, Semigroup, Monoid, Outputable)

-- | Use Map to keep types and their relations
newtype Uniquable a => Map a b = Map { _getUFM :: UniqFM b }
  deriving (Functor, Foldable, Traversable, Eq, Semigroup, Monoid, Outputable)

-- | Compare two types for equality
data InferEq = InferEq !Type !Type | Eliminated

type HsList = Tagged "[k]" Type
type ListOp = Tagged "List k" Type

-- | Assume equations:
--     xsL ~ ToList xs
--     xs  ~ EvalCons xs
--
--   This pair of mappings is used to substitute (ToList xs) type families with
--   type variables. This way, SimplifyList family can do its job better.
data TyVarRelations = TyVarRelations
  { toListRels   :: !(Map HsList ListOp)
    -- ^  Map xs -> xsL
  , evalConsRels :: !(Map ListOp HsList)
    -- ^ Map xsL -> xs
  }


instance Semigroup TyVarRelations where
  TyVarRelations a b <> TyVarRelations x y = TyVarRelations (a <> x) (b <> y)

instance Monoid TyVarRelations where
  mempty = TyVarRelations mempty mempty
  mappend = (<>)




data InferenceTypes = InferenceTypes
  { tcToList       :: !TyCon
  , tcList         :: !TyCon
  , tcToListNat    :: !TyCon
  , tcEvalCons     :: !TyCon
  , tcEvalConsNat  :: !TyCon
  , tcSimplifyList :: !TyCon
  , mDimensions    :: !Module
  , tyVarRelations :: !TyVarRelations
  }

instance Semigroup InferenceTypes where
  itA <> itB = itA { tyVarRelations = tyVarRelations itA <> tyVarRelations itB }


-- | Initialize important types from Numeric.Dimensions module
initInferenceTypes :: TcPluginM (Maybe InferenceTypes)
initInferenceTypes = do
  frModule <- findImportedModule (mkModuleName "Numeric.Dimensions.List") Nothing
  case frModule of
    Found _ mDimensions -> do
      tcSimplifyList <- lookupOrig mDimensions (mkOccName tcName "SimplifyList") >>= tcLookupTyCon
      tcList <- lookupOrig mDimensions (mkOccName tcName "List") >>= tcLookupTyCon
      tcToList <- lookupOrig mDimensions (mkOccName tcName "ToList") >>= tcLookupTyCon
      tcToListNat <- lookupOrig mDimensions (mkOccName tcName "ToListNat") >>= tcLookupTyCon
      tcEvalCons <- lookupOrig mDimensions (mkOccName tcName "EvalCons") >>= tcLookupTyCon
      tcEvalConsNat <- lookupOrig mDimensions (mkOccName tcName "EvalConsNat") >>= tcLookupTyCon
      let tyVarRelations = mempty
      return $ Just InferenceTypes {..}
    _ -> return Nothing




-- | Updates a state in InferenceTypes, and keeps additional Bool value
--   indicating whether a work has been done or not;
--   if it stays False then we could not infer any useful info about a type.
newtype InferenceT m a = InferenceT
   { _runInference :: (InferenceTypes, Bool) -> m (a, (InferenceTypes, Bool)) }

runInference :: Functor m => InferenceT m a -> InferenceTypes -> m (a, Bool)
runInference i it = second snd <$> _runInference i (it, False)

type Inference = InferenceT Identity
type InferencePlugin = InferenceT TcPluginM


instance Functor m => Functor (InferenceT m) where
  fmap f x = InferenceT $ fmap (first f) . _runInference x

instance Applicative m => Applicative (InferenceT m) where
  pure x = InferenceT $ pure . (,) x
  af <*> ax = InferenceT $ \s -> g <$> _runInference af s <*> _runInference ax s
    where
      g (f, (itA, bA)) (x, (itB, bB)) = (f x, (itA <> itB, bA || bB))

instance Monad m => Monad (InferenceT m) where
  return = pure
  mx >>= f = InferenceT $ _runInference mx >=>
           \(x, s) -> g s <$> _runInference (f x) s
    where
      g (itA, bA) (y, (itB, bB)) = (y, (itA <> itB, bA || bB))

-- | Get from pure inference monad to TcPluginM
liftToPlugin :: Inference a -> InferencePlugin a
liftToPlugin x = InferenceT $ pure . runIdentity . _runInference x

getState :: Applicative m => InferenceT m InferenceTypes
getState = InferenceT $ \(it, s) -> pure (it, (it, s))

setState :: Applicative m => InferenceTypes -> InferenceT m ()
setState it = InferenceT $ \(_, b) -> pure ((), (it, b))

isModified :: Applicative m => InferenceT m Bool
isModified = InferenceT $ \(it, s) -> pure (s, (it, s))

whenModified :: Applicative m => InferenceT m a -> InferenceT m (Maybe a)
whenModified m = InferenceT g
  where
    g (it, False) = pure (Nothing, (it, False))
    g (it, True ) = first Just <$> _runInference m (it, True)

whenNotModified :: Applicative m => InferenceT m a -> InferenceT m (Maybe a)
whenNotModified m = InferenceT g
  where
    g (it, True) = pure (Nothing, (it, True))
    g (it, False ) = first Just <$> _runInference m (it, False)

modified :: Applicative m => a -> InferenceT m a
modified a = InferenceT $ \(it, _) -> pure (a, (it, True))


-- | Try deconstruct type into constructor application, and pass result into a function
withTypeConstr :: Applicative m => Type -> ((TyCon, [Type]) -> InferenceT m Type) -> InferenceT m Type
withTypeConstr t f = case tcSplitTyConApp_maybe t of
    Nothing -> pure t
    Just x  -> f x


-- | Try deconstruct type into a type variable, and pass result into a function
withTypeVar :: Applicative m => Type -> (TyVar -> InferenceT m Type) -> InferenceT m Type
withTypeVar t f = case tcGetTyVar_maybe t of
    Nothing -> pure t
    Just x  -> f x
