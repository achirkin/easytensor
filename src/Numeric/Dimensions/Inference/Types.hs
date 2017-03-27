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
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Numeric.Dimensions.Inference.Types
  ( Tagged (..), Map (..)
  , HsListType, ListOpType, HsListVar, ListOpVar
  , TyVarRelations (..)
  , InferenceTypes (..), initInferenceTypes
  , InferenceT (), Inference, InferencePlugin, runInference, runPrepInference, lift
  , liftToPlugin
  , getState, setState
  , isModified, whenModified, whenNotModified, modified
  , InferEq (..)
  , withTypeConstr, withTypeVar
  , trySubstituteToList, trySubstituteEvalCons
  , lookupToList, lookupEvalCons
  , withListOpType, withHsListType
  ) where

import           Control.Arrow
import           Control.Monad         ((>=>))
import           Data.Functor.Identity
import           Data.Semigroup
import           GHC.TypeLits

import           Module
import           Name
import           Outputable            (Outputable ())
import           TcPluginM
import           TcType
import           TyCon
import           UniqFM
import           Unique
import           Var


-- | Tag GHC types with my own annotations
newtype Tagged (tag :: Symbol) a = Tagged { unTag :: a}
  deriving ( Eq, Ord, Show, Num, Fractional, Floating, Real, RealFrac, RealFloat
           , Functor, Foldable, Traversable, Semigroup, Monoid, Outputable)

-- | Use Map to keep types and their relations
newtype Uniquable a => Map a b = Map { _getUFM :: UniqFM b }
  deriving (Functor, Foldable, Traversable, Eq, Semigroup, Monoid, Outputable)

-- | Compare two types for equality
data InferEq = InferEq !Type !Type | Eliminated

type HsListType = Tagged "[k]" Type
type ListOpType = Tagged "List k" Type
type HsListVar = Tagged "[k]" TyVar
type ListOpVar = Tagged "List k" TyVar


-- | Assume equations:
--     xsL ~ ToList xs
--     xs  ~ EvalCons xs
--
--   This pair of mappings is used to substitute (ToList xs) type families with
--   type variables. This way, SimplifyList family can do its job better.
data TyVarRelations = TyVarRelations
  { toListRels   :: !(Map HsListVar ListOpVar)
    -- ^  Map xs -> xsL
  , evalConsRels :: !(Map ListOpVar HsListVar)
    -- ^ Map xsL -> xs
  }

lookupToList :: TyVarRelations -> HsListVar -> Maybe ListOpVar
lookupToList tvr (Tagged v) = lookupUFM (_getUFM $ toListRels tvr) v

lookupEvalCons :: TyVarRelations -> ListOpVar -> Maybe HsListVar
lookupEvalCons tvr (Tagged v) = lookupUFM (_getUFM $ evalConsRels tvr) v

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

runPrepInference :: Functor m => InferenceT m a -> InferenceTypes -> m (a, InferenceTypes)
runPrepInference i it = second fst <$> _runInference i (it, False)

lift :: Functor m => m a -> InferenceT m a
lift m = InferenceT $ \s -> flip (,) s <$> m

-- | Run inference within inference and check if it modified anything.
--   Before running inner inference it assumes modified = False,
--   return modification state of inner inference, and does not change `modified` for outer inference.
--   If you want to set modified on outer inference, set it manually using function `modified`.
--   Does modify InferenceTypes state.
subInference :: Functor m => InferenceT m a -> InferenceT m (a, Bool)
subInference i = InferenceT $ \(it,b) -> (\(x, (it2,b2)) -> ((x,b2), (it2, b))) <$> _runInference i (it, False)

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
    g (it, True)   = pure (Nothing, (it, True))
    g (it, False ) = first Just <$> _runInference m (it, False)

modified :: Applicative m => a -> InferenceT m a
modified a = InferenceT $ \(it, _) -> pure (a, (it, True))


-- | Try deconstruct type into constructor application, and pass result into a function.
--   Return default value if not deconstructable.
withTypeConstr :: Applicative m => Type -> a -> ((TyCon, [Type]) -> InferenceT m a) -> InferenceT m a
withTypeConstr t y f = case tcSplitTyConApp_maybe t of
    Nothing -> pure y
    Just x  -> f x


-- | Try deconstruct type into a type variable, and pass result into a function.
--   Return default value if not deconstructable.
withTypeVar :: Applicative m => Type -> a -> (TyVar -> InferenceT m a) -> InferenceT m a
withTypeVar t y f = case tcGetTyVar_maybe t of
    Nothing -> pure y
    Just x  -> f x

-- | If this type is a `ToList xs` construct, then replace it with (ys :: List k)
trySubstituteToList :: ListOpType -> Inference (ListOpType, Bool)
trySubstituteToList t = do
  InferenceTypes {..} <- getState
  subInference -- do not change outer `modified` state
    . withTypeConstr (unTag t) t
    $ \(constr, subts) ->
        if constr == tcToList || constr == tcToListNat
        then case subts of
          _:innerT:_ -> withTypeVar innerT t (f . lookupToList tyVarRelations . Tagged)
          [innerT]   -> withTypeVar innerT t (f . lookupToList tyVarRelations . Tagged)
          []         -> pure t
        else pure t
  where
    f Nothing            = pure t
    f (Just (Tagged tv)) = modified . Tagged $ mkTyVarTy tv

-- | If this type is a `EvalCons xs` construct, then replace it with (ys :: [k])
trySubstituteEvalCons :: HsListType -> Inference (HsListType, Bool)
trySubstituteEvalCons t = do
  InferenceTypes {..} <- getState
  subInference -- do not change outer `modified` state
    . withTypeConstr (unTag t) t
    $ \(constr, subts) ->
        if constr == tcEvalCons || constr == tcEvalConsNat
        then case subts of
          _:innerT:_ -> withTypeVar innerT t (f . lookupEvalCons tyVarRelations . Tagged)
          [innerT]   -> withTypeVar innerT t (f . lookupEvalCons tyVarRelations . Tagged)
          []         -> pure t
        else pure t
  where
    f Nothing            = pure t
    f (Just (Tagged tv)) = modified . Tagged $ mkTyVarTy tv

-- | Run inference if this is some `List k` kind
withListOpType ::  Monad m => Type -> a -> (ListOpType -> InferenceT m a) -> InferenceT m a
withListOpType t y f = case tcSplitTyConApp_maybe (typeKind t) of
  Nothing -> pure y
  Just (c,_)  -> do
    InferenceTypes {..} <- getState
    if c == tcList then f (Tagged t)
                   else pure y

-- | Run inference if this is some `[k]` kind
withHsListType ::  Monad m => Type -> a -> (HsListType -> InferenceT m a) -> InferenceT m a
withHsListType t y f = case tcSplitTyConApp_maybe (typeKind t) of
  Nothing -> pure y
  Just (c,_)  -> if getOccName c == mkOccName tcName "[]"
                 then f (Tagged t)
                 else pure y
