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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Numeric.Dimensions.Inference.Types
  ( Tagged (..), Map (..), Set
  , empty, singleton, (!), insert, keys, elems, mkSet, mkMap, filterMap, difference
  , HsListType, ListOpType, HsListVar, ListOpVar
  , TyVarRelations (..)
  , InferenceTypes (..), initInferenceTypes, incrementPhase, resetPhase
  , InferenceT (), Inference, InferencePlugin, runInference, runPrepInference, lift
  , liftToPlugin, subInference
  , getState, setState
  , isModified, whenModified, whenNotModified, modified
  , InferEq (..)
  , withTypeConstr, withTypeVar
  , trySubstituteToList, trySubstituteEvalCons
  , lookupToList, lookupEvalCons
  , withListOpType, withHsListType
  , Mapping (..), AllVarRelations (..), allReachableRelations
  , ToMapping (..), mappingType, mappingDest
  , foldMapMon
  , getTyVarKindParam, isListKind, isHsListKind
  ) where

import           Control.Arrow
import           Control.Monad         (foldM, (>=>))
import           Data.Functor.Identity
import           Data.Maybe            (fromMaybe)
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeLits

import           IOEnv
import           Module
import           Name
import           Outputable            (Outputable ())
import           TcPluginM
import           TcType
import           TyCon
import           TysWiredIn            (listTyCon)
import           UniqFM
import           Unique
import           Var


-- | Tag GHC types with my own annotations
newtype Tagged (tag :: Symbol) a = Tagged { unTag :: a}
  deriving ( Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
           , Functor, Foldable, Traversable, Semigroup, Monoid, Outputable, Uniquable)

-- | Use Map to keep types and their relations
newtype Uniquable a => Map a b = Map { _getUFM :: UniqFM b }
  deriving (Functor, Foldable, Traversable, Eq, Semigroup, Monoid, Outputable)

type Set a = Map a a

-- | List to set
mkSet :: Uniquable a => [a] -> Set a
mkSet xs = Map . listToUFM $ map (\x -> (x,x)) xs

-- | List of tuples to map
mkMap :: Uniquable a => [(a,b)] -> Map a b
mkMap xs = Map $ listToUFM xs

-- | Empty map or set
empty :: Map a b
empty = Map emptyUFM

-- | Only one element
singleton :: Uniquable a => a -> b -> Map a b
singleton k = Map . unitUFM k

-- | Filter by a predicate
filterMap :: (b -> Bool) -> Map a b -> Map a b
filterMap f = Map . filterUFM f . _getUFM

-- | Insert a single element
insert :: Uniquable a => Map a b -> a -> b -> Map a b
insert m k = Map . addToUFM (_getUFM m) k

-- | Difference (`minusUFM`)
difference :: Uniquable a => Map a b -> Map a c -> Map a b
difference (Map a) (Map b) = Map $ a `minusUFM` b


-- | Using `lookupUFM`
(!) :: Uniquable a => Map a b -> a -> Maybe b
(!) = lookupUFM . _getUFM
infixl 9 !

-- | All map keys. Note that keys theirselves are not saved,
--   only their Unique "hashes" are used.
keys :: Map a b -> [Unique]
keys = keysUFM . _getUFM

-- | Transform to list (`eltsUFM`)
elems :: Map a b -> [b]
elems = eltsUFM . _getUFM

-- | Compare two types for equality
data InferEq = InferEq !Type !Type | Eliminated

type HsListType = Tagged "[k]" Type
type ListOpType = Tagged "List k" Type
type HsListVar = Tagged "[k]" TyVar
type ListOpVar = Tagged "List k" TyVar

showTag :: KnownSymbol s => Tagged s t -> String
showTag = symbolVal . proxS
  where
    proxS :: Tagged s t -> Proxy s
    proxS _ = Proxy


instance (Show t, KnownSymbol s) => Show (Tagged s t) where
  show tt@(Tagged t) = show t ++ " :: " ++ showTag tt


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

-- | Shortcut to get List k variable substitution
lookupToList :: TyVarRelations -> HsListVar -> Maybe ListOpVar
lookupToList tvr (Tagged v) = lookupUFM (_getUFM $ toListRels tvr) v

-- | Shortcut to get [k] variable substitution
lookupEvalCons :: TyVarRelations -> ListOpVar -> Maybe HsListVar
lookupEvalCons tvr (Tagged v) = lookupUFM (_getUFM $ evalConsRels tvr) v

instance Semigroup TyVarRelations where
  TyVarRelations a b <> TyVarRelations x y = TyVarRelations (a <> x) (b <> y)

instance Monoid TyVarRelations where
  mempty = TyVarRelations mempty mempty
  mappend = (<>)


-- | Keep useful inference information here
data InferenceTypes = InferenceTypes
  { tcToList       :: !TyCon
  , tcList         :: !TyCon
  , tcToListNat    :: !TyCon
  , tcEvalCons     :: !TyCon
  , tcEvalConsNat  :: !TyCon
  , tcSimplifyList :: !TyCon
  , mDimensions    :: !Module
  , tyVarRelations :: !TyVarRelations
  , phaseNum       :: !(IORef Int)
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
      phaseNum <- unsafeTcPluginTcM $ newMutVar 0
      let tyVarRelations = mempty
      return $ Just InferenceTypes {..}
    _ -> return Nothing

-- | returns a new phase number
incrementPhase :: InferenceTypes -> TcPluginM Int
incrementPhase InferenceTypes{phaseNum = refn} = unsafeTcPluginTcM $ do
    updMutVar refn (+1)
    readMutVar refn

resetPhase :: InferenceTypes -> TcPluginM ()
resetPhase InferenceTypes{phaseNum = refn}
  = unsafeTcPluginTcM $ writeMutVar refn 0

-- | Updates a state in InferenceTypes, and keeps additional Bool value
--   indicating whether a work has been done or not;
--   if it stays False then we could not infer any useful info about a type.
newtype InferenceT m a = InferenceT
   { _runInference :: (InferenceTypes, Bool) -> m (a, (InferenceTypes, Bool)) }


-- | Run inference monad and return bool whether the type was modified
runInference :: Functor m => InferenceT m a -> InferenceTypes -> m (a, Bool)
runInference i it = second snd <$> _runInference i (it, False)

-- | Run inference monad, modify InferenceTypes and return them
runPrepInference :: Functor m => InferenceT m a -> InferenceTypes -> m (a, InferenceTypes)
runPrepInference i it = second fst <$> _runInference i (it, False)

-- | Run a child monad within Inference
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

-- | Get current InferenceTypes state
getState :: Applicative m => InferenceT m InferenceTypes
getState = InferenceT $ \(it, s) -> pure (it, (it, s))

-- | Modify InferenceTypes state
setState :: Applicative m => InferenceTypes -> InferenceT m ()
setState it = InferenceT $ \(_, b) -> pure ((), (it, b))

-- | Check if `modified` trigger was already used.
--   True means we did some modifications of a type inside.
--   False means we did not useful work.
isModified :: Applicative m => InferenceT m Bool
isModified = InferenceT $ \(it, s) -> pure (s, (it, s))

-- | What to do if `modified` was already triggered
whenModified :: Applicative m => InferenceT m a -> InferenceT m (Maybe a)
whenModified m = InferenceT g
  where
    g (it, False) = pure (Nothing, (it, False))
    g (it, True ) = first Just <$> _runInference m (it, True)

-- | What to do if `modified` was not yet triggered
whenNotModified :: Applicative m => InferenceT m a -> InferenceT m (Maybe a)
whenNotModified m = InferenceT g
  where
    g (it, True)   = pure (Nothing, (it, True))
    g (it, False ) = first Just <$> _runInference m (it, False)

-- | Trigger `modified` state in the Inference monad.
--   This means we did some useful work and want to use its result later (to send it to TC)
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
trySubstituteToList :: Monad m => ListOpType -> InferenceT m (ListOpType, Bool)
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
trySubstituteEvalCons :: Monad m => HsListType -> InferenceT m (HsListType, Bool)
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
  Just (c,_)  -> if c == listTyCon
                 then f (Tagged t)
                 else pure y


-- | Relation from somewhere to a particular type variable within a given type
data Mapping
  = IsToList HsListVar ListOpType
  | IsEvalCons ListOpVar HsListType
  | IsSame TyVar Type

mappingDest :: Mapping -> TyVar
mappingDest (IsToList (Tagged v) _)   = v
mappingDest (IsEvalCons (Tagged v) _) = v
mappingDest (IsSame v _)              = v

mappingType :: Mapping -> Type
mappingType (IsToList _ (Tagged v))   = v
mappingType (IsEvalCons _ (Tagged v)) = v
mappingType (IsSame _ v)              = v

newtype AllVarRelations = AVR { _getAVR :: Map TyVar (TyVar, Map TyVar Mapping) }

instance Semigroup AllVarRelations where
  (AVR (Map a)) <> (AVR (Map b)) = AVR . Map $ plusUFM_C f a b
    where
      f (v, m1) (_, m2) = (v, m1 <> m2)

instance Monoid AllVarRelations where
  mempty = AVR mempty
  mappend = (<>)

class ToMapping a where
  toMapping :: a -> Mapping

instance ToMapping (HsListVar, ListOpType) where
  toMapping (a,b) = IsToList a b

instance ToMapping (ListOpVar, HsListType) where
  toMapping (a,b) = IsEvalCons a b

instance ToMapping (TyVar, Type) where
  toMapping (a,b) = IsSame a b

-- | Make each connected component of a graph full.
--   At this moment, I write here a primitive, slow algorithm
--   that runs in O(n) for each single relation.
--   I hope for the assumption that connected components are small (usually, two or four elements each).
--   So the algorithm is as simple as this:
--      for each node, go through all relatives and check if they are in the map already.
allReachableRelations :: Monad m => AllVarRelations -> InferenceT m AllVarRelations
allReachableRelations (AVR avr) = do
    InferenceTypes {..} <- getState

    let -- find a connected component.
        findCC :: Set TyVar  -> TyVar -> Set TyVar
        findCC done tv = singleton tv tv <> foldMap (findCC (done <> singleton tv tv)) toDo
          where
            adj = fromMaybe mempty $ fmap mappingDest . snd <$> avr ! tv
            toDo = adj `difference` done
        -- first argument keeps track of all passed nodes
        go :: (TyVar, Map TyVar Mapping) -> AllVarRelations
        go (tv, _) = AVR $ singleton tv (tv, foldMap (connect tv) (findCC mempty tv) `difference` singleton tv tv)
        -- add a relation
        connect :: TyVar -> TyVar -> Map TyVar Mapping
        connect lhs rhs = case (isListK lhs, isListK rhs) of
          (False, False) -> singleton rhs (IsSame rhs $ mkTyVarTy rhs)
          (True , True ) -> singleton rhs (IsSame rhs $ mkTyVarTy rhs)
          (False, True ) -> singleton rhs ( IsEvalCons (Tagged rhs)
                                          . Tagged $ mkTyConApp tcEvalCons [getTyVarKindParam rhs, mkTyVarTy rhs])
          (True , False) -> singleton rhs ( IsToList (Tagged rhs)
                                          . Tagged $ mkTyConApp tcToList [getTyVarKindParam rhs, mkTyVarTy rhs])
        isListK :: TyVar -> Bool
        isListK v = case tcSplitTyConApp_maybe (tyVarKind v) of
          Nothing    -> False
          Just (c,_) -> c == tcList

    pure $ foldMap go avr



foldMapMon :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapMon f = foldM (\acc x -> mappend acc <$> f x) mempty


getTyVarKindParam :: TyVar -> Kind
getTyVarKindParam v = case tcSplitTyConApp_maybe (tyVarKind v) of
   Just (_, k:_) -> k
   _             -> tyVarKind v


isListKind :: TyVar -> Bool
isListKind = isGood . tcSplitTyConApp_maybe . tyVarKind
  where
    isGood Nothing            = False
    isGood (Just (constr, _)) = getOccName constr == mkOccName tcName "List"

isHsListKind :: TyVar -> Bool
isHsListKind = isGood . tcSplitTyConApp_maybe . tyVarKind
  where
    isGood Nothing            = False
    isGood (Just (constr, _)) = constr == listTyCon
