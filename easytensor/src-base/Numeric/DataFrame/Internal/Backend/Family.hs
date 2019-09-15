{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
-- Combined GADT patterns are a bit of a trouble
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
-- Constraints @b ~ BackendFamily t ds@ is not redundant; they allows to avoid AllowAmbigousTypes.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{- |
This module is to be reimplemented multiple times based on a platform capabilities.
For example, I can add a SIMD implementation in a separate folder and use AVX
processsor extensions to make faster 4D vector operations.

Various implementations would be picked by activating corresponding cabal flags.

The following export list must be the same for every implementation
 -}
module Numeric.DataFrame.Internal.Backend.Family
  ( BackendFamily, KnownBackend ()
  , inferKnownBackend, inferPrimElem, inferPrimArray, inferBackendInstance
  ) where


import Data.Constraint
import GHC.Base

import Numeric.DataFrame.Internal.Backend.Family.ArrayBase
import Numeric.DataFrame.Internal.Backend.Family.DoubleX2
import Numeric.DataFrame.Internal.Backend.Family.DoubleX3
import Numeric.DataFrame.Internal.Backend.Family.DoubleX4
import Numeric.DataFrame.Internal.Backend.Family.FloatX2
import Numeric.DataFrame.Internal.Backend.Family.FloatX3
import Numeric.DataFrame.Internal.Backend.Family.FloatX4
import Numeric.DataFrame.Internal.Backend.Family.ScalarBase
import Numeric.DataFrame.Internal.PrimArray
import Numeric.Dimensions
import Numeric.PrimBytes


-- | This type family aggregates all types used for arrays with different
--   dimensioinality.
--   The family is injective; thus, it is possible to get type family instance
--   given the data constructor (and vice versa).
--   If GHC knows the dimensionality of a backend at compile time, it chooses
--   a more efficient specialized instance of @BackendFamily@, e.g. Scalar newtype wrapper.
--   Otherwise, it falls back to the generic ArrayBase implementation.
--
--   Data family would not work here, because it would give overlapping instances.
type family BackendFamily (t :: Type) (ds :: [Nat]) = (v :: Type) | v -> t ds where
    BackendFamily t      '[]    = ScalarBase t
    BackendFamily Float  '[2]   = FloatX2
    BackendFamily Float  '[3]   = FloatX3
    BackendFamily Float  '[4]   = FloatX4
    BackendFamily Double '[2]   = DoubleX2
    BackendFamily Double '[3]   = DoubleX3
    BackendFamily Double '[4]   = DoubleX4
    BackendFamily t       ds    = ArrayBase t ds

-- | Promise that we are sure the backend is ArrayBase.
unsafeDefault :: forall t ds . Dict (BackendFamily t ds ~ ArrayBase t ds)
unsafeDefault = unsafeCoerce# (Dict @(ArrayBase t ds ~ ArrayBase t ds))

-- | Singleton type used to determine the currently used DataFrame backend;
--   establishes a good bijection @b <~> t ds@ (better than just using @BackendFamily@ everywhere).
data BackendSing (t :: Type) (ds :: [Nat]) (backend :: Type) where
    BSC :: BackendSing t      '[]  (ScalarBase t)
    BF2 :: BackendSing Float  '[2]  FloatX2
    BF3 :: BackendSing Float  '[3]  FloatX3
    BF4 :: BackendSing Float  '[4]  FloatX4
    BD2 :: BackendSing Double '[2]  DoubleX2
    BD3 :: BackendSing Double '[3]  DoubleX3
    BD4 :: BackendSing Double '[4]  DoubleX4
    BPB :: ( PrimBytes t, BackendFamily t ds ~ ArrayBase t ds )
        => BackendSing t ds (ArrayBase t ds)

-- | A framework for using DataFrame type family instances.
class KnownBackend (t :: Type) (ds :: [Nat]) (backend :: Type) where
    -- | Get DataFrame backend type family instance
    bSing :: BackendSing t ds backend

-- When compiled for haddock, some of the modules complain about missing KnownBackend
-- instance. To workaround this, I add an instance stub.
--
-- Also note, these may be related issues:
--
-- https://github.com/haskell/haddock/issues/680
-- https://github.com/haskell/cabal/issues/4513
--
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
instance KnownBackend t ds b where bSing = undefined
#else
instance KnownBackend t      '[]  (ScalarBase t)   where bSing = BSC; {-# INLINE bSing #-}
instance KnownBackend Float  '[2]  FloatX2         where bSing = BF2; {-# INLINE bSing #-}
instance KnownBackend Float  '[3]  FloatX3         where bSing = BF3; {-# INLINE bSing #-}
instance KnownBackend Float  '[4]  FloatX4         where bSing = BF4; {-# INLINE bSing #-}
instance KnownBackend Double '[2]  DoubleX2        where bSing = BD2; {-# INLINE bSing #-}
instance KnownBackend Double '[3]  DoubleX3        where bSing = BD3; {-# INLINE bSing #-}
instance KnownBackend Double '[4]  DoubleX4        where bSing = BD4; {-# INLINE bSing #-}
instance PrimBytes t
      => KnownBackend t       ds  (ArrayBase t ds) where
    bSing = case unsafeDefault @t @ds of Dict -> BPB
    {-# INLINE bSing #-}
instance {-# INCOHERENT #-}
         (PrimBytes t, Dimensions ds, b ~ BackendFamily t ds)
      => KnownBackend t ds b where
    bSing = case (dims @ds, primTag @t undefined) of
      (U, _)                -> BSC
      (D2 :* U, PTagFloat)  -> BF2
      (D3 :* U, PTagFloat)  -> BF3
      (D4 :* U, PTagFloat)  -> BF4
      (D2 :* U, PTagDouble) -> BD2
      (D3 :* U, PTagDouble) -> BD3
      (D4 :* U, PTagDouble) -> BD4
      _                     -> case unsafeDefault @t @ds of Dict -> BPB
    {-# INLINE bSing #-}
#endif

-- | Find an instance of `KnownBackend` class using `PrimBytes` and `Dimensions`.
inferKnownBackend :: forall t ds b
                  . (PrimBytes t, Dimensions ds, b ~ BackendFamily t ds)
                  => Dict (KnownBackend t ds b)
inferKnownBackend = Dict
{-# INLINE inferKnownBackend #-}


-- This function determines the logic of instance selection
-- for the type family
inferBackendInstance
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (c :: Type -> Constraint)
   . ( KnownBackend t ds b
     , c (ScalarBase t)
     , c FloatX2,  c FloatX3,  c FloatX4
     , c DoubleX2, c DoubleX3, c DoubleX4
     , c (ArrayBase t ds)
     , b ~ BackendFamily t ds
     )
  => Dict (c b)
inferBackendInstance = case bSing @t @ds @b of
    BSC -> Dict
    BF2 -> Dict
    BF3 -> Dict
    BF4 -> Dict
    BD2 -> Dict
    BD3 -> Dict
    BD4 -> Dict
    BPB -> Dict
{-# INLINE inferBackendInstance #-}

inferPrimElem :: forall (t :: Type) (d :: Nat) (ds :: [Nat]) (b :: Type)
               . ( KnownBackend t (d ': ds) b
                 , b ~ BackendFamily t (d ': ds)
                 )
              => b -> Dict (PrimBytes t)
inferPrimElem _ = case bSing @t @(d ': ds) @b of
    BF2 -> Dict
    BF3 -> Dict
    BF4 -> Dict
    BD2 -> Dict
    BD3 -> Dict
    BD4 -> Dict
    BPB -> Dict

-- Need a separate function for this one due to a functional dependency
inferPrimArray
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (PrimBytes t, KnownBackend t ds b, b ~ BackendFamily t ds)
  => Dict (PrimArray t b)
inferPrimArray = case bSing @t @ds @b of
    BSC -> Dict
    BF2 -> Dict
    BF3 -> Dict
    BF4 -> Dict
    BD2 -> Dict
    BD3 -> Dict
    BD4 -> Dict
    BPB -> Dict
{-# INLINE inferPrimArray #-}
