{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
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


instance KnownBackend t      '[]  (ScalarBase t)   where bSing = BSC
instance KnownBackend Float  '[2]  FloatX2         where bSing = BF2
instance KnownBackend Float  '[3]  FloatX3         where bSing = BF3
instance KnownBackend Float  '[4]  FloatX4         where bSing = BF4
instance KnownBackend Double '[2]  DoubleX2        where bSing = BD2
instance KnownBackend Double '[3]  DoubleX3        where bSing = BD3
instance KnownBackend Double '[4]  DoubleX4        where bSing = BD4
instance PrimBytes t
      => KnownBackend t       ds  (ArrayBase t ds) where
    bSing = case unsafeDefault @t @ds of Dict -> BPB

-- | Find an instance of `KnownBackend` class using `PrimBytes` and `Dimensions`.
inferKnownBackend :: forall t ds b
                  . (PrimBytes t, Dimensions ds, b ~ BackendFamily t ds)
                  => Dict (KnownBackend t ds b)
inferKnownBackend = case (dims @_ @ds, primTag @t undefined) of
  (U, _) -> Dict
  (d :* U, PTagFloat)
      | Just Dict <- sameDim (D @2) d -> Dict
      | Just Dict <- sameDim (D @3) d -> Dict
      | Just Dict <- sameDim (D @4) d -> Dict
  (d :* U, PTagDouble)
      | Just Dict <- sameDim (D @2) d -> Dict
      | Just Dict <- sameDim (D @3) d -> Dict
      | Just Dict <- sameDim (D @4) d -> Dict
  _ -> case unsafeDefault @t @ds of Dict -> Dict
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

inferPrimElem :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
               . (KnownBackend t ds b, b ~ BackendFamily t ds)
              => b -> Maybe (Dict (PrimBytes t))
inferPrimElem _ = case bSing @t @ds @b of
    BSC -> Nothing
    BF2 -> Just Dict
    BF3 -> Just Dict
    BF4 -> Just Dict
    BD2 -> Just Dict
    BD3 -> Just Dict
    BD4 -> Just Dict
    BPB -> Just Dict

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
