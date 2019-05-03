{-# LANGUAGE AllowAmbiguousTypes    #-}
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

data BackendSing (t :: Type) (ds :: [Nat]) where
    BSC :: ( BackendFamily t ds ~ ScalarBase t   ) => BackendSing t      '[]
    BF2 :: ( BackendFamily t ds ~ FloatX2        ) => BackendSing Float  '[2]
    BF3 :: ( BackendFamily t ds ~ FloatX3        ) => BackendSing Float  '[3]
    BF4 :: ( BackendFamily t ds ~ FloatX4        ) => BackendSing Float  '[4]
    BD2 :: ( BackendFamily t ds ~ DoubleX2       ) => BackendSing Double '[2]
    BD3 :: ( BackendFamily t ds ~ DoubleX3       ) => BackendSing Double '[3]
    BD4 :: ( BackendFamily t ds ~ DoubleX4       ) => BackendSing Double '[4]
    BPB :: ( BackendFamily t ds ~ ArrayBase t ds
           , PrimBytes t                         ) => BackendSing t ds

-- | A framework for using DataFrame type family instances.
class KnownBackend (t :: Type) (ds :: [Nat]) where
    -- | Get DataFrame backend type family instance
    bSing :: BackendSing t ds

instance {-# OVERLAPPING #-}  KnownBackend t      '[]    where bSing = BSC
instance {-# OVERLAPPING #-}  KnownBackend Float  '[2]   where bSing = BF2
instance {-# OVERLAPPING #-}  KnownBackend Float  '[3]   where bSing = BF3
instance {-# OVERLAPPING #-}  KnownBackend Float  '[4]   where bSing = BF4
instance {-# OVERLAPPING #-}  KnownBackend Double '[2]   where bSing = BD2
instance {-# OVERLAPPING #-}  KnownBackend Double '[3]   where bSing = BD3
instance {-# OVERLAPPING #-}  KnownBackend Double '[4]   where bSing = BD4
instance {-# OVERLAPPABLE #-} (BackendFamily t ds ~ ArrayBase t ds, PrimBytes t)
                           => KnownBackend t ds          where bSing = BPB

-- | Find an instance of `KnownBackend` class using `PrimBytes` and `Dimensions`.
inferKnownBackend :: forall t ds
                  . (PrimBytes t, Dimensions ds) => Dict (KnownBackend t ds)
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
  _ -> case (unsafeCoerce# (Dict @(ds ~ ds)) :: Dict (ds ~ '[0])) of
        Dict -> Dict
{-# INLINE inferKnownBackend #-}


-- This function determines the logic of instance selection
-- for the type family
inferBackendInstance
  :: forall (t :: Type) (ds :: [Nat]) (c :: Type -> Constraint)
   . ( KnownBackend t ds
     , c (ScalarBase t)
     , c FloatX2,  c FloatX3,  c FloatX4
     , c DoubleX2, c DoubleX3, c DoubleX4
     , c (ArrayBase t ds)
     )
  => Dict (c (BackendFamily t ds))
inferBackendInstance = case (bSing :: BackendSing t ds) of
    BSC -> Dict
    BF2 -> Dict
    BF3 -> Dict
    BF4 -> Dict
    BD2 -> Dict
    BD3 -> Dict
    BD4 -> Dict
    BPB -> Dict
{-# INLINE inferBackendInstance #-}

inferPrimElem :: forall t ds
               . KnownBackend t ds
              => Maybe (Dict (PrimBytes t))
inferPrimElem = case (bSing :: BackendSing t ds) of
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
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, KnownBackend t ds)
  => Dict (PrimArray t (BackendFamily t ds))
inferPrimArray = case (bSing :: BackendSing t ds) of
    BSC -> Dict
    BF2 -> Dict
    BF3 -> Dict
    BF4 -> Dict
    BD2 -> Dict
    BD3 -> Dict
    BD4 -> Dict
    BPB -> Dict
{-# INLINE inferPrimArray #-}
