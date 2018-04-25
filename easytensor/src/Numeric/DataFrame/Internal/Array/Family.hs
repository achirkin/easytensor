{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Internal.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Internal.Array.Family
  ( Array, Scalar (..), ArrayBase (..)
  , ArraySingleton (..)
  --  Array
  -- , ArrayF (..), ArrayD (..)
  -- , ArrayI (..), ArrayI8 (..), ArrayI16 (..), ArrayI32 (..), ArrayI64 (..)
  -- , ArrayW (..), ArrayW8 (..), ArrayW16 (..), ArrayW32 (..), ArrayW64 (..)
  -- , Scalar (..)
  -- , FloatX2 (..), FloatX3 (..), FloatX4 (..)
  -- , DoubleX2 (..), DoubleX3 (..), DoubleX4 (..)
  -- , ArrayInstanceInference, ElemType (..), ArraySize (..)
  -- , ElemTypeInference (..), ArraySizeInference (..), ArrayInstanceEvidence
  -- , getArrayInstance, ArrayInstance (..), inferArrayInstance
  ) where


-- import           Data.Proxy
-- import           Data.Int                  (Int16, Int32, Int64, Int8)
-- import           Data.Type.Equality        ((:~:) (..))
-- import           Data.Word                 (Word16, Word32, Word64, Word8)
import           GHC.Base
-- (ByteArray#, Double#, Float#, Int#
                                           --,Word#, unsafeCoerce#
-- #if WORD_SIZE_IN_BITS < 64
--                                            ,Int64#, Word64#
-- #endif
                                        --   )

-- import           Numeric.DataFrame.Internal.Array.ElementWise
-- import           Numeric.Commons
-- import           Numeric.DataFrame.Internal.Array.Class
-- import           Numeric.DataFrame.Internal.Array.Internal
import           Numeric.Dimensions
-- import           Numeric.PrimBytes

import           Numeric.DataFrame.Internal.Array.Family.ArrayBase
import           Numeric.DataFrame.Internal.Array.Family.DoubleX2
import           Numeric.DataFrame.Internal.Array.Family.DoubleX3
import           Numeric.DataFrame.Internal.Array.Family.DoubleX4
import           Numeric.DataFrame.Internal.Array.Family.FloatX2
import           Numeric.DataFrame.Internal.Array.Family.FloatX3
import           Numeric.DataFrame.Internal.Array.Family.FloatX4
import           Numeric.DataFrame.Internal.Array.Family.Scalar


-- | This type family aggregates all types used for arrays with different
--   dimensioinality.
--   The family is injective; thus, it is possible to get type family instance
--   given the data constructor (and vice versa).
--   If GHC knows the dimensionality of an array at compile time, it chooses
--   a more efficient specialized instance of Array, e.g. Scalar newtype wrapper.
--   Otherwise, it falls back to the generic ArrayBase implementation.
--
--   Data family would not work here, because it would give overlapping instances.
--
--   We have two types of dimension lists here: @[Nat]@ and @[XNat]@.
--   Thus, all types are indexed by the kind of the Dims, either @Nat@ or @XNat@.
type family Array k t (ds :: [k]) = v | v -> t ds k where
    Array k    t      '[]    = Scalar k t
    Array Nat  Float  '[2]   = FloatX2 Nat
    Array Nat  Float  '[3]   = FloatX3 Nat
    Array Nat  Float  '[4]   = FloatX4 Nat
    Array Nat  Double '[2]   = DoubleX2 Nat
    Array Nat  Double '[3]   = DoubleX3 Nat
    Array Nat  Double '[4]   = DoubleX4 Nat
    Array XNat Float  '[N 2] = FloatX2 XNat
    Array XNat Float  '[N 3] = FloatX3 XNat
    Array XNat Float  '[N 4] = FloatX4 XNat
    Array XNat Double '[N 2] = DoubleX2 XNat
    Array XNat Double '[N 3] = DoubleX3 XNat
    Array XNat Double '[N 4] = DoubleX4 XNat
    Array k    t       ds    = ArrayBase k t ds

-- | A framework for using Array type family instances.
class ArraySingleton k t (ds :: [k]) where
    -- | Get Array type family instance
    aSing :: ArraySing k t ds

data ArraySing k t (ds :: [k]) where
    AScalar :: (Array k t ds ~ Scalar k t)       => ArraySing k t     '[]
    AF2     :: (Array k t ds ~ FloatX2 k)        => ArraySing k Float  ds
    AF3     :: (Array k t ds ~ FloatX3 k)        => ArraySing k Float  ds
    AF4     :: (Array k t ds ~ FloatX4 k)        => ArraySing k Float  ds
    AD2     :: (Array k t ds ~ DoubleX2 k)       => ArraySing k Double ds
    AD3     :: (Array k t ds ~ DoubleX3 k)       => ArraySing k Double ds
    AD4     :: (Array k t ds ~ DoubleX4 k)       => ArraySing k Double ds
    ABase   :: (Array k t ds ~ ArrayBase k t ds) => ArraySing k t      ds

deriving instance Eq (ArraySing k t ds)
deriving instance Ord (ArraySing k t ds)
deriving instance Show (ArraySing k t ds)


--
-- -- | This function does GHC's magic to convert user-supplied `aSing` function
-- --   to create an instance of `ArraySingleton` typeclass at runtime.
-- --   The trick is taken from Edward Kmett's reflection library explained
-- --   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
-- reifyArraySing :: forall r k t ds
--                 . ArraySing k t ds -> ( ArraySingleton k t ds => r) -> r
-- reifyArraySing as k
--   = unsafeCoerce# (MagicArraySing k :: MagicArraySing k t ds r) as
-- {-# INLINE reifyArraySing #-}
-- newtype MagicArraySing k t (ds :: [k]) r
--   = MagicArraySing (ArraySingleton k t ds => r)
--
-- aSingEv :: ArraySing k t ds -> Evidence (ArraySingleton k t ds)
-- aSingEv ds = reifyArraySing ds E
-- {-# INLINE aSingEv #-}



instance {-# OVERLAPPABLE #-} ArraySingleton k t (ds :: [k])    where
    aSing = unsafeCoerce# (ABase :: ArraySing XNat t '[XN 0])
instance {-# OVERLAPPING #-}  ArraySingleton k    t      '[]    where
    aSing = AScalar
instance {-# OVERLAPPING #-}  ArraySingleton Nat  Float  '[2]   where
    aSing = AF2
instance {-# OVERLAPPING #-}  ArraySingleton Nat  Float  '[3]   where
    aSing = AF3
instance {-# OVERLAPPING #-}  ArraySingleton Nat  Float  '[4]   where
    aSing = AF4
instance {-# OVERLAPPING #-}  ArraySingleton XNat Float  '[N 2] where
    aSing = AF2
instance {-# OVERLAPPING #-}  ArraySingleton XNat Float  '[N 3] where
    aSing = AF3
instance {-# OVERLAPPING #-}  ArraySingleton XNat Float  '[N 4] where
    aSing = AF4
instance {-# OVERLAPPING #-}  ArraySingleton Nat  Double '[2]   where
    aSing = AD2
instance {-# OVERLAPPING #-}  ArraySingleton Nat  Double '[3]   where
    aSing = AD3
instance {-# OVERLAPPING #-}  ArraySingleton Nat  Double '[4]   where
    aSing = AD4
instance {-# OVERLAPPING #-}  ArraySingleton XNat Double '[N 2] where
    aSing = AD2
instance {-# OVERLAPPING #-}  ArraySingleton XNat Double '[N 3] where
    aSing = AD3
instance {-# OVERLAPPING #-}  ArraySingleton XNat Double '[N 4] where
    aSing = AD4
