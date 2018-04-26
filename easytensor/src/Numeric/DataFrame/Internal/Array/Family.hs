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
-- import           Numeric.Dimensions
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
type family Array (t :: Type) (ds :: [Nat]) = (v :: Type) | v -> t ds where
    Array t      '[]    = Scalar t
    Array Float  '[2]   = FloatX2
    Array Float  '[3]   = FloatX3
    Array Float  '[4]   = FloatX4
    Array Double '[2]   = DoubleX2
    Array Double '[3]   = DoubleX3
    Array Double '[4]   = DoubleX4
    Array t       ds    = ArrayBase t ds

-- | A framework for using Array type family instances.
class ArraySingleton (t :: Type) (ds :: [Nat]) where
    -- | Get Array type family instance
    aSing :: ArraySing t ds

data ArraySing t (ds :: [Nat]) where
    AScalar :: (Array t ds ~ Scalar t)       => ArraySing t     '[]
    AF2     :: (Array t ds ~ FloatX2)        => ArraySing Float  ds
    AF3     :: (Array t ds ~ FloatX3)        => ArraySing Float  ds
    AF4     :: (Array t ds ~ FloatX4)        => ArraySing Float  ds
    AD2     :: (Array t ds ~ DoubleX2)       => ArraySing Double ds
    AD3     :: (Array t ds ~ DoubleX3)       => ArraySing Double ds
    AD4     :: (Array t ds ~ DoubleX4)       => ArraySing Double ds
    ABase   :: (Array t ds ~ ArrayBase t ds) => ArraySing t      ds

deriving instance Eq (ArraySing t ds)
deriving instance Ord (ArraySing t ds)
deriving instance Show (ArraySing t ds)


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



instance {-# OVERLAPPABLE #-} ArraySingleton t ds where
    aSing = unsafeCoerce# (ABase :: ArraySing t '[0])
instance {-# OVERLAPPING #-}  ArraySingleton t      '[]    where
    aSing = AScalar
instance {-# OVERLAPPING #-}  ArraySingleton Float  '[2]   where
    aSing = AF2
instance {-# OVERLAPPING #-}  ArraySingleton Float  '[3]   where
    aSing = AF3
instance {-# OVERLAPPING #-}  ArraySingleton Float  '[4]   where
    aSing = AF4
instance {-# OVERLAPPING #-}  ArraySingleton Double '[2]   where
    aSing = AD2
instance {-# OVERLAPPING #-}  ArraySingleton Double '[3]   where
    aSing = AD3
instance {-# OVERLAPPING #-}  ArraySingleton Double '[4]   where
    aSing = AD4
