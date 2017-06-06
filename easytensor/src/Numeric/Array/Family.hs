
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification  #-}
-- {-# LANGUAGE TypeApplications  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family
  ( ArrayType
  -- , VectorType
  -- , MatrixType
  , ArrayF (..), ArrayD (..)
  , ArrayI (..), ArrayI8 (..), ArrayI16 (..), ArrayI32 (..), ArrayI64 (..)
  , ArrayW (..), ArrayW8 (..), ArrayW16 (..), ArrayW32 (..), ArrayW64 (..)
  , Scalar (..)
  , ArrayInstanceInference (..), ArrayInstance (..)
  ) where

import GHC.TypeLits (Nat)
import GHC.Prim
import Numeric.Commons
import Numeric.Dimensions
import Data.Int
import Data.Word

-- | Full collection of n-order arrays
type family ArrayType t (ds :: [Nat]) = v | v -> t ds where
  ArrayType t     '[]  = Scalar t
  -- ArrayType Float '[2] = VFloatX2
  ArrayType Float  (d ': ds)    = ArrayF (d ': ds)
  ArrayType Double (d ': ds)    = ArrayD (d ': ds)
  ArrayType Int    (d ': ds)    = ArrayI (d ': ds)
  ArrayType Int8   (d ': ds)    = ArrayI8 (d ': ds)
  ArrayType Int16  (d ': ds)    = ArrayI16 (d ': ds)
  ArrayType Int32  (d ': ds)    = ArrayI32 (d ': ds)
  ArrayType Int64  (d ': ds)    = ArrayI64 (d ': ds)
  ArrayType Word   (d ': ds)    = ArrayW (d ': ds)
  ArrayType Word8  (d ': ds)    = ArrayW8 (d ': ds)
  ArrayType Word16 (d ': ds)    = ArrayW16 (d ': ds)
  ArrayType Word32 (d ': ds)    = ArrayW32 (d ': ds)
  ArrayType Word64 (d ': ds)    = ArrayW64 (d ': ds)


-- | Specialize scalar type without any arrays
newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes, FloatBytes, DoubleBytes, IntBytes, WordBytes)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"
type instance ElemRep (Scalar t) = ElemRep t

-- | Indexing over scalars is trivial...
instance ElementWise (Idx ('[] :: [Nat])) t (Scalar t) where
  (!) x _ = _unScalar x
  {-# INLINE (!) #-}
  ewmap f = Scalar . f Z . _unScalar
  {-# INLINE ewmap #-}
  ewgen f = Scalar $ f Z
  {-# INLINE ewgen #-}
  ewfold f x0 x = f Z (_unScalar x) x0
  {-# INLINE ewfold #-}
  elementWise f = fmap Scalar . f . _unScalar
  {-# INLINE elementWise #-}
  indexWise f = fmap Scalar . f Z . _unScalar
  {-# INLINE indexWise #-}
  broadcast = Scalar
  {-# INLINE broadcast #-}

-- * Array implementations.
--   All array implementations have the same structure:
--   Array[Type] (element offset :: Int#) (element length :: Int#)
--                 (content :: ByteArray#)
--   All types can also be instantiated with only a single scalar.


data ArrayF   (ds :: [Nat]) = ArrayF# Int# Int# ByteArray#
                            | FromScalarF# Float#
data ArrayD   (ds :: [Nat]) = ArrayD# Int# Int# ByteArray#
                            | FromScalarD# Double#
data ArrayI   (ds :: [Nat]) = ArrayI# Int# Int# ByteArray#
                            | FromScalarI# Int#
data ArrayI8  (ds :: [Nat]) = ArrayI8# Int# Int# ByteArray#
                            | FromScalarI8# Int#
data ArrayI16 (ds :: [Nat]) = ArrayI16# Int# Int# ByteArray#
                            | FromScalarI16# Int#
data ArrayI32 (ds :: [Nat]) = ArrayI32# Int# Int# ByteArray#
                            | FromScalarI32# Int#
data ArrayI64 (ds :: [Nat]) = ArrayI64# Int# Int# ByteArray#
                            | FromScalarI64# Int#
data ArrayW   (ds :: [Nat]) = ArrayW# Int# Int# ByteArray#
                            | FromScalarW# Word#
data ArrayW8  (ds :: [Nat]) = ArrayW8# Int# Int# ByteArray#
                            | FromScalarW8# Word#
data ArrayW16 (ds :: [Nat]) = ArrayW16# Int# Int# ByteArray#
                            | FromScalarW16# Word#
data ArrayW32 (ds :: [Nat]) = ArrayW32# Int# Int# ByteArray#
                            | FromScalarW32# Word#
data ArrayW64 (ds :: [Nat]) = ArrayW64# Int# Int# ByteArray#
                            | FromScalarW64# Word#


-- | Keep information about the instance behind ArrayType family
data ArrayInstance t (ds :: [Nat])
  = ArrayType t ds ~ Scalar t    => AIScalar
  | ArrayType t ds ~ ArrayF ds   => AIArrayF
  | ArrayType t ds ~ ArrayD ds   => AIArrayD
  | ArrayType t ds ~ ArrayI ds   => AIArrayI
  | ArrayType t ds ~ ArrayI8 ds  => AIArrayI8
  | ArrayType t ds ~ ArrayI16 ds => AIArrayI16
  | ArrayType t ds ~ ArrayI32 ds => AIArrayI32
  | ArrayType t ds ~ ArrayI64 ds => AIArrayI64
  | ArrayType t ds ~ ArrayW ds   => AIArrayW
  | ArrayType t ds ~ ArrayW8 ds  => AIArrayW8
  | ArrayType t ds ~ ArrayW16 ds => AIArrayW16
  | ArrayType t ds ~ ArrayW32 ds => AIArrayW32
  | ArrayType t ds ~ ArrayW64 ds => AIArrayW64

-- | Use this typeclass constraint in libraries functions if there is a need
--   to select an instance of ArrayType famility at runtime.
class ArrayInstanceInference t ds where
  -- | Get the actual instance of an array behind the type family.
  --   Use TypeApplications extension:
  --   e.g. pattern matching on `inferArrayInstance @Int @'[]` produces evidence
  --        that `ArrayType Int '[] ~ Scalar Int`
  inferArrayInstance :: ArrayInstance t ds


instance ArrayInstanceInference t '[] where
  inferArrayInstance = AIScalar

instance ArrayInstanceInference Float (d ': ds) where
  inferArrayInstance = AIArrayF
instance ArrayInstanceInference Double (d ': ds) where
  inferArrayInstance = AIArrayD
instance ArrayInstanceInference Int (d ': ds) where
  inferArrayInstance = AIArrayI
instance ArrayInstanceInference Int8 (d ': ds) where
  inferArrayInstance = AIArrayI8
instance ArrayInstanceInference Int16 (d ': ds) where
  inferArrayInstance = AIArrayI16
instance ArrayInstanceInference Int32 (d ': ds) where
  inferArrayInstance = AIArrayI32
instance ArrayInstanceInference Int64 (d ': ds) where
  inferArrayInstance = AIArrayI64
instance ArrayInstanceInference Word (d ': ds) where
  inferArrayInstance = AIArrayW
instance ArrayInstanceInference Word8 (d ': ds) where
  inferArrayInstance = AIArrayW8
instance ArrayInstanceInference Word16 (d ': ds) where
  inferArrayInstance = AIArrayW16
instance ArrayInstanceInference Word32 (d ': ds) where
  inferArrayInstance = AIArrayW32
instance ArrayInstanceInference Word64 (d ': ds) where
  inferArrayInstance = AIArrayW64



_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
