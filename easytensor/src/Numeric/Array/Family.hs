
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family
  ( Array
  , ArrayF (..), ArrayD (..)
  , ArrayI (..), ArrayI8 (..), ArrayI16 (..), ArrayI32 (..), ArrayI64 (..)
  , ArrayW (..), ArrayW8 (..), ArrayW16 (..), ArrayW32 (..), ArrayW64 (..)
  , Scalar (..)
  , FloatX2 (..), FloatX3 (..), FloatX4 (..)
  , ArrayInstanceInference (..), ArrayInstance (..)
  ) where

import GHC.TypeLits (Nat, natVal', KnownNat) --, type (<=))
-- import GHC.Types (Type)
import GHC.Prim
import Numeric.Commons
import Numeric.Dimensions
import Data.Int
import Data.Word
import Data.Type.Equality

-- | Full collection of n-order arrays
type family Array t (ds :: [Nat]) = v | v -> t ds where
  Array t     '[]  = Scalar t
  Array Float '[2] = FloatX2
  Array Float '[3] = FloatX3
  Array Float '[4] = FloatX4
  Array Float  (d ': ds)    = ArrayF (d ': ds)
  Array Double (d ': ds)    = ArrayD (d ': ds)
  Array Int    (d ': ds)    = ArrayI (d ': ds)
  Array Int8   (d ': ds)    = ArrayI8 (d ': ds)
  Array Int16  (d ': ds)    = ArrayI16 (d ': ds)
  Array Int32  (d ': ds)    = ArrayI32 (d ': ds)
  Array Int64  (d ': ds)    = ArrayI64 (d ': ds)
  Array Word   (d ': ds)    = ArrayW (d ': ds)
  Array Word8  (d ': ds)    = ArrayW8 (d ': ds)
  Array Word16 (d ': ds)    = ArrayW16 (d ': ds)
  Array Word32 (d ': ds)    = ArrayW32 (d ': ds)
  Array Word64 (d ': ds)    = ArrayW64 (d ': ds)


-- | Specialize scalar type without any arrays
newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes)
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

data FloatX2 = FloatX2# Float# Float#
data FloatX3 = FloatX3# Float# Float# Float#
data FloatX4 = FloatX4# Float# Float# Float# Float#


-- | Keep information about the instance behind Array family
data ArrayInstance t (ds :: [Nat])
  = Array t ds ~ Scalar t    => AIScalar
  | Array t ds ~ ArrayF ds   => AIArrayF
  | Array t ds ~ ArrayD ds   => AIArrayD
  | Array t ds ~ ArrayI ds   => AIArrayI
  | Array t ds ~ ArrayI8 ds  => AIArrayI8
  | Array t ds ~ ArrayI16 ds => AIArrayI16
  | Array t ds ~ ArrayI32 ds => AIArrayI32
  | Array t ds ~ ArrayI64 ds => AIArrayI64
  | Array t ds ~ ArrayW ds   => AIArrayW
  | Array t ds ~ ArrayW8 ds  => AIArrayW8
  | Array t ds ~ ArrayW16 ds => AIArrayW16
  | Array t ds ~ ArrayW32 ds => AIArrayW32
  | Array t ds ~ ArrayW64 ds => AIArrayW64
  | Array t ds ~ FloatX2     => AIFloatX2
  | Array t ds ~ FloatX3     => AIFloatX3
  | Array t ds ~ FloatX4     => AIFloatX4

-- | A singleton type used to prove that the given Array family instance
--   has a known instance
data ArrayInstanceEvidence t (ds :: [Nat])
  = ArrayInstanceInference t ds => ArrayInstanceEvidence

-- | Use this typeclass constraint in libraries functions if there is a need
--   to select an instance of Array famility at runtime.
class ArrayInstanceInference t ds where
    -- | Get the actual instance of an array behind the type family.
    --   Use TypeApplications extension:
    --   e.g. pattern matching on `inferArrayInstance @Int @'[]` produces evidence
    --        that `Array Int '[] ~ Scalar Int`
    inferArrayInstance :: ArrayInstance t ds
    inferConsArrayInstance :: KnownNat d => p d -> q t ds -> ArrayInstanceEvidence t (d :+ ds)
    -- inferConcatArrayInstance :: forall (bs :: [Nat]) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
    --                           . ArrayInstanceInference t bs
    --                          => p ds
    --                          -> q bs
    --                          -> ArrayInstanceEvidence t (ds ++ bs)
    -- inferPrefixArrayInstance :: (IsSuffix bs ds ~ 'True, FiniteList bs, Dimensions ds)
    --                          => p bs
    --                          -> q ds
    --                          -> ArrayInstanceEvidence t (Prefix bs ds)
    -- inferSuffixArrayInstance :: (IsPrefix as ds ~ 'True, FiniteList as, Dimensions ds)
    --                          => p as
    --                          -> q ds
    --                          -> ArrayInstanceEvidence t (Suffix as ds)
    -- inferSnocArrayInstance :: (KnownNat z, 2 <= z)
    --                        => p ds -> q z -> ArrayInstanceEvidence t (ds +: z)
    -- inferInitArrayInstance :: Dimensions (d :+ ds)
    --                        => p (d :+ ds) -> ArrayInstanceEvidence t (Init (d :+ ds))
    -- inferTailArrayInstance :: Dimensions ds
    --                        => p ds -> ArrayInstanceEvidence t (Tail ds)
    -- inferTakeNArrayInstance :: (KnownNat n, Dimensions ds)
    --                         => p n -> q ds -> ArrayInstanceEvidence t (Take n ds)
    -- inferDropNArrayInstance :: (KnownNat n, Dimensions ds)
    --                         => p n -> q ds -> ArrayInstanceEvidence t (Drop n ds)
    -- inferReverseArrayInstance :: Dimensions ds
    --                           => q ds -> ArrayInstanceEvidence t (Reverse ds)

instance ArrayInstanceInference Float '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Double '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Int '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Int8 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Int16 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Int32 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Int64 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Word '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Word8 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Word16 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Word32 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance ArrayInstanceInference Word64 '[] where
    inferArrayInstance = AIScalar
    inferConsArrayInstance _ _ = ArrayInstanceEvidence

instance KnownNat d => ArrayInstanceInference Float '[d] where
  inferArrayInstance = case natVal' (proxy# :: Proxy# d) of
    2 -> unsafeCoerce# AIFloatX2
    3 -> unsafeCoerce# AIFloatX3
    4 -> unsafeCoerce# AIFloatX4
    _ -> case (unsafeCoerce# Refl :: Array Float '[d] :~: ArrayF '[d]) of
           Refl -> AIArrayF
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Float (d1 ': d2 ': ds) where
  inferArrayInstance = AIArrayF
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Double (d ': ds) where
  inferArrayInstance = AIArrayD
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Int (d ': ds) where
  inferArrayInstance = AIArrayI
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Int8 (d ': ds) where
  inferArrayInstance = AIArrayI8
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Int16 (d ': ds) where
  inferArrayInstance = AIArrayI16
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Int32 (d ': ds) where
  inferArrayInstance = AIArrayI32
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Int64 (d ': ds) where
  inferArrayInstance = AIArrayI64
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Word (d ': ds) where
  inferArrayInstance = AIArrayW
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Word8 (d ': ds) where
  inferArrayInstance = AIArrayW8
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Word16 (d ': ds) where
  inferArrayInstance = AIArrayW16
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Word32 (d ': ds) where
  inferArrayInstance = AIArrayW32
  inferConsArrayInstance _ _ = ArrayInstanceEvidence
instance ArrayInstanceInference Word64 (d ': ds) where
  inferArrayInstance = AIArrayW64
  inferConsArrayInstance _ _ = ArrayInstanceEvidence



_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
