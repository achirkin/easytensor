{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
  , ArrayInstanceInference, ElemType (..), ArraySize (..)
  , ElemTypeInference (..), ArraySizeInference (..), ArrayInstanceEvidence
  , getArrayInstance, ArrayInstance (..), inferArrayInstance
  ) where


import           Data.Int                  (Int16, Int32, Int64, Int8)
import           Data.Type.Equality        ((:~:) (..))
import           Data.Word                 (Word16, Word32, Word64, Word8)
import           GHC.Prim                  (ByteArray#, Double#, Float#, Int#,Int64#,Word64#
                                           ,Word#, unsafeCoerce#)
import           GHC.Exts                  (RuntimeRep(..))

import           Numeric.Array.ElementWise
import           Numeric.Commons
import           Numeric.TypeLits
import           Numeric.Dimensions

-- | Full collection of n-order arrays
type family Array t (ds :: [Nat]) = v | v -> t ds where
  Array t      '[]          = Scalar t
  Array Float  '[2]         = FloatX2
  Array Float  '[3]         = FloatX3
  Array Float  '[4]         = FloatX4
  Array Float  (d ': ds)    = ArrayF   (d ': ds)
  Array Double (d ': ds)    = ArrayD   (d ': ds)
  Array Int    (d ': ds)    = ArrayI   (d ': ds)
  Array Int8   (d ': ds)    = ArrayI8  (d ': ds)
  Array Int16  (d ': ds)    = ArrayI16 (d ': ds)
  Array Int32  (d ': ds)    = ArrayI32 (d ': ds)
  Array Int64  (d ': ds)    = ArrayI64 (d ': ds)
  Array Word   (d ': ds)    = ArrayW   (d ': ds)
  Array Word8  (d ': ds)    = ArrayW8  (d ': ds)
  Array Word16 (d ': ds)    = ArrayW16 (d ': ds)
  Array Word32 (d ': ds)    = ArrayW32 (d ': ds)
  Array Word64 (d ': ds)    = ArrayW64 (d ': ds)


-- | Specialize scalar type without any arrays
newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"

#if SIZEOF_HSWORD < 8
type instance ElemRep (Scalar Float ) = 'FloatRep
type instance ElemRep (Scalar Double) = 'DoubleRep
type instance ElemRep (Scalar Int   ) = 'IntRep
type instance ElemRep (Scalar Int8  ) = 'IntRep
type instance ElemRep (Scalar Int16 ) = 'IntRep
type instance ElemRep (Scalar Int32 ) = 'IntRep
type instance ElemRep (Scalar Int64 ) = 'Int64Rep
type instance ElemRep (Scalar Word  ) = 'WordRep
type instance ElemRep (Scalar Word8 ) = 'WordRep
type instance ElemRep (Scalar Word16) = 'WordRep
type instance ElemRep (Scalar Word32) = 'WordRep
type instance ElemRep (Scalar Word64) = 'Word64Rep
#else
type instance ElemRep  (Scalar t) = ElemRep t
#endif

type instance ElemPrim (Scalar Float ) = Float#
type instance ElemPrim (Scalar Double) = Double#
type instance ElemPrim (Scalar Int   ) = Int#
type instance ElemPrim (Scalar Int8  ) = Int#
type instance ElemPrim (Scalar Int16 ) = Int#
type instance ElemPrim (Scalar Int32 ) = Int#
#if SIZEOF_HSWORD < 8
type instance ElemPrim (Scalar Int64 ) = Int64#
#else
type instance ElemPrim (Scalar Int64 ) = Int#
#endif
type instance ElemPrim (Scalar Word  ) = Word#
type instance ElemPrim (Scalar Word8 ) = Word#
type instance ElemPrim (Scalar Word16) = Word#
type instance ElemPrim (Scalar Word32) = Word#
#if SIZEOF_HSWORD < 8
type instance ElemPrim (Scalar Word64) = Word64#
#else
type instance ElemPrim (Scalar Word64) = Word#
#endif

deriving instance PrimBytes (Scalar Float)
deriving instance PrimBytes (Scalar Double)
deriving instance PrimBytes (Scalar Int)
deriving instance PrimBytes (Scalar Int8)
deriving instance PrimBytes (Scalar Int16)
deriving instance PrimBytes (Scalar Int32)
deriving instance PrimBytes (Scalar Int64)
deriving instance PrimBytes (Scalar Word)
deriving instance PrimBytes (Scalar Word8)
deriving instance PrimBytes (Scalar Word16)
deriving instance PrimBytes (Scalar Word32)
deriving instance PrimBytes (Scalar Word64)

-- | Indexing over scalars is trivial...
instance ElementWise (Idx ('[] :: [Nat])) t (Scalar t) where
  indexOffset# x _ = _unScalar x
  (!) x _ = _unScalar x
  {-# INLINE (!) #-}
  ewmap f = Scalar . f Z . _unScalar
  {-# INLINE ewmap #-}
  ewgen f = Scalar $ f Z
  {-# INLINE ewgen #-}
  ewgenA f = Scalar <$> f Z
  {-# INLINE ewgenA #-}
  ewfoldl f x0 = f Z x0 . _unScalar
  {-# INLINE ewfoldl #-}
  ewfoldr f x0 x = f Z (_unScalar x) x0
  {-# INLINE ewfoldr #-}
  elementWise f = fmap Scalar . f . _unScalar
  {-# INLINE elementWise #-}
  indexWise f = fmap Scalar . f Z . _unScalar
  {-# INLINE indexWise #-}
  broadcast = Scalar
  {-# INLINE broadcast #-}
  update _ x _ = Scalar x
  {-# INLINE update #-}


-- * Array implementations.
--   All array implementations have the same structure:
--   Array[Type] (element offset :: Int#) (element length :: Int#)
--                 (content :: ByteArray#)
--   All types can also be instantiated with a single scalar value.


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
#if SIZEOF_HSWORD < 8
data ArrayI64 (ds :: [Nat]) = ArrayI64# Int# Int# ByteArray#
                            | FromScalarI64# Int64#
#else
data ArrayI64 (ds :: [Nat]) = ArrayI64# Int# Int# ByteArray#
                            | FromScalarI64# Int#
#endif
data ArrayW   (ds :: [Nat]) = ArrayW# Int# Int# ByteArray#
                            | FromScalarW# Word#
data ArrayW8  (ds :: [Nat]) = ArrayW8# Int# Int# ByteArray#
                            | FromScalarW8# Word#
data ArrayW16 (ds :: [Nat]) = ArrayW16# Int# Int# ByteArray#
                            | FromScalarW16# Word#
data ArrayW32 (ds :: [Nat]) = ArrayW32# Int# Int# ByteArray#
                            | FromScalarW32# Word#
#if SIZEOF_HSWORD < 8
data ArrayW64 (ds :: [Nat]) = ArrayW64# Int# Int# ByteArray#
                            | FromScalarW64# Word64#
#else
data ArrayW64 (ds :: [Nat]) = ArrayW64# Int# Int# ByteArray#
                            | FromScalarW64# Word#
#endif

-- * Specialized types
--   More efficient data types for small fixed-size tensors
data FloatX2 = FloatX2# Float# Float#
data FloatX3 = FloatX3# Float# Float# Float#
data FloatX4 = FloatX4# Float# Float# Float# Float#

-- * Recovering type instances at runtime
--   A combination of `ElemType t` and `ArraySize ds` should
--   define an instance of `Array t ds` unambiguously.


-- | Keep information about the element type instance.
--
--   Warning! This part of the code is platform and flag dependent.
data ElemType t
  = t ~ Float  => ETFloat
  | t ~ Double => ETDouble
  | t ~ Int    => ETInt
  | t ~ Int8   => ETInt8
  | t ~ Int16  => ETInt16
  | t ~ Int32  => ETInt32
  | t ~ Int64  => ETInt64
  | t ~ Word   => ETWord
  | t ~ Word8  => ETWord8
  | t ~ Word16 => ETWord16
  | t ~ Word32 => ETWord32
  | t ~ Word64 => ETWord64

-- | Keep information about the array dimensionality
--
--   Warning! This part of the code is platform and flag dependent.
data ArraySize (ds :: [Nat])
  = ds ~ '[]   => ASScalar
  | ds ~ '[2]  => ASX2
  | ds ~ '[3]  => ASX3
  | ds ~ '[4]  => ASX4
  | forall n . (ds ~ '[n], 5 <= n) => ASXN
  | forall n1 n2 ns . ds ~ (n1 ': n2 ': ns) => ASArray

-- | Keep information about the instance behind Array family
--
--   Warning! This part of the code is platform and flag dependent.
data ArrayInstance t (ds :: [Nat])
  = ( Array t ds ~ Scalar t, ds ~ '[]) => AIScalar
  | forall n ns . ( Array t ds ~ ArrayF   ds, ds ~ (n ': ns), t ~ Float ) => AIArrayF
  | forall n ns . ( Array t ds ~ ArrayD   ds, ds ~ (n ': ns), t ~ Double) => AIArrayD
  | forall n ns . ( Array t ds ~ ArrayI   ds, ds ~ (n ': ns), t ~ Int   ) => AIArrayI
  | forall n ns . ( Array t ds ~ ArrayI8  ds, ds ~ (n ': ns), t ~ Int8  ) => AIArrayI8
  | forall n ns . ( Array t ds ~ ArrayI16 ds, ds ~ (n ': ns), t ~ Int16 ) => AIArrayI16
  | forall n ns . ( Array t ds ~ ArrayI32 ds, ds ~ (n ': ns), t ~ Int32 ) => AIArrayI32
  | forall n ns . ( Array t ds ~ ArrayI64 ds, ds ~ (n ': ns), t ~ Int64 ) => AIArrayI64
  | forall n ns . ( Array t ds ~ ArrayW   ds, ds ~ (n ': ns), t ~ Word  ) => AIArrayW
  | forall n ns . ( Array t ds ~ ArrayW8  ds, ds ~ (n ': ns), t ~ Word8 ) => AIArrayW8
  | forall n ns . ( Array t ds ~ ArrayW16 ds, ds ~ (n ': ns), t ~ Word16) => AIArrayW16
  | forall n ns . ( Array t ds ~ ArrayW32 ds, ds ~ (n ': ns), t ~ Word32) => AIArrayW32
  | forall n ns . ( Array t ds ~ ArrayW64 ds, ds ~ (n ': ns), t ~ Word64) => AIArrayW64
  | ( Array t ds ~ FloatX2, ds ~ '[2], t ~ Float) => AIFloatX2
  | ( Array t ds ~ FloatX3, ds ~ '[3], t ~ Float) => AIFloatX3
  | ( Array t ds ~ FloatX4, ds ~ '[4], t ~ Float) => AIFloatX4

-- | A singleton type used to prove that the given Array family instance
--   has a known instance
type ArrayInstanceEvidence t (ds :: [Nat])
  = Evidence (ArrayInstanceInference t ds)


class ElemTypeInference t where
    -- | Pattern match against result to get specific element type
    elemTypeInstance  :: ElemType t

class ArraySizeInference ds where
    -- | Pattern match agains result to get actual array dimensionality
    arraySizeInstance :: ArraySize ds
    inferSnocArrayInstance :: (ElemTypeInference t, KnownDim z)
                           => p t ds -> q z -> ArrayInstanceEvidence t (ds +: z)
    inferConsArrayInstance :: (ElemTypeInference t, KnownDim z)
                           => q z -> p t ds -> ArrayInstanceEvidence t (z :+ ds)
    inferInitArrayInstance :: ElemTypeInference t
                           => p t ds -> ArrayInstanceEvidence t (Init ds)


-- | Use this typeclass constraint in libraries functions if there is a need
--   to select an instance of Array famility at runtime.
--   Combination of `elemTypeInstance` and `arraySizeInstance` allows
--   to bring into typechecker's scope any specific typeclass instance
type ArrayInstanceInference t ds = (ElemTypeInference t, ArraySizeInference ds)



instance ElemTypeInference Float where
    elemTypeInstance = ETFloat
instance ElemTypeInference Double where
    elemTypeInstance = ETDouble
instance ElemTypeInference Int where
    elemTypeInstance = ETInt
instance ElemTypeInference Int8 where
    elemTypeInstance = ETInt8
instance ElemTypeInference Int16 where
    elemTypeInstance = ETInt16
instance ElemTypeInference Int32 where
    elemTypeInstance = ETInt32
instance ElemTypeInference Int64 where
    elemTypeInstance = ETInt64
instance ElemTypeInference Word where
    elemTypeInstance = ETWord
instance ElemTypeInference Word8 where
    elemTypeInstance = ETWord8
instance ElemTypeInference Word16 where
    elemTypeInstance = ETWord16
instance ElemTypeInference Word32 where
    elemTypeInstance = ETWord32
instance ElemTypeInference Word64 where
    elemTypeInstance = ETWord64

instance ArraySizeInference '[] where
    arraySizeInstance = ASScalar
    {-# INLINE arraySizeInstance #-}
    inferSnocArrayInstance _ _ = Evidence
    {-# INLINE inferSnocArrayInstance #-}
    inferConsArrayInstance _ _ = Evidence
    {-# INLINE inferConsArrayInstance #-}
    inferInitArrayInstance _ = error "Init -- empty type-level list"
    {-# INLINE inferInitArrayInstance #-}

instance KnownDim d => ArraySizeInference '[d] where
    arraySizeInstance = case dimVal' @d of
        0 -> unsafeCoerce# ASScalar
        1 -> unsafeCoerce# ASScalar
        2 -> unsafeCoerce# ASX2
        3 -> unsafeCoerce# ASX3
        4 -> unsafeCoerce# ASX4
        _ -> case (unsafeCoerce# Refl :: (5 <=? d) :~: 'True) of Refl -> ASXN
    {-# INLINE arraySizeInstance #-}
    inferSnocArrayInstance _ _ = Evidence
    {-# INLINE inferSnocArrayInstance #-}
    inferConsArrayInstance _ _ = Evidence
    {-# INLINE inferConsArrayInstance #-}
    inferInitArrayInstance _ = Evidence
    {-# INLINE inferInitArrayInstance #-}

instance KnownDim d1 => ArraySizeInference '[d1, d2] where
    arraySizeInstance = ASArray
    {-# INLINE arraySizeInstance #-}
    inferSnocArrayInstance _ _ = Evidence
    {-# INLINE inferSnocArrayInstance #-}
    inferConsArrayInstance _ _ = Evidence
    {-# INLINE inferConsArrayInstance #-}
    inferInitArrayInstance _ = Evidence
    {-# INLINE inferInitArrayInstance #-}


instance ArraySizeInference (d1 ': d2 ': d3 ': ds) where
    arraySizeInstance = ASArray
    {-# INLINE arraySizeInstance #-}
    -- I know that for dimensionality > 2 all instances are the same.
    -- Hence this dirty hack should work.
    -- I have to change this when I have customized N*M instances
    inferSnocArrayInstance p q = unsafeCoerce# (inferConsArrayInstance q p)
    {-# INLINE inferSnocArrayInstance #-}
    inferConsArrayInstance _ _ = Evidence
    {-# INLINE inferConsArrayInstance #-}
    -- I know that for dimensionality > 2 all instances are the same.
    -- Hence this dirty hack should work.
    -- I have to change this when I have customized N*M instances
    inferInitArrayInstance p = unsafeCoerce# (inferConsArrayInstance (Proxy @3) p)
    {-# INLINE inferInitArrayInstance #-}



getArrayInstance :: forall t (ds :: [Nat])
                  . ArrayInstanceInference t ds
                 => ArrayInstance t ds
getArrayInstance = case (elemTypeInstance @t, arraySizeInstance @ds) of
    (ETFloat  , ASScalar) -> AIScalar
    (ETDouble , ASScalar) -> AIScalar
    (ETInt    , ASScalar) -> AIScalar
    (ETInt8   , ASScalar) -> AIScalar
    (ETInt16  , ASScalar) -> AIScalar
    (ETInt32  , ASScalar) -> AIScalar
    (ETInt64  , ASScalar) -> AIScalar
    (ETWord   , ASScalar) -> AIScalar
    (ETWord8  , ASScalar) -> AIScalar
    (ETWord16 , ASScalar) -> AIScalar
    (ETWord32 , ASScalar) -> AIScalar
    (ETWord64 , ASScalar) -> AIScalar

    (ETFloat  , ASX2) -> AIFloatX2
    (ETDouble , ASX2) -> AIArrayD
    (ETInt    , ASX2) -> AIArrayI
    (ETInt8   , ASX2) -> AIArrayI8
    (ETInt16  , ASX2) -> AIArrayI16
    (ETInt32  , ASX2) -> AIArrayI32
    (ETInt64  , ASX2) -> AIArrayI64
    (ETWord   , ASX2) -> AIArrayW
    (ETWord8  , ASX2) -> AIArrayW8
    (ETWord16 , ASX2) -> AIArrayW16
    (ETWord32 , ASX2) -> AIArrayW32
    (ETWord64 , ASX2) -> AIArrayW64

    (ETFloat  , ASX3) -> AIFloatX3
    (ETDouble , ASX3) -> AIArrayD
    (ETInt    , ASX3) -> AIArrayI
    (ETInt8   , ASX3) -> AIArrayI8
    (ETInt16  , ASX3) -> AIArrayI16
    (ETInt32  , ASX3) -> AIArrayI32
    (ETInt64  , ASX3) -> AIArrayI64
    (ETWord   , ASX3) -> AIArrayW
    (ETWord8  , ASX3) -> AIArrayW8
    (ETWord16 , ASX3) -> AIArrayW16
    (ETWord32 , ASX3) -> AIArrayW32
    (ETWord64 , ASX3) -> AIArrayW64

    (ETFloat  , ASX4) -> AIFloatX4
    (ETDouble , ASX4) -> AIArrayD
    (ETInt    , ASX4) -> AIArrayI
    (ETInt8   , ASX4) -> AIArrayI8
    (ETInt16  , ASX4) -> AIArrayI16
    (ETInt32  , ASX4) -> AIArrayI32
    (ETInt64  , ASX4) -> AIArrayI64
    (ETWord   , ASX4) -> AIArrayW
    (ETWord8  , ASX4) -> AIArrayW8
    (ETWord16 , ASX4) -> AIArrayW16
    (ETWord32 , ASX4) -> AIArrayW32
    (ETWord64 , ASX4) -> AIArrayW64

    (ETFloat  , ASXN) -> unsafeCoerce# (AIArrayF :: ArrayInstance Float '[5])
    (ETDouble , ASXN) -> AIArrayD
    (ETInt    , ASXN) -> AIArrayI
    (ETInt8   , ASXN) -> AIArrayI8
    (ETInt16  , ASXN) -> AIArrayI16
    (ETInt32  , ASXN) -> AIArrayI32
    (ETInt64  , ASXN) -> AIArrayI64
    (ETWord   , ASXN) -> AIArrayW
    (ETWord8  , ASXN) -> AIArrayW8
    (ETWord16 , ASXN) -> AIArrayW16
    (ETWord32 , ASXN) -> AIArrayW32
    (ETWord64 , ASXN) -> AIArrayW64

    (ETFloat  , ASArray) -> AIArrayF
    (ETDouble , ASArray) -> AIArrayD
    (ETInt    , ASArray) -> AIArrayI
    (ETInt8   , ASArray) -> AIArrayI8
    (ETInt16  , ASArray) -> AIArrayI16
    (ETInt32  , ASArray) -> AIArrayI32
    (ETInt64  , ASArray) -> AIArrayI64
    (ETWord   , ASArray) -> AIArrayW
    (ETWord8  , ASArray) -> AIArrayW8
    (ETWord16 , ASArray) -> AIArrayW16
    (ETWord32 , ASArray) -> AIArrayW32
    (ETWord64 , ASArray) -> AIArrayW64

-- | Given element type instance and proper dimension list,
--   infer a corresponding array instance
inferArrayInstance :: forall t ds
                    . ( FiniteList ds
                      , KnownDims ds
                      , ElemTypeInference t
                      )
                  => ArrayInstanceEvidence t ds
inferArrayInstance = case tList @_ @ds of
    TLEmpty                          -> Evidence
    TLCons _ TLEmpty                 -> Evidence
    TLCons _ (TLCons _ TLEmpty)      -> Evidence
    TLCons _ (TLCons _ (TLCons _ _)) -> Evidence


_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
