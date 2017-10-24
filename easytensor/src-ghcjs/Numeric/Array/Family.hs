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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE UnliftedFFITypes           #-}
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
  , ArrayT (..), MutableArrayT (..), Scalar (..), Word8Clamped (..)
  , ArrayInstanceInference, ElemType (..), ArraySize (..)
  , ElemTypeInference (..), ArraySizeInference (..), ArrayInstanceEvidence
  , getArrayInstance, ArrayInstance (..), inferArrayInstance
  ) where


import           Data.Int                  (Int16, Int32, Int8)
import           Data.Type.Equality        ((:~:) (..))
import           Data.Word                 (Word16, Word32, Word8)
import           GHC.Prim                  (Double#, Float#, Int#,
                                            Word#, unsafeCoerce#, ByteArray#)
import           GHC.Types                 (Int (..))
import           GHCJS.Types

import           Numeric.Array.ElementWise
import           Numeric.Commons
import           Numeric.TypeLits
import           Numeric.Dimensions

-- | Full collection of n-order arrays
type family Array t (ds :: [Nat]) = v | v -> t ds where
  Array t '[]       = Scalar t
  Array t (d ': ds) = ArrayT t (d ': ds)


-- | Specialize scalar type without any arrays
newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat, IsJSVal)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"

instance Bounded (Scalar Double) where
  maxBound = Scalar inftyD
  minBound = Scalar $ negate inftyD
instance Bounded (Scalar Float) where
  maxBound = Scalar inftyF
  minBound = Scalar $ negate inftyF
inftyD :: Double
inftyD = read "Infinity"
inftyF :: Float
inftyF = read "Infinity"


-- | Support for Uint8ClampedArray in JS.
--   This is backed by an ordinary Int type, but clamped to range 0..255 when used in an array
newtype Word8Clamped = Clamped { _fromClamped :: Int } deriving
    (Ord,Num,Eq,Enum,Integral,Real,Show) -- ,Data,Ix,FiniteBits,Bits,Storable)
instance Bounded Word8Clamped where
    maxBound = 255
    {-# INLINE maxBound #-}
    minBound = 0
    {-# INLINE minBound #-}
type instance ElemRep Word8Clamped = ElemRep Int
type instance ElemPrim Word8Clamped = Int#
instance PrimBytes Word8Clamped where
  toBytes v = (# 0#, 1#, js_wrapWord8Clamped v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapWord8Clamped arr off
  {-# INLINE fromBytes #-}
  byteSize _ = 1#
  {-# INLINE byteSize #-}
  byteAlign _ = 1#
  {-# INLINE byteAlign #-}
  elementByteSize _ = 1#
  {-# INLINE elementByteSize #-}
  ix _ (Clamped (I# x)) = x
  {-# INLINE ix #-}
foreign import javascript unsafe "h$wrapBuffer((new Uint8ClampedArray([$1])).buffer)" js_wrapWord8Clamped :: Word8Clamped -> ByteArray#
foreign import javascript unsafe "($1.uc || new Uint8ClampedArray($1.buf))[$2]" js_unwrapWord8Clamped :: ByteArray# -> Int# -> Word8Clamped

instance ElementWise (Idx ('[] :: [Nat])) Word8Clamped Word8Clamped where
  indexOffset# x _ = x
  {-# INLINE indexOffset# #-}
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f Z
  {-# INLINE ewmap #-}
  ewgen f = f Z
  {-# INLINE ewgen #-}
  ewgenA f = f Z
  {-# INLINE ewgenA #-}
  ewfoldl f x0 = f Z x0
  {-# INLINE ewfoldl #-}
  ewfoldr f x0 x = f Z x x0
  {-# INLINE ewfoldr #-}
  elementWise f = f
  {-# INLINE elementWise #-}
  indexWise f = f Z
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ x _ = x
  {-# INLINE update #-}


type instance ElemRep  (Scalar t) = ElemRep t
type instance ElemPrim (Scalar Float ) = Float#
type instance ElemPrim (Scalar Double) = Double#
type instance ElemPrim (Scalar Int   ) = Int#
type instance ElemPrim (Scalar Int8  ) = Int#
type instance ElemPrim (Scalar Int16 ) = Int#
type instance ElemPrim (Scalar Int32 ) = Int#
type instance ElemPrim (Scalar Word  ) = Word#
type instance ElemPrim (Scalar Word8 ) = Word#
type instance ElemPrim (Scalar Word16) = Word#
type instance ElemPrim (Scalar Word32) = Word#
type instance ElemPrim (Scalar Word8Clamped) = Int#

deriving instance PrimBytes (Scalar Float)
deriving instance PrimBytes (Scalar Double)
deriving instance PrimBytes (Scalar Int)
deriving instance PrimBytes (Scalar Int8)
deriving instance PrimBytes (Scalar Int16)
deriving instance PrimBytes (Scalar Int32)
deriving instance PrimBytes (Scalar Word)
deriving instance PrimBytes (Scalar Word8)
deriving instance PrimBytes (Scalar Word16)
deriving instance PrimBytes (Scalar Word32)
deriving instance PrimBytes (Scalar Word8Clamped)

-- | Indexing over scalars is trivial...
instance ElementWise (Idx ('[] :: [Nat])) t (Scalar t) where
  indexOffset# x _ = _unScalar x
  {-# INLINE indexOffset# #-}
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


newtype ArrayT t (ds :: [Nat]) = ArrayT JSVal
instance IsJSVal (ArrayT t ds)
newtype MutableArrayT s t (ds :: [Nat]) = MutableArrayT JSVal
instance IsJSVal (MutableArrayT s t ds)


-- * Recovering type instances at runtime
--   A combination of `ElemType t` and `ArraySize ds` should
--   define an instance of `Array t ds` unambiguously.


-- | Keep information about the element type instance
--
--   Warning! This part of the code is platform and flag dependent.
data ElemType t
  = t ~ Float  => ETFloat
  | t ~ Double => ETDouble
  | t ~ Int    => ETInt
  | t ~ Int8   => ETInt8
  | t ~ Int16  => ETInt16
  | t ~ Int32  => ETInt32
  | t ~ Word   => ETWord
  | t ~ Word8  => ETWord8
  | t ~ Word16 => ETWord16
  | t ~ Word32 => ETWord32
  | t ~ Word8Clamped => ETWord8C

-- | Keep information about the array dimensionality
--
--   Warning! This part of the code is platform and flag dependent.
data ArraySize (ds :: [Nat])
  = ds ~ '[]   => ASScalar
  | forall n ns . ds ~ (n ': ns) => ASArray

-- | Keep information about the instance behind Array family
--
--   Warning! This part of the code is platform and flag dependent.
data ArrayInstance t (ds :: [Nat])
  = ( Array t ds ~ Scalar t, ds ~ '[]) => AIScalar
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Float ) => AIArrayF
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Double) => AIArrayD
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Int   ) => AIArrayI
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Int8  ) => AIArrayI8
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Int16 ) => AIArrayI16
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Int32 ) => AIArrayI32
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Word  ) => AIArrayW
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Word8 ) => AIArrayW8
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Word16) => AIArrayW16
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Word32) => AIArrayW32
  | forall n ns . ( Array t ds ~ ArrayT t ds, ds ~ (n ': ns), t ~ Word8Clamped) => AIArrayW8C

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
instance ElemTypeInference Word where
    elemTypeInstance = ETWord
instance ElemTypeInference Word8 where
    elemTypeInstance = ETWord8
instance ElemTypeInference Word16 where
    elemTypeInstance = ETWord16
instance ElemTypeInference Word32 where
    elemTypeInstance = ETWord32
instance ElemTypeInference Word8Clamped where
    elemTypeInstance = ETWord8C

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
        _ -> case (unsafeCoerce# Refl :: (5 <=? d) :~: 'True) of Refl -> ASArray
    {-# INLINE arraySizeInstance #-}
    inferSnocArrayInstance _ _ = Evidence
    {-# INLINE inferSnocArrayInstance #-}
    inferConsArrayInstance _ _ = Evidence
    {-# INLINE inferConsArrayInstance #-}
    inferInitArrayInstance _ = Evidence
    {-# INLINE inferInitArrayInstance #-}


instance ArraySizeInference (d1 ': d2 ': ds) where
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
    (_        , ASScalar) -> AIScalar

    (ETFloat  , ASArray) -> AIArrayF
    (ETDouble , ASArray) -> AIArrayD
    (ETInt    , ASArray) -> AIArrayI
    (ETInt8   , ASArray) -> AIArrayI8
    (ETInt16  , ASArray) -> AIArrayI16
    (ETInt32  , ASArray) -> AIArrayI32
    (ETWord   , ASArray) -> AIArrayW
    (ETWord8  , ASArray) -> AIArrayW8
    (ETWord16 , ASArray) -> AIArrayW16
    (ETWord32 , ASArray) -> AIArrayW32
    (ETWord8C , ASArray) -> AIArrayW8C

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
