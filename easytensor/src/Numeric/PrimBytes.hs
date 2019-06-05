{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{- |
Module      :  Numeric.PrimBytes
Copyright   :  (c) Artem Chirkin
License     :  BSD3

Facilities for converting Haskell data to and from raw bytes.

 -}
module Numeric.PrimBytes
  ( -- * PrimBytes API
    PrimBytes (..)
  , bSizeOf, bAlignOf, bFieldOffsetOf
  , PrimTag (..), primTag
  ) where

#include "MachDeps.h"

import           Data.Kind            (Type)
import           Data.Proxy           (Proxy (..))
import           Data.Type.Equality   ((:~:) (..))
import qualified Data.Type.List       as L
import           Data.Type.Lits
import           GHC.Exts
import           GHC.Generics
import           GHC.Int
import           GHC.Word
import           Numeric.Dimensions
import qualified Numeric.Tuple.Lazy   as TL
import qualified Numeric.Tuple.Strict as TS

{- |

Defines how to read and write your data to and from Haskell unboxed byte arrays
and plain pointers.

Similarly to `Foreign.Storable.Storable`, this class provides functions to get
the size and alignment of a data via phantom arguments.
Thus, the size and alignment of the data must not depend on the data content
(they depend only on the type of the data).
In particular, this means that dynamically sized structures like Haskell lists
or maps are not allowed.

This module provides default implementations for all methods of this class via
`GHC.Generics.Generic`. Hence, to make your data an instance of @PrimBytes@,
it is sufficient to write the instance head:

@
data MyData a b = ...
  deriving Generic

instance (PrimBytes a, PrimBytes b) => PrimBytes (MyData a b)
@

.. or use the @DeriveAnyClass@ extension to make it even shorter:

@
data MyData a b = ...
  deriving (Generic, PrimBytes)
@

The derived instance tries to pack the data as dense as possible, but sometimes
it is better to write the instance by hand.
If a derived type has more than one constructor, the derived instance puts
a @Word32@ tag at the beginning of the byte representation.
All fields of a constructor are packed in a C-like fashion next to each other,
while respecting their alignments.

 -}
class PrimTagged a => PrimBytes a where
    {- | List of field names.

       It is used to get field offsets using `byteFieldOffset` function.

       A Generic-derived instance has this list non-empty only if two
       obvious conditions are met:

       1. The data has only one constructor.
       2. The data uses record syntax to define its fields.
     -}
    type PrimFields a :: [Symbol]
    type PrimFields a = GPrimFields (Rep a)
    -- | Store content of a data type in a primitive byte array
    --   (should be used together with @byteOffset@ function).
    --
    --   Note, the default implementation of this function returns a not pinned
    --   array, which is aligned to @SIZEOF_HSWORD@.
    --   Thus, it ignores the alignment of the underlying data type if it is larger.
    --   However, alignment calculation still makes sense for data types
    --   that are smaller than @SIZEOF_HSWORD@ bytes: they are packed more densely.
    getBytes :: a -> ByteArray#
    getBytes a = case runRW#
       ( \s0 -> case newByteArray# (byteSize a) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr (writeBytes marr 0# a s1)
       ) of (# _, r #) -> r
    {-# NOINLINE getBytes #-}
    -- | Load content of a data type from a primitive byte array given an offset in bytes.
    fromBytes :: Int# -- ^ Offset in bytes
              -> ByteArray# -- ^ Source array
              -> a
    -- | Read data from a mutable byte array given an offset in bytes.
    readBytes :: MutableByteArray# s -- ^ Source array
              -> Int# -- ^ Byte offset in the source array
              -> State# s -> (# State# s, a #)
    -- | Write data into a mutable byte array at a given position (offset in bytes).
    writeBytes :: MutableByteArray# s -- ^ Destination array
               -> Int# -- ^ Byte offset in the destination array
               -> a -- ^ Data to write into the array
               -> State# s -> State# s
    -- | Read data from a specified address.
    readAddr :: Addr# -> State# s -> (# State# s, a #)
    -- | Write data to a specified address.
    writeAddr :: a -> Addr# -> State# s -> State# s
    -- | Size of a data type in bytes.
    --
    --   Implementation of this function must not inspect the argument value;
    --   a caller may provide @undefined@ in place of the argument.
    byteSize :: a -> Int#
    -- | Alignment of a data type in bytes.
    --   @byteOffset@ should be multiple of this value.
    --
    --   Implementation of this function must not inspect the argument value;
    --   a caller may provide @undefined@ in place of the argument.
    byteAlign :: a -> Int#
    -- | Offset of the data in a byte array used to store the data,
    --   measured in bytes.
    --   Should be used together with @getBytes@ function.
    --   Unless in case of special data types represented by ByteArrays,
    --   it is equal to zero.
    --
    --   Implementation of this function may inspect the argument value;
    --   a caller must not provide @undefined@ in place of the argument.
    byteOffset :: a -> Int#
    byteOffset _ = 0#

    -- | Offset of a data record within the data type in bytes.
    --
    --   Implementation of this function must not inspect the argument value;
    --   a caller may provide @undefined@ in place of the argument.
    --
    --   The default (generic) implementation of this fucntion looks for the
    --   leftmost occurrence of a given field name (in case of multiple constructors).
    --   If a field with the given name is not found, it returns @-1@,
    --   but this is not possible thanks to @Elem name (PrimFields a)@ constraint.
    byteFieldOffset :: (Elem name (PrimFields a), KnownSymbol name)
                    => Proxy# name -> a -> Int#

    -- | Index array given an element offset
    --   (which is the @byteSize t@ rounded up to multiple of @byteAlign t@).
    indexArray :: ByteArray# -> Int# -> a
    indexArray ba i = fromBytes (i *# roundedUpBS# @a undefined) ba
    {-# INLINE indexArray #-}

    -- | Read a mutable array given an element offset
    --   (which is the @byteSize t@ rounded up to multiple of @byteAlign t@).
    readArray  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    readArray ba i = readBytes ba (i *# roundedUpBS# @a undefined)
    {-# INLINE readArray #-}

    -- | Write a mutable array given an element offset
    --   (which is the @byteSize t@ rounded up to multiple of @byteAlign t@).
    writeArray :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
    writeArray ba i = writeBytes ba (i *# roundedUpBS# @a undefined)
    {-# INLINE writeArray #-}


    default fromBytes :: (Generic a, GPrimBytes (Rep a))
                      => Int# -> ByteArray# -> a
    fromBytes i arr = to (gfromBytes proxy# 0## 0# i arr)
    {-# INLINE fromBytes #-}

    default readBytes :: (Generic a, GPrimBytes (Rep a))
                      => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    readBytes mba i s = case greadBytes proxy# 0## 0# mba i s of
      (# s', x #) -> (# s', to x #)
    {-# INLINE readBytes #-}

    default writeBytes :: (Generic a, GPrimBytes (Rep a))
                       => MutableByteArray# s -> Int# -> a -> State# s -> State# s
    writeBytes mba i = gwriteBytes proxy# 0## 0# mba i . from
    {-# INLINE writeBytes #-}

    default readAddr :: (Generic a, GPrimBytes (Rep a))
                      => Addr# -> State# s -> (# State# s, a #)
    readAddr a s = case greadAddr proxy# 0## 0# a s of
      (# s', x #) -> (# s', to x #)
    {-# INLINE readAddr #-}

    default writeAddr :: (Generic a, GPrimBytes (Rep a))
                       => a -> Addr# -> State# s -> State# s
    writeAddr = gwriteAddr proxy# 0## 0# . from
    {-# INLINE writeAddr #-}

    default byteSize :: (Generic a, GPrimBytes (Rep a))
                     => a -> Int#
    byteSize a = gbyteSize proxy# 0## 0# (from a)
    {-# INLINE byteSize #-}

    default byteAlign :: (Generic a, GPrimBytes (Rep a))
                     => a -> Int#
    byteAlign a = gbyteAlign proxy# (from a)
    {-# INLINE byteAlign #-}

    default byteFieldOffset :: ( Generic a, GPrimBytes (Rep a)
                               , KnownSymbol name)
                            => Proxy# name -> a -> Int#
    byteFieldOffset p a = gbyteFieldOffset proxy# 0## 0# p (from a)
    {-# INLINE byteFieldOffset #-}



-- | A wrapper on `byteSize`
bSizeOf :: (PrimBytes a, Num b) => a -> b
bSizeOf a = fromIntegral (I# (byteSize a))

-- | A wrapper on `byteAlign`
bAlignOf :: (PrimBytes a, Num b) => a -> b
bAlignOf a = fromIntegral (I# (byteAlign a))

-- | A wrapper on `byteFieldOffset`.
bFieldOffsetOf :: forall (name :: Symbol) (a :: Type) (b :: Type)
                . ( PrimBytes a, Elem name (PrimFields a)
                  , KnownSymbol name, Num b)
               => a -> b
bFieldOffsetOf a = fromIntegral (I# (byteFieldOffset (proxy# @Symbol @name) a))



-- | Derive a list of data selectors from the data representation @Rep a@.
type family GPrimFields (rep :: Type -> Type) :: [Symbol] where
    GPrimFields (M1 D _ f) = GPrimFields f
    GPrimFields (M1 C _ f) = GPrimFields f
    GPrimFields (M1 S ('MetaSel ('Just n) _ _ _) _) = '[n]
    GPrimFields (f :*: g) = Concat (GPrimFields f) (GPrimFields g)
    GPrimFields _ = '[]


{- | Deriving `PrimBytes` using generics

This implementation relies on two assumptions, which are probably true
in the GHC implementation of derived generics and __is not checked here__:

1. @Rep a@ is a sum-of-products.
     This means the struct offset is always @4@ for the parts of the sum type,
     and a constructor tag is always at position @0@ in the struct.

2. The @Rep a@ tree is balanced.
     Thus, I can implement a simple tag encoding:
     each bit in a tag corresponds to a nesting level.
     That is, maximum possible nesting level is 31 and minimum is 0.

Therefore, the general logic for the sum type is summarized as follows:
   reserve 4 bytes for the tag and try to pack alternatives as good as possible.

If a data type has only one constructor (@Rep a@ contains no @:+:@),
then the tag is not added.


Every function in @GPrimBytes@ has the first Proxy# argument;
it is simply used to enforce type parameter and allows easy @coerce@ implementations
for @Meta@ wrapper types.

All functions except @gbyteAlign@ have the second and third arguments:
tag mask (@Word#@) and current struct size (@Int#@);
both start with zero at the top of the @Rep a@ hierarchy.

  The tag mask is used by the sum constructors to find out where to write a bit value
  to encode left or right branch.

  The current struct size is the size (in bytes) of all elements to the left of
  the current one (before alignment).

 -}
class GPrimBytes f where
    gfromBytes :: Proxy# p
               -> Word# -- ^ Constructor tag position (mask)
               -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
               -> Int# -> ByteArray# -> f p
    greadBytes :: Proxy# p
               -> Word# -- ^ Constructor tag position (mask)
               -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
               -> MutableByteArray# s -> Int#  -> State# s -> (# State# s, f p #)
    gwriteBytes :: Proxy# p
                -> Word# -- ^ Constructor tag position (mask)
                -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
                -> MutableByteArray# s -> Int# -> f p -> State# s -> State# s
    greadAddr :: Proxy# p
              -> Word# -- ^ Constructor tag position (mask)
              -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
              -> Addr# -> State# s -> (# State# s, f p #)
    gwriteAddr :: Proxy# p
               -> Word# -- ^ Constructor tag position (mask)
               -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
               -> f p -> Addr# -> State# s -> State# s
    -- | Cumulative size of a Rep structure
    gbyteSize :: Proxy# p
              -> Word# -- ^ Constructor tag position (mask)
              -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
              -> f p -> Int#
    gbyteAlign :: Proxy# p
               -> f p -> Int#
    -- | Gives an offset of the current piece of a Rep structure
    gbyteFieldOffset :: KnownSymbol name
                     => Proxy# p
                     -> Word# -- ^ Constructor tag position (mask)
                     -> Int# -- ^ Left neighbour cumulative size (current offset before alignment)
                     -> Proxy# name -> f p -> Int#


instance GPrimBytes V1 where
    gfromBytes _ _ _ _ _ = undefined
    {-# INLINE gfromBytes #-}
    greadBytes _ _ _ _ _ s = (# s, undefined #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ _ _ _ _ s = s
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ _ _ s = (# s, undefined #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ _ _ _ s = s
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = ps
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = 1#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

instance GPrimBytes U1 where
    gfromBytes _ _ _ _ _ = U1
    {-# INLINE gfromBytes #-}
    greadBytes _ _ _ _ _ s = (# s, U1 #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ _ _ _ _ s = s
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ _ _ s = (# s, U1 #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ _ _ _ s = s
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = ps
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = 1#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

getGOff :: forall a . PrimBytes a
        => Int# --  parent cumulative size
        -> Int# --  original offset
        -> Int# --  new offset
getGOff ps i = i +# roundUpInt# ps (byteAlign @a undefined)

instance PrimBytes a => GPrimBytes (K1 i a) where
    gfromBytes _ _ ps i ba = K1 (fromBytes (getGOff @a ps i) ba)
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba i = coerce (readBytes @a mba (getGOff @a ps i))
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba i = coerce (writeBytes @a mba (getGOff @a ps i))
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps addr = coerce (readAddr @a (plusAddr# addr (getGOff @a ps 0#)))
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps ka addr = writeAddr (unK1 ka) (plusAddr# addr (getGOff @a ps 0#))
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps ~(K1 a) = roundUpInt# ps (byteAlign a) +# byteSize a
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = coerce (byteAlign @a)
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

instance {-# OVERLAPPING #-}
         (GPrimBytes f, KnownSymbol sn)
      => GPrimBytes (M1 S ('MetaSel ('Just sn) a b c) f) where
    gfromBytes p = coerce (gfromBytes @f p)
    {-# INLINE gfromBytes #-}
    greadBytes p = coerce (greadBytes @f p)
    {-# INLINE greadBytes #-}
    gwriteBytes p = coerce (gwriteBytes @f p)
    {-# INLINE gwriteBytes #-}
    greadAddr p = coerce (greadAddr @f p)
    {-# INLINE greadAddr #-}
    gwriteAddr p = coerce (gwriteAddr @f p)
    {-# INLINE gwriteAddr #-}
    gbyteSize p = coerce (gbyteSize @f p)
    {-# INLINE gbyteSize #-}
    gbyteAlign p = coerce (gbyteAlign @f p)
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset p _ off (_ :: Proxy# n) ma
      | Just Refl <- sameSymbol (undefined :: Proxy n) (undefined :: Proxy sn)
        = off `roundUpInt#` gbyteAlign p ma
      | otherwise
        = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

instance GPrimBytes f => GPrimBytes (M1 i c f) where
    gfromBytes p = coerce (gfromBytes @f p)
    {-# INLINE gfromBytes #-}
    greadBytes p = coerce (greadBytes @f p)
    {-# INLINE greadBytes #-}
    gwriteBytes p = coerce (gwriteBytes @f p)
    {-# INLINE gwriteBytes #-}
    greadAddr p = coerce (greadAddr @f p)
    {-# INLINE greadAddr #-}
    gwriteAddr p = coerce (gwriteAddr @f p)
    {-# INLINE gwriteAddr #-}
    gbyteSize p = coerce (gbyteSize @f p)
    {-# INLINE gbyteSize #-}
    gbyteAlign p = coerce (gbyteAlign @f p)
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset p = coerce (gbyteFieldOffset @f p)
    {-# INLINE gbyteFieldOffset #-}

instance (GPrimBytes f, GPrimBytes g) => GPrimBytes (f :*: g) where
    gfromBytes p t ps i ba = x :*: y
      where
        x = gfromBytes p t ps i ba
        y = gfromBytes p t (gbyteSize p t ps x) i ba
    {-# INLINE gfromBytes #-}
    greadBytes p t ps mba i s0
      | (# s1, x #) <- greadBytes p t ps mba i s0
      , (# s2, y #) <- greadBytes p t (gbyteSize p t ps x) mba i s1
        = (# s2, x :*: y #)
    {-# INLINE greadBytes #-}
    gwriteBytes p t ps mba off (x :*: y) s0
      | s1 <- gwriteBytes p t ps mba off x s0
      , s2 <- gwriteBytes p t (gbyteSize p t ps x) mba off y s1
        = s2
    {-# INLINE gwriteBytes #-}
    greadAddr p t ps addr s0
      | (# s1, x #) <- greadAddr p t ps addr s0
      , (# s2, y #) <- greadAddr p t (gbyteSize p t ps x) addr s1
        = (# s2, x :*: y #)
    {-# INLINE greadAddr #-}
    gwriteAddr p t ps (x :*: y) addr s0
      | s1 <- gwriteAddr p t ps x addr s0
      , s2 <- gwriteAddr p t (gbyteSize p t ps x) y addr s1
        = s2
    {-# INLINE gwriteAddr #-}
    gbyteSize p t ps ~(x :*: y) = gbyteSize p t (gbyteSize p t ps x) y
    {-# INLINE gbyteSize #-}
    gbyteAlign p ~(x :*: y) = commonAlign# (gbyteAlign p x) (gbyteAlign p y)
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset p t ps n ~(x :*: y)
      | offX <- gbyteFieldOffset p t ps n x
      , bsX  <- gbyteSize p t ps x
      , offY <- gbyteFieldOffset p t bsX n y
      = if isTrue# (offX <# 0#) then offY else offX
    {-# INLINE gbyteFieldOffset #-}

instance (GPrimBytes f, GPrimBytes g) => GPrimBytes (f :+: g) where
    gfromBytes p t _ off ba
      | c <- indexWord32Array# ba (uncheckedIShiftRL# off 2#)
        = if isTrue# (eqWord# (and# c t1) 0##)
          then L1 (gfromBytes p t1 4# off ba)
          else R1 (gfromBytes p t1 4# off ba)
      where
        t1 = upTag t
    {-# INLINE gfromBytes #-}
    greadBytes p t _ mba off s0
      | (# s1, c #) <- readWord32Array# mba (uncheckedIShiftRL# off 2#) s0
        = if isTrue# (eqWord# (and# c t1) 0##)
          then case greadBytes p t1 4# mba off s1 of
            (# s2, x #) -> (# s2, L1 x #)
          else case greadBytes p t1 4# mba off s1 of
            (# s2, y #) -> (# s2, R1 y #)
      where
        t1 = upTag t
    {-# INLINE greadBytes #-}
    -- if this is the uppermost sum, overwrite the tag.
    gwriteBytes p 0## _ mba off (L1 x) s0
      | s1 <- writeWord32Array# mba (uncheckedIShiftRL# off 2#) 0## s0
      , s2 <- gwriteBytes p 1## 4# mba off x s1 = s2
    gwriteBytes p 0## _ mba off (R1 y) s0
      | s1 <- writeWord32Array# mba (uncheckedIShiftRL# off 2#) 1## s0
      , s2 <- gwriteBytes p 1## 4# mba off y s1 = s2
    -- here I know that I have written zero to the corresponding bit already
    gwriteBytes p t _ mba off (L1 x) s0
      | s1 <- gwriteBytes p (upTag t) 4# mba off x s0 = s1
    -- otherwise, carefully write a single corresponding bit
    gwriteBytes p t _ mba off (R1 y) s0
      | (# s1, c #) <- readWord32Array# mba off s0
      , s2 <- writeWord32Array# mba (uncheckedIShiftRL# off 2#) (or# c t1) s1
      , s3 <- gwriteBytes p t1 4# mba off y s2 = s3
      where
        t1 = upTag t
    {-# INLINE gwriteBytes #-}
    greadAddr p t _ addr s0
      | (# s1, c #) <- readWord32OffAddr# addr 0# s0
        = if isTrue# (eqWord# (and# c t1) 0##)
          then case greadAddr p t1 4# addr s1 of
            (# s2, x #) -> (# s2, L1 x #)
          else case greadAddr p t1 4# addr s1 of
            (# s2, y #) -> (# s2, R1 y #)
      where
        t1 = upTag t
    {-# INLINE greadAddr #-}
    -- if this is the uppermost sum, overwrite the tag.
    gwriteAddr p 0## _ (L1 x) addr s0
      | s1 <- writeWord32OffAddr# addr 0# 0## s0
      , s2 <- gwriteAddr p 1## 4# x addr s1 = s2
    gwriteAddr p 0## _ (R1 y) addr s0
      | s1 <- writeWord32OffAddr# addr 0# 1## s0
      , s2 <- gwriteAddr p 1## 4# y addr s1 = s2
    -- here I know that I have written zero to the corresponding bit already
    gwriteAddr p t _ (L1 x) addr s0
      | s1 <- gwriteAddr p (upTag t) 4# x addr s0 = s1
    -- otherwise, carefully write a single corresponding bit
    gwriteAddr p t _ (R1 y) addr s0
      | (# s1, c #) <- readWord32OffAddr# addr 0# s0
      , s2 <- writeWord32OffAddr# addr 0# (or# c t1) s1
      , s3 <- gwriteAddr p t1 4# y addr s2 = s3
      where
        t1 = upTag t
    {-# INLINE gwriteAddr #-}
    gbyteSize p 0## ps xy
      = maxInt#
        (roundUpInt# 4# (gbyteAlign p x) +# gbyteSize p 1## ps x)
        (roundUpInt# 4# (gbyteAlign p y) +# gbyteSize p 1## ps y)
      where
        x = undef1 @f xy
        y = undef1 @g xy
    gbyteSize p t ps xy
      = maxInt#
        (gbyteSize p (upTag t) ps (undef1 @f xy))
        (gbyteSize p (upTag t) ps (undef1 @g xy))
    {-# INLINE gbyteSize #-}
    gbyteAlign p xy = 4# `commonAlign#`
        maxInt# (gbyteAlign p (undef1 @f xy))
                (gbyteAlign p (undef1 @g xy))
    {-# INLINE gbyteAlign #-}
    -- check both branches if any of them contain the field.
    -- If there are more than one branches containing the field, the left one
    -- is preferred.
    gbyteFieldOffset p t ps n xy
      | offX <- gbyteFieldOffset p (upTag t) ps n (undef1 @f xy)
      , offY <- gbyteFieldOffset p (upTag t) ps n (undef1 @g xy)
      = if isTrue# (offX <# 0#) then offY else offX
    {-# INLINE gbyteFieldOffset #-}

upTag :: Word# -> Word#
upTag 0## = 1##
upTag t   = uncheckedShiftL# t 1#


maxInt# :: Int# -> Int# -> Int#
maxInt# a b | isTrue# (a ># b) = a
            | otherwise        = b

-- I assume alignment is always a power of two in a sane world
-- And also the alignment fits into 16 bits
commonAlign# :: Int# -> Int# -> Int#
commonAlign# a b
  | c <- maxInt# a b
    = if isTrue# (eqWord# (popCnt# (int2Word# c)) 1##)
      then c
      else uncheckedIShiftRL# 0x10000# (word2Int# (clz16# (int2Word# c)))

roundUpInt# :: Int# -> Int# -> Int#
roundUpInt# a b = case remInt# a b of
  0# -> a
  q  -> a +# b -# q
{-# INLINE roundUpInt# #-}

roundedUpBS# :: PrimBytes a => a -> Int#
roundedUpBS# a = byteSize a `roundUpInt#` byteAlign a

undef1 :: forall p q a . q a -> p a
undef1 = const undefined
{-# INLINE undef1 #-}



#if SIZEOF_HSWORD == 4
#define OFFSHIFT_W 2
#else
#define OFFSHIFT_W 3
#endif

instance GPrimBytes (URec Word) where
    gfromBytes _ _ ps off ba
      = UWord (indexWordArray# ba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSWORD#) OFFSHIFT_W#))
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba off s
      = case readWordArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSWORD#) OFFSHIFT_W#) s of
          (# s1, r #) -> (# s1, UWord r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba off x
      = writeWordArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSWORD#) OFFSHIFT_W#) (uWord# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps a s
      = case readWordOffAddr# a (roundUpInt# ps ALIGNMENT_HSWORD#) s of (# s', x #) -> (# s', UWord x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps x a
      = writeWordOffAddr# a (roundUpInt# ps ALIGNMENT_HSWORD#) (uWord# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = roundUpInt# ps ALIGNMENT_HSWORD# +# SIZEOF_HSWORD#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = ALIGNMENT_HSWORD#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

#if SIZEOF_HSINT == 4
#define OFFSHIFT_I 2
#else
#define OFFSHIFT_I 3
#endif

instance GPrimBytes (URec Int) where
    gfromBytes _ _ ps off ba
      = UInt (indexIntArray# ba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSINT#) OFFSHIFT_I#))
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba off s
      = case readIntArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSINT#) OFFSHIFT_I#) s of
          (# s1, r #) -> (# s1, UInt r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba off x
      = writeIntArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSINT#) OFFSHIFT_I#) (uInt# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps a s
      = case readIntOffAddr# a (roundUpInt# ps ALIGNMENT_HSINT#) s of (# s', x #) -> (# s', UInt x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps x a
      = writeIntOffAddr# a (roundUpInt# ps ALIGNMENT_HSINT#) (uInt# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = roundUpInt# ps ALIGNMENT_HSINT# +# SIZEOF_HSINT#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = ALIGNMENT_HSINT#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

#if SIZEOF_HSFLOAT == 4
#define OFFSHIFT_F 2
#else
#define OFFSHIFT_F 3
#endif

instance GPrimBytes (URec Float) where
    gfromBytes _ _ ps off ba
      = UFloat (indexFloatArray# ba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSFLOAT#) OFFSHIFT_F#))
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba off s
      = case readFloatArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSFLOAT#) OFFSHIFT_F#) s of
          (# s1, r #) -> (# s1, UFloat r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba off x
      = writeFloatArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSFLOAT#) OFFSHIFT_F#) (uFloat# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps a s
      = case readFloatOffAddr# a (roundUpInt# ps ALIGNMENT_HSFLOAT#) s of (# s', x #) -> (# s', UFloat x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps x a
      = writeFloatOffAddr# a (roundUpInt# ps ALIGNMENT_HSFLOAT#) (uFloat# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = roundUpInt# ps ALIGNMENT_HSFLOAT# +# SIZEOF_HSFLOAT#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = ALIGNMENT_HSFLOAT#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

#if SIZEOF_HSDOUBLE == 4
#define OFFSHIFT_D 2
#else
#define OFFSHIFT_D 3
#endif

instance GPrimBytes (URec Double) where
    gfromBytes _ _ ps off ba
      = UDouble (indexDoubleArray# ba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSDOUBLE#) OFFSHIFT_D#))
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba off s
      = case readDoubleArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSDOUBLE#) OFFSHIFT_D#) s of
          (# s1, r #) -> (# s1, UDouble r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba off x
      = writeDoubleArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSDOUBLE#) OFFSHIFT_D#) (uDouble# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps a s
      = case readDoubleOffAddr# a (roundUpInt# ps ALIGNMENT_HSDOUBLE#) s of (# s', x #) -> (# s', UDouble x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps x a
      = writeDoubleOffAddr# a (roundUpInt# ps ALIGNMENT_HSDOUBLE#) (uDouble# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = roundUpInt# ps ALIGNMENT_HSDOUBLE# +# SIZEOF_HSDOUBLE#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = ALIGNMENT_HSDOUBLE#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

-- I believe Char is always 31 bit, but checking this does not hurt
#if SIZEOF_HSCHAR == 2
#define OFFSHIFT_C 1
#elif SIZEOF_HSCHAR == 4
#define OFFSHIFT_C 2
#else
#define OFFSHIFT_C 3
#endif

instance GPrimBytes (URec Char) where
    gfromBytes _ _ ps off ba
      = UChar (indexWideCharArray# ba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSCHAR#) OFFSHIFT_C#))
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba off s
      = case readWideCharArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSCHAR#) OFFSHIFT_C#) s of
          (# s1, r #) -> (# s1, UChar r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba off x
      = writeWideCharArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSCHAR#) OFFSHIFT_C#) (uChar# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps a s
      = case readCharOffAddr# a (roundUpInt# ps ALIGNMENT_HSCHAR#) s of (# s', x #) -> (# s', UChar x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps x a
      = writeWideCharOffAddr# a (roundUpInt# ps ALIGNMENT_HSCHAR#) (uChar# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = roundUpInt# ps ALIGNMENT_HSCHAR# +# SIZEOF_HSCHAR#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = ALIGNMENT_HSCHAR#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

#if SIZEOF_HSPTR == 4
#define OFFSHIFT_P 2
#else
#define OFFSHIFT_P 3
#endif

instance GPrimBytes (URec (Ptr ())) where
    gfromBytes _ _ ps off ba
      = UAddr (indexAddrArray# ba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSPTR#) OFFSHIFT_P#))
    {-# INLINE gfromBytes #-}
    greadBytes _ _ ps mba off s
      = case readAddrArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSPTR#) OFFSHIFT_P#) s of
          (# s1, r #) -> (# s1, UAddr r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ ps mba off x
      = writeAddrArray# mba (uncheckedIShiftRL# (off +# roundUpInt# ps ALIGNMENT_HSPTR#) OFFSHIFT_P#) (uAddr# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ ps a s
      = case readAddrOffAddr# a (roundUpInt# ps ALIGNMENT_HSPTR#) s of (# s', x #) -> (# s', UAddr x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ ps x a
      = writeAddrOffAddr# a (roundUpInt# ps ALIGNMENT_HSPTR#) (uAddr# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ _ ps _ = roundUpInt# ps ALIGNMENT_HSPTR# +# SIZEOF_HSPTR#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ _ = ALIGNMENT_HSPTR#
    {-# INLINE gbyteAlign #-}
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}




--------------------------------------------------------------------------------
-- Basic instances
--------------------------------------------------------------------------------


instance PrimBytes Word where
    type PrimFields Word = '[]
    getBytes (W# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSWORD# s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = W# (indexWordArray# ba (uncheckedIShiftRL# off OFFSHIFT_W#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_W#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_W#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWordOffAddr# a 0# s of (# s', x #) -> (# s', W# x #)
    {-# INLINE readAddr #-}
    writeAddr (W# x) a
      = writeWordOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSWORD#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSWORD#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = W# (indexWordArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWordArray# mba i s of (# s', x #) -> (# s', W# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W# x) = writeWordArray# mba i x
    {-# INLINE writeArray #-}


instance PrimBytes Int where
    type PrimFields Int = '[]
    getBytes (I# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSINT# s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = I# (indexIntArray# ba (uncheckedIShiftRL# off OFFSHIFT_I#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_I#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_I#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readIntOffAddr# a 0# s of (# s', x #) -> (# s', I# x #)
    {-# INLINE readAddr #-}
    writeAddr (I# x) a
      = writeIntOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSINT#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSINT#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = I# (indexIntArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readIntArray# mba i s of (# s', x #) -> (# s', I# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I# x) = writeIntArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Float where
    type PrimFields Float = '[]
    getBytes (F# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSFLOAT# s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = F# (indexFloatArray# ba (uncheckedIShiftRL# off OFFSHIFT_F#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_F#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_F#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readFloatOffAddr# a 0# s of (# s', x #) -> (# s', F# x #)
    {-# INLINE readAddr #-}
    writeAddr (F# x) a
      = writeFloatOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSFLOAT#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSFLOAT#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = F# (indexFloatArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readFloatArray# mba i s of (# s', x #) -> (# s', F# x #)
    {-# INLINE readArray #-}
    writeArray mba i (F# x) = writeFloatArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Double where
    type PrimFields Double = '[]
    getBytes (D# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSDOUBLE# s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = D# (indexDoubleArray# ba (uncheckedIShiftRL# off OFFSHIFT_D#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_D#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_D#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readDoubleOffAddr# a 0# s of (# s', x #) -> (# s', D# x #)
    {-# INLINE readAddr #-}
    writeAddr (D# x) a
      = writeDoubleOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSDOUBLE#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSDOUBLE#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = D# (indexDoubleArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readDoubleArray# mba i s of (# s', x #) -> (# s', D# x #)
    {-# INLINE readArray #-}
    writeArray mba i (D# x) = writeDoubleArray# mba i x
    {-# INLINE writeArray #-}


instance PrimBytes (Ptr a) where
    type PrimFields (Ptr a) = '[]
    getBytes (Ptr x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSPTR# s0 of
         (# s1, marr #) -> case writeAddrArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = Ptr (indexAddrArray# ba (uncheckedIShiftRL# off OFFSHIFT_P#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_P#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_P#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readAddrOffAddr# a 0# s of (# s', x #) -> (# s', Ptr x #)
    {-# INLINE readAddr #-}
    writeAddr (Ptr x) a
      = writeAddrOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSPTR#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSPTR#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = Ptr (indexAddrArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readAddrArray# mba i s of (# s', x #) -> (# s', Ptr x #)
    {-# INLINE readArray #-}
    writeArray mba i (Ptr x) = writeAddrArray# mba i x
    {-# INLINE writeArray #-}


instance PrimBytes Int8 where
    type PrimFields Int8 = '[]
    getBytes (I8# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT8# s0 of
         (# s1, marr #) -> case writeInt8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba = indexArray ba off
    {-# INLINE fromBytes #-}
    readBytes = readArray
    {-# INLINE readBytes #-}
    writeBytes = writeArray
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt8OffAddr# a 0# s of (# s', x #) -> (# s', I8# x #)
    {-# INLINE readAddr #-}
    writeAddr (I8# x) a
      = writeInt8OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT8#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT8#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = I8# (indexInt8Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt8Array# mba i s of (# s', x #) -> (# s', I8# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I8# x) = writeInt8Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Int16 where
    type PrimFields Int16 = '[]
    getBytes (I16# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT16# s0 of
         (# s1, marr #) -> case writeInt16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 1#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt16OffAddr# a 0# s of (# s', x #) -> (# s', I16# x #)
    {-# INLINE readAddr #-}
    writeAddr (I16# x) a
      = writeInt16OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT16#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT16#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = I16# (indexInt16Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt16Array# mba i s of (# s', x #) -> (# s', I16# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I16# x) = writeInt16Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Int32 where
    type PrimFields Int32 = '[]
    getBytes (I32# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT32# s0 of
         (# s1, marr #) -> case writeInt32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 2#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt32OffAddr# a 0# s of (# s', x #) -> (# s', I32# x #)
    {-# INLINE readAddr #-}
    writeAddr (I32# x) a
      = writeInt32OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT32#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT32#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = I32# (indexInt32Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt32Array# mba i s of (# s', x #) -> (# s', I32# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I32# x) = writeInt32Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Int64 where
    type PrimFields Int64 = '[]
    getBytes (I64# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT64# s0 of
         (# s1, marr #) -> case writeInt64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 3#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt64OffAddr# a 0# s of (# s', x #) -> (# s', I64# x #)
    {-# INLINE readAddr #-}
    writeAddr (I64# x) a
      = writeInt64OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT64#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT64#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = I64# (indexInt64Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt64Array# mba i s of (# s', x #) -> (# s', I64# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I64# x) = writeInt64Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word8 where
    type PrimFields Word8 = '[]
    getBytes (W8# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD8# s0 of
         (# s1, marr #) -> case writeWord8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba = indexArray ba off
    {-# INLINE fromBytes #-}
    readBytes = readArray
    {-# INLINE readBytes #-}
    writeBytes = writeArray
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord8OffAddr# a 0# s of (# s', x #) -> (# s', W8# x #)
    {-# INLINE readAddr #-}
    writeAddr (W8# x) a
      = writeWord8OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD8#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD8#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = W8# (indexWord8Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord8Array# mba i s of (# s', x #) -> (# s', W8# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W8# x) = writeWord8Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word16 where
    type PrimFields Word16 = '[]
    getBytes (W16# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD16# s0 of
         (# s1, marr #) -> case writeWord16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 1#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord16OffAddr# a 0# s of (# s', x #) -> (# s', W16# x #)
    {-# INLINE readAddr #-}
    writeAddr (W16# x) a
      = writeWord16OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD16#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD16#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = W16# (indexWord16Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord16Array# mba i s of (# s', x #) -> (# s', W16# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W16# x) = writeWord16Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word32 where
    type PrimFields Word32 = '[]
    getBytes (W32# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD32# s0 of
         (# s1, marr #) -> case writeWord32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 2#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord32OffAddr# a 0# s of (# s', x #) -> (# s', W32# x #)
    {-# INLINE readAddr #-}
    writeAddr (W32# x) a
      = writeWord32OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD32#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD32#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = W32# (indexWord32Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord32Array# mba i s of (# s', x #) -> (# s', W32# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W32# x) = writeWord32Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word64 where
    type PrimFields Word64 = '[]
    getBytes (W64# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD64# s0 of
         (# s1, marr #) -> case writeWord64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 3#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord64OffAddr# a 0# s of (# s', x #) -> (# s', W64# x #)
    {-# INLINE readAddr #-}
    writeAddr (W64# x) a
      = writeWord64OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD64#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD64#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = W64# (indexWord64Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord64Array# mba i s of (# s', x #) -> (# s', W64# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W64# x) = writeWord64Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Char where
    type PrimFields Char = '[]
    getBytes (C# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSCHAR# s0 of
         (# s1, marr #) -> case writeWideCharArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = C# (indexWideCharArray# ba (uncheckedIShiftRL# off OFFSHIFT_C#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_C#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_C#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWideCharOffAddr# a 0# s of (# s', x #) -> (# s', C# x #)
    {-# INLINE readAddr #-}
    writeAddr (C# x) a
      = writeWideCharOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSCHAR#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSCHAR#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = C# (indexWideCharArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWideCharArray# mba i s of (# s', x #) -> (# s', C# x #)
    {-# INLINE readArray #-}
    writeArray mba i (C# x) = writeWideCharArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes (Idx x) where
    type PrimFields (Idx x) = '[]
    getBytes :: Idx x -> ByteArray#
    getBytes = unsafeCoerce#
      (getBytes :: Word -> ByteArray#)
    {-# INLINE getBytes #-}
    fromBytes :: Int# -> ByteArray# -> Idx x
    fromBytes  = unsafeCoerce#
      (fromBytes :: Int# -> ByteArray# -> Word)
    {-# INLINE fromBytes #-}
    readBytes :: forall s . MutableByteArray# s -> Int# -> State# s -> (# State# s, Idx x #)
    readBytes = unsafeCoerce#
      (readBytes :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word #))
    {-# INLINE readBytes #-}
    writeBytes :: forall s . MutableByteArray# s -> Int# -> Idx x -> State# s -> State# s
    writeBytes = unsafeCoerce#
      (writeBytes :: MutableByteArray# s -> Int# -> Word -> State# s -> State# s)
    {-# INLINE writeBytes #-}
    readAddr :: forall s . Addr# -> State# s -> (# State# s, Idx x #)
    readAddr = unsafeCoerce#
      (readAddr :: Addr# -> State# s -> (# State# s, Word #))
    {-# INLINE readAddr #-}
    writeAddr :: forall s . Idx x -> Addr# -> State# s -> State# s
    writeAddr = unsafeCoerce#
      (writeAddr :: Word -> Addr# -> State# s -> State# s)
    {-# INLINE writeAddr #-}
    byteSize :: Idx x -> Int#
    byteSize = unsafeCoerce#
      (byteSize :: Word -> Int#)
    {-# INLINE byteSize #-}
    byteAlign :: Idx x -> Int#
    byteAlign = unsafeCoerce#
      (byteAlign :: Word -> Int#)
    {-# INLINE byteAlign #-}
    byteOffset :: Idx x -> Int#
    byteOffset = unsafeCoerce#
      (byteOffset :: Word -> Int#)
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray :: ByteArray# -> Int# -> Idx x
    indexArray = unsafeCoerce#
      (indexArray :: ByteArray# -> Int# -> Word)
    {-# INLINE indexArray #-}
    readArray :: forall s . MutableByteArray# s -> Int# -> State# s -> (# State# s, Idx x #)
    readArray = unsafeCoerce#
      (readArray :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word #))
    {-# INLINE readArray #-}
    writeArray :: forall s . MutableByteArray# s -> Int# -> Idx x -> State# s -> State# s
    writeArray = unsafeCoerce#
      (writeArray :: MutableByteArray# s -> Int# -> Word -> State# s -> State# s)
    {-# INLINE writeArray #-}

instance RepresentableList xs => PrimBytes (Idxs xs) where
    type PrimFields (Idxs xs) = '[]
    getBytes is = case runRW#
       ( \s0 -> case newByteArray# (byteSize is) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr
             (writeBytes marr 0# is s1)
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}
    fromBytes off ba = unsafeCoerce#
        (go (uncheckedIShiftRL# off OFFSHIFT_W#) (unsafeCoerce# (tList @_ @xs)))
      where
        go _ []       = []
        go i (_ : ls) = W# (indexWordArray# ba i) : go (i +# 1#) ls
    {-# INLINE fromBytes #-}
    readBytes mba off s = unsafeCoerce#
        (go (uncheckedIShiftRL# off OFFSHIFT_W#) (unsafeCoerce# (tList @_ @xs)) s)
      where
        go _ [] s0 = (# s0, [] #)
        go i (_ : ls) s0 = case readWordArray# mba off s0 of
          (# s1, w #) -> case go (i +# 1#) ls s1 of
             (# s2, xs #) -> (# s2, W# w : xs #)
    {-# INLINE readBytes #-}
    writeBytes mba off is
        = go (uncheckedIShiftRL# off OFFSHIFT_W#) (listIdxs is)
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (i +# 1#) xs (writeWordArray# mba i x s)
    {-# INLINE writeBytes #-}
    readAddr addr s = unsafeCoerce#
        (go addr (unsafeCoerce# (tList @_ @xs)) s)
      where
        go :: forall s . Addr# -> [Any] -> State# s -> (# State# s, [Word] #)
        go _ [] s0 = (# s0, [] #)
        go i (_ : ls) s0 = case readWordOffAddr# i 0# s0 of
          (# s1, w #) -> case go (plusAddr# i SIZEOF_HSWORD#) ls s1 of
             (# s2, xs #) -> (# s2, W# w : xs #)
    {-# INLINE readAddr #-}
    writeAddr is addr
        = go addr (listIdxs is)
      where
        go :: forall s . Addr# -> [Word] -> State# s -> State# s
        go _ [] s         = s
        go i (W# x :xs) s = go (plusAddr# i SIZEOF_HSWORD#) xs
                               (writeWordOffAddr# i 0# x s)
    {-# INLINE writeAddr #-}
    byteSize _ = case dimVal (order' @_ @xs) of
      W# n -> byteSize (undefined :: Idx x) *# word2Int# n
    {-# INLINE byteSize #-}
    byteAlign _ = byteAlign (undefined :: Idx x)
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba off
      | n@(W# n#) <- dimVal (order' @_ @xs)
        = unsafeCoerce# (go (off *# word2Int# n#) n)
      where
        go _ 0 = []
        go i n = W# (indexWordArray# ba i) : go (i +# 1#) (n-1)
    {-# INLINE indexArray #-}
    readArray mba off s
      | n@(W# n#) <- dimVal (order' @_ @xs)
        = unsafeCoerce# (go (off *# word2Int# n#) n s)
      where
        go _ 0 s0 = (# s0, [] #)
        go i n s0 = case readWordArray# mba off s0 of
          (# s1, w #) -> case go (i +# 1#) (n-1) s1 of
             (# s2, xs #) -> (# s2, W# w : xs #)
    {-# INLINE readArray #-}
    writeArray mba off is
      | W# n# <- dimVal (order' @_ @xs)
        = go (off *# word2Int# n#) (listIdxs is)
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (i +# 1#) xs (writeWordArray# mba i x s)
    {-# INLINE writeArray #-}

instance ( RepresentableList xs
         , L.All PrimBytes xs
         ) => PrimBytes (TL.Tuple xs) where
    type PrimFields (TL.Tuple xs) = '[]
    getBytes   = unsafeCoerce# (getBytes @(TS.Tuple xs))
    {-# INLINE getBytes #-}
    fromBytes  = unsafeCoerce# (fromBytes @(TS.Tuple xs))
    {-# INLINE fromBytes #-}
    readBytes  = unsafeCoerce# (readBytes @(TS.Tuple xs))
    {-# INLINE readBytes #-}
    writeBytes = unsafeCoerce# (writeBytes @(TS.Tuple xs))
    {-# INLINE writeBytes #-}
    readAddr   = unsafeCoerce# (readAddr  @(TS.Tuple xs))
    {-# INLINE readAddr  #-}
    writeAddr  = unsafeCoerce# (writeAddr  @(TS.Tuple xs))
    {-# INLINE writeAddr  #-}
    byteSize   = unsafeCoerce# (byteSize @(TS.Tuple xs))
    {-# INLINE byteSize #-}
    byteAlign  = unsafeCoerce# (byteAlign @(TS.Tuple xs))
    {-# INLINE byteAlign #-}
    byteOffset = unsafeCoerce# (byteOffset @(TS.Tuple xs))
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray = unsafeCoerce# (indexArray @(TS.Tuple xs))
    {-# INLINE indexArray #-}
    readArray  = unsafeCoerce# (readArray @(TS.Tuple xs))
    {-# INLINE readArray #-}
    writeArray = unsafeCoerce# (writeArray @(TS.Tuple xs))
    {-# INLINE writeArray #-}

instance ( RepresentableList xs
         , L.All PrimBytes xs
         ) => PrimBytes (TS.Tuple xs) where
    type PrimFields (TS.Tuple xs) = '[]
    getBytes tup = case runRW#
        ( \s0 -> case newByteArray# (byteSize tup) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr
             (go marr 0# tup (types tup) s1)
        ) of (# _, a #) -> a
      where
        go :: L.All PrimBytes ds => MutableByteArray# s
          -> Int# -> TS.Tuple ds -> TypeList ds -> State# s -> State# s
        go _ _ _ Empty s = s
        go mb n (TS.Id x :* xs) (_ :* ts@TypeList) s
          | n' <- roundUpInt# n (byteAlign x)
          = go mb (n' +# byteSize x) xs ts (writeBytes mb n' x s)
    {-# INLINE getBytes #-}
    fromBytes off ba = go 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ds
           => Int# -> TypeList ds -> TS.Tuple ds
        go _ Empty = Empty
        go n (t :* ts@TypeList)
          | x <- undefP t
          , n' <- roundUpInt# n (byteAlign x)
          = TS.Id (fromBytes (off +# n') ba) :* go (n' +# byteSize x) ts
    {-# INLINE fromBytes #-}
    readBytes mb off = go mb 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ds
           => MutableByteArray# s
           -> Int# -> TypeList ds -> State# s -> (# State# s, TS.Tuple ds #)
        go _ _ Empty s0 = (# s0, Empty #)
        go mba n (t :* ts@TypeList) s0
          | x <- undefP t
          , n' <- roundUpInt# n (byteAlign x)
          = case readBytes mba (off +# n') s0 of
              (# s1, r #) -> case go mba (n' +# byteSize x) ts s1 of
                (# s2, rs #) -> (# s2, TS.Id r :* rs #)
    {-# INLINE readBytes #-}
    writeBytes mba off tup = go mba 0# tup (types tup)
      where
        go :: L.All PrimBytes ds => MutableByteArray# s
           -> Int# -> TS.Tuple ds -> TypeList ds -> State# s -> State# s
        go _ _ _ Empty s = s
        go mb n (TS.Id x :* xs) (_ :* ts@TypeList) s
          | n' <- roundUpInt# n (byteAlign x)
          = go mb (n' +# byteSize x) xs ts (writeBytes mb (off +# n') x s)
    {-# INLINE writeBytes #-}
    readAddr addr = go 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ds
           => Int# -> TypeList ds -> State# s -> (# State# s, TS.Tuple ds #)
        go _ Empty s0 = (# s0, Empty #)
        go n (t :* ts@TypeList) s0
          | x <- undefP t
          , n' <- roundUpInt# n (byteAlign x)
          = case readAddr (plusAddr# addr n') s0 of
              (# s1, r #) -> case go (n' +# byteSize x) ts s1 of
                (# s2, rs #) -> (# s2, TS.Id r :* rs #)
    {-# INLINE readAddr #-}
    writeAddr tup addr = go 0# tup (types tup)
      where
        go :: L.All PrimBytes ds
           => Int# -> TS.Tuple ds -> TypeList ds -> State# s -> State# s
        go _ _ Empty s = s
        go n (TS.Id x :* xs) (_ :* ts@TypeList) s
          | n' <- roundUpInt# n (byteAlign x)
          = go (n' +# byteSize x) xs ts (writeAddr x (plusAddr# addr n') s)
    {-# INLINE writeAddr #-}
    byteSize _  = go 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ys => Int# -> TypeList ys -> Int#
        go s Empty     = s
        go s (p :* ps) = let x = undefP p
                         in  go (roundUpInt# s (byteAlign x) +# byteSize x) ps
    {-# INLINE byteSize #-}
    byteAlign _ = go (tList @_ @xs)
      where
        go :: L.All PrimBytes ys => TypeList ys -> Int#
        go Empty     = 0#
        go (p :* ps) = maxInt# (byteAlign (undefP p)) (go ps)
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}


undefP :: Proxy p -> p
undefP = const undefined
{-# INLINE undefP #-}


instance PrimBytes a => PrimBytes (Maybe a)
instance ( PrimBytes a, PrimBytes b ) => PrimBytes (Either a b)
instance ( PrimBytes a, PrimBytes b )
      => PrimBytes (a, b)
instance ( PrimBytes a, PrimBytes b, PrimBytes c )
      => PrimBytes (a, b, c)
instance ( PrimBytes a, PrimBytes b, PrimBytes c, PrimBytes d )
      => PrimBytes (a, b, c, d)
instance ( PrimBytes a, PrimBytes b, PrimBytes c, PrimBytes d, PrimBytes e )
      => PrimBytes (a, b, c, d, e)
instance ( PrimBytes a, PrimBytes b, PrimBytes c, PrimBytes d, PrimBytes e
         , PrimBytes f )
      => PrimBytes (a, b, c, d, e, f)
instance ( PrimBytes a, PrimBytes b, PrimBytes c, PrimBytes d, PrimBytes e
         , PrimBytes f, PrimBytes g )
      => PrimBytes (a, b, c, d, e, f, g)


-- | Find out which basic GHC type it is at runtime.
--   It is used for @DataFrame@ backend specialization:
--   by matching a @PrimTag a@ against its constructors, you can figure out
--   a specific implementation of @Backend a ds@
--     (e.g. whether this is a specialized float array, or a generic polymorphic array).
--   For non-basic types it defaults to `PTagOther`.
data PrimTag a where
    PTagFloat  :: PrimTag Float
    PTagDouble :: PrimTag Double
    PTagInt    :: PrimTag Int
    PTagInt8   :: PrimTag Int8
    PTagInt16  :: PrimTag Int16
    PTagInt32  :: PrimTag Int32
    PTagInt64  :: PrimTag Int64
    PTagWord   :: PrimTag Word
    PTagWord8  :: PrimTag Word8
    PTagWord16 :: PrimTag Word16
    PTagWord32 :: PrimTag Word32
    PTagWord64 :: PrimTag Word64
    PTagChar   :: PrimTag Char
    PTagPtr    :: PrimTag (Ptr a)
    PTagOther  :: PrimTag a

deriving instance Show (PrimTag a)


-- | Find out which basic GHC type it is at runtime.
class PrimTagged a where
    -- | This function allows to find out a type by comparing its tag
    primTag' :: a -> PrimTag a

-- | This function allows to find out a type by comparing its tag.
--   This is needed for backend specialization, to infer array instances.
--   For non-basic types it defaults to `PTagOther`.
primTag :: PrimBytes a => a -> PrimTag a
primTag = primTag'
{-# INLINE primTag #-}

instance {-# OVERLAPPABLE #-} PrimTagged a where
    primTag' = const PTagOther
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Float where
    primTag' = const PTagFloat
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Double where
    primTag' = const PTagDouble
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int where
    primTag' = const PTagInt
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int8 where
    primTag' = const PTagInt8
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int16 where
    primTag' = const PTagInt16
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int32 where
    primTag' = const PTagInt32
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int64 where
    primTag' = const PTagInt64
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word where
    primTag' = const PTagWord
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word8 where
    primTag' = const PTagWord8
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word16 where
    primTag' = const PTagWord16
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word32 where
    primTag' = const PTagWord32
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word64 where
    primTag' = const PTagWord64
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Char where
    primTag' = const PTagChar
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged (Ptr a) where
    primTag' = const PTagPtr
    {-# INLINE primTag' #-}
