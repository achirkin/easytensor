{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{- |
Module      :  Numeric.PrimBytes
Copyright   :  (c) Artem Chirkin
License     :  BSD3

Facilities for converting Haskell data to and from raw bytes.

The main purpose of this module is to support the implementation of the @DataFrame@
`Numeric.DataFrame.Internal.Backend.Backend`. However, it also comes very useful for
writing FFI. To that end, the `PrimBytes` class is similar to
the `Foreign.Storable.Storable` class: it provides means to write
your data to and read from a raw memory area. Though, it is more flexible in that
it can work with both, foreign pointers and primitive byte arrays,
and it provides means to get data field offsets by their selector names.
On top of that, a `PrimBytes` instance can be derived via
the `GHC.Generics.Generic` machinery.

A derived `PrimBytes` instance tries to pack the data as dense as possible,
while respecting the alignment requirements. In all cases known to me,
the resulting data layout coincides with a corresponding C struct, allowing
to marshal the data without any boilerplate. However, this is not guaranteed,
but you can write a `PrimBytes` instance manually if necessary
(and report an issue plz).


__Note about alignment, size, and padding of the data.__
There are two basic sanity assumptions about these, which are not checked
in this module at all:

  * the alignment is always a power of 2;
  * the size is always rounded up to a multiple of the alignment.

Generated instances of `PrimBytes` meet these assumptions if all components of
a data meet these assumptions too.
You are strongly advised to provide all byte offset arguments to the `PrimBytes`
functions respecting the alignment of the data;
otherwise, the data may be written or read incorrectly.
 -}
module Numeric.PrimBytes
  ( -- * PrimBytes API
    PrimBytes (..)
  , bSizeOf, bAlignOf, bFieldOffsetOf
    -- * Storable API
    --
    -- |
    -- `Foreign.Storable.Storable` can be defined in terms of `PrimBytes`
    -- by doing something like the following for your data type:
    --
    -- @
    --   instance PrimBytes a => Storable a where
    --       sizeOf = bSizeOf
    --       alignment = bAlignOf
    --       peekElemOff = bPeekElemOff
    --       pokeElemOff = bPokeElemOff
    --       peekByteOff = bPeekByteOff
    --       pokeByteOff = bPokeByteOff
    --       peek = bPeek
    --       poke = bPoke
    -- @
  , bPeekElemOff, bPokeElemOff, bPeekByteOff, bPokeByteOff, bPeek, bPoke
    -- * Specialization tools
  , PrimTag (..), primTag
  ) where

#include "MachDeps.h"

import           Data.Kind            (Type)
import           Data.Proxy           (Proxy (..))
import           Data.Type.Equality   ((:~:) (..))
import qualified Data.Type.List       as L
import           Data.Type.Lits
import           Foreign.C.Types
import           GHC.Exts
import           GHC.Generics
import           GHC.Int
import           GHC.IO               (IO (..))
import           GHC.Stable
import           GHC.Word
import           Numeric.Dimensions
import qualified Numeric.Tuple.Lazy   as TL
import qualified Numeric.Tuple.Strict as TS
import           Text.Read            (readMaybe)

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
    -- | Store content of a data type in a primitive byte array
    --   (should be used together with @byteOffset@ function).
    --
    --   In contrast to `getBytes`, this function returns a pinned byte array,
    --   aligned to the @byteAlign@ bytes of this data.
    --
    --   Note, GC guarantees not to move the created array.
    --   While this is very useful sometimes, it incurs a certain performance penalty.
    getBytesPinned :: a -> ByteArray#
    getBytesPinned a = case runRW#
       ( \s0 -> case newAlignedPinnedByteArray# (byteSize a) (byteAlign a) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr (writeBytes marr 0# a s1)
       ) of (# _, r #) -> r
    {-# NOINLINE getBytesPinned #-}
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
    --   It should be a multiple of @byteAlign@ for indexing functions to operate
    --   correctly.
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
    {-# INLINE byteOffset #-}

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
    --   (which is @byteSize a@ and should be a multiple of @byteAlign a@).
    indexArray :: ByteArray# -> Int# -> a
    indexArray ba i = fromBytes (i *# byteSize @a undefined) ba
    {-# INLINE indexArray #-}

    -- | Read a mutable array given an element offset
    --   (which is @byteSize a@ and should be a multiple of @byteAlign a@).
    readArray  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    readArray ba i = readBytes ba (i *# byteSize @a undefined)
    {-# INLINE readArray #-}

    -- | Write a mutable array given an element offset
    --   (which is @byteSize a@ and should be a multiple of @byteAlign a@).
    writeArray :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
    writeArray ba i = writeBytes ba (i *# byteSize @a undefined)
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
    byteSize a = gbyteSize proxy# 0## 0# (from a) `roundUpInt` byteAlign a
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

-- | Same as `Foreign.Storable.peekElemOff`: peek an element @a@ by the offset
--   measured in @byteSize a@.
--
--   Note: the size of the element must be a multiple of its alignment for
--         a correct operation of this function.
bPeekElemOff :: forall (a :: Type) . PrimBytes a => Ptr a -> Int -> IO a
bPeekElemOff (Ptr addr) (I# i)
  = IO (readAddr (plusAddr# addr (i *# byteSize @a undefined)))

-- | Same as `Foreign.Storable.pokeElemOff`: poke an element @a@ by the offset
--   measured in @byteSize a@.
--
--   Note: the size of the element must be a multiple of its alignment for
--         a correct operation of this function.
bPokeElemOff :: forall (a :: Type) . PrimBytes a => Ptr a -> Int -> a -> IO ()
bPokeElemOff (Ptr addr) (I# i) a
  = IO (\s -> (# writeAddr a (plusAddr# addr (i *# byteSize a)) s, () #))

-- | Same as `Foreign.Storable.peekByteOff`: peek an element @a@ by the offset
--   measured in bytes.
--
--   Note: you'd better be sure the address is a multiple of
--         the data alignment (`Foreign.Storable.peek`).
bPeekByteOff :: forall (a :: Type) (b :: Type) . PrimBytes a => Ptr b -> Int -> IO a
bPeekByteOff (Ptr addr) (I# i)
  = IO (readAddr (plusAddr# addr i))

-- | Same as `Foreign.Storable.pokeByteOff`: poke an element @a@ by the offset
--   measured in bytes.
--
--   Note: you'd better be sure the address is a multiple of
--         the data alignment (`Foreign.Storable.peek`).
bPokeByteOff :: forall (a :: Type) (b :: Type) . PrimBytes a => Ptr b -> Int -> a -> IO ()
bPokeByteOff (Ptr addr) (I# i) a
  = IO (\s -> (# writeAddr a (plusAddr# addr i) s, () #))

-- | Same as `Foreign.Storable.peek`: read a data from a pointer.
--
--   Note: you'd better be sure the address is a multiple of
--         the data alignment (`Foreign.Storable.peek`).
bPeek :: forall (a :: Type) . PrimBytes a => Ptr a -> IO a
bPeek (Ptr addr) = IO (readAddr addr)

-- | Same as `Foreign.Storable.poke`: write a data to a pointer.
--
--   Note: you'd better be sure the address is a multiple of
--         the data alignment (`Foreign.Storable.peek`).
bPoke :: forall (a :: Type) . PrimBytes a => Ptr a -> a -> IO ()
bPoke (Ptr addr) a = IO (\s -> (# writeAddr a addr s, () #))

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
    gbyteFieldOffset _ _ _ _ _ = negateInt# 1#
    {-# INLINE gbyteFieldOffset #-}

instance GPrimBytes V1 where
    gfromBytes _ _ _ _ _ = undefined
    greadBytes _ _ _ _ _ s = (# s, undefined #)
    gwriteBytes _ _ _ _ _ _ s = s
    greadAddr _ _ _ _ s = (# s, undefined #)
    gwriteAddr _ _ _ _ _ s = s
    gbyteSize _ _ ps _ = ps
    gbyteAlign _ _ = 1#

instance GPrimBytes U1 where
    gfromBytes _ _ _ _ _ = U1
    greadBytes _ _ _ _ _ s = (# s, U1 #)
    gwriteBytes _ _ _ _ _ _ s = s
    greadAddr _ _ _ _ s = (# s, U1 #)
    gwriteAddr _ _ _ _ _ s = s
    gbyteSize _ _ ps _ = ps
    gbyteAlign _ _ = 1#

getGOff :: forall a . PrimBytes a
        => Int# --  parent cumulative size
        -> Int# --  original offset
        -> Int# --  new offset
getGOff ps i = i +# roundUpInt ps (byteAlign @a undefined)

instance PrimBytes a => GPrimBytes (K1 i a) where
    gfromBytes _ _ ps i ba = K1 (fromBytes (getGOff @a ps i) ba)
    greadBytes _ _ ps mba i = coerce (readBytes @a mba (getGOff @a ps i))
    gwriteBytes _ _ ps mba i = coerce (writeBytes @a mba (getGOff @a ps i))
    greadAddr _ _ ps addr = coerce (readAddr @a (plusAddr# addr (getGOff @a ps 0#)))
    gwriteAddr _ _ ps ka addr = writeAddr (unK1 ka) (plusAddr# addr (getGOff @a ps 0#))
    gbyteSize _ _ ps ~(K1 a) = roundUpInt ps (byteAlign a) +# byteSize a
    gbyteAlign _ = coerce (byteAlign @a)

instance {-# OVERLAPPING #-}
         (GPrimBytes f, KnownSymbol sn)
      => GPrimBytes (M1 S ('MetaSel ('Just sn) a b c) f) where
    gfromBytes p = coerce (gfromBytes @f p)
    greadBytes p = coerce (greadBytes @f p)
    gwriteBytes p = coerce (gwriteBytes @f p)
    greadAddr p = coerce (greadAddr @f p)
    gwriteAddr p = coerce (gwriteAddr @f p)
    gbyteSize p = coerce (gbyteSize @f p)
    gbyteAlign p = coerce (gbyteAlign @f p)
    gbyteFieldOffset p _ off (_ :: Proxy# n) ma
      | Just Refl <- sameSymbol (undefined :: Proxy n) (undefined :: Proxy sn)
        = off `roundUpInt` gbyteAlign p ma
      | otherwise
        = negateInt# 1#

instance GPrimBytes f => GPrimBytes (M1 i c f) where
    gfromBytes p = coerce (gfromBytes @f p)
    greadBytes p = coerce (greadBytes @f p)
    gwriteBytes p = coerce (gwriteBytes @f p)
    greadAddr p = coerce (greadAddr @f p)
    gwriteAddr p = coerce (gwriteAddr @f p)
    gbyteSize p = coerce (gbyteSize @f p)
    gbyteAlign p = coerce (gbyteAlign @f p)
    gbyteFieldOffset p = coerce (gbyteFieldOffset @f p)

instance (GPrimBytes f, GPrimBytes g) => GPrimBytes (f :*: g) where
    gfromBytes p t ps i ba = x :*: y
      where
        x = gfromBytes p t ps i ba
        y = gfromBytes p t (gbyteSize p t ps x) i ba
    greadBytes p t ps mba i s0
      | (# s1, x #) <- greadBytes p t ps mba i s0
      , (# s2, y #) <- greadBytes p t (gbyteSize p t ps x) mba i s1
        = (# s2, x :*: y #)
    gwriteBytes p t ps mba off (x :*: y) s0
      | s1 <- gwriteBytes p t ps mba off x s0
      , s2 <- gwriteBytes p t (gbyteSize p t ps x) mba off y s1
        = s2
    greadAddr p t ps addr s0
      | (# s1, x #) <- greadAddr p t ps addr s0
      , (# s2, y #) <- greadAddr p t (gbyteSize p t ps x) addr s1
        = (# s2, x :*: y #)
    gwriteAddr p t ps (x :*: y) addr s0
      | s1 <- gwriteAddr p t ps x addr s0
      , s2 <- gwriteAddr p t (gbyteSize p t ps x) y addr s1
        = s2
    gbyteSize p t ps ~(x :*: y) = gbyteSize p t (gbyteSize p t ps x) y
    gbyteAlign p ~(x :*: y) = gbyteAlign p x `maxInt` gbyteAlign p y
    gbyteFieldOffset p t ps n ~(x :*: y)
      | offX <- gbyteFieldOffset p t ps n x
      , bsX  <- gbyteSize p t ps x
      , offY <- gbyteFieldOffset p t bsX n y
      = if isTrue# (offX <# 0#) then offY else offX

instance (GPrimBytes f, GPrimBytes g) => GPrimBytes (f :+: g) where
    gfromBytes p t _ off ba
      | c <- indexWord8ArrayAsWord32# ba off
        = if isTrue# (eqWord# (and# c t1) 0##)
          then L1 (gfromBytes p t1 4# off ba)
          else R1 (gfromBytes p t1 4# off ba)
      where
        t1 = upTag t
    greadBytes p t _ mba off s0
      | (# s1, c #) <- readWord8ArrayAsWord32# mba off s0
        = if isTrue# (eqWord# (and# c t1) 0##)
          then case greadBytes p t1 4# mba off s1 of
            (# s2, x #) -> (# s2, L1 x #)
          else case greadBytes p t1 4# mba off s1 of
            (# s2, y #) -> (# s2, R1 y #)
      where
        t1 = upTag t
    -- if this is the uppermost sum, overwrite the tag.
    gwriteBytes p 0## _ mba off (L1 x) s0
      | s1 <- writeWord8ArrayAsWord32# mba off 0## s0
      , s2 <- gwriteBytes p 1## 4# mba off x s1 = s2
    gwriteBytes p 0## _ mba off (R1 y) s0
      | s1 <- writeWord8ArrayAsWord32# mba off 1## s0
      , s2 <- gwriteBytes p 1## 4# mba off y s1 = s2
    -- here I know that I have written zero to the corresponding bit already
    gwriteBytes p t _ mba off (L1 x) s0
      | s1 <- gwriteBytes p (upTag t) 4# mba off x s0 = s1
    -- otherwise, carefully write a single corresponding bit
    gwriteBytes p t _ mba off (R1 y) s0
      | (# s1, c #) <- readWord8ArrayAsWord32# mba off s0
      , s2 <- writeWord8ArrayAsWord32# mba off (or# c t1) s1
      , s3 <- gwriteBytes p t1 4# mba off y s2 = s3
      where
        t1 = upTag t
    greadAddr p t _ addr s0
      | (# s1, c #) <- readWord32OffAddr# addr 0# s0
        = if isTrue# (eqWord# (and# c t1) 0##)
          then case greadAddr p t1 4# addr s1 of
            (# s2, x #) -> (# s2, L1 x #)
          else case greadAddr p t1 4# addr s1 of
            (# s2, y #) -> (# s2, R1 y #)
      where
        t1 = upTag t
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
    gbyteSize p 0## ps xy
      = maxInt
        (roundUpInt 4# (gbyteAlign p x) +# gbyteSize p 1## ps x)
        (roundUpInt 4# (gbyteAlign p y) +# gbyteSize p 1## ps y)
      where
        x = undef1 @f xy
        y = undef1 @g xy
    gbyteSize p t ps xy
      = maxInt
        (gbyteSize p (upTag t) ps (undef1 @f xy))
        (gbyteSize p (upTag t) ps (undef1 @g xy))
    gbyteAlign p xy = 4# `maxInt`
        maxInt (gbyteAlign p (undef1 @f xy))
               (gbyteAlign p (undef1 @g xy))
    -- check both branches if any of them contain the field.
    -- If there are more than one branches containing the field, the left one
    -- is preferred.
    gbyteFieldOffset p t ps n xy
      | offX <- gbyteFieldOffset p (upTag t) ps n (undef1 @f xy)
      , offY <- gbyteFieldOffset p (upTag t) ps n (undef1 @g xy)
      = if isTrue# (offX <# 0#) then offY else offX

upTag :: Word# -> Word#
upTag 0## = 1##
upTag t   = uncheckedShiftL# t 1#
{-# INLINE upTag #-}


maxInt :: Int# -> Int# -> Int#
maxInt a b = if isTrue# (a ># b) then a else b
{-# INLINE maxInt #-}

-- | Round up the first numer to a multiple of the second.
--
--   NB: this function is only used with alignment as the second number,
--       which is always a power of 2.
roundUpInt :: Int# -> Int# -> Int#
roundUpInt a b = (a +# b -# 1#) `andI#` negateInt# b
{-# INLINE roundUpInt #-}
-- It's pity that the assertion would not work due to kind of the result
-- not being Type.
-- assert (isTrue# (eqWord# (popCnt# (int2Word# b)) 1##))
--
-- The version above is optimized for second number being power of two (align)
-- The baseline implementation would be as follows:
-- roundUpInt a b = case remInt# a b of
--   0# -> a
  -- q  -> a +# b -# q

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
      = UWord (indexWord8ArrayAsWord# ba (off +# roundUpInt ps ALIGNMENT_HSWORD#))
    greadBytes _ _ ps mba off s
      = case readWord8ArrayAsWord# mba (off +# roundUpInt ps ALIGNMENT_HSWORD#) s of
          (# s1, r #) -> (# s1, UWord r #)
    gwriteBytes _ _ ps mba off x
      = writeWord8ArrayAsWord# mba (off +# roundUpInt ps ALIGNMENT_HSWORD#) (uWord# x)
    greadAddr _ _ ps a s
      = case readWordOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSWORD#)) 0# s of
          (# s', x #) -> (# s', UWord x #)
    gwriteAddr _ _ ps x a
      = writeWordOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSWORD#)) 0# (uWord# x)
    gbyteSize _ _ ps _ = roundUpInt ps ALIGNMENT_HSWORD# +# SIZEOF_HSWORD#
    gbyteAlign _ _ = ALIGNMENT_HSWORD#

#if SIZEOF_HSINT == 4
#define OFFSHIFT_I 2
#else
#define OFFSHIFT_I 3
#endif

instance GPrimBytes (URec Int) where
    gfromBytes _ _ ps off ba
      = UInt (indexWord8ArrayAsInt# ba (off +# roundUpInt ps ALIGNMENT_HSINT#))
    greadBytes _ _ ps mba off s
      = case readWord8ArrayAsInt# mba (off +# roundUpInt ps ALIGNMENT_HSINT#) s of
          (# s1, r #) -> (# s1, UInt r #)
    gwriteBytes _ _ ps mba off x
      = writeWord8ArrayAsInt# mba (off +# roundUpInt ps ALIGNMENT_HSINT#) (uInt# x)
    greadAddr _ _ ps a s
      = case readIntOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSINT#)) 0# s of
          (# s', x #) -> (# s', UInt x #)
    gwriteAddr _ _ ps x a
      = writeIntOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSINT#)) 0# (uInt# x)
    gbyteSize _ _ ps _ = roundUpInt ps ALIGNMENT_HSINT# +# SIZEOF_HSINT#
    gbyteAlign _ _ = ALIGNMENT_HSINT#

#if SIZEOF_HSFLOAT == 4
#define OFFSHIFT_F 2
#else
#define OFFSHIFT_F 3
#endif

instance GPrimBytes (URec Float) where
    gfromBytes _ _ ps off ba
      = UFloat (indexWord8ArrayAsFloat# ba (off +# roundUpInt ps ALIGNMENT_HSFLOAT#))
    greadBytes _ _ ps mba off s
      = case readWord8ArrayAsFloat# mba (off +# roundUpInt ps ALIGNMENT_HSFLOAT#) s of
          (# s1, r #) -> (# s1, UFloat r #)
    gwriteBytes _ _ ps mba off x
      = writeWord8ArrayAsFloat# mba (off +# roundUpInt ps ALIGNMENT_HSFLOAT#) (uFloat# x)
    greadAddr _ _ ps a s
      = case readFloatOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSFLOAT#)) 0# s of
          (# s', x #) -> (# s', UFloat x #)
    gwriteAddr _ _ ps x a
      = writeFloatOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSFLOAT#)) 0# (uFloat# x)
    gbyteSize _ _ ps _ = roundUpInt ps ALIGNMENT_HSFLOAT# +# SIZEOF_HSFLOAT#
    gbyteAlign _ _ = ALIGNMENT_HSFLOAT#

#if SIZEOF_HSDOUBLE == 4
#define OFFSHIFT_D 2
#else
#define OFFSHIFT_D 3
#endif

instance GPrimBytes (URec Double) where
    gfromBytes _ _ ps off ba
      = UDouble (indexWord8ArrayAsDouble# ba (off +# roundUpInt ps ALIGNMENT_HSDOUBLE#))
    greadBytes _ _ ps mba off s
      = case readWord8ArrayAsDouble# mba (off +# roundUpInt ps ALIGNMENT_HSDOUBLE#) s of
          (# s1, r #) -> (# s1, UDouble r #)
    gwriteBytes _ _ ps mba off x
      = writeWord8ArrayAsDouble# mba (off +# roundUpInt ps ALIGNMENT_HSDOUBLE#) (uDouble# x)
    greadAddr _ _ ps a s
      = case readDoubleOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSDOUBLE#)) 0# s of
          (# s', x #) -> (# s', UDouble x #)
    gwriteAddr _ _ ps x a
      = writeDoubleOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSDOUBLE#)) 0# (uDouble# x)
    gbyteSize _ _ ps _ = roundUpInt ps ALIGNMENT_HSDOUBLE# +# SIZEOF_HSDOUBLE#
    gbyteAlign _ _ = ALIGNMENT_HSDOUBLE#

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
      = UChar (indexWord8ArrayAsWideChar# ba (off +# roundUpInt ps ALIGNMENT_HSCHAR#))
    greadBytes _ _ ps mba off s
      = case readWord8ArrayAsWideChar# mba (off +# roundUpInt ps ALIGNMENT_HSCHAR#) s of
          (# s1, r #) -> (# s1, UChar r #)
    gwriteBytes _ _ ps mba off x
      = writeWord8ArrayAsWideChar# mba (off +# roundUpInt ps ALIGNMENT_HSCHAR#) (uChar# x)
    greadAddr _ _ ps a s
      = case readWideCharOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSCHAR#)) 0# s of
          (# s', x #) -> (# s', UChar x #)
    gwriteAddr _ _ ps x a
      = writeWideCharOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSCHAR#)) 0# (uChar# x)
    gbyteSize _ _ ps _ = roundUpInt ps ALIGNMENT_HSCHAR# +# SIZEOF_HSCHAR#
    gbyteAlign _ _ = ALIGNMENT_HSCHAR#

#if SIZEOF_HSPTR == 4
#define OFFSHIFT_P 2
#else
#define OFFSHIFT_P 3
#endif

instance GPrimBytes (URec (Ptr ())) where
    gfromBytes _ _ ps off ba
      = UAddr (indexWord8ArrayAsAddr# ba (off +# roundUpInt ps ALIGNMENT_HSPTR#))
    greadBytes _ _ ps mba off s
      = case readWord8ArrayAsAddr# mba (off +# roundUpInt ps ALIGNMENT_HSPTR#) s of
          (# s1, r #) -> (# s1, UAddr r #)
    gwriteBytes _ _ ps mba off x
      = writeWord8ArrayAsAddr# mba (off +# roundUpInt ps ALIGNMENT_HSPTR#) (uAddr# x)
    greadAddr _ _ ps a s
      = case readAddrOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSPTR#)) 0# s of
          (# s', x #) -> (# s', UAddr x #)
    gwriteAddr _ _ ps x a
      = writeAddrOffAddr# (plusAddr# a (roundUpInt ps ALIGNMENT_HSPTR#)) 0# (uAddr# x)
    gbyteSize _ _ ps _ = roundUpInt ps ALIGNMENT_HSPTR# +# SIZEOF_HSPTR#
    gbyteAlign _ _ = ALIGNMENT_HSPTR#




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
      = W# (indexWord8ArrayAsWord# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsWord# mba off s of (# s', r #) -> (# s', W# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (W# x)
      = writeWord8ArrayAsWord# mba off x
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
      = I# (indexWord8ArrayAsInt# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsInt# mba off s of (# s', r #) -> (# s', I# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (I# x)
      = writeWord8ArrayAsInt# mba off x
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
      = F# (indexWord8ArrayAsFloat# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsFloat# mba off s of (# s', r #) -> (# s', F# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (F# x)
      = writeWord8ArrayAsFloat# mba off x
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
      = D# (indexWord8ArrayAsDouble# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsDouble# mba off s of (# s', r #) -> (# s', D# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (D# x)
      = writeWord8ArrayAsDouble# mba off x
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
      = Ptr (indexWord8ArrayAsAddr# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsAddr# mba off s of (# s', r #) -> (# s', Ptr r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (Ptr x)
      = writeWord8ArrayAsAddr# mba off x
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
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = Ptr (indexAddrArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readAddrArray# mba i s of (# s', x #) -> (# s', Ptr x #)
    {-# INLINE readArray #-}
    writeArray mba i (Ptr x) = writeAddrArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes (FunPtr a) where
    type PrimFields (FunPtr a) = '[]
    getBytes (FunPtr x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSFUNPTR# s0 of
         (# s1, marr #) -> case writeAddrArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = FunPtr (indexWord8ArrayAsAddr# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsAddr# mba off s of (# s', r #) -> (# s', FunPtr r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (FunPtr x)
      = writeWord8ArrayAsAddr# mba off x
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readAddrOffAddr# a 0# s of (# s', x #) -> (# s', FunPtr x #)
    {-# INLINE readAddr #-}
    writeAddr (FunPtr x) a
      = writeAddrOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSFUNPTR#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSFUNPTR#
    {-# INLINE byteAlign #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = FunPtr (indexAddrArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readAddrArray# mba i s of (# s', x #) -> (# s', FunPtr x #)
    {-# INLINE readArray #-}
    writeArray mba i (FunPtr x) = writeAddrArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes (StablePtr a) where
    type PrimFields (StablePtr a) = '[]
    getBytes (StablePtr x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSSTABLEPTR# s0 of
         (# s1, marr #) -> case writeStablePtrArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = StablePtr (indexWord8ArrayAsStablePtr# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsStablePtr# mba off s of (# s', r #) -> (# s', StablePtr r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (StablePtr x)
      = writeWord8ArrayAsStablePtr# mba off x
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readStablePtrOffAddr# a 0# s of (# s', x #) -> (# s', StablePtr x #)
    {-# INLINE readAddr #-}
    writeAddr (StablePtr x) a
      = writeStablePtrOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSSTABLEPTR#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSSTABLEPTR#
    {-# INLINE byteAlign #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = StablePtr (indexStablePtrArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readStablePtrArray# mba i s of (# s', x #) -> (# s', StablePtr x #)
    {-# INLINE readArray #-}
    writeArray mba i (StablePtr x) = writeStablePtrArray# mba i x
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
      = I16# (indexWord8ArrayAsInt16# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsInt16# mba off s of (# s', r #) -> (# s', I16# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (I16# x)
      = writeWord8ArrayAsInt16# mba off x
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
      = I32# (indexWord8ArrayAsInt32# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsInt32# mba off s of (# s', r #) -> (# s', I32# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (I32# x)
      = writeWord8ArrayAsInt32# mba off x
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
      = I64# (indexWord8ArrayAsInt64# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsInt64# mba off s of (# s', r #) -> (# s', I64# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (I64# x)
      = writeWord8ArrayAsInt64# mba off x
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
      = W16# (indexWord8ArrayAsWord16# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsWord16# mba off s of (# s', r #) -> (# s', W16# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (W16# x)
      = writeWord8ArrayAsWord16# mba off x
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
      = W32# (indexWord8ArrayAsWord32# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsWord32# mba off s of (# s', r #) -> (# s', W32# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (W32# x)
      = writeWord8ArrayAsWord32# mba off x
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
      = W64# (indexWord8ArrayAsWord64# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsWord64# mba off s of (# s', r #) -> (# s', W64# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (W64# x)
      = writeWord8ArrayAsWord64# mba off x
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
      = C# (indexWord8ArrayAsWideChar# ba off)
    {-# INLINE fromBytes #-}
    readBytes mba off s
      = case readWord8ArrayAsWideChar# mba off s of (# s', r #) -> (# s', C# r #)
    {-# INLINE readBytes #-}
    writeBytes mba off (C# x)
      = writeWord8ArrayAsWideChar# mba off x
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
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba i = C# (indexWideCharArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWideCharArray# mba i s of (# s', x #) -> (# s', C# x #)
    {-# INLINE readArray #-}
    writeArray mba i (C# x) = writeWideCharArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes (Idx (x :: k)) where
    type PrimFields (Idx x) = '[]
    getBytes = unsafeCoerce# (getBytes @Word)
    {-# INLINE getBytes #-}
    fromBytes = unsafeCoerce# (fromBytes @Word)
    {-# INLINE fromBytes #-}
    readBytes = unsafeCoerce# (readBytes @Word)
    {-# INLINE readBytes #-}
    writeBytes = unsafeCoerce# (writeBytes @Word)
    {-# INLINE writeBytes #-}
    readAddr = unsafeCoerce# (readAddr @Word)
    {-# INLINE readAddr #-}
    writeAddr = unsafeCoerce# (writeAddr @Word)
    {-# INLINE writeAddr #-}
    byteSize = unsafeCoerce# (byteSize @Word)
    {-# INLINE byteSize #-}
    byteAlign = unsafeCoerce# (byteAlign @Word)
    {-# INLINE byteAlign #-}
    byteFieldOffset b = unsafeCoerce# (byteFieldOffset @Word b)
    {-# INLINE byteFieldOffset #-}
    indexArray = unsafeCoerce# (indexArray @Word)
    {-# INLINE indexArray #-}
    readArray = unsafeCoerce# (readArray @Word)
    {-# INLINE readArray #-}
    writeArray = unsafeCoerce# (writeArray @Word)
    {-# INLINE writeArray #-}

deriving instance PrimBytes CChar
deriving instance PrimBytes CSChar
deriving instance PrimBytes CUChar
deriving instance PrimBytes CShort
deriving instance PrimBytes CUShort
deriving instance PrimBytes CInt
deriving instance PrimBytes CUInt
deriving instance PrimBytes CLong
deriving instance PrimBytes CULong
deriving instance PrimBytes CPtrdiff
deriving instance PrimBytes CSize
deriving instance PrimBytes CWchar
deriving instance PrimBytes CSigAtomic
deriving instance PrimBytes CLLong
deriving instance PrimBytes CULLong
deriving instance PrimBytes CBool
deriving instance PrimBytes CIntPtr
deriving instance PrimBytes CUIntPtr
deriving instance PrimBytes CIntMax
deriving instance PrimBytes CUIntMax
deriving instance PrimBytes CClock
deriving instance PrimBytes CTime
deriving instance PrimBytes CUSeconds
deriving instance PrimBytes CSUSeconds
deriving instance PrimBytes CFloat
deriving instance PrimBytes CDouble

anyList :: forall (k :: Type) (xs :: [k])
        . RepresentableList xs => [Any]
anyList = unsafeCoerce# (tList @xs)
{-# INLINE anyList #-}

instance RepresentableList xs => PrimBytes (Idxs (xs :: [k])) where
    type PrimFields (Idxs xs) = '[]
    fromBytes off ba = unsafeCoerce# (go off (anyList @_ @xs))
      where
        go _ []       = []
        go i (_ : ls) = W# (indexWord8ArrayAsWord# ba i) : go (i +# SIZEOF_HSWORD#) ls
    {-# INLINE fromBytes #-}
    readBytes mba = unsafeCoerce# (go (anyList @_ @xs))
      where
        go [] _ s0 = (# s0, [] #)
        go (_ : ls) i s0
          | (# s1, w  #) <- readWord8ArrayAsWord# mba i s0
          , (# s2, ws #) <- go ls (i +# SIZEOF_HSWORD#) s1
            = (# s2, W# w : ws #)
    {-# INLINE readBytes #-}
    writeBytes mba off = go off . listIdxs
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (i +# SIZEOF_HSWORD#) xs (writeWord8ArrayAsWord# mba i x s)
    {-# INLINE writeBytes #-}
    readAddr addr = unsafeCoerce# (go addr (anyList @_ @xs))
      where
        go :: forall s . Addr# -> [Any] -> State# s -> (# State# s, [Word] #)
        go _ [] s0 = (# s0, [] #)
        go i (_ : ls) s0
          | (# s1, w #)  <- readWordOffAddr# i 0# s0
          , (# s2, xs #) <- go (plusAddr# i SIZEOF_HSWORD#) ls s1
            = (# s2, W# w : xs #)
    {-# INLINE readAddr #-}
    writeAddr is addr
        = go addr (listIdxs is)
      where
        go :: forall s . Addr# -> [Word] -> State# s -> State# s
        go _ [] s         = s
        go i (W# x :xs) s = go (plusAddr# i SIZEOF_HSWORD#) xs
                               (writeWordOffAddr# i 0# x s)
    {-# INLINE writeAddr #-}
    byteSize _ = case dimVal (order' @xs) of
      W# n -> byteSize (undefined :: Idx x) *# word2Int# n
    {-# INLINE byteSize #-}
    byteAlign _ = byteAlign (undefined :: Idx x)
    {-# INLINE byteAlign #-}
    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}
    indexArray ba off
      | n@(W# n#) <- dimVal (order' @xs)
        = unsafeCoerce# (go (off *# word2Int# n#) n)
      where
        go _ 0 = []
        go i n = W# (indexWordArray# ba i) : go (i +# 1#) (n-1)
    {-# INLINE indexArray #-}
    readArray mba off s
      | n@(W# n#) <- dimVal (order' @xs)
        = unsafeCoerce# (go (off *# word2Int# n#) n s)
      where
        go _ 0 s0 = (# s0, [] #)
        go i n s0
          | (# s1, w #)  <- readWordArray# mba i s0
          , (# s2, xs #) <- go (i +# 1#) (n-1) s1
            = (# s2, W# w : xs #)
    {-# INLINE readArray #-}
    writeArray mba off is
      | W# n# <- dimVal (order' @xs)
        = go (off *# word2Int# n#) (listIdxs is)
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (i +# 1#) xs (writeWordArray# mba i x s)
    {-# INLINE writeArray #-}

type family TupleFields (n :: Nat) (xs :: [Type]) :: [Symbol] where
    TupleFields _ '[] = '[]
    TupleFields n (_ ': xs) = ShowNat n ': TupleFields (n + 1) xs

instance ( RepresentableList xs
         , L.All PrimBytes xs
         ) => PrimBytes (TL.Tuple xs) where
    type PrimFields (TL.Tuple xs) = TupleFields 1 xs
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
    byteFieldOffset p = unsafeCoerce# (byteFieldOffset @(TS.Tuple xs) p)
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
    type PrimFields (TS.Tuple xs) = TupleFields 1 xs
    fromBytes off ba = go 0# (tList @xs)
      where
        go :: L.All PrimBytes ds
           => Int# -> TypeList ds -> TS.Tuple ds
        go _ Empty = Empty
        go n (t :* ts@TypeList)
          | x <- undefP t
          , n' <- roundUpInt n (byteAlign x)
          = TS.Id (fromBytes (off +# n') ba) :* go (n' +# byteSize x) ts
    {-# INLINE fromBytes #-}
    readBytes mb off = go mb 0# (tList @xs)
      where
        go :: L.All PrimBytes ds
           => MutableByteArray# s
           -> Int# -> TypeList ds -> State# s -> (# State# s, TS.Tuple ds #)
        go _ _ Empty s0 = (# s0, Empty #)
        go mba n (t :* ts@TypeList) s0
          | x <- undefP t
          , n' <- roundUpInt n (byteAlign x)
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
          | n' <- roundUpInt n (byteAlign x)
          = go mb (n' +# byteSize x) xs ts (writeBytes mb (off +# n') x s)
    {-# INLINE writeBytes #-}
    readAddr addr = go 0# (tList @xs)
      where
        go :: L.All PrimBytes ds
           => Int# -> TypeList ds -> State# s -> (# State# s, TS.Tuple ds #)
        go _ Empty s0 = (# s0, Empty #)
        go n (t :* ts@TypeList) s0
          | x <- undefP t
          , n' <- roundUpInt n (byteAlign x)
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
          | n' <- roundUpInt n (byteAlign x)
          = go (n' +# byteSize x) xs ts (writeAddr x (plusAddr# addr n') s)
    {-# INLINE writeAddr #-}
    byteSize _ = go 0# 1# (tList @xs)
      where
        go :: L.All PrimBytes ys => Int# -> Int# -> TypeList ys -> Int#
        go s a Empty     = s `roundUpInt` a
        go s a (p :* ps) = let x = undefP p
                               xa = byteAlign x
                           in  go ( roundUpInt s xa +# byteSize x)
                                  ( maxInt a xa ) ps
    {-# INLINE byteSize #-}
    byteAlign _ = go (tList @xs)
      where
        go :: L.All PrimBytes ys => TypeList ys -> Int#
        go Empty     = 0#
        go (p :* ps) = maxInt (byteAlign (undefP p)) (go ps)
    {-# INLINE byteAlign #-}
    byteFieldOffset name _
      | Just n <- readMaybe $ symbolVal' name
        = go (n-1) 0# (tList @xs)
      | otherwise = negateInt# 1#
      where
        go :: L.All PrimBytes ys => Word -> Int# -> TypeList ys -> Int#
        go 0 s (p :* _)  = s `roundUpInt` byteAlign (undefP p)
        go n s (p :* ps) = let x = undefP p
                           in  go (n-1) ( roundUpInt s (byteAlign x) +# byteSize x) ps
        go _ _ Empty     = negateInt# 1#
    {-# INLINE byteFieldOffset #-}


undefP :: Proxy p -> p
undefP = const undefined
{-# INLINE undefP #-}


instance PrimBytes ()
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



#if !(MIN_VERSION_base(4,12,0))
-- these functions were introduced in base-4.12.0

writeWord8ArrayAsWideChar# :: MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsWideChar# mba off = writeWideCharArray# mba (uncheckedIShiftRL# off OFFSHIFT_C#)
{-# INLINE writeWord8ArrayAsWideChar# #-}

writeWord8ArrayAsAddr# :: MutableByteArray# d -> Int# -> Addr# -> State# d -> State# d
writeWord8ArrayAsAddr# mba off = writeAddrArray# mba (uncheckedIShiftRL# off OFFSHIFT_P#)
{-# INLINE writeWord8ArrayAsAddr# #-}

writeWord8ArrayAsStablePtr# :: MutableByteArray# d -> Int# -> StablePtr# a -> State# d -> State# d
writeWord8ArrayAsStablePtr# mba off = writeStablePtrArray# mba (uncheckedIShiftRL# off OFFSHIFT_P#)
{-# INLINE writeWord8ArrayAsStablePtr# #-}

writeWord8ArrayAsFloat# :: MutableByteArray# d -> Int# -> Float# -> State# d -> State# d
writeWord8ArrayAsFloat# mba off = writeFloatArray# mba (uncheckedIShiftRL# off OFFSHIFT_F#)
{-# INLINE writeWord8ArrayAsFloat# #-}

writeWord8ArrayAsDouble# :: MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeWord8ArrayAsDouble# mba off = writeDoubleArray# mba (uncheckedIShiftRL# off OFFSHIFT_D#)
{-# INLINE writeWord8ArrayAsDouble# #-}

writeWord8ArrayAsInt16# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt16# mba off = writeInt16Array# mba (uncheckedIShiftRL# off 1#)
{-# INLINE writeWord8ArrayAsInt16# #-}

writeWord8ArrayAsInt32# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt32# mba off = writeInt32Array# mba (uncheckedIShiftRL# off 2#)
{-# INLINE writeWord8ArrayAsInt32# #-}

writeWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt64# mba off = writeInt64Array# mba (uncheckedIShiftRL# off 3#)
{-# INLINE writeWord8ArrayAsInt64# #-}

writeWord8ArrayAsInt# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt# mba off = writeIntArray# mba (uncheckedIShiftRL# off OFFSHIFT_I#)
{-# INLINE writeWord8ArrayAsInt# #-}

writeWord8ArrayAsWord16# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord16# mba off = writeWord16Array# mba (uncheckedIShiftRL# off 1#)
{-# INLINE writeWord8ArrayAsWord16# #-}

writeWord8ArrayAsWord32# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord32# mba off = writeWord32Array# mba (uncheckedIShiftRL# off 2#)
{-# INLINE writeWord8ArrayAsWord32# #-}

writeWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord64# mba off = writeWord64Array# mba (uncheckedIShiftRL# off 3#)
{-# INLINE writeWord8ArrayAsWord64# #-}

writeWord8ArrayAsWord# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord# mba off = writeWordArray# mba (uncheckedIShiftRL# off OFFSHIFT_W#)
{-# INLINE writeWord8ArrayAsWord# #-}

readWord8ArrayAsWideChar# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsWideChar# mba off = readWideCharArray# mba (uncheckedIShiftRL# off OFFSHIFT_C#)
{-# INLINE readWord8ArrayAsWideChar# #-}

readWord8ArrayAsAddr# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Addr# #)
readWord8ArrayAsAddr# mba off = readAddrArray# mba (uncheckedIShiftRL# off OFFSHIFT_P#)
{-# INLINE readWord8ArrayAsAddr# #-}

readWord8ArrayAsStablePtr# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, StablePtr# a #)
readWord8ArrayAsStablePtr# mba off = readStablePtrArray# mba (uncheckedIShiftRL# off OFFSHIFT_P#)
{-# INLINE readWord8ArrayAsStablePtr# #-}

readWord8ArrayAsFloat# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Float# #)
readWord8ArrayAsFloat# mba off = readFloatArray# mba (uncheckedIShiftRL# off OFFSHIFT_F#)
{-# INLINE readWord8ArrayAsFloat# #-}

readWord8ArrayAsDouble# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readWord8ArrayAsDouble# mba off = readDoubleArray# mba (uncheckedIShiftRL# off OFFSHIFT_D#)
{-# INLINE readWord8ArrayAsDouble# #-}

readWord8ArrayAsInt16# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt16# mba off = readInt16Array# mba (uncheckedIShiftRL# off 1#)
{-# INLINE readWord8ArrayAsInt16# #-}

readWord8ArrayAsInt32# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt32# mba off = readInt32Array# mba (uncheckedIShiftRL# off 2#)
{-# INLINE readWord8ArrayAsInt32# #-}

readWord8ArrayAsInt64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt64# mba off = readInt64Array# mba (uncheckedIShiftRL# off 3#)
{-# INLINE readWord8ArrayAsInt64# #-}

readWord8ArrayAsInt# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt# mba off = readIntArray# mba (uncheckedIShiftRL# off OFFSHIFT_I#)
{-# INLINE readWord8ArrayAsInt# #-}

readWord8ArrayAsWord16# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord16# mba off = readWord16Array# mba (uncheckedIShiftRL# off 1#)
{-# INLINE readWord8ArrayAsWord16# #-}

readWord8ArrayAsWord32# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord32# mba off = readWord32Array# mba (uncheckedIShiftRL# off 2#)
{-# INLINE readWord8ArrayAsWord32# #-}

readWord8ArrayAsWord64# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord64# mba off = readWord64Array# mba (uncheckedIShiftRL# off 3#)
{-# INLINE readWord8ArrayAsWord64# #-}

readWord8ArrayAsWord# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord# mba off = readWordArray# mba (uncheckedIShiftRL# off OFFSHIFT_W#)
{-# INLINE readWord8ArrayAsWord# #-}

indexWord8ArrayAsWideChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsWideChar# ba off = indexWideCharArray# ba (uncheckedIShiftRL# off OFFSHIFT_C#)
{-# INLINE indexWord8ArrayAsWideChar# #-}

indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#
indexWord8ArrayAsAddr# ba off = indexAddrArray# ba (uncheckedIShiftRL# off OFFSHIFT_P#)
{-# INLINE indexWord8ArrayAsAddr# #-}

indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a
indexWord8ArrayAsStablePtr# ba off = indexStablePtrArray# ba (uncheckedIShiftRL# off OFFSHIFT_P#)
{-# INLINE indexWord8ArrayAsStablePtr# #-}

indexWord8ArrayAsFloat# :: ByteArray# -> Int# -> Float#
indexWord8ArrayAsFloat# ba off = indexFloatArray# ba (uncheckedIShiftRL# off OFFSHIFT_F#)
{-# INLINE indexWord8ArrayAsFloat# #-}

indexWord8ArrayAsDouble# :: ByteArray# -> Int# -> Double#
indexWord8ArrayAsDouble# ba off = indexDoubleArray# ba (uncheckedIShiftRL# off OFFSHIFT_D#)
{-# INLINE indexWord8ArrayAsDouble# #-}

indexWord8ArrayAsInt16# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt16# ba off = indexInt16Array# ba (uncheckedIShiftRL# off 1#)
{-# INLINE indexWord8ArrayAsInt16# #-}

indexWord8ArrayAsInt32# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt32# ba off = indexInt32Array# ba (uncheckedIShiftRL# off 2#)
{-# INLINE indexWord8ArrayAsInt32# #-}

indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt64# ba off = indexInt64Array# ba (uncheckedIShiftRL# off 3#)
{-# INLINE indexWord8ArrayAsInt64# #-}

indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt# ba off = indexIntArray# ba (uncheckedIShiftRL# off OFFSHIFT_I#)
{-# INLINE indexWord8ArrayAsInt# #-}

indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord16# ba off = indexWord16Array# ba (uncheckedIShiftRL# off 1#)
{-# INLINE indexWord8ArrayAsWord16# #-}

indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord32# ba off = indexWord32Array# ba (uncheckedIShiftRL# off 2#)
{-# INLINE indexWord8ArrayAsWord32# #-}

indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord64# ba off = indexWord64Array# ba (uncheckedIShiftRL# off 3#)
{-# INLINE indexWord8ArrayAsWord64# #-}

indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord# ba off = indexWordArray# ba (uncheckedIShiftRL# off OFFSHIFT_W#)
{-# INLINE indexWord8ArrayAsWord# #-}

#endif
