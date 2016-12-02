{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Commons
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Commons
  ( PrimBytes (..)
  , FloatBytes (..)
  , DoubleBytes (..)
  , IntBytes (..)
  , WordBytes (..)
  ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import GHC.Base (runRW#)
import GHC.Ptr
import GHC.Prim
import GHC.Types
import GHC.Int
import GHC.Word
import Foreign.Storable


class PrimBytes a where
  -- | Store content of a data type in a primitive byte array
  toBytes :: a -> ByteArray#
  -- | Load content of a data type from a primitive byte array
  fromBytes :: ByteArray# -> a
  -- | Size of a data type in bytes
  byteSize :: a -> Int#
  -- | Alignment of a data type in bytes
  byteAlign :: a -> Int#

-- | Primitive indexing. No checks, no safety.
class FloatBytes a where
  -- | Primitive get Float# (element offset)
  ixF :: Int# -> a -> Float#

-- | Primitive indexing. No checks, no safety.
class DoubleBytes a where
  -- | Primitive get Double# (element offset)
  ixD :: Int# -> a -> Double#

-- | Primitive indexing. No checks, no safety.
class IntBytes a where
  -- | Primitive get Int# (element offset)
  ixI :: Int# -> a -> Int#

-- | Primitive indexing. No checks, no safety.
class WordBytes a where
  -- | Primitive get Word# (element offset)
  ixW :: Int# -> a -> Word#

instance PrimBytes a => Storable a where
  sizeOf x = I# (byteSize x)
  alignment x = I# (byteAlign x)
  peekElemOff ptr (I# offset) = peekByteOff ptr (I# (offset *# byteSize (undefined :: a)))
  pokeElemOff ptr (I# offset) = pokeByteOff ptr (I# (offset *# byteSize (undefined :: a)))
  peekByteOff (Ptr addr) (I# offset) = IO $ \s0 -> case newByteArray# bsize s0 of
        (# s1, marr #) -> case copyAddrToByteArray# (addr `plusAddr#` offset) marr 0# bsize s1 of
          s2 -> case unsafeFreezeByteArray# marr s2 of
            (# s3, arr #) -> (# s3, fromBytes arr #)
    where
      bsize = byteSize (undefined :: a)
  pokeByteOff (Ptr addr) (I# offset) x = IO $ \s0 -> case copyByteArrayToAddr# (toBytes x)
                                                                               0#
                                                                               (addr `plusAddr#` offset)
                                                                               bsize s0 of
       s2 -> (# s2, () #)
    where
      bsize = byteSize (undefined :: a)
  peek ptr = peekByteOff ptr 0
  poke ptr = pokeByteOff ptr 0


instance PrimBytes Float where
  toBytes v@(F# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = F# (indexFloatArray# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}

instance FloatBytes Float where
  ixF _ (F# x) = x
  {-# INLINE ixF #-}

instance PrimBytes Double where
  toBytes v@(D# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = D# (indexDoubleArray# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSDOUBLE#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSDOUBLE#
  {-# INLINE byteAlign #-}

instance DoubleBytes Double where
  ixD _ (D# x) = x
  {-# INLINE ixD #-}

instance PrimBytes Int where
  toBytes v@(I# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = I# (indexIntArray# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSINT#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSINT#
  {-# INLINE byteAlign #-}

instance IntBytes Int where
  ixI _ (I# x) = x
  {-# INLINE ixI #-}

instance PrimBytes Int8 where
  toBytes v@(I8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = I8# (indexInt8Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT8#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT8#
  {-# INLINE byteAlign #-}

instance IntBytes Int8 where
  ixI _ (I8# x) = x
  {-# INLINE ixI #-}

instance PrimBytes Int16 where
  toBytes v@(I16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = I16# (indexInt16Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT16#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT16#
  {-# INLINE byteAlign #-}

instance IntBytes Int16 where
  ixI _ (I16# x) = x
  {-# INLINE ixI #-}

instance PrimBytes Int32 where
  toBytes v@(I32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = I32# (indexInt32Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT32#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT32#
  {-# INLINE byteAlign #-}

instance IntBytes Int32 where
  ixI _ (I32# x) = x
  {-# INLINE ixI #-}

instance PrimBytes Int64 where
  toBytes v@(I64# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = I64# (indexInt64Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT64#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT64#
  {-# INLINE byteAlign #-}

instance IntBytes Int64 where
  ixI _ (I64# x) = x
  {-# INLINE ixI #-}

instance PrimBytes Word where
  toBytes v@(W# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = W# (indexWordArray# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSWORD#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSWORD#
  {-# INLINE byteAlign #-}

instance WordBytes Word where
  ixW _ (W# x) = x
  {-# INLINE ixW #-}

instance PrimBytes Word8 where
  toBytes v@(W8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = W8# (indexWord8Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD8#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD8#
  {-# INLINE byteAlign #-}

instance WordBytes Word8 where
  ixW _ (W8# x) = x
  {-# INLINE ixW #-}

instance PrimBytes Word16 where
  toBytes v@(W16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = W16# (indexWord16Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD16#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD16#
  {-# INLINE byteAlign #-}

instance WordBytes Word16 where
  ixW _ (W16# x) = x
  {-# INLINE ixW #-}

instance PrimBytes Word32 where
  toBytes v@(W32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = W32# (indexWord32Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD32#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD32#
  {-# INLINE byteAlign #-}

instance WordBytes Word32 where
  ixW _ (W32# x) = x
  {-# INLINE ixW #-}

instance PrimBytes Word64 where
  toBytes v@(W64# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  {-# INLINE toBytes #-}
  fromBytes arr = W64# (indexWord64Array# arr 0#)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD64#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD64#
  {-# INLINE byteAlign #-}

instance WordBytes Word64 where
  ixW _ (W64# x) = x
  {-# INLINE ixW #-}



