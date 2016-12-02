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
  fromBytes arr = F# (indexFloatArray# arr 0#)
  byteSize _ = case SIZEOF_HSFLOAT of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_HSFLOAT of I# s -> s
  {-# INLINE byteAlign #-}

instance PrimBytes Double where
  toBytes v@(D# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = D# (indexDoubleArray# arr 0#)
  byteSize _ = case SIZEOF_HSDOUBLE of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_HSDOUBLE of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Int where
  toBytes v@(I# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = I# (indexIntArray# arr 0#)
  byteSize _ = case SIZEOF_HSINT of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_HSINT of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Int8 where
  toBytes v@(I8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = I8# (indexInt8Array# arr 0#)
  byteSize _ = case SIZEOF_INT8 of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_INT8 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Int16 where
  toBytes v@(I16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = I16# (indexInt16Array# arr 0#)
  byteSize _ = case SIZEOF_INT16 of I16# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_INT16 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Int32 where
  toBytes v@(I32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = I32# (indexInt32Array# arr 0#)
  byteSize _ = case SIZEOF_INT32 of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_INT32 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Int64 where
  toBytes v@(I64# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = I64# (indexInt64Array# arr 0#)
  byteSize _ = case SIZEOF_INT64 of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_INT64 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Word where
  toBytes v@(W# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = W# (indexWordArray# arr 0#)
  byteSize _ = case SIZEOF_HSWORD of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_HSWORD of I# s -> s
  {-# INLINE byteAlign #-}

instance PrimBytes Word8 where
  toBytes v@(W8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = W8# (indexWord8Array# arr 0#)
  byteSize _ = case SIZEOF_WORD8 of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_WORD8 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Word16 where
  toBytes v@(W16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = W16# (indexWord16Array# arr 0#)
  byteSize _ = case SIZEOF_WORD16 of I16# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_WORD16 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Word32 where
  toBytes v@(W32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = W32# (indexWord32Array# arr 0#)
  byteSize _ = case SIZEOF_WORD32 of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_WORD32 of I# s -> s
  {-# INLINE byteAlign #-}


instance PrimBytes Word64 where
  toBytes v@(W64# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> a
  fromBytes arr = W64# (indexWord64Array# arr 0#)
  byteSize _ = case SIZEOF_WORD64 of I# s -> s
  {-# INLINE byteSize #-}
  byteAlign _ = case ALIGNMENT_WORD64 of I# s -> s
  {-# INLINE byteAlign #-}




