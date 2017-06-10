{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Commons
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Commons
  ( ElemRep
  , PrimBytes (..), FloatBytes, DoubleBytes, IntBytes, WordBytes
  ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

--import           Foreign.Storable
import           GHC.Base         (runRW#)
import           GHC.Int
import           GHC.Prim
--import           GHC.Ptr
import           GHC.Types
import           GHC.Word


type family ElemRep a :: RuntimeRep
type instance ElemRep Float  = 'FloatRep
type instance ElemRep Double = 'DoubleRep
type instance ElemRep Int    = 'IntRep
type instance ElemRep Int8   = 'IntRep
type instance ElemRep Int16  = 'IntRep
type instance ElemRep Int32  = 'IntRep
type instance ElemRep Int64  = 'IntRep
type instance ElemRep Word   = 'WordRep
type instance ElemRep Word8  = 'WordRep
type instance ElemRep Word16 = 'WordRep
type instance ElemRep Word32 = 'WordRep
type instance ElemRep Word64 = 'WordRep

type FloatBytes a  = (PrimBytes a, ElemRep a ~ 'FloatRep , ElemPrim a ~ Float#)
type DoubleBytes a = (PrimBytes a, ElemRep a ~ 'DoubleRep, ElemPrim a ~ Double#)
type IntBytes a    = (PrimBytes a, ElemRep a ~ 'IntRep   , ElemPrim a ~ Int#)
type WordBytes a   = (PrimBytes a, ElemRep a ~ 'WordRep  , ElemPrim a ~ Word#)

-- | Facilities to convert to and from raw byte array.
--   Warning! offsets and sizes are in elements, not in bytes!
--   Therefore one must be really carefull if having a crazy idea of
--     converting between types of different element sizes.
class PrimBytes (a :: Type) where
  type ElemPrim a :: TYPE (r :: RuntimeRep)
  -- | Store content of a data type in a primitive byte array
  --   (ElementOffset, NumberOfElements, ByteArrayContent )
  toBytes :: a -> (# Int# , Int# , ByteArray# #)
  -- | Load content of a data type from a primitive byte array
  --   (ElementOffset, NumberOfElements, ByteArrayContent )
  fromBytes :: (# Int# , Int# , ByteArray# #) -> a
  -- | Size of a data type in bytes
  byteSize :: a -> Int#
  -- | Alignment of a data type in bytes
  byteAlign :: a -> Int#
  -- | Size of a conainer type elements in bytes
  elementByteSize :: a -> Int#
  -- | Primitive indexing
  ix  :: Int# -> a -> (ElemPrim a :: TYPE (ElemRep a))

-- instance PrimBytes a => Storable (Store a) where
--   sizeOf x = I# (byteSize x)
--   alignment x = I# (byteAlign x)
--   peekElemOff ptr (I# offset) =
--     peekByteOff ptr (I# (offset *# byteSize (undefined :: a)))
--   pokeElemOff ptr (I# offset) =
--     pokeByteOff ptr (I# (offset *# byteSize (undefined :: a)))
--   peekByteOff (Ptr addr) (I# offset) = IO $ \s0 -> case newByteArray# bsize s0 of
--     (# s1, marr #) -> case copyAddrToByteArray# (addr `plusAddr#` offset)
--                                                  marr 0# bsize s1 of
--       s2 -> case unsafeFreezeByteArray# marr s2 of
--         (# s3, arr #) -> (# s3, fromBytes (# 0#, bsize `quotInt#` ebsize, arr #) #)
--     where
--       bsize = byteSize (undefined :: a)
--       ebsize = elementByteSize (undefined :: a)
--   pokeByteOff (Ptr addr) (I# offset) x = IO
--           $ \s0 -> case copyByteArrayToAddr# xbytes xboff
--                                              (addr `plusAddr#` offset)
--                                               bsize s0 of
--        s2 -> (# s2, () #)
--     where
--       !(# elOff, elNum, xbytes #) = toBytes x
--       bsize = elementByteSize x *# elNum
--       xboff  = elementByteSize x *# elOff
--   peek ptr = peekByteOff ptr 0
--   poke ptr = pokeByteOff ptr 0


instance PrimBytes Float where
  type ElemPrim Float = Float#
  toBytes v@(F# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = F# (indexFloatArray# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (F# x) = x
  {-# INLINE ix #-}

instance PrimBytes Double where
  type ElemPrim Double = Double#
  toBytes v@(D# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = D# (indexDoubleArray# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSDOUBLE#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSDOUBLE#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (D# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int where
  type ElemPrim Int = Int#
  toBytes v@(I# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I# (indexIntArray# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSINT#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSINT#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int8 where
  type ElemPrim Int8 = Int#
  toBytes v@(I8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I8# (indexInt8Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT8#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT8#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I8# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int16 where
  type ElemPrim Int16 = Int#
  toBytes v@(I16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I16# (indexInt16Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT16#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT16#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I16# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int32 where
  type ElemPrim Int32 = Int#
  toBytes v@(I32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I32# (indexInt32Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT32#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT32#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I32# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int64 where
  type ElemPrim Int64 = Int#
  toBytes v@(I64# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I64# (indexInt64Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_INT64#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT64#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I64# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word where
  type ElemPrim Word = Word#
  toBytes v@(W# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W# (indexWordArray# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSWORD#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSWORD#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word8 where
  type ElemPrim Word8 = Word#
  toBytes v@(W8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W8# (indexWord8Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD8#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD8#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W8# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word16 where
  type ElemPrim Word16 = Word#
  toBytes v@(W16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W16# (indexWord16Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD16#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD16#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W16# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word32 where
  type ElemPrim Word32 = Word#
  toBytes v@(W32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W32# (indexWord32Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD32#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD32#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W32# x) = x
  {-# INLINE ix #-}


instance PrimBytes Word64 where
  type ElemPrim Word64 = Word#
  toBytes v@(W64# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W64# (indexWord64Array# arr off)
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_WORD64#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD64#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W64# x) = x
  {-# INLINE ix #-}
