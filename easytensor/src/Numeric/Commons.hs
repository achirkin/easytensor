{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  ( ElemRep, ElemPrim
  , PrimBytes (..), FloatBytes, DoubleBytes, IntBytes, WordBytes
  ) where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import           GHC.Base  (runRW#)
import           GHC.Int   (Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import           GHC.Prim
import           GHC.Types (Double (..), Float (..), Int (..), RuntimeRep (..),
                            Type, Word (..))
import           GHC.Word  (Word16 (..), Word32 (..), Word64 (..), Word8 (..))


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

type family ElemPrim a :: TYPE (r :: RuntimeRep)
type instance ElemPrim Float = Float#
type instance ElemPrim Double = Double#
type instance ElemPrim Int = Int#
type instance ElemPrim Int8 = Int#
type instance ElemPrim Int16 = Int#
type instance ElemPrim Int32 = Int#
type instance ElemPrim Int64 = Int#
type instance ElemPrim Word = Word#
type instance ElemPrim Word8 = Word#
type instance ElemPrim Word16 = Word#
type instance ElemPrim Word32 = Word#
type instance ElemPrim Word64 = Word#

type FloatBytes a  = (PrimBytes a, ElemRep a ~ 'FloatRep , ElemPrim a ~ Float#)
type DoubleBytes a = (PrimBytes a, ElemRep a ~ 'DoubleRep, ElemPrim a ~ Double#)
type IntBytes a    = (PrimBytes a, ElemRep a ~ 'IntRep   , ElemPrim a ~ Int#)
type WordBytes a   = (PrimBytes a, ElemRep a ~ 'WordRep  , ElemPrim a ~ Word#)

-- | Facilities to convert to and from raw byte array.
--   Warning! offsets and sizes are in elements, not in bytes!
--   Therefore one must be really carefull if having a crazy idea of
--     converting between types of different element sizes.
class PrimBytes (a :: Type) where
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

instance PrimBytes Float where
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
