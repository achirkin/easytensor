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
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE UnliftedFFITypes           #-}
#endif
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

#ifndef ghcjs_HOST_OS
import           GHC.Base  (runRW#)
#endif
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
#if SIZEOF_HSWORD < 8
type instance ElemRep Int64  = 'Int64Rep
#else
type instance ElemRep Int64  = 'IntRep
#endif
type instance ElemRep Word   = 'WordRep
type instance ElemRep Word8  = 'WordRep
type instance ElemRep Word16 = 'WordRep
type instance ElemRep Word32 = 'WordRep
#if SIZEOF_HSWORD < 8
type instance ElemRep Word64 = 'Word64Rep
#else
type instance ElemRep Word64 = 'WordRep
#endif

type family ElemPrim a :: TYPE (r :: RuntimeRep)
type instance ElemPrim Float = Float#
type instance ElemPrim Double = Double#
type instance ElemPrim Int = Int#
type instance ElemPrim Int8 = Int#
type instance ElemPrim Int16 = Int#
type instance ElemPrim Int32 = Int#
#if SIZEOF_HSWORD < 8
type instance ElemPrim Int64 = Int64#
#else
type instance ElemPrim Int64 = Int#
#endif
type instance ElemPrim Word = Word#
type instance ElemPrim Word8 = Word#
type instance ElemPrim Word16 = Word#
type instance ElemPrim Word32 = Word#
#if SIZEOF_HSWORD < 8
type instance ElemPrim Word64 = Word64#
#else
type instance ElemPrim Word64 = Word#
#endif


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
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapFloat v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapFloat arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(F# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = F# (indexFloatArray# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (F# x) = x
  {-# INLINE ix #-}

instance PrimBytes Double where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapDouble v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapDouble arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(D# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = D# (indexDoubleArray# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_HSDOUBLE#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSDOUBLE#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (D# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapInt v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapInt arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(I# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I# (indexIntArray# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_HSINT#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSINT#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int8 where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapInt8 v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapInt8 arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(I8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I8# (indexInt8Array# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_INT8#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT8#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I8# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int16 where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapInt16 v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapInt16 arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(I16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I16# (indexInt16Array# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_INT16#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT16#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I16# x) = x
  {-# INLINE ix #-}

instance PrimBytes Int32 where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapInt32 v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapInt32 arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(I32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeInt32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = I32# (indexInt32Array# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_INT32#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_INT32#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (I32# x) = x
  {-# INLINE ix #-}

#ifndef ghcjs_HOST_OS
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
#endif

instance PrimBytes Word where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapWord v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapWord arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(W# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W# (indexWordArray# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_HSWORD#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSWORD#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word8 where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapWord8 v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapWord8 arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(W8# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W8# (indexWord8Array# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_WORD8#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD8#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W8# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word16 where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapWord16 v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapWord16 arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(W16# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W16# (indexWord16Array# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_WORD16#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD16#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W16# x) = x
  {-# INLINE ix #-}

instance PrimBytes Word32 where
#ifdef ghcjs_HOST_OS
  toBytes v = (# 0#, 1#, js_wrapWord32 v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = js_unwrapWord32 arr off
  {-# INLINE fromBytes #-}
#else
  toBytes v@(W32# x) = case runRW#
     ( \s0 -> case newByteArray# (byteSize v) s0 of
         (# s1, marr #) -> case writeWord32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, a #) -> (# 0#, 1#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = W32# (indexWord32Array# arr off)
  {-# INLINE fromBytes #-}
#endif
  byteSize _ = SIZEOF_WORD32#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_WORD32#
  {-# INLINE byteAlign #-}
  elementByteSize = byteSize
  {-# INLINE elementByteSize #-}
  ix _ (W32# x) = x
  {-# INLINE ix #-}


#ifndef ghcjs_HOST_OS
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
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "h$wrapBuffer((new Float32Array([$1])).buffer)"      js_wrapFloat        :: Float -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Float64Array([$1])).buffer)"      js_wrapDouble       :: Double -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Int32Array([$1])).buffer)"        js_wrapInt          :: Int -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Int32Array([$1])).buffer)"        js_wrapInt32        :: Int32 -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Int16Array([$1])).buffer)"        js_wrapInt16        :: Int16 -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Int8Array([$1])).buffer)"         js_wrapInt8         :: Int8 -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Uint32Array([$1])).buffer)"       js_wrapWord         :: Word -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Uint32Array([$1])).buffer)"       js_wrapWord32       :: Word32 -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Uint16Array([$1])).buffer)"       js_wrapWord16       :: Word16 -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer((new Uint8Array([$1])).buffer)"        js_wrapWord8        :: Word8 -> ByteArray#



foreign import javascript unsafe "($1.f3 || new Float32Array($1.buf))[$2]"      js_unwrapFloat        :: ByteArray# -> Int# -> Float
foreign import javascript unsafe "($1.f6 || new Float64Array($1.buf))[$2]"      js_unwrapDouble       :: ByteArray# -> Int# -> Double
foreign import javascript unsafe "($1.i3 || new Int32Array($1.buf))[$2]"        js_unwrapInt          :: ByteArray# -> Int# -> Int
foreign import javascript unsafe "($1.i3 || new Int32Array($1.buf))[$2]"        js_unwrapInt32        :: ByteArray# -> Int# -> Int32
foreign import javascript unsafe "($1.i1 || new Int16Array($1.buf))[$2]"        js_unwrapInt16        :: ByteArray# -> Int# -> Int16
foreign import javascript unsafe "($1.i8 || new Int8Array($1.buf))[$2]"         js_unwrapInt8         :: ByteArray# -> Int# -> Int8
foreign import javascript unsafe "($1.u3 || new Uint32Array($1.buf))[$2]"       js_unwrapWord         :: ByteArray# -> Int# -> Word
foreign import javascript unsafe "($1.u3 || new Uint32Array($1.buf))[$2]"       js_unwrapWord32       :: ByteArray# -> Int# -> Word32
foreign import javascript unsafe "($1.u1 || new Uint16Array($1.buf))[$2]"       js_unwrapWord16       :: ByteArray# -> Int# -> Word16
foreign import javascript unsafe "($1.u8 || new Uint8Array($1.buf))[$2]"        js_unwrapWord8        :: ByteArray# -> Int# -> Word8
#endif
