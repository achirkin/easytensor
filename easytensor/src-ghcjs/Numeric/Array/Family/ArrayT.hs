{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
-- {-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE GHCForeignImportPrim  #-}
{-# LANGUAGE UnliftedFFITypes      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Array.Family.ArrayT () where


import           GHC.Int   (Int16 (..), Int32 (..), Int8 (..))
import           GHC.Word  (Word16 (..), Word32 (..), Word8 (..))
import           GHC.Prim
import           GHC.Types                 (Float (..), Int (..),
                                            RuntimeRep (..))
import           GHCJS.Types

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse
import           Numeric.TypeLits
import           Numeric.Matrix.Type


type instance ElemRep  (ArrayT t      ds) = ElemRep t
type instance ElemRep  Word8Clamped       = ElemRep Word8
type instance ElemPrim (ArrayT Float  ds) = Float#
type instance ElemPrim (ArrayT Double ds) = Double#
type instance ElemPrim (ArrayT Int    ds) = Int#
type instance ElemPrim (ArrayT Int8   ds) = Int#
type instance ElemPrim (ArrayT Int16  ds) = Int#
type instance ElemPrim (ArrayT Int32  ds) = Int#
type instance ElemPrim (ArrayT Word   ds) = Word#
type instance ElemPrim (ArrayT Word8  ds) = Word#
type instance ElemPrim (ArrayT Word16 ds) = Word#
type instance ElemPrim (ArrayT Word32 ds) = Word#
type instance ElemPrim (ArrayT Word8Clamped ds) = Word#


-- | Stub for Uint8ClampedArray in JS
newtype Word8Clamped = Clamped Word8 deriving
    (Ord,Num,Eq,Bounded,Enum,Integral,Real,Show) -- ,Data,Ix,FiniteBits,Bits,Storable)


instance Dimensions ds => PrimBytes (ArrayT Float ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapFloatArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Float)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Float)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Float)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetFloat#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Double ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapDoubleArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Double)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Double)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Double)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetDouble#
  {-# INLINE ix #-}


instance Dimensions ds => PrimBytes (ArrayT Int ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapIntArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Int)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Int)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Int8 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapInt8ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int8)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Int8)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Int8)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt8#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Int16 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapInt16ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int16)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Int16)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Int16)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt16#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Int32 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapInt32ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int32)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Int32)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Int32)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt32#
  {-# INLINE ix #-}




instance Dimensions ds => PrimBytes (ArrayT Word ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWordArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Word)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Word)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word8 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord8ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word8)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Word8)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Word8)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord8#
  {-# INLINE ix #-}


instance Dimensions ds => PrimBytes (ArrayT Word8Clamped ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord8ClampedArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word8)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Word8)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Word8)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord8Clamped#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word16 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord16ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word16)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Word16)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Word16)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord16#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word32 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord32ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize _ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word32)
  {-# INLINE byteSize #-}
  byteAlign _ = byteAlign (undefined :: Word32)
  {-# INLINE byteAlign #-}
  elementByteSize _ = byteSize (undefined :: Word32)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord32#
  {-# INLINE ix #-}



instance Dimensions ds => ElementWise (Idx ds) Float (ArrayT Float ds) where
    x ! i = case fromEnum i of I# j -> F# (js_indexArrayOffsetFloat# j x)
    {-# INLINE (!) #-}
    broadcast = js_fillNewFloatArray (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (F# v) = case fromEnum i of I# j -> js_setArrayOffsetFloat# j v
    {-# INLINE update #-}


--
--class ElementWise i x t | t -> x i where
--  -- | map all elements with index
--  ewmap :: (i -> x -> x) -> t -> t
--  -- | generate data from elements
--  ewgen :: (i -> x) -> t
--  -- | generate data from elements in applicative functor
--  ewgenA :: forall f . Applicative f => (i -> f x) -> f t
--  -- | fold all element with index
--  ewfoldl :: (i -> a -> x -> a) -> a -> t -> a
--  -- | fold all element with index
--  ewfoldr :: (i -> x -> a -> a) -> a -> t -> a
--  -- | Apply an applicative functor on each element (Lens-like traversal)
--  elementWise :: forall f . Applicative f => (x -> f x) -> t -> f t
--  -- | Apply an applicative functor on each element with its index
--  --     (Lens-like indexed traversal)
--  indexWise :: forall f . Applicative f => (i -> x -> f x) -> t -> f t


instance Dimensions ds => ElementWise (Idx ds) Double (ArrayT Double ds) where
instance Dimensions ds => ElementWise (Idx ds) Int (ArrayT Int ds) where
instance Dimensions ds => ElementWise (Idx ds) Int8 (ArrayT Int8 ds) where
instance Dimensions ds => ElementWise (Idx ds) Int16 (ArrayT Int16 ds) where
instance Dimensions ds => ElementWise (Idx ds) Int32 (ArrayT Int32 ds) where
instance Dimensions ds => ElementWise (Idx ds) Word (ArrayT Word ds) where
instance Dimensions ds => ElementWise (Idx ds) Word8 (ArrayT Word8 ds) where
instance Dimensions ds => ElementWise (Idx ds) Word16 (ArrayT Word16 ds) where
instance Dimensions ds => ElementWise (Idx ds) Word32 (ArrayT Word32 ds) where

instance Show (ArrayT t ds)
instance Eq (ArrayT t ds)
instance Ord (ArrayT t ds)
instance Num (ArrayT t ds)
instance Floating (ArrayT t ds)
instance Fractional (ArrayT t ds)
instance Bounded (ArrayT t ds)





instance (KnownDim n, KnownDim m, ArrayT Float '[n,m] ~ Array Float '[n,m], 2 <= n, 2 <= m)
      => MatrixCalculus Float n m where

instance ( KnownDim n, ArrayT Float '[n,n] ~ Array Float '[n,n] )
      => SquareMatrixCalculus Float n where


instance (KnownNat n, ArrayT Float '[n,n] ~ Array Float '[n,n], 2 <= n) => MatrixInverse Float n where



instance (KnownDim n, KnownDim m, ArrayT Double '[n,m] ~ Array Double '[n,m], 2 <= n, 2 <= m)
      => MatrixCalculus Double n m where

instance ( KnownDim n, ArrayT Double '[n,n] ~ Array Double '[n,n] )
      => SquareMatrixCalculus Double n where


instance (KnownNat n, ArrayT Double '[n,n] ~ Array Double '[n,n], 2 <= n) => MatrixInverse Double n where
















unsafeFreezeArrayT :: MutableArrayT s t ds -> State# s -> (# State# s, ArrayT t ds #)
unsafeFreezeArrayT a s = (# s, coerce a #)
{-# INLINE unsafeFreezeArrayT #-}

unsafeThawArrayT :: ArrayT t ds -> State# s -> (#State# s, MutableArrayT s t ds #)
unsafeThawArrayT a s = (# s, coerce a #)
{-# INLINE unsafeThawArrayT #-}


foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetFloat#        :: Int# -> ArrayT Float        ds -> Float#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetDouble#       :: Int# -> ArrayT Double       ds -> Double#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt#          :: Int# -> ArrayT Int          ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt8#         :: Int# -> ArrayT Int8         ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt16#        :: Int# -> ArrayT Int16        ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt32#        :: Int# -> ArrayT Int32        ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord#         :: Int# -> ArrayT Word         ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord8#        :: Int# -> ArrayT Word8        ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord8Clamped# :: Int# -> ArrayT Word8Clamped ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord16#       :: Int# -> ArrayT Word16       ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord32#       :: Int# -> ArrayT Word32       ds -> Word#


foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetFloat#        :: Int# -> Float#  -> ArrayT Float        ds -> ArrayT Float ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetDouble#       :: Int# -> Double# -> ArrayT Double       ds -> ArrayT Double ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetInt#          :: Int# -> Int#    -> ArrayT Int          ds -> ArrayT Int ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetInt8#         :: Int# -> Int#    -> ArrayT Int8         ds -> ArrayT Int8 ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetInt16#        :: Int# -> Int#    -> ArrayT Int16        ds -> ArrayT Int16 ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetInt32#        :: Int# -> Int#    -> ArrayT Int32        ds -> ArrayT Int32 ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetWord#         :: Int# -> Word#   -> ArrayT Word         ds -> ArrayT Word ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetWord8#        :: Int# -> Word#   -> ArrayT Word8        ds -> ArrayT Word8 ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetWord8Clamped# :: Int# -> Word#   -> ArrayT Word8Clamped ds ->ArrayT Word8Clamped ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetWord16#       :: Int# -> Word#   -> ArrayT Word16       ds -> ArrayT Word16 ds
foreign import javascript unsafe "$r = new $3.constructor($3.buffer.slice()); $r[$1] = $2;" js_setArrayOffsetWord32#       :: Int# -> Word#   -> ArrayT Word32       ds -> ArrayT Word32 ds




foreign import javascript unsafe "$2[$1]" js_readArrayOffsetFloat#        :: Int# -> MutableArrayT s Float        ds -> State# s -> (# State# s, Float# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetDouble#       :: Int# -> MutableArrayT s Double       ds -> State# s -> (# State# s, Double# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt#          :: Int# -> MutableArrayT s Int8         ds -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt8#         :: Int# -> MutableArrayT s Int8         ds -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt16#        :: Int# -> MutableArrayT s Int16        ds -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt32#        :: Int# -> MutableArrayT s Int32        ds -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord#         :: Int# -> MutableArrayT s Word         ds -> State# s -> (# State# s, Word# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord8#        :: Int# -> MutableArrayT s Word8        ds -> State# s -> (# State# s, Word# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord8Clamped# :: Int# -> MutableArrayT s Word8Clamped ds -> State# s -> (# State# s, Word# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord16#       :: Int# -> MutableArrayT s Word16       ds -> State# s -> (# State# s, Word# #)
foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord32#       :: Int# -> MutableArrayT s Word32       ds -> State# s -> (# State# s, Word# #)


foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetFloat#        :: Int# -> Float#  -> MutableArrayT s Float        ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetDouble#       :: Int# -> Double# -> MutableArrayT s Double       ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt#          :: Int# -> Int#    -> MutableArrayT s Int8         ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt8#         :: Int# -> Int#    -> MutableArrayT s Int8         ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt16#        :: Int# -> Int#    -> MutableArrayT s Int16        ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt32#        :: Int# -> Int#    -> MutableArrayT s Int32        ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord#         :: Int# -> Word#   -> MutableArrayT s Word         ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord8#        :: Int# -> Word#   -> MutableArrayT s Word8        ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord8Clamped# :: Int# -> Word#   -> MutableArrayT s Word8Clamped ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord16#       :: Int# -> Word#   -> MutableArrayT s Word16       ds -> State# s -> (# State# s, () #)
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord32#       :: Int# -> Word#   -> MutableArrayT s Word32       ds -> State# s -> (# State# s, () #)








-----------------------------------------------------------------------------
-- Conversions between types
-----------------------------------------------------------------------------




foreign import javascript unsafe "$1.length"     js_length     :: ArrayT t ds -> Int#
foreign import javascript unsafe "$1.byteOffset" js_byteOffset :: ArrayT t ds -> Int#
foreign import javascript unsafe "$1.byteLength" js_byteLength :: ArrayT t ds -> Int#


foreign import javascript unsafe "$1.length"     js_lengthM     :: MutableArrayT s t ds -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe "$1.byteOffset" js_byteOffsetM :: MutableArrayT s t ds -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe "$1.byteLength" js_byteLengthM :: MutableArrayT s t ds -> State# s -> (# State# s, Int# #)

foreign import javascript unsafe "h$wrapBuffer($1.buffer)" js_wrapArrayT        :: ArrayT t ds -> ByteArray#
foreign import javascript unsafe "h$wrapBuffer($1.buffer)" js_wrapMutableArrayT :: MutableArrayT s t ds -> State# s -> (# State# s, MutableByteArray# s #)


foreign import javascript unsafe "$1.f3 || new Float32Array($1.buf)"      js_unwrapFloatArray        :: ByteArray# -> ArrayT Float ds
foreign import javascript unsafe "$1.f6 || new Float64Array($1.buf)"      js_unwrapDoubleArray       :: ByteArray# -> ArrayT Double ds
foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapIntArray          :: ByteArray# -> ArrayT Int ds
foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapInt32Array        :: ByteArray# -> ArrayT Int32 ds
foreign import javascript unsafe "$1.i1 || new Int16Array($1.buf)"        js_unwrapInt16Array        :: ByteArray# -> ArrayT Int16 ds
foreign import javascript unsafe "$1.i8 || new Int8Array($1.buf)"         js_unwrapInt8Array         :: ByteArray# -> ArrayT Int8 ds
foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapWordArray         :: ByteArray# -> ArrayT Word ds
foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapWord32Array       :: ByteArray# -> ArrayT Word32 ds
foreign import javascript unsafe "$1.u1 || new Uint16Array($1.buf)"       js_unwrapWord16Array       :: ByteArray# -> ArrayT Word16 ds
foreign import javascript unsafe "$1.u8 || new Uint8Array($1.buf)"        js_unwrapWord8Array        :: ByteArray# -> ArrayT Word8 ds
foreign import javascript unsafe "$1.uc || new Uint8ClampedArray($1.buf)" js_unwrapWord8ClampedArray :: ByteArray# -> ArrayT Word8Clamped ds



foreign import javascript unsafe "new Float32Array($1.buf, $2*4, $3)"    js_unwrapFloatArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Float ds
foreign import javascript unsafe "new Float64Array($1.buf, $2*8, $3)"    js_unwrapDoubleArrayOffLen       :: ByteArray# -> Int# -> Int# -> ArrayT Double ds
foreign import javascript unsafe "new Int32Array($1.buf, $2*4, $3)"      js_unwrapIntArrayOffLen          :: ByteArray# -> Int# -> Int# -> ArrayT Int ds
foreign import javascript unsafe "new Int32Array($1.buf, $2*4, $3)"      js_unwrapInt32ArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Int32 ds
foreign import javascript unsafe "new Int16Array($1.buf, $2*2, $3)"      js_unwrapInt16ArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Int16 ds
foreign import javascript unsafe "new Int8Array($1.buf, $2, $3)"         js_unwrapInt8ArrayOffLen         :: ByteArray# -> Int# -> Int# -> ArrayT Int8 ds
foreign import javascript unsafe "new Uint32Array($1.buf, $2*4, $3)"     js_unwrapWordArrayOffLen         :: ByteArray# -> Int# -> Int# -> ArrayT Word ds
foreign import javascript unsafe "new Uint32Array($1.buf, $2*4, $3)"     js_unwrapWord32ArrayOffLen       :: ByteArray# -> Int# -> Int# -> ArrayT Word32 ds
foreign import javascript unsafe "new Uint16Array($1.buf, $2*2, $3)"     js_unwrapWord16ArrayOffLen       :: ByteArray# -> Int# -> Int# -> ArrayT Word16 ds
foreign import javascript unsafe "new Uint8Array($1.buf, $2, $3)"        js_unwrapWord8ArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Word8 ds
foreign import javascript unsafe "new Uint8ClampedArray($1.buf, $2, $3)" js_unwrapWord8ClampedArrayOffLen :: ByteArray# -> Int# -> Int# -> ArrayT Word8Clamped ds


foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapMutableIntArray          :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int ds #);
foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapMutableInt32Array        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int32 ds #);
foreign import javascript unsafe "$1.i1 || new Int16Array($1.buf)"        js_unwrapMutableInt16Array        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int16 ds #);
foreign import javascript unsafe "$1.i8 || new Int8Array($1.buf)"         js_unwrapMutableInt8Array         :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int8 ds #);
foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapMutableWordArray         :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word ds #);
foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapMutableWord32Array       :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word32 ds #);
foreign import javascript unsafe "$1.u1 || new Uint16Array($1.buf)"       js_unwrapMutableWord16Array       :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word16 ds #);
foreign import javascript unsafe "$1.u8 || new Uint8Array($1.buf)"        js_unwrapMutableWord8Array        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word8 ds #);
foreign import javascript unsafe "$1.f3 || new Float32Array($1.buf)"      js_unwrapMutableFloatArray        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Float ds #);
foreign import javascript unsafe "$1.f6 || new Float64Array($1.buf)"      js_unwrapMutableDoubleArray       :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Double ds #);
foreign import javascript unsafe "$1.uc || new Uint8ClampedArray($1.buf)" js_unwrapMutableWord8ClampedArray :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word8Clamped ds #);



-----------------------------------------------------------------------------
-- Create new arrays
-----------------------------------------------------------------------------

foreign import javascript unsafe "new Float32Array($1)"      js_createFloatArray        :: Int -> ArrayT Float ds
foreign import javascript unsafe "new Float64Array($1)"      js_createDoubleArray       :: Int -> ArrayT Double ds
foreign import javascript unsafe "new Int32Array($1)"        js_createIntArray          :: Int -> ArrayT Int ds
foreign import javascript unsafe "new Int32Array($1)"        js_createInt32Array        :: Int -> ArrayT Int32 ds
foreign import javascript unsafe "new Int16Array($1)"        js_createInt16Array        :: Int -> ArrayT Int16 ds
foreign import javascript unsafe "new Int8Array($1)"         js_createInt8Array         :: Int -> ArrayT Int8 ds
foreign import javascript unsafe "new Uint32Array($1)"       js_createWordArray         :: Int -> ArrayT Word ds
foreign import javascript unsafe "new Uint32Array($1)"       js_createWord32Array       :: Int -> ArrayT Word32 ds
foreign import javascript unsafe "new Uint16Array($1)"       js_createWord16Array       :: Int -> ArrayT Word16 ds
foreign import javascript unsafe "new Uint8Array($1)"        js_createWord8Array        :: Int -> ArrayT Word8 ds
foreign import javascript unsafe "new Uint8ClampedArray($1)" js_createWord8ClampedArray :: Int -> ArrayT Word8Clamped ds

foreign import javascript unsafe "new Float32Array($1).fill($2)"      js_fillNewFloatArray        :: Int -> Float        -> ArrayT Float ds
foreign import javascript unsafe "new Float64Array($1).fill($2)"      js_fillNewDoubleArray       :: Int -> Double       -> ArrayT Double ds
foreign import javascript unsafe "new Int32Array($1).fill($2)"        js_fillNewIntArray          :: Int -> Int          -> ArrayT Int ds
foreign import javascript unsafe "new Int32Array($1).fill($2)"        js_fillNewInt32Array        :: Int -> Int32        -> ArrayT Int32 ds
foreign import javascript unsafe "new Int16Array($1).fill($2)"        js_fillNewInt16Array        :: Int -> Int16        -> ArrayT Int16 ds
foreign import javascript unsafe "new Int8Array($1).fill($2)"         js_fillNewInt8Array         :: Int -> Int8         -> ArrayT Int8 ds
foreign import javascript unsafe "new Uint32Array($1).fill($2)"       js_fillNewWordArray         :: Int -> Word         -> ArrayT Word ds
foreign import javascript unsafe "new Uint32Array($1).fill($2)"       js_fillNewWord32Array       :: Int -> Word32       -> ArrayT Word32 ds
foreign import javascript unsafe "new Uint16Array($1).fill($2)"       js_fillNewWord16Array       :: Int -> Word16       -> ArrayT Word16 ds
foreign import javascript unsafe "new Uint8Array($1).fill($2)"        js_fillNewWord8Array        :: Int -> Word8        -> ArrayT Word8 ds
foreign import javascript unsafe "new Uint8ClampedArray($1).fill($2)" js_fillNewWord8ClampedArray :: Int -> Word8Clamped -> ArrayT Word8Clamped ds




-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Float32Array(arr.length); $r.set(arr);"      js_fromListFloatArray        :: Exts.Any -> ArrayT Float ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Float64Array(arr.length); $r.set(arr);"      js_fromListDoubleArray       :: Exts.Any -> ArrayT Double ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int32Array(arr.length); $r.set(arr);"        js_fromListIntArray          :: Exts.Any -> ArrayT Int ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int32Array(arr.length); $r.set(arr);"        js_fromListInt32Array        :: Exts.Any -> ArrayT Int32 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int16Array(arr.length); $r.set(arr);"        js_fromListInt16Array        :: Exts.Any -> ArrayT Int16 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int8Array(arr.length); $r.set(arr);"         js_fromListInt8Array         :: Exts.Any -> ArrayT Int8 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint32Array(arr.length); $r.set(arr);"       js_fromListWordArray         :: Exts.Any -> ArrayT Word ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint32Array(arr.length); $r.set(arr);"       js_fromListWord32Array       :: Exts.Any -> ArrayT Word32 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint16Array(arr.length); $r.set(arr);"       js_fromListWord16Array       :: Exts.Any -> ArrayT Word16 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint8Array(arr.length); $r.set(arr);"        js_fromListWord8Array        :: Exts.Any -> ArrayT Word8 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint8ClampedArray(arr.length); $r.set(arr);" js_fromListWord8ClampedArray :: Exts.Any -> ArrayT Word8Clamped ds


-- foreign import javascript unsafe "$r = new Float32Array($1.length); $r.set($1);" js_fromArrayFloatArray        :: SomeTypedArray m0 t -> ArrayT Float ds
-- foreign import javascript unsafe "new Float32Array($1)" js_viewFloatArray        :: SomeArrayBuffer m -> ArrayT Float ds
--
-- foreign import javascript unsafe "$r = new Float64Array($1.length); $r.set($1);" js_fromArrayDoubleArray       :: SomeTypedArray m0 t -> ArrayT Double ds
-- foreign import javascript unsafe "new Float64Array($1)" js_viewDoubleArray       :: SomeArrayBuffer m -> ArrayT Double ds
--
-- foreign import javascript unsafe "$r = new Int32Array($1.length); $r.set($1);" js_fromArrayIntArray          :: SomeTypedArray m0 t -> ArrayT Int ds
-- foreign import javascript unsafe "new Int32Array($1)" js_viewIntArray          :: SomeArrayBuffer m -> ArrayT Int ds
--
-- foreign import javascript unsafe "$r = new Int32Array($1.length); $r.set($1);" js_fromArrayInt32Array :: SomeTypedArray m0 t -> ArrayT Int32 ds
-- foreign import javascript unsafe "new Int32Array($1)" js_viewInt32Array :: SomeArrayBuffer m -> ArrayT Int32 ds
--
-- foreign import javascript unsafe "$r = new Int16Array($1.length); $r.set($1);" js_fromArrayInt16Array :: SomeTypedArray m0 t -> ArrayT Int16 ds
-- foreign import javascript unsafe "new Int16Array($1)" js_viewInt16Array :: SomeArrayBuffer m -> ArrayT Int16 ds
--
-- foreign import javascript unsafe "$r = new Int8Array($1.length); $r.set($1);" js_fromArrayInt8Array :: SomeTypedArray m0 t -> ArrayT Int8 ds
-- foreign import javascript unsafe "new Int8Array($1)" js_viewInt8Array :: SomeArrayBuffer m -> ArrayT Int8 ds
--
-- foreign import javascript unsafe "$r = new Uint32Array($1.length); $r.set($1);" js_fromArrayWordArray :: SomeTypedArray m0 t -> ArrayT Word ds
-- foreign import javascript unsafe "new Uint32Array($1)" js_viewWordArray :: SomeArrayBuffer m -> ArrayT Word ds
--
-- foreign import javascript unsafe "$r = new Uint32Array($1.length); $r.set($1);" js_fromArrayWord32Array :: SomeTypedArray m0 t -> ArrayT Word32 ds
-- foreign import javascript unsafe "new Uint32Array($1)" js_viewWord32Array :: SomeArrayBuffer m -> ArrayT Word32 ds
--
-- foreign import javascript unsafe "$r = new Uint16Array($1.length); $r.set($1);" js_fromArrayWord16Array :: SomeTypedArray m0 t -> ArrayT Word16 ds
-- foreign import javascript unsafe "new Uint16Array($1)" js_viewWord16Array :: SomeArrayBuffer m -> ArrayT Word16 ds
--
-- foreign import javascript unsafe "$r = new Uint8Array($1.length); $r.set($1);" js_fromArrayWord8Array        :: SomeTypedArray m0 t -> ArrayT Word8 ds
-- foreign import javascript unsafe "new Uint8Array($1)" js_viewWord8Array        :: SomeArrayBuffer m -> ArrayT Word8 ds
--
-- foreign import javascript unsafe "$r = new Uint8ClampedArray($1.length); $r.set($1);" js_fromArrayWord8ClampedArray :: SomeTypedArray m0 t -> ArrayT Word8Clamped ds
-- foreign import javascript unsafe "new Uint8ClampedArray($1)" js_viewWord8ClampedArray :: SomeArrayBuffer m -> ArrayT Word8Clamped ds
