{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE GHCForeignImportPrim      #-}
{-# LANGUAGE UnliftedFFITypes          #-}
{-# LANGUAGE TypeOperators             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Mutable
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Interfrace to perform primitive stateful operations on mutable frames.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Mutable
    ( MutableFrame (..), MDataFrame (..)
    , newDataFrame#, copyDataFrame#, copyMDataFrame#, unsafeFreezeDataFrame#
    , freezeDataFrame#, thawDataFrame#
    , writeDataFrame#, readDataFrame#
    , newArrayBuffer#, arrayBuffer#, viewFloatArray#, viewDoubleArray#
    , viewIntArray#, viewInt32Array#, viewInt16Array#, viewInt8Array#
    , viewWordArray#, viewWord32Array#, viewWord16Array#, viewWord8Array#, viewWord8ClampedArray#
    ) where



import           GHCJS.Types            (IsJSVal(), JSVal)
import           GHC.Int                (Int16 (..), Int32 (..),Int8 (..))
import           GHC.Prim
import           GHC.TypeLits           (type (<=))
import           GHC.Types              (Double (..), Float (..), Int (..), Word (..))
import           GHC.Word               (Word16 (..), Word32 (..), Word8 (..))
import           Unsafe.Coerce          (unsafeCoerce)

import           Numeric.DataFrame.Type
import           Numeric.Array.Family
import           Numeric.Dimensions


-- | Mutable DataFrame type
newtype MDataFrame s t (ns :: [Nat]) = MDataFrame (MutableArrayT s t ns)
instance IsJSVal (MDataFrame s t ds)


-- | Create a new mutable DataFrame.
newDataFrame# :: forall t (ns :: [Nat]) s
               . (ElemTypeInference t, Dimensions ns)
              => State# s -> (# State# s, MDataFrame s t ns #)
newDataFrame# = case elemTypeInstance @t of
    ETFloat  -> js_createFloatArray n
    ETDouble -> js_createDoubleArray n
    ETInt    -> js_createIntArray n
    ETInt8   -> js_createInt8Array n
    ETInt16  -> js_createInt16Array n
    ETInt32  -> js_createInt32Array n
    ETWord   -> js_createWordArray n
    ETWord8  -> js_createWord8Array n
    ETWord16 -> js_createWord16Array n
    ETWord32 -> js_createWord32Array n
    ETWord8C -> js_createWord8ClampedArray n
  where
    n = dimVal (dim @ns)
{-# INLINE newDataFrame# #-}


-- | Copy one DataFrame into another mutable DataFrame at specified position.
copyDataFrame# :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( ArraySizeInference (as +: b')
                  , ConcatList as (b :+ bs) asbs
                  , Dimensions (b :+ bs)
                  , b' <= b
                  )
               => DataFrame t (as +: b') -> Idx (b :+ bs) -> MDataFrame s t asbs -> State# s -> (# State# s, () #)
copyDataFrame# df i mdf s0 = case arraySizeInstance @(as +: b') of
    ASScalar -> df `seq` (# js_writeArrayOffsetJSVal# mdf (fromEnum i) (unsafeCoerce df) s0, () #)
    ASArray -> js_copyDataFrame (coerce df) (fromEnum i) mdf s0
{-# INLINE copyDataFrame# #-}



-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMDataFrame# :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( ConcatList as (b :+ bs) asbs
                  , Dimensions (b :+ bs)
                  )
               => MDataFrame s t (as +: b') -> Idx (b :+ bs) -> MDataFrame s t asbs -> State# s -> (# State# s, () #)
copyMDataFrame# d = js_copyMDataFrame d . fromEnum
{-# INLINE copyMDataFrame# #-}


-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame# :: forall t (ns :: [Nat]) s
                        . (MutableFrame t ns, ArraySizeInference ns)
                       => MDataFrame s t ns -> State# s -> (# State# s, DataFrame t ns #)
unsafeFreezeDataFrame# a s = case arraySizeInstance @ns of
    ASScalar -> case readDataFrameOff# a 0# s of
        (# s1, v #) -> (# s1, coerce v #)
    ASArray -> (# s, coerce a #)
{-# INLINE unsafeFreezeDataFrame# #-}


--unsafeThawArrayT# :: ArrayT t ds -> State# s -> (#State# s, MutableArrayT s t ds #)
--unsafeThawArrayT# a s = (# s, coerce a #)
--{-# INLINE unsafeThawArrayT# #-}


-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame# :: forall t (ns :: [Nat]) s
                  . (MutableFrame t ns, ArraySizeInference ns)
                 => MDataFrame s t ns -> State# s -> (# State# s, DataFrame t ns #)
freezeDataFrame# a s = case arraySizeInstance @ns of
    ASScalar -> case readDataFrameOff# a 0# s of
        (# s1, v #) -> (# s1, coerce v #)
    ASArray -> case js_freeze a s of
        (# s1, v #) -> (# s1, coerce v #)
{-# INLINE freezeDataFrame# #-}



-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame# :: forall t (ns :: [Nat]) s
                . (MutableFrame t ns, ArrayInstanceInference t ns)
               => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawDataFrame# a s = case arraySizeInstance @ns of
    ASScalar -> case newDataFrame# @t @'[] s of
        (# s1, df #) -> (# writeDataFrameOff# df 0# (coerce a) s1, df #)
    ASArray -> js_thaw (coerce a) s
{-# INLINE thawDataFrame# #-}

-- | Write a single element at the specified index
writeDataFrame# :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => MDataFrame s t ns -> Idx ns -> t -> State# s -> (# State# s, () #)
writeDataFrame# mdf ei x s | I# i <- fromEnum ei = (# writeDataFrameOff# mdf i x s, () #)
{-# INLINE writeDataFrame# #-}

-- | Read a single element at the specified index
readDataFrame# :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => MDataFrame s t ns -> Idx ns -> State# s -> (# State# s, t #)
readDataFrame# mdf ei | I# i <- fromEnum ei = readDataFrameOff# mdf i
{-# INLINE readDataFrame# #-}

class MutableFrame t (ns :: [Nat]) where
    -- | Write a single element at the specified element offset
    writeDataFrameOff# :: MDataFrame s t ns -> Int# -> t -> State# s -> State# s
    -- | Read a single element at the specified element offset
    readDataFrameOff# :: MDataFrame s t ns -> Int# -> State# s -> (# State# s, t #)

instance MutableFrame Float (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetFloat#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetFloat#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Double (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetDouble#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetDouble#
    {-# INLINE readDataFrameOff# #-}


instance MutableFrame Int (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetInt#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetInt#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int8 (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetInt8#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetInt8#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int16 (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetInt16#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetInt16#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int32 (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetInt32#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetInt32#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetWord#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetWord#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word8 (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetWord8#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetWord8#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word16 (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetWord16#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetWord16#
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word32 (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetWord32#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# = js_readArrayOffsetWord32#
    {-# INLINE readDataFrameOff# #-}


instance MutableFrame Word8Clamped (ns :: [Nat]) where
    writeDataFrameOff# = js_writeArrayOffsetWord8Clamped#
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff# m o s = case js_readArrayOffsetWord8Clamped# m o s of
        (# s1, y #) -> (# s1, coerce y #)
    {-# INLINE readDataFrameOff# #-}



foreign import javascript unsafe "new Float32Array($1)"      js_createFloatArray        :: Int -> State# s -> (# State# s, MDataFrame s Float ds #)
foreign import javascript unsafe "new Float64Array($1)"      js_createDoubleArray       :: Int -> State# s -> (# State# s, MDataFrame s Double ds #)
foreign import javascript unsafe "new Int32Array($1)"        js_createIntArray          :: Int -> State# s -> (# State# s, MDataFrame s Int ds #)
foreign import javascript unsafe "new Int32Array($1)"        js_createInt32Array        :: Int -> State# s -> (# State# s, MDataFrame s Int32 ds #)
foreign import javascript unsafe "new Int16Array($1)"        js_createInt16Array        :: Int -> State# s -> (# State# s, MDataFrame s Int16 ds #)
foreign import javascript unsafe "new Int8Array($1)"         js_createInt8Array         :: Int -> State# s -> (# State# s, MDataFrame s Int8 ds #)
foreign import javascript unsafe "new Uint32Array($1)"       js_createWordArray         :: Int -> State# s -> (# State# s, MDataFrame s Word ds #)
foreign import javascript unsafe "new Uint32Array($1)"       js_createWord32Array       :: Int -> State# s -> (# State# s, MDataFrame s Word32 ds #)
foreign import javascript unsafe "new Uint16Array($1)"       js_createWord16Array       :: Int -> State# s -> (# State# s, MDataFrame s Word16 ds #)
foreign import javascript unsafe "new Uint8Array($1)"        js_createWord8Array        :: Int -> State# s -> (# State# s, MDataFrame s Word8 ds #)
foreign import javascript unsafe "new Uint8ClampedArray($1)" js_createWord8ClampedArray :: Int -> State# s -> (# State# s, MDataFrame s Word8Clamped ds #)


foreign import javascript unsafe "$1[$2]" js_readArrayOffsetFloat#        :: MDataFrame s Float        ds -> Int# -> State# s -> (# State# s, Float #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetDouble#       :: MDataFrame s Double       ds -> Int# -> State# s -> (# State# s, Double #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetInt#          :: MDataFrame s Int          ds -> Int# -> State# s -> (# State# s, Int #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetInt8#         :: MDataFrame s Int8         ds -> Int# -> State# s -> (# State# s, Int8 #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetInt16#        :: MDataFrame s Int16        ds -> Int# -> State# s -> (# State# s, Int16 #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetInt32#        :: MDataFrame s Int32        ds -> Int# -> State# s -> (# State# s, Int32 #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetWord#         :: MDataFrame s Word         ds -> Int# -> State# s -> (# State# s, Word #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetWord8#        :: MDataFrame s Word8        ds -> Int# -> State# s -> (# State# s, Word8 #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetWord8Clamped# :: MDataFrame s Word8Clamped ds -> Int# -> State# s -> (# State# s, Int  #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetWord16#       :: MDataFrame s Word16       ds -> Int# -> State# s -> (# State# s, Word16 #)
foreign import javascript unsafe "$1[$2]" js_readArrayOffsetWord32#       :: MDataFrame s Word32       ds -> Int# -> State# s -> (# State# s, Word32 #)



foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetFloat#        :: MDataFrame s Float        ds -> Int# -> Float        -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetDouble#       :: MDataFrame s Double       ds -> Int# -> Double       -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetInt#          :: MDataFrame s Int          ds -> Int# -> Int          -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetInt8#         :: MDataFrame s Int8         ds -> Int# -> Int8         -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetInt16#        :: MDataFrame s Int16        ds -> Int# -> Int16        -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetInt32#        :: MDataFrame s Int32        ds -> Int# -> Int32        -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetWord#         :: MDataFrame s Word         ds -> Int# -> Word         -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetWord8#        :: MDataFrame s Word8        ds -> Int# -> Word8        -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetWord8Clamped# :: MDataFrame s Word8Clamped ds -> Int# -> Word8Clamped -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetWord16#       :: MDataFrame s Word16       ds -> Int# -> Word16       -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetWord32#       :: MDataFrame s Word32       ds -> Int# -> Word32       -> State# s -> State# s
foreign import javascript unsafe "$1[$2] = $3;" js_writeArrayOffsetJSVal#        :: MDataFrame s t            ds -> Int  -> JSVal        -> State# s -> State# s


foreign import javascript unsafe "new Uint8ClampedArray($1)" js_copyDataFrame  :: ArrayT t as -> Int -> MDataFrame s t asbs -> State# s -> (# State# s, () #)
foreign import javascript unsafe "new Uint8ClampedArray($1)" js_copyMDataFrame :: MDataFrame s t as -> Int -> MDataFrame s t asbs -> State# s -> (# State# s, () #)


foreign import javascript unsafe "$1.slice()" js_freeze :: MDataFrame s t as -> State# s -> (# State# s, ArrayT t ds #)
foreign import javascript unsafe "$1.slice()" js_thaw   :: ArrayT t as       -> State# s -> (# State# s, MDataFrame s t ds #)



foreign import javascript unsafe "new ArrayBuffer($1)"       newArrayBuffer#        :: Int -> State# s -> (# State# s, JSVal #)
foreign import javascript unsafe "new Float32Array($1)"      viewFloatArray#        :: JSVal -> State# s -> (# State# s, MDataFrame s Float ds #)
foreign import javascript unsafe "new Float64Array($1)"      viewDoubleArray#       :: JSVal -> State# s -> (# State# s, MDataFrame s Double ds #)
foreign import javascript unsafe "new Int32Array($1)"        viewIntArray#          :: JSVal -> State# s -> (# State# s, MDataFrame s Int ds #)
foreign import javascript unsafe "new Int32Array($1)"        viewInt32Array#        :: JSVal -> State# s -> (# State# s, MDataFrame s Int32 ds #)
foreign import javascript unsafe "new Int16Array($1)"        viewInt16Array#        :: JSVal -> State# s -> (# State# s, MDataFrame s Int16 ds #)
foreign import javascript unsafe "new Int8Array($1)"         viewInt8Array#         :: JSVal -> State# s -> (# State# s, MDataFrame s Int8 ds #)
foreign import javascript unsafe "new Uint32Array($1)"       viewWordArray#         :: JSVal -> State# s -> (# State# s, MDataFrame s Word ds #)
foreign import javascript unsafe "new Uint32Array($1)"       viewWord32Array#       :: JSVal -> State# s -> (# State# s, MDataFrame s Word32 ds #)
foreign import javascript unsafe "new Uint16Array($1)"       viewWord16Array#       :: JSVal -> State# s -> (# State# s, MDataFrame s Word16 ds #)
foreign import javascript unsafe "new Uint8Array($1)"        viewWord8Array#        :: JSVal -> State# s -> (# State# s, MDataFrame s Word8 ds #)
foreign import javascript unsafe "new Uint8ClampedArray($1)" viewWord8ClampedArray# :: JSVal -> State# s -> (# State# s, MDataFrame s Word8Clamped ds #)
foreign import javascript unsafe "$1.buffer"                 arrayBuffer#           :: MDataFrame s t ds -> State# s -> (# State# s, JSVal #)
