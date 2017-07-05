{-# LANGUAGE CPP                       #-}
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
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE UnliftedFFITypes          #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.ST
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Mutable DataFrames living in ST.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.ST
    ( MutableFrame (), STDataFrame ()
    , newDataFrame, copyDataFrame, copyMutableDataFrame
    , unsafeFreezeDataFrame
    , freezeDataFrame, thawDataFrame
    , writeDataFrame, readDataFrame
    , writeDataFrameOff, readDataFrameOff
#ifdef ghcjs_HOST_OS
      -- * JavaScript-specific functions
    , STArrayBuffer
    , newArrayBuffer, arrayBuffer, viewFloatArray, viewDoubleArray
    , viewIntArray, viewInt32Array, viewInt16Array, viewInt8Array
    , viewWordArray, viewWord32Array, viewWord16Array, viewWord8Array, viewWord8ClampedArray
#endif
    ) where


import           GHC.Types              (Int (..))
import           GHC.ST                 (ST(..))

#ifdef ghcjs_HOST_OS
import           Numeric.Array.Family (ElemTypeInference, ArraySizeInference, ArrayInstanceInference,Word8Clamped)
import           JavaScript.TypedArray.ArrayBuffer.ST
import           GHC.Prim
import           Data.Int
import           Data.Word
import           GHCJS.Types
#endif
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.DataFrame.Mutable
import           Numeric.Dimensions
import           Numeric.Scalar


-- | Mutable DataFrame that lives in ST.
--   Internal representation is always a ByteArray.
newtype STDataFrame s t (ns :: [Nat]) = STDataFrame (MDataFrame s t (ns :: [Nat]))


-- | Create a new mutable DataFrame.
newDataFrame :: forall t (ns :: [Nat]) s
#ifdef ghcjs_HOST_OS
               . ( ElemTypeInference t, Dimensions ns)
#else
               . ( PrimBytes t, Dimensions ns)
#endif
              => ST s (STDataFrame s t ns)
newDataFrame = STDataFrame <$> ST (newDataFrame# @t @ns)
{-# INLINE newDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
copyDataFrame :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
               . ( ConcatList as bs asbs, Dimensions bs
#ifdef ghcjs_HOST_OS
                 , ArraySizeInference as
#else
                 , PrimBytes (DataFrame t as)
#endif
                 )
               => DataFrame t as -> Idx bs -> STDataFrame s t asbs -> ST s ()
copyDataFrame df ei (STDataFrame mdf) = ST (copyDataFrame# df ei mdf)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMutableDataFrame :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( PrimBytes t
                  , ConcatList as bs asbs
                  , Dimensions bs
                  )
               => STDataFrame s t as -> Idx bs -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame (STDataFrame mdfA) ei (STDataFrame mdfB)
    = ST (copyMDataFrame# mdfA ei mdfB)
{-# INLINE copyMutableDataFrame #-}


-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame :: forall t (ns :: [Nat]) s
#ifdef ghcjs_HOST_OS
                  . (MutableFrame t ns, ArraySizeInference ns)
#else
                  . PrimBytes (DataFrame t ns)
#endif
                       => STDataFrame s t ns -> ST s (DataFrame t ns)
unsafeFreezeDataFrame (STDataFrame mdf) = ST (unsafeFreezeDataFrame# mdf)
{-# INLINE unsafeFreezeDataFrame #-}


-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame :: forall t (ns :: [Nat]) s
#ifdef ghcjs_HOST_OS
                  . (MutableFrame t ns, ArraySizeInference ns)
#else
                  . PrimBytes (DataFrame t ns)
#endif
                 => STDataFrame s t ns -> ST s (DataFrame t ns)
freezeDataFrame (STDataFrame mdf) = ST (freezeDataFrame# mdf)
{-# INLINE freezeDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame :: forall t (ns :: [Nat]) s
#ifdef ghcjs_HOST_OS
               . (MutableFrame t ns, ArrayInstanceInference t ns)
#else
               . PrimBytes (DataFrame t ns)
#endif
               => DataFrame t ns -> ST s (STDataFrame s t ns)
thawDataFrame df = STDataFrame <$> ST (thawDataFrame# df)
{-# INLINE thawDataFrame #-}


-- | Write a single element at the specified index
writeDataFrame :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => STDataFrame s t ns -> Idx ns -> Scalar t -> ST s ()
writeDataFrame (STDataFrame mdf) ei = ST . writeDataFrame# mdf ei . unScalar
{-# INLINE writeDataFrame #-}


-- | Read a single element at the specified index
readDataFrame :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => STDataFrame s t ns -> Idx ns -> ST s (Scalar t)
readDataFrame (STDataFrame mdf) = fmap scalar . ST . readDataFrame# mdf
{-# INLINE readDataFrame #-}


-- | Write a single element at the specified element offset
writeDataFrameOff :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => STDataFrame s t ns -> Int -> Scalar t -> ST s ()
writeDataFrameOff (STDataFrame mdf) (I# i) x = ST $ \s -> (# writeDataFrameOff# mdf i (unScalar x) s, () #)
{-# INLINE writeDataFrameOff #-}


-- | Read a single element at the specified element offset
readDataFrameOff :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => STDataFrame s t ns -> Int -> ST s (Scalar t)
readDataFrameOff (STDataFrame mdf) (I# i) = scalar <$> ST (readDataFrameOff# mdf i)
{-# INLINE readDataFrameOff #-}


#ifdef ghcjs_HOST_OS
newArrayBuffer :: Int -> ST s (STArrayBuffer s)
newArrayBuffer n = unsafeCoerce# <$> ST (newArrayBuffer# n)

viewFloatArray :: STArrayBuffer s -> ST s (STDataFrame s Float ds)
viewFloatArray = fmap STDataFrame . ST . viewFloatArray# . jsval

viewDoubleArray :: STArrayBuffer s -> ST s (STDataFrame s Double ds)
viewDoubleArray = fmap STDataFrame . ST . viewDoubleArray# . jsval

viewIntArray :: STArrayBuffer s -> ST s (STDataFrame s Int ds)
viewIntArray = fmap STDataFrame . ST . viewIntArray# . jsval

viewInt32Array :: STArrayBuffer s -> ST s (STDataFrame s Int32 ds)
viewInt32Array = fmap STDataFrame . ST . viewInt32Array# . jsval

viewInt16Array :: STArrayBuffer s -> ST s (STDataFrame s Int16 ds)
viewInt16Array = fmap STDataFrame . ST . viewInt16Array# . jsval

viewInt8Array :: STArrayBuffer s -> ST s (STDataFrame s Int8 ds)
viewInt8Array = fmap STDataFrame . ST . viewInt8Array# . jsval

viewWordArray :: STArrayBuffer s -> ST s (STDataFrame s Word ds)
viewWordArray = fmap STDataFrame . ST . viewWordArray# . jsval

viewWord32Array :: STArrayBuffer s -> ST s (STDataFrame s Word32 ds)
viewWord32Array = fmap STDataFrame . ST . viewWord32Array# . jsval

viewWord16Array :: STArrayBuffer s -> ST s (STDataFrame s Word16 ds)
viewWord16Array = fmap STDataFrame . ST . viewWord16Array# . jsval

viewWord8Array :: STArrayBuffer s -> ST s (STDataFrame s Word8 ds)
viewWord8Array = fmap STDataFrame . ST . viewWord8Array# . jsval

viewWord8ClampedArray :: STArrayBuffer s -> ST s (STDataFrame s Word8Clamped ds)
viewWord8ClampedArray = fmap STDataFrame . ST . viewWord8ClampedArray# . jsval

arrayBuffer :: STDataFrame s t ds ->  ST s (STArrayBuffer s)
arrayBuffer (STDataFrame x) = unsafeCoerce# <$> ST (arrayBuffer# x)

#endif
