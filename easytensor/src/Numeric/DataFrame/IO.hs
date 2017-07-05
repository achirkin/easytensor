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
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.IO
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
---- Mutable DataFrames living in IO.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.IO
    ( MutableFrame (), IODataFrame ()
    , newDataFrame, copyDataFrame, copyMutableDataFrame
    , unsafeFreezeDataFrame
    , freezeDataFrame, thawDataFrame
    , writeDataFrame, readDataFrame
    , writeDataFrameOff, readDataFrameOff
#ifdef ghcjs_HOST_OS
      -- * JavaScript-specific functions
    , MutableArrayBuffer
    , newArrayBuffer, arrayBuffer, viewFloatArray, viewDoubleArray
    , viewIntArray, viewInt32Array, viewInt16Array, viewInt8Array
    , viewWordArray, viewWord32Array, viewWord16Array, viewWord8Array, viewWord8ClampedArray
#endif
    ) where

import           GHC.Prim               (RealWorld)
import           GHC.Types              (Int (..), IO (..))


#ifdef ghcjs_HOST_OS
import           Numeric.Array.Family (ElemTypeInference, ArraySizeInference, ArrayInstanceInference,Word8Clamped)
import           JavaScript.TypedArray.ArrayBuffer
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

-- | Mutable DataFrame that lives in IO.
--   Internal representation is always a ByteArray.
newtype IODataFrame t (ns :: [Nat]) = IODataFrame (MDataFrame RealWorld t (ns :: [Nat]))


-- | Create a new mutable DataFrame.
newDataFrame :: forall t (ns :: [Nat])
#ifdef ghcjs_HOST_OS
               . ( ElemTypeInference t, Dimensions ns)
#else
               . ( PrimBytes t, Dimensions ns)
#endif
              => IO (IODataFrame t ns)
newDataFrame = IODataFrame <$> IO (newDataFrame# @t @ns)
{-# INLINE newDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
copyDataFrame :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
               . ( ConcatList as bs asbs, Dimensions bs
#ifdef ghcjs_HOST_OS
                 , ArraySizeInference as
#else
                 , PrimBytes (DataFrame t as)
#endif
                 )
               => DataFrame t as -> Idx bs -> IODataFrame t asbs -> IO ()
copyDataFrame df ei (IODataFrame mdf) = IO (copyDataFrame# df ei mdf)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMutableDataFrame :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                . ( PrimBytes t
                  , ConcatList as bs asbs
                  , Dimensions bs
                  )
               => IODataFrame t as -> Idx bs -> IODataFrame t asbs -> IO ()
copyMutableDataFrame (IODataFrame mdfA) ei (IODataFrame mdfB)
    = IO (copyMDataFrame# mdfA ei mdfB)
{-# INLINE copyMutableDataFrame #-}


-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame :: forall t (ns :: [Nat])
#ifdef ghcjs_HOST_OS
                  . (MutableFrame t ns, ArraySizeInference ns)
#else
                  . PrimBytes (DataFrame t ns)
#endif
                       => IODataFrame t ns -> IO (DataFrame t ns)
unsafeFreezeDataFrame (IODataFrame mdf) = IO (unsafeFreezeDataFrame# mdf)
{-# INLINE unsafeFreezeDataFrame #-}


-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame :: forall t (ns :: [Nat])
#ifdef ghcjs_HOST_OS
                 . (MutableFrame t ns, ArraySizeInference ns)
#else
                 . PrimBytes (DataFrame t ns)
#endif
                => IODataFrame t ns -> IO (DataFrame t ns)
freezeDataFrame (IODataFrame mdf) = IO (freezeDataFrame# mdf)
{-# INLINE freezeDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame :: forall t (ns :: [Nat])
#ifdef ghcjs_HOST_OS
               . (MutableFrame t ns, ArrayInstanceInference t ns)
#else
               . PrimBytes (DataFrame t ns)
#endif
               => DataFrame t ns -> IO (IODataFrame t ns)
thawDataFrame df = IODataFrame <$> IO (thawDataFrame# df)
{-# INLINE thawDataFrame #-}


-- | Write a single element at the specified index
writeDataFrame :: forall t (ns :: [Nat])
                . ( MutableFrame t ns, Dimensions ns )
               => IODataFrame t ns -> Idx ns -> Scalar t -> IO ()
writeDataFrame (IODataFrame mdf) ei = IO . writeDataFrame# mdf ei . unScalar
{-# INLINE writeDataFrame #-}


-- | Read a single element at the specified index
readDataFrame :: forall t (ns :: [Nat])
                . ( MutableFrame t ns, Dimensions ns )
               => IODataFrame t ns -> Idx ns -> IO (Scalar t)
readDataFrame (IODataFrame mdf) = fmap scalar . IO . readDataFrame# mdf
{-# INLINE readDataFrame #-}


-- | Write a single element at the specified element offset
writeDataFrameOff :: forall t (ns :: [Nat])
                . ( MutableFrame t ns, Dimensions ns )
               => IODataFrame t ns -> Int -> Scalar t -> IO ()
writeDataFrameOff (IODataFrame mdf) (I# i) x = IO $ \s -> (# writeDataFrameOff# mdf i (unScalar x) s, () #)
{-# INLINE writeDataFrameOff #-}


-- | Read a single element at the specified element offset
readDataFrameOff :: forall t (ns :: [Nat])
                . ( MutableFrame t ns, Dimensions ns )
               => IODataFrame t ns -> Int -> IO (Scalar t)
readDataFrameOff (IODataFrame mdf) (I# i) = scalar <$> IO (readDataFrameOff# mdf i)
{-# INLINE readDataFrameOff #-}


#ifdef ghcjs_HOST_OS
newArrayBuffer :: Int -> IO MutableArrayBuffer
newArrayBuffer n = unsafeCoerce# <$> IO (newArrayBuffer# n)

viewFloatArray :: MutableArrayBuffer -> IO (IODataFrame Float ds)
viewFloatArray = fmap IODataFrame . IO . viewFloatArray# . jsval

viewDoubleArray :: MutableArrayBuffer -> IO (IODataFrame Double ds)
viewDoubleArray = fmap IODataFrame . IO . viewDoubleArray# . jsval

viewIntArray :: MutableArrayBuffer -> IO (IODataFrame Int ds)
viewIntArray = fmap IODataFrame . IO . viewIntArray# . jsval

viewInt32Array :: MutableArrayBuffer -> IO (IODataFrame Int32 ds)
viewInt32Array = fmap IODataFrame . IO . viewInt32Array# . jsval

viewInt16Array :: MutableArrayBuffer -> IO (IODataFrame Int16 ds)
viewInt16Array = fmap IODataFrame . IO . viewInt16Array# . jsval

viewInt8Array :: MutableArrayBuffer -> IO (IODataFrame Int8 ds)
viewInt8Array = fmap IODataFrame . IO . viewInt8Array# . jsval

viewWordArray :: MutableArrayBuffer -> IO (IODataFrame Word ds)
viewWordArray = fmap IODataFrame . IO . viewWordArray# . jsval

viewWord32Array :: MutableArrayBuffer -> IO (IODataFrame Word32 ds)
viewWord32Array = fmap IODataFrame . IO . viewWord32Array# . jsval

viewWord16Array :: MutableArrayBuffer -> IO (IODataFrame Word16 ds)
viewWord16Array = fmap IODataFrame . IO . viewWord16Array# . jsval

viewWord8Array :: MutableArrayBuffer -> IO (IODataFrame Word8 ds)
viewWord8Array = fmap IODataFrame . IO . viewWord8Array# . jsval

viewWord8ClampedArray :: MutableArrayBuffer -> IO (IODataFrame Word8Clamped ds)
viewWord8ClampedArray = fmap IODataFrame . IO . viewWord8ClampedArray# . jsval

arrayBuffer :: IODataFrame t ds ->  IO MutableArrayBuffer
arrayBuffer (IODataFrame x) = unsafeCoerce# <$> IO (arrayBuffer# x)

#endif

