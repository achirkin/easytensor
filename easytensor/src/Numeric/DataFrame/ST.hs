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
{-# LANGUAGE TypeOperators             #-}
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
    ( MutableFrame (), STDataFrame (), SomeSTDataFrame (..)
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


import           GHC.TypeLits           (type (<=))
import           GHC.Types              (Int (..))
import           GHC.ST                 (ST(..))

#ifdef ghcjs_HOST_OS
import           Numeric.Array.Family hiding (Scalar)
import           JavaScript.TypedArray.ArrayBuffer.ST
import           GHC.Prim
import           Data.Int
import           Data.Word
import           Data.Maybe
import           GHCJS.Types
import           Numeric.DataFrame.Inference
#endif
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.DataFrame.Mutable
import           Numeric.Dimensions
import           Numeric.Scalar


-- | Mutable DataFrame that lives in ST.
--   Internal representation is always a ByteArray.
newtype STDataFrame s t (ns :: [Nat]) = STDataFrame (MDataFrame s t (ns :: [Nat]))
-- | Mutable DataFrame of unknown dimensionality
data SomeSTDataFrame s t (xns :: [XNat])
  = forall (ns :: [Nat])
  . ( FixedDim xns ns ~ ns
    , FixedXDim xns ns ~ xns
    , NumericFrame t ns
    )
  => SomeSTDataFrame (STDataFrame s t ns)

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
copyDataFrame :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat]) s
               . ( ConcatList as (b :+ bs) asbs, Dimensions (b :+ bs), b' <= b
#ifdef ghcjs_HOST_OS
                 , ArraySizeInference (as +: b')
#else
                 , PrimBytes (DataFrame t (as +: b'))
#endif
                 )
               => DataFrame t (as +: b') -> Idx (b :+ bs) -> STDataFrame s t asbs -> ST s ()
copyDataFrame df ei (STDataFrame mdf) = ST (copyDataFrame# df ei mdf)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMutableDataFrame :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( PrimBytes t
                  , ConcatList as (b :+ bs) asbs
                  , Dimensions (b :+ bs)
                  , b' <= b
                  )
               => STDataFrame s t (as +: b') -> Idx (b :+ bs) -> STDataFrame s t asbs -> ST s ()
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

viewFloatArray :: forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Float (AsXDims ds +: XN 0))
viewFloatArray ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Float))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewFloatArray# (jsval ab) :: ST s (STDataFrame s Float (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Float ds) pn
                 of
        Evidence -> case inferNumericFrame @Float @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewDoubleArray ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Double (AsXDims ds +: XN 0))
viewDoubleArray ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Double))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewDoubleArray# (jsval ab) :: ST s (STDataFrame s Double (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Double ds) pn
                 of
        Evidence -> case inferNumericFrame @Double @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewIntArray ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Int (AsXDims ds +: XN 0))
viewIntArray ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Int))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewIntArray# (jsval ab) :: ST s (STDataFrame s Int (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Int ds) pn
                 of
        Evidence -> case inferNumericFrame @Int @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewInt32Array ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Int32 (AsXDims ds +: XN 0))
viewInt32Array ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Int32))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewInt32Array# (jsval ab) :: ST s (STDataFrame s Int32 (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Int32 ds) pn
                 of
        Evidence -> case inferNumericFrame @Int32 @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewInt16Array ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Int16 (AsXDims ds +: XN 0))
viewInt16Array ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Int16))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewInt16Array# (jsval ab) :: ST s (STDataFrame s Int16 (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Int16 ds) pn
                 of
        Evidence -> case inferNumericFrame @Int16 @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewInt8Array ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Int8 (AsXDims ds +: XN 0))
viewInt8Array ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Int8))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewInt8Array# (jsval ab) :: ST s (STDataFrame s Int8 (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Int8 ds) pn
                 of
        Evidence -> case inferNumericFrame @Int8 @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewWordArray ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Word (AsXDims ds +: XN 0))
viewWordArray ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Word))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewWordArray# (jsval ab) :: ST s (STDataFrame s Word (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Word ds) pn
                 of
        Evidence -> case inferNumericFrame @Word @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewWord32Array ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Word32 (AsXDims ds +: XN 0))
viewWord32Array ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Word32))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewWord32Array# (jsval ab) :: ST s (STDataFrame s Word32 (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Word32 ds) pn
                 of
        Evidence -> case inferNumericFrame @Word32 @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewWord16Array ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Word16 (AsXDims ds +: XN 0))
viewWord16Array ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Word16))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewWord16Array# (jsval ab) :: ST s (STDataFrame s Word16 (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Word16 ds) pn
                 of
        Evidence -> case inferNumericFrame @Word16 @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewWord8Array ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Word8 (AsXDims ds +: XN 0))
viewWord8Array ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Word8))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewWord8Array# (jsval ab) :: ST s (STDataFrame s Word8 (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Word8 ds) pn
                 of
        Evidence -> case inferNumericFrame @Word8 @(ds +: n) of
            Evidence -> SomeSTDataFrame df

viewWord8ClampedArray ::  forall s ds
                . ( Dimensions ds, ArraySizeInference ds)
               => STArrayBuffer s -> ST s (SomeSTDataFrame s Word8Clamped (AsXDims ds +: XN 0))
viewWord8ClampedArray ab = do
    SomeDim (pn@Dn :: Dim (n :: Nat)) <- abDim (I# (byteSize (undefined :: Word8Clamped))) (dim @ds) ab
    df <- fmap STDataFrame . ST $ viewWord8ClampedArray# (jsval ab) :: ST s (STDataFrame s Word8Clamped (ds +: n))
    return $ case unsafeForceFixedDims @ds @n
         `sumEvs` inferSnocDimensions @ds @n
         `sumEvs` inferSnocArrayInstance (undefined :: DataFrame Word8Clamped ds) pn
                 of
        Evidence -> case inferNumericFrame @Word8Clamped @(ds +: n) of
            Evidence -> SomeSTDataFrame df

arrayBuffer :: STDataFrame s t ds ->  ST s (STArrayBuffer s)
arrayBuffer (STDataFrame x) = unsafeCoerce# <$> ST (arrayBuffer# x)


foreign import javascript unsafe "$1.length"     js_abLength     :: STArrayBuffer s -> Int

abDim :: Int -> Dim (ds :: [Nat]) -> STArrayBuffer s -> ST s SomeDim
abDim elS d ab = fromMaybe (SomeDim (Dn :: Dim 0)) . someDimVal . (`quot` (elS * dimVal d)) <$> pure (js_abLength ab)
{-# NOINLINE abDim #-}

unsafeForceFixedDims :: forall ds n
                      . Evidence ( FixedDim (AsXDims ds +: XN 0) (ds +: n) ~ (ds +: n)
                                 , FixedXDim (AsXDims ds +: XN 0) (ds +: n) ~ (AsXDims ds +: XN 0)
                                 )
unsafeForceFixedDims = unsafeCoerce# (Evidence :: Evidence ( (ds +: n) ~  (ds +: n) ,  (ds +: n) ~  (ds +: n) ))

#endif
