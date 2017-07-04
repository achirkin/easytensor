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
    ) where

import           GHC.Prim               (RealWorld)
import           GHC.Types              (Int (..), IO (..))

#ifdef ghcjs_HOST_OS
import           Numeric.Array.Family (ElemTypeInference, ArraySizeInference, ArrayInstanceInference)
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
