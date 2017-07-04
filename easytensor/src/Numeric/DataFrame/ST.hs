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
    ) where


import           GHC.Types              (Int (..))
import           GHC.ST                 (ST(..))

#ifdef ghcjs_HOST_OS
import           Numeric.Array.Family (ElemTypeInference, ArraySizeInference, ArrayInstanceInference)
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
