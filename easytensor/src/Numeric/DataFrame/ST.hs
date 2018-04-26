{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
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
    ( STDataFrame (XSTFrame), SomeSTDataFrame (..)
    , newDataFrame, newPinnedDataFrame
    , copyDataFrame, copyMutableDataFrame
    , freezeDataFrame, unsafeFreezeDataFrame
    , thawDataFrame, thawPinDataFrame, unsafeThawDataFrame
    , writeDataFrame, writeDataFrameOff
    , readDataFrame, readDataFrameOff
    ) where


import           GHC.Base
import           GHC.ST                                 (ST (..))

import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Mutable
import           Numeric.Dimensions
import           Numeric.PrimBytes


-- | Mutable DataFrame that lives in ST.
--   Internal representation is always a MutableByteArray.
data family STDataFrame s (t :: Type) (ns :: [k])

-- | Pure wrapper on a mutable byte array
newtype instance STDataFrame s t (ns :: [Nat]) = STDataFrame (MDataFrame s t (ns :: [Nat]))

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed mutable data frame.
data instance STDataFrame s t (xs :: [XNat])
  = forall (ns :: [Nat]) . Dimensions ns
  => XSTFrame (STDataFrame s t ns)

-- | Mutable DataFrame of unknown dimensionality
data SomeSTDataFrame s (t :: Type)
  = forall (ns :: [Nat]) . Dimensions ns => SomeSTDataFrame (STDataFrame s t ns)

-- | Create a new mutable DataFrame.
newDataFrame :: forall t (ns :: [Nat]) s
              . ( PrimBytes t, Dimensions ns)
             => ST s (STDataFrame s t ns)
newDataFrame = STDataFrame <$> ST (newDataFrame# @t @ns)
{-# INLINE newDataFrame #-}


-- | Create a new mutable DataFrame.
newPinnedDataFrame :: forall t (ns :: [Nat]) s
                    . ( PrimBytes t, Dimensions ns)
                   => ST s (STDataFrame s t ns)
newPinnedDataFrame = STDataFrame <$> ST (newPinnedDataFrame# @t @ns)
{-# INLINE newPinnedDataFrame #-}


-- | Copy one DataFrame into another mutable DataFrame at specified position.
copyDataFrame :: forall (t :: Type) (as :: [Nat]) (b' :: Nat) (b :: Nat)
                                    (bs :: [Nat]) (asbs :: [Nat]) s
               . ( PrimBytes t
                 , PrimBytes (DataFrame t (as +: b'))
                 , ConcatList as (b :+ bs) asbs
                 , Dimensions (b :+ bs)
                 )
               => DataFrame t (as +: b') -> Idxs (b :+ bs) -> STDataFrame s t asbs -> ST s ()
copyDataFrame df ei (STDataFrame mdf) = ST (copyDataFrame# df ei mdf)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMutableDataFrame :: forall (t :: Type) (as :: [Nat]) (b' :: Nat) (b :: Nat)
                               (bs :: [Nat]) (asbs :: [Nat]) s
                      . ( PrimBytes t
                        , ConcatList as (b :+ bs) asbs
                        , Dimensions (b :+ bs)
                        )
                     => STDataFrame s t (as +: b') -> Idxs (b :+ bs)
                     -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame (STDataFrame mdfA) ei (STDataFrame mdfB)
    = ST (copyMDataFrame# mdfA ei mdfB)
{-# INLINE copyMutableDataFrame #-}


-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                       . PrimArray t (DataFrame t ns)
                      => STDataFrame s t ns -> ST s (DataFrame t ns)
unsafeFreezeDataFrame (STDataFrame mdf) = ST (unsafeFreezeDataFrame# mdf)
{-# INLINE unsafeFreezeDataFrame #-}


-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                 . PrimArray t (DataFrame t ns)
                => STDataFrame s t ns -> ST s (DataFrame t ns)
freezeDataFrame (STDataFrame mdf) = ST (freezeDataFrame# mdf)
{-# INLINE freezeDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame :: forall (t :: Type) (ns :: [Nat]) s
               . (PrimBytes (DataFrame t ns), PrimBytes t)
              => DataFrame t ns -> ST s (STDataFrame s t ns)
thawDataFrame df = STDataFrame <$> ST (thawDataFrame# df)
{-# INLINE thawDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                  . (PrimBytes (DataFrame t ns), PrimBytes t)
                 => DataFrame t ns -> ST s (STDataFrame s t ns)
thawPinDataFrame df = STDataFrame <$> ST (thawPinDataFrame# df)
{-# INLINE thawPinDataFrame #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                     . (PrimBytes (DataFrame t ns), PrimBytes t)
                    => DataFrame t ns -> ST s (STDataFrame s t ns)
unsafeThawDataFrame df = STDataFrame <$> ST (unsafeThawDataFrame# df)
{-# INLINE unsafeThawDataFrame #-}


-- | Write a single element at the specified index
writeDataFrame :: forall t (ns :: [Nat]) s
                . ( PrimBytes t, Dimensions ns )
               => STDataFrame s t ns -> Idxs ns -> DataFrame t '[] -> ST s ()
writeDataFrame (STDataFrame mdf) ei = ST . writeDataFrame# mdf ei . unsafeCoerce#
{-# INLINE writeDataFrame #-}


-- | Read a single element at the specified index
readDataFrame :: forall (t :: Type) (ns :: [Nat]) s
               . ( PrimBytes t, Dimensions ns )
              => STDataFrame s t ns -> Idxs ns -> ST s (DataFrame t '[])
readDataFrame (STDataFrame mdf) = unsafeCoerce# . ST . readDataFrame# mdf
{-# INLINE readDataFrame #-}


-- | Write a single element at the specified element offset
writeDataFrameOff :: forall (t :: Type) (ns :: [Nat]) s
                   . PrimBytes t
               => STDataFrame s t ns -> Int -> DataFrame t '[]  -> ST s ()
writeDataFrameOff (STDataFrame mdf) (I# i)
  = ST . writeDataFrameOff# mdf i . unsafeCoerce#
{-# INLINE writeDataFrameOff #-}


-- | Read a single element at the specified element offset
readDataFrameOff :: forall (t :: Type) (ns :: [Nat]) s
                  . PrimBytes t
               => STDataFrame s t ns -> Int -> ST s (DataFrame t '[])
readDataFrameOff (STDataFrame mdf) (I# i)
  = unsafeCoerce# (ST (readDataFrameOff# mdf i))
{-# INLINE readDataFrameOff #-}
