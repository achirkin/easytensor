{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.ST
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Mutable DataFrames living in ST.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.ST
    ( STDataFrame (XSTFrame), SomeSTDataFrame (..)
    , newDataFrame, newPinnedDataFrame
    , subDataFrameView, subDataFrameView'
    , copyDataFrame, copyMutableDataFrame
    , copyDataFrame', copyMutableDataFrame'
    , freezeDataFrame, unsafeFreezeDataFrame
    , thawDataFrame, thawPinDataFrame, unsafeThawDataFrame
    , writeDataFrame, writeDataFrameOff
    , readDataFrame, readDataFrameOff
    , isDataFramePinned
    ) where


import GHC.Base
import GHC.ST   (ST (..))

import Numeric.DataFrame.Internal.Mutable
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions


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

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
subDataFrameView :: forall (t :: Type)
                           (b :: Nat) (bi :: Nat) (bd :: Nat)
                           (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                 . ( b ~ (bi + bd - 1)
                   , KnownDim bd
                   , ConcatList as (b :+ bs) asbs
                   )
                => Idxs (as +: bi) -> STDataFrame s t asbs -> STDataFrame s t (bd :+ bs)
subDataFrameView = coerce (subDataFrameView# @t @b @bi @bd @as @bs @asbs @s)

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   This is a simpler version of @subDataFrameView@ that allows to view over one index at a time.
subDataFrameView' :: forall (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                   . ConcatList as bs asbs
                  => Idxs as -> STDataFrame s t asbs -> STDataFrame s t bs
subDataFrameView' = coerce (subDataFrameView'# @t @as @bs @asbs @s)


-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyDataFrame :: forall (t :: Type)
                        (b :: Nat) (bi :: Nat) (bd :: Nat)
                        (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
               . ( b ~ (bi + bd - 1)
                 , PrimBytes t
                 , PrimBytes (DataFrame t (bd :+ bs))
                 , ConcatList as (b :+ bs) asbs
                 )
              => Idxs (as +: bi) -> DataFrame t (bd :+ bs)
              -> STDataFrame s t asbs -> ST s ()
copyDataFrame ei df (STDataFrame mdf) = ST (copyDataFrame# ei df mdf)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyMutableDataFrame :: forall (t :: Type)
                               (b :: Nat) (bi :: Nat) (bd :: Nat)
                               (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                      . ( b ~ (bi + bd - 1)
                        , PrimBytes t
                        , ConcatList as (b :+ bs) asbs
                        )
                     => Idxs (as +: bi) -> STDataFrame s t (bd :+ bs)
                     -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame ei (STDataFrame mdfA) (STDataFrame mdfB)
    = ST (copyMDataFrame# ei mdfA mdfB)
{-# INLINE copyMutableDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows to copy over one index at a time.
copyDataFrame' :: forall (t :: Type)
                         (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( PrimBytes t
                  , PrimBytes (DataFrame t bs)
                  , ConcatList as bs asbs
                 )
               => Idxs as -> DataFrame t bs
               -> STDataFrame s t asbs -> ST s ()
copyDataFrame' ei df (STDataFrame mdf) = ST (copyDataFrame'# ei df mdf)
{-# INLINE copyDataFrame' #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows to copy over one index at a time.
copyMutableDataFrame' :: forall (t :: Type)
                                (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                       . ( PrimBytes t
                         , ConcatList as bs asbs
                         )
                      => Idxs as -> STDataFrame s t bs
                      -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame' ei (STDataFrame mdfA) (STDataFrame mdfB)
  = ST (copyMDataFrame'# ei mdfA mdfB)
{-# INLINE copyMutableDataFrame' #-}

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
               . (Dimensions ns, PrimBytes (DataFrame t ns))
              => DataFrame t ns -> ST s (STDataFrame s t ns)
thawDataFrame df = STDataFrame <$> ST (thawDataFrame# df)
{-# INLINE thawDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                  . (Dimensions ns, PrimBytes (DataFrame t ns))
                 => DataFrame t ns -> ST s (STDataFrame s t ns)
thawPinDataFrame df = STDataFrame <$> ST (thawPinDataFrame# df)
{-# INLINE thawPinDataFrame #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                     . ( Dimensions ns
                       , PrimBytes (DataFrame t ns), PrimBytes t)
                    => DataFrame t ns -> ST s (STDataFrame s t ns)
unsafeThawDataFrame df = STDataFrame <$> ST (unsafeThawDataFrame# df)
{-# INLINE unsafeThawDataFrame #-}


-- | Write a single element at the specified index
writeDataFrame :: forall t (ns :: [Nat]) s
                . PrimBytes t
               => STDataFrame s t ns -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> ST s ()
writeDataFrame (STDataFrame mdf) ei = ST . writeDataFrame# mdf ei . unsafeCoerce#
{-# INLINE writeDataFrame #-}


-- | Read a single element at the specified index
readDataFrame :: forall (t :: Type) (ns :: [Nat]) s
               . PrimBytes t
              => STDataFrame s t ns -> Idxs ns -> ST s (DataFrame t ('[] :: [Nat]))
readDataFrame (STDataFrame mdf) = unsafeCoerce# . ST . readDataFrame# mdf
{-# INLINE readDataFrame #-}


-- | Write a single element at the specified element offset
writeDataFrameOff :: forall (t :: Type) (ns :: [Nat]) s
                   . PrimBytes t
               => STDataFrame s t ns -> Int -> DataFrame t ('[] :: [Nat])  -> ST s ()
writeDataFrameOff (STDataFrame mdf) (I# i)
  = ST . writeDataFrameOff# mdf i . unsafeCoerce#
{-# INLINE writeDataFrameOff #-}


-- | Read a single element at the specified element offset
readDataFrameOff :: forall (t :: Type) (ns :: [Nat]) s
                  . PrimBytes t
               => STDataFrame s t ns -> Int -> ST s (DataFrame t ('[] :: [Nat]))
readDataFrameOff (STDataFrame mdf) (I# i)
  = unsafeCoerce# (ST (readDataFrameOff# mdf i))
{-# INLINE readDataFrameOff #-}


-- | Check if the byte array wrapped by this DataFrame is pinned,
--   which means cannot be relocated by GC.
isDataFramePinned :: forall (k :: Type) (t :: Type) (ns :: [k]) s
                   . KnownDimKind k
                  => STDataFrame s t ns -> Bool
isDataFramePinned df = case dimKind @k of
    DimNat -> case df of
      STDataFrame x -> isDataFramePinned# x
    DimXNat -> case df of
      XSTFrame (STDataFrame x) -> isDataFramePinned# x
