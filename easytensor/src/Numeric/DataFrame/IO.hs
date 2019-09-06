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
-- Module      :  Numeric.DataFrame.IO
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Mutable DataFrames living in IO.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.IO
    ( IODataFrame (XIOFrame), SomeIODataFrame (..)
    , newDataFrame, newPinnedDataFrame
    , subDataFrameView, subDataFrameView'
    , copyDataFrame, copyMutableDataFrame
    , copyDataFrame', copyMutableDataFrame'
    , freezeDataFrame, unsafeFreezeDataFrame
    , thawDataFrame, thawPinDataFrame, unsafeThawDataFrame
    , writeDataFrame, writeDataFrameOff
    , readDataFrame, readDataFrameOff
    , withDataFramePtr, isDataFramePinned
    ) where


import GHC.Base
import GHC.IO   (IO (..))
import GHC.Ptr  (Ptr (..))

import Numeric.DataFrame.Internal.Mutable
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions


-- | Mutable DataFrame that lives in IO.
--   Internal representation is always a MutableByteArray.
data family IODataFrame (t :: Type) (ns :: [k])

-- | Pure wrapper on a mutable byte array
newtype instance IODataFrame t (ns :: [Nat]) = IODataFrame (MDataFrame RealWorld t (ns :: [Nat]))

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed mutable data frame.
data instance IODataFrame t (xs :: [XNat])
  = forall (ns :: [Nat]) . Dimensions ns
  => XIOFrame (IODataFrame t ns)

-- | Mutable DataFrame of unknown dimensionality
data SomeIODataFrame (t :: Type)
  = forall (ns :: [Nat]) . Dimensions ns => SomeIODataFrame (IODataFrame t ns)

-- | Create a new mutable DataFrame.
newDataFrame :: forall t (ns :: [Nat])
              . ( PrimBytes t, Dimensions ns)
             => IO (IODataFrame t ns)
newDataFrame = IODataFrame <$> IO (newDataFrame# @t @ns)
{-# INLINE newDataFrame #-}


-- | Create a new mutable DataFrame.
newPinnedDataFrame :: forall t (ns :: [Nat])
                    . ( PrimBytes t, Dimensions ns)
                   => IO (IODataFrame t ns)
newPinnedDataFrame = IODataFrame <$> IO (newPinnedDataFrame# @t @ns)
{-# INLINE newPinnedDataFrame #-}

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
subDataFrameView :: forall (t :: Type)
                           (b :: Nat) (bi :: Nat) (bd :: Nat)
                           (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                 . ( b ~ (bi + bd - 1)
                   , KnownDim bd
                   , ConcatList as (b :+ bs) asbs
                   )
                => Idxs (as +: bi) -> IODataFrame t asbs -> IODataFrame t (bd :+ bs)
subDataFrameView = coerce (subDataFrameView# @t @b @bi @bd @as @bs @asbs)

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   This is a simpler version of @subDataFrameView@ that allows to view over one index at a time.
subDataFrameView' :: forall (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                   . ConcatList as bs asbs
                  => Idxs as -> IODataFrame t asbs -> IODataFrame t bs
subDataFrameView' = coerce (subDataFrameView'# @t @as @bs @asbs)

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyDataFrame :: forall (t :: Type)
                        (b :: Nat) (bi :: Nat) (bd :: Nat)
                        (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
               . ( b ~ (bi + bd - 1)
                 , PrimBytes t
                 , PrimBytes (DataFrame t (bd :+ bs))
                 , ConcatList as (b :+ bs) asbs
                 )
              => Idxs (as +: bi) -> DataFrame t (bd :+ bs)
              -> IODataFrame t asbs -> IO ()
copyDataFrame ei df (IODataFrame mdf) = IO (copyDataFrame# ei df mdf)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyMutableDataFrame :: forall (t :: Type)
                               (b :: Nat) (bi :: Nat) (bd :: Nat)
                               (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                      . ( b ~ (bi + bd - 1)
                        , PrimBytes t
                        , ConcatList as (b :+ bs) asbs
                        )
                     => Idxs (as +: bi) -> IODataFrame t (bd :+ bs)
                     -> IODataFrame t asbs -> IO ()
copyMutableDataFrame ei (IODataFrame mdfA) (IODataFrame mdfB)
    = IO (copyMDataFrame# ei mdfA mdfB)
{-# INLINE copyMutableDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows to copy over one index at a time.
copyDataFrame' :: forall (t :: Type)
                         (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                . ( PrimBytes t
                  , PrimBytes (DataFrame t bs)
                  , ConcatList as bs asbs
                 )
               => Idxs as -> DataFrame t bs
               -> IODataFrame t asbs -> IO ()
copyDataFrame' ei df (IODataFrame mdf) = IO (copyDataFrame'# ei df mdf)
{-# INLINE copyDataFrame' #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows to copy over one index at a time.
copyMutableDataFrame' :: forall (t :: Type)
                                (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                       . ( PrimBytes t
                         , ConcatList as bs asbs
                         )
                      => Idxs as -> IODataFrame t bs
                      -> IODataFrame t asbs -> IO ()
copyMutableDataFrame' ei (IODataFrame mdfA) (IODataFrame mdfB)
  = IO (copyMDataFrame'# ei mdfA mdfB)
{-# INLINE copyMutableDataFrame' #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame :: forall (t :: Type) (ns :: [Nat])
                       . PrimArray t (DataFrame t ns)
                      => IODataFrame t ns -> IO (DataFrame t ns)
unsafeFreezeDataFrame (IODataFrame mdf) = IO (unsafeFreezeDataFrame# mdf)
{-# INLINE unsafeFreezeDataFrame #-}


-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame :: forall (t :: Type) (ns :: [Nat])
                 . PrimArray t (DataFrame t ns)
                => IODataFrame t ns -> IO (DataFrame t ns)
freezeDataFrame (IODataFrame mdf) = IO (freezeDataFrame# mdf)
{-# INLINE freezeDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame :: forall (t :: Type) (ns :: [Nat])
               . (Dimensions ns, PrimBytes (DataFrame t ns))
              => DataFrame t ns -> IO (IODataFrame t ns)
thawDataFrame df = IODataFrame <$> IO (thawDataFrame# df)
{-# INLINE thawDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame :: forall (t :: Type) (ns :: [Nat])
                  . (Dimensions ns, PrimBytes (DataFrame t ns))
                 => DataFrame t ns -> IO (IODataFrame t ns)
thawPinDataFrame df = IODataFrame <$> IO (thawPinDataFrame# df)
{-# INLINE thawPinDataFrame #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame :: forall (t :: Type) (ns :: [Nat])
                     . ( Dimensions ns
                       , PrimBytes (DataFrame t ns), PrimBytes t)
                    => DataFrame t ns -> IO (IODataFrame t ns)
unsafeThawDataFrame df = IODataFrame <$> IO (unsafeThawDataFrame# df)
{-# INLINE unsafeThawDataFrame #-}


-- | Write a single element at the specified index
writeDataFrame :: forall t (ns :: [Nat])
                . PrimBytes t
               => IODataFrame t ns -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> IO ()
writeDataFrame (IODataFrame mdf) ei = IO . writeDataFrame# mdf ei . unsafeCoerce#
{-# INLINE writeDataFrame #-}


-- | Read a single element at the specified index
readDataFrame :: forall (t :: Type) (ns :: [Nat])
               . PrimBytes t
              => IODataFrame t ns -> Idxs ns -> IO (DataFrame t ('[] :: [Nat]))
readDataFrame (IODataFrame mdf) = unsafeCoerce# . IO . readDataFrame# mdf
{-# INLINE readDataFrame #-}


-- | Write a single element at the specified element offset
writeDataFrameOff :: forall (t :: Type) (ns :: [Nat])
                   . PrimBytes t
               => IODataFrame t ns -> Int -> DataFrame t ('[] :: [Nat])  -> IO ()
writeDataFrameOff (IODataFrame mdf) (I# i)
  = IO . writeDataFrameOff# mdf i . unsafeCoerce#
{-# INLINE writeDataFrameOff #-}


-- | Read a single element at the specified element offset
readDataFrameOff :: forall (t :: Type) (ns :: [Nat])
                  . PrimBytes t
               => IODataFrame t ns -> Int -> IO (DataFrame t ('[] :: [Nat]))
readDataFrameOff (IODataFrame mdf) (I# i)
  = unsafeCoerce# (IO (readDataFrameOff# mdf i))
{-# INLINE readDataFrameOff #-}


-- | Check if the byte array wrapped by this DataFrame is pinned,
--   which means cannot be relocated by GC.
isDataFramePinned :: forall (k :: Type) (t :: Type) (ns :: [k])
                   . KnownDimKind k
                  => IODataFrame t ns -> Bool
isDataFramePinned df = case dimKind @k of
    DimNat -> case df of
      IODataFrame x -> isDataFramePinned# x
    DimXNat -> case df of
      XIOFrame (IODataFrame x) -> isDataFramePinned# x


-- | Allow arbitrary IO operations on a pointer to the beginning of the data
--   keeping the data from garbage collecting until the arg function returns.
--
--   Warning: do not let @Ptr t@ leave the scope of the arg function,
--            the data may be garbage-collected by then.
--
--   Warning: use this function on a pinned DataFrame only;
--            otherwise, the data may be relocated before the arg fun finishes.
withDataFramePtr :: forall (k :: Type) (t :: Type) (ns :: [k]) (r :: Type)
                  . (PrimBytes t, KnownDimKind k)
                 => IODataFrame t ns
                 -> ( Ptr t -> IO r )
                 -> IO r
withDataFramePtr df k = case dimKind @k of
    DimNat -> case df of
      IODataFrame x
        -> IO $ withDataFramePtr# x (\p -> case k (Ptr p) of IO f -> f)
    DimXNat -> case df of
      XIOFrame (IODataFrame x)
        -> IO $ withDataFramePtr# x (\p -> case k (Ptr p) of IO f -> f)
