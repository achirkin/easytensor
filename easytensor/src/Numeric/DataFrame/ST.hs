{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
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
    , castDataFrame
    , newDataFrame, newPinnedDataFrame, oneMoreDataFrame
    , subDataFrameView, subDataFrameView'
    , copyDataFrame, copyMutableDataFrame
    , copyDataFrame', copyMutableDataFrame'
    , copyDataFrameOff, copyMutableDataFrameOff
    , freezeDataFrame, unsafeFreezeDataFrame
    , thawDataFrame, thawPinDataFrame, unsafeThawDataFrame, withThawDataFrame
    , writeDataFrame, writeDataFrameOff
    , readDataFrame, readDataFrameOff
    , isDataFramePinned, getDataFrameSteps
    ) where

import GHC.ST (ST (..))

import Data.Coerce
import Data.Kind
import Numeric.DataFrame.Internal.Mutable
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Unsafe.Coerce


-- | Mutable DataFrame that lives in ST.
--   Internal representation is always a MutableByteArray.
newtype STDataFrame s (t :: Type) (ns :: [k]) = STDataFrame (MDataFrame s t (ns :: [k]))

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed mutable data frame.
pattern XSTFrame :: forall s  (t :: Type) (xns :: [XNat]) . ()
                 => forall (ns :: [Nat]) . (FixedDims xns ns, Dimensions ns)
                 => STDataFrame s t ns -> STDataFrame s t xns
pattern XSTFrame df <- (mkXSTFramePat -> XSTFramePat df)
  where
    XSTFrame = castDataFrame
{-# COMPLETE XSTFrame #-}

data XSTFramePat s (t :: Type) (xns :: [XNat])
  = forall (ns :: [Nat]) . (FixedDims xns ns, Dimensions ns)
  => XSTFramePat (STDataFrame s t ns)

mkXSTFramePat :: forall s  (t :: Type) (xns :: [XNat])
               . STDataFrame s t xns -> XSTFramePat s t xns
mkXSTFramePat df
  | SomeDims ds <- fromSteps (getDataFrameSteps df)
  , XDims (Dims :: Dims ns) <- (unsafeCoerce ds) :: Dims xns
    = XSTFramePat @s @t @xns @ns (unsafeCoerce df)
  | otherwise
    = error "XSTFrame pattern: impossible args"

-- | Mutable DataFrame of unknown dimensionality
data SomeSTDataFrame s (t :: Type)
  = forall (ns :: [Nat]) . Dimensions ns => SomeSTDataFrame (STDataFrame s t ns)

-- | Allow coercing between @XNat@-indexed and @Nat@-indexed Mutable DataFrames.
castDataFrame ::
       forall (t :: Type) (xns :: [XNat]) (ns :: [Nat]) s
     . FixedDims xns ns
    => STDataFrame s t ns -> STDataFrame s t xns
castDataFrame = coerce (castDataFrame# @t @xns @ns)
{-# INLINE castDataFrame #-}

-- | Create a new mutable DataFrame.
newDataFrame ::
       forall (t :: Type) ns s
     . (PrimBytes t, Dimensions ns) => ST s (STDataFrame s t ns)
newDataFrame = coerce (newDataFrame# @t @_ @ns)
{-# INLINE newDataFrame #-}

-- | Create a new mutable DataFrame.
newPinnedDataFrame ::
       forall (t :: Type) ns s
     . (PrimBytes t, Dimensions ns) => ST s (STDataFrame s t ns)
newPinnedDataFrame = coerce (newPinnedDataFrame# @t @_ @ns)
{-# INLINE newPinnedDataFrame #-}

-- | Create a new mutable DataFrame of the same size.
oneMoreDataFrame ::
       forall (t :: Type) ns s
     . STDataFrame s t ns -> ST s (STDataFrame s t ns)
oneMoreDataFrame = coerce (oneMoreDataFrame# @t @_ @ns)
{-# INLINE oneMoreDataFrame #-}

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   If any of the dims in @as@ or @b@ is unknown (@a ~ XN m@),
--   then this function is unsafe and can throw an `OutOfDimBounds` exception.
--   Otherwise, its safety is guaranteed by the type system.
subDataFrameView ::
       forall (t :: Type) b bi bd as bs asbs s
     . (SubFrameIndexCtx b bi bd, KnownDim bd, ConcatList as (b :+ bs) asbs)
    => Idxs (as +: bi) -> STDataFrame s t asbs -> STDataFrame s t (bd :+ bs)
subDataFrameView = coerce (subDataFrameView# @t @_ @b @bi @bd @as @bs @asbs)

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   This is a simpler version of @subDataFrameView@ that allows
--    to view over one index at a time.
--
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   then this function is unsafe and can throw an `OutOfDimBounds` exception.
--   Otherwise, its safety is guaranteed by the type system.
subDataFrameView' ::
       forall (t :: Type) as bs asbs s
     . ConcatList as bs asbs
    => Idxs as -> STDataFrame s t asbs -> STDataFrame s t bs
subDataFrameView' = coerce (subDataFrameView'# @t @_ @as @bs @asbs)

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyDataFrame'@, this function allows to copy over a range
--    of contiguous indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
--   If (@b ~ XN m@) and (@Idx bi + Dim bd > Dim b@), this function copies only as
--   many elements as fits into the dataframe along this dimension (possibly none).
copyDataFrame ::
       forall (t :: Type) b bi bd as bs asbs s
     . ( SubFrameIndexCtx b bi bd, KnownDim bd, ExactDims bs
       , PrimArray t (DataFrame t (bd :+ bs))
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> DataFrame t (bd :+ bs) -> STDataFrame s t asbs -> ST s ()
copyDataFrame = coerce (copyDataFrame# @t @_ @b @bi @bd @as @bs @asbs)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMutableDataFrame'@, this function allows to copy over a range
--    of contiguous indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
--   If (@b ~ XN m@) and (@Idx bi + Dim bd > Dim b@), this function copies only as
--   many elements as fits into the dataframe along this dimension (possibly none).
copyMutableDataFrame ::
       forall (t :: Type) b bi bd as bs asbs s
     . ( SubFrameIndexCtx b bi bd
       , ExactDims bs
       , PrimBytes t
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> STDataFrame s t (bd :+ bs) -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame = coerce (copyMDataFrame# @t @_ @b @bi @bd @as @bs @asbs)
{-# INLINE copyMutableDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows
--     to copy over one index at a time.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
copyDataFrame' ::
       forall (t :: Type) as bs asbs s
     . ( ExactDims bs
       , PrimArray t (DataFrame t bs)
       , ConcatList as bs asbs )
    => Idxs as -> DataFrame t bs -> STDataFrame s t asbs -> ST s ()
copyDataFrame' = coerce (copyDataFrame'# @t @_ @as @bs @asbs)
{-# INLINE copyDataFrame' #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyMutableDataFrame@ that allows
--     to copy over one index at a time.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
copyMutableDataFrame' ::
       forall (t :: Type) as bs asbs s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Idxs as -> STDataFrame s t bs -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame' = coerce (copyMDataFrame'# @t @_ @as @bs @asbs)
{-# INLINE copyMutableDataFrame' #-}

-- | Copy one DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you write beyond the DataFrame bounds.
copyDataFrameOff ::
       forall (t :: Type) as bs asbs s
     . ( Dimensions bs
       , PrimArray t (DataFrame t bs)
       , ConcatList as bs asbs )
    => Int -> DataFrame t bs -> STDataFrame s t asbs -> ST s ()
copyDataFrameOff = coerce (copyDataFrameOff# @t @_ @as @bs @asbs)
{-# INLINE copyDataFrameOff #-}

-- | Copy one mutable DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you write beyond the DataFrame bounds
copyMutableDataFrameOff ::
       forall (t :: Type) as bs asbs s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Int -> STDataFrame s t bs -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrameOff = coerce (copyMDataFrameOff# @t @_ @as @bs @asbs)
{-# INLINE copyMutableDataFrameOff #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame ::
       forall (t :: Type) ns s
     . PrimArray t (DataFrame t ns)
    => STDataFrame s t ns -> ST s (DataFrame t ns)
unsafeFreezeDataFrame = coerce (unsafeFreezeDataFrame# @t @_ @ns)
{-# INLINE unsafeFreezeDataFrame #-}

-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame ::
       forall (t :: Type) ns s
     . PrimArray t (DataFrame t ns)
    => STDataFrame s t ns -> ST s (DataFrame t ns)
freezeDataFrame = coerce (freezeDataFrame# @t @_ @ns)
{-# INLINE freezeDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame ::
       forall (t :: Type) ns s
     . (Dimensions ns, PrimArray t (DataFrame t ns))
    => DataFrame t ns -> ST s (STDataFrame s t ns)
thawDataFrame = coerce (thawDataFrame# @t @_ @ns)
{-# INLINE thawDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame ::
       forall (t :: Type) ns s
     . (Dimensions ns, PrimArray t (DataFrame t ns))
    => DataFrame t ns -> ST s (STDataFrame s t ns)
thawPinDataFrame = coerce (thawPinDataFrame# @t @_ @ns)
{-# INLINE thawPinDataFrame #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame ::
       forall (t :: Type) ns s
     . (Dimensions ns, PrimArray t (DataFrame t ns))
    => DataFrame t ns
    -> ST s (STDataFrame s t ns)
unsafeThawDataFrame = coerce (unsafeThawDataFrame# @t @_ @ns)
{-# INLINE unsafeThawDataFrame #-}

-- | Given two continuations @f@ and @g@.
--   If the input DataFrame is a single broadcast value, use it in @f@.
--   Otherwise, create a new mutable DataFrame and copy content of immutable one
--   in there; then use it in @g@.
--
--   This function is useful when @thawDataFrame@ cannot be used due to
--   @Dimensions ns@ constraint being not available.
withThawDataFrame ::
       forall (t :: Type) ns r s
     . PrimArray t (DataFrame t ns)
    => (t -> ST s r)
    -> (STDataFrame s t ns -> ST s r)
    -> DataFrame t ns -> ST s r
withThawDataFrame = coerce (withThawDataFrame# @t @_ @ns @r)

-- | Write a single element at the specified element offset.
--
--   This is a low-level write function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you write beyond the DataFrame bounds.
writeDataFrameOff ::
       forall (t :: Type) ns s
     . PrimBytes (DataFrame t ('[] :: KindOf ns))
    => STDataFrame s t ns -> Int -> DataFrame t ('[] :: KindOf ns) -> ST s ()
writeDataFrameOff = coerce (writeDataFrameOff# @t @_ @ns)
{-# INLINE writeDataFrameOff #-}

-- | Write a single element at the specified index.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @ns@ is unknown (@n ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
writeDataFrame ::
       forall (t :: Type) ns s
     . PrimBytes (DataFrame t ('[] :: KindOf ns))
    => STDataFrame s t ns -> Idxs ns -> DataFrame t ('[] :: KindOf ns) -> ST s ()
writeDataFrame = coerce (writeDataFrame# @t @_ @ns)
{-# INLINE writeDataFrame #-}

-- | Read a single element at the specified element offset.
--
--   This is a low-level read function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you read beyond the DataFrame bounds.
readDataFrameOff ::
       forall (t :: Type) ns s
     . PrimBytes (DataFrame t ('[] :: KindOf ns))
    => STDataFrame s t ns -> Int -> ST s (DataFrame t ('[] :: KindOf ns))
readDataFrameOff = coerce (readDataFrameOff# @t @_ @ns)
{-# INLINE readDataFrameOff #-}

-- | Read a single element at the specified index.
--
--   If any of the dims in @ns@ is unknown (@n ~ XN m@),
--   then this function is unsafe and can throw an `OutOfDimBounds` exception.
--   Otherwise, its safety is guaranteed by the type system.
readDataFrame ::
       forall (t :: Type) ns s
     . PrimBytes (DataFrame t ('[] :: KindOf ns))
    => STDataFrame s t ns -> Idxs ns -> ST s (DataFrame t ('[] :: KindOf ns))
readDataFrame = coerce (readDataFrame# @t @_ @ns)
{-# INLINE readDataFrame #-}

-- | Check if the byte array wrapped by this DataFrame is pinned,
--   which means cannot be relocated by GC.
isDataFramePinned ::
       forall (t :: Type) ns s . STDataFrame s t ns -> Bool
isDataFramePinned = coerce (isDataFramePinned# @t @_ @ns)
{-# INLINE isDataFramePinned #-}

-- | Get cumulative dimensions @ns@ of a @STDataFrame s t ns@
getDataFrameSteps ::
       forall (t :: Type) ns s . STDataFrame s t ns -> CumulDims
getDataFrameSteps = coerce (getDataFrameSteps# @t @_ @ns)
{-# INLINE getDataFrameSteps #-}

_unusedTopBind :: STDataFrame s t ns
_unusedTopBind = STDataFrame undefined
