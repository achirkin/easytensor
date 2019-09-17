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
    , thawDataFrame, thawPinDataFrame, unsafeThawDataFrame
    , uncheckedThawDataFrame
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
    XSTFrame = (unsafeCoerce :: STDataFrame s t ns -> STDataFrame s t xns)
{-# COMPLETE XSTFrame #-}

data XSTFramePat s (t :: Type) (xns :: [XNat])
  = forall (ns :: [Nat]) . (FixedDims xns ns, Dimensions ns)
  => XSTFramePat (STDataFrame s t ns)

mkXSTFramePat :: forall s  (t :: Type) (xns :: [XNat])
               . STDataFrame s t xns -> XSTFramePat s t xns
mkXSTFramePat df
  | SomeDims (ds@Dims :: Dims ns) <- fromSteps (getDataFrameSteps df)
  , Dict <- go @xns @ns (unsafeCoerce ds) ds
    = XSTFramePat @s @t @xns @ns (unsafeCoerce df)
  | otherwise
    = error "XSTFrame pattern: impossible args"
  where
    -- Warning! I rely here on the fact that FixedDim xn n has the same
    -- runtime rep as a single coercion.
    -- If that changes, then the code is broke.
    go :: forall (xs :: [XNat]) (ns :: [Nat])
        . Dims xs -> Dims ns -> Dict (FixedDims xs ns)
    go U U = Dict
    go ((_ :: Dim xn) :* xds) ((_ :: Dim n) :* ds)
      | Dict <- unsafeCoerce (Dict @(n ~ n)) :: Dict (FixedDim xn n)
      , Dict <- go xds ds
        = Dict
    go _ _ = error "XSTFrame pattern / go: impossible args"

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
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (PrimBytes t, Dimensions ns) => ST s (STDataFrame s t ns)
newDataFrame = coerce (newDataFrame# @t @k @ns)
{-# INLINE newDataFrame #-}

-- | Create a new mutable DataFrame.
newPinnedDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (PrimBytes t, Dimensions ns) => ST s (STDataFrame s t ns)
newPinnedDataFrame = coerce (newPinnedDataFrame# @t @k @ns)
{-# INLINE newPinnedDataFrame #-}

-- | Create a new mutable DataFrame of the same size.
oneMoreDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . STDataFrame s t ns -> ST s (STDataFrame s t ns)
oneMoreDataFrame = coerce (oneMoreDataFrame# @t @k @ns)
{-# INLINE oneMoreDataFrame #-}

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
subDataFrameView ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (SubFrameIndexCtx b bi bd, KnownDim bd, ConcatList as (b :+ bs) asbs)
    => Idxs (as +: bi) -> STDataFrame s t asbs -> STDataFrame s t (bd :+ bs)
subDataFrameView = coerce (subDataFrameView# @t @k @b @bi @bd @as @bs @asbs)

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   This is a simpler version of @subDataFrameView@ that allows
--    to view over one index at a time.
subDataFrameView' ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ConcatList as bs asbs
    => Idxs as -> STDataFrame s t asbs -> STDataFrame s t bs
subDataFrameView' = coerce (subDataFrameView'# @t @k @as @bs @asbs)

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyDataFrame'@, this function allows to copy over a range
--    of contiguous indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyDataFrame ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( SubFrameIndexCtx b bi bd
       , ExactDims bs
       , PrimBytes t
       , PrimBytes (DataFrame t (bd :+ bs))
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> DataFrame t (bd :+ bs) -> STDataFrame s t asbs -> ST s ()
copyDataFrame = coerce (copyDataFrame# @t @k @b @bi @bd @as @bs @asbs)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMutableDataFrame'@, this function allows to copy over a range
--    of contiguous indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyMutableDataFrame ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( SubFrameIndexCtx b bi bd
       , ExactDims bs
       , PrimBytes t
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> STDataFrame s t (bd :+ bs) -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame = coerce (copyMDataFrame# @t @k @b @bi @bd @as @bs @asbs)
{-# INLINE copyMutableDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows
--     to copy over one index at a time.
copyDataFrame' ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( ExactDims bs
       , PrimBytes t
       , PrimBytes (DataFrame t bs)
       , ConcatList as bs asbs )
    => Idxs as -> DataFrame t bs -> STDataFrame s t asbs -> ST s ()
copyDataFrame' = coerce (copyDataFrame'# @t @k @as @bs @asbs)
{-# INLINE copyDataFrame' #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyMutableDataFrame@ that allows
--     to copy over one index at a time.
copyMutableDataFrame' ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Idxs as -> STDataFrame s t bs -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame' = coerce (copyMDataFrame'# @t @k @as @bs @asbs)
{-# INLINE copyMutableDataFrame' #-}

-- | Copy one DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
copyDataFrameOff ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( ExactDims bs
       , PrimBytes t
       , PrimBytes (DataFrame t bs)
       , ConcatList as bs asbs )
    => Int -> DataFrame t bs -> STDataFrame s t asbs -> ST s ()
copyDataFrameOff = coerce (copyDataFrameOff# @t @k @as @bs @asbs)
{-# INLINE copyDataFrameOff #-}

-- | Copy one mutable DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
copyMutableDataFrameOff ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Int -> STDataFrame s t bs -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrameOff = coerce (copyMDataFrameOff# @t @k @as @bs @asbs)
{-# INLINE copyMutableDataFrameOff #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => STDataFrame s t ns -> ST s (DataFrame t ns)
unsafeFreezeDataFrame = coerce (unsafeFreezeDataFrame# @t @k @ns)
{-# INLINE unsafeFreezeDataFrame #-}

-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => STDataFrame s t ns -> ST s (DataFrame t ns)
freezeDataFrame = coerce (freezeDataFrame# @t @k @ns)
{-# INLINE freezeDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimBytes (DataFrame t ns))
    => DataFrame t ns -> ST s (STDataFrame s t ns)
thawDataFrame = coerce (thawDataFrame# @t @k @ns)
{-# INLINE thawDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimBytes (DataFrame t ns))
    => DataFrame t ns -> ST s (STDataFrame s t ns)
thawPinDataFrame = coerce (thawPinDataFrame# @t @k @ns)
{-# INLINE thawPinDataFrame #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimBytes (DataFrame t ns), PrimBytes t)
    => DataFrame t ns
    -> ST s (STDataFrame s t ns)
unsafeThawDataFrame = coerce (unsafeThawDataFrame# @t @k @ns)
{-# INLINE unsafeThawDataFrame #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   This function is unsafe in that it assumes that the input DataFrame is
--   not @fromScalar@-made (i.e. I assume CumulDims is available).
--   It is only safe to call this function if @uniqueOrCumulDims@ from @PrimArray@
--   returns @Right CumulDims@.
uncheckedThawDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => DataFrame t ns -> ST s (STDataFrame s t ns)
uncheckedThawDataFrame = coerce (uncheckedThawDataFrame# @t @k @ns)

-- | Write a single element at the specified element offset
writeDataFrameOff ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => STDataFrame s t ns -> Int -> DataFrame t ('[] :: [k]) -> ST s ()
writeDataFrameOff = coerce (writeDataFrameOff# @t @k @ns)
{-# INLINE writeDataFrameOff #-}

-- | Write a single element at the specified index
writeDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => STDataFrame s t ns -> Idxs ns -> DataFrame t ('[] :: [k]) -> ST s ()
writeDataFrame = coerce (writeDataFrame# @t @k @ns)
{-# INLINE writeDataFrame #-}

-- | Read a single element at the specified element offset
readDataFrameOff ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => STDataFrame s t ns -> Int -> ST s (DataFrame t ('[] :: [k]))
readDataFrameOff = coerce (readDataFrameOff# @t @k @ns)
{-# INLINE readDataFrameOff #-}

-- | Read a single element at the specified index
readDataFrame ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => STDataFrame s t ns -> Idxs ns -> ST s (DataFrame t ('[] :: [k]))
readDataFrame = coerce (readDataFrame# @t @k @ns)
{-# INLINE readDataFrame #-}

-- | Check if the byte array wrapped by this DataFrame is pinned,
--   which means cannot be relocated by GC.
isDataFramePinned ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s . STDataFrame s t ns -> Bool
isDataFramePinned = coerce (isDataFramePinned# @t @k @ns)
{-# INLINE isDataFramePinned #-}

-- | Get cumulative dimensions @ns@ of a @STDataFrame s t ns@
getDataFrameSteps ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s . STDataFrame s t ns -> CumulDims
getDataFrameSteps = coerce (getDataFrameSteps# @t @k @ns)
{-# INLINE getDataFrameSteps #-}

_unusedTopBind :: STDataFrame s t ns
_unusedTopBind = STDataFrame undefined
