{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
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
    , getDataFrameSteps
    ) where

import GHC.ST (ST (..))

import Data.Coerce
import Data.Kind
import Numeric.DataFrame.Internal.Mutable
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Unsafe.Coerce                        (unsafeCoerce)


-- | Mutable DataFrame that lives in ST.
--   Internal representation is always a MutableByteArray.
data family STDataFrame s (t :: Type) (ns :: [k])

-- | Pure wrapper on a mutable byte array
newtype instance STDataFrame s t (ns :: [Nat]) = STDataFrame (MDataFrame s t (ns :: [Nat]))

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed mutable data frame.
data instance STDataFrame s t (xs :: [XNat])
  = forall (ns :: [Nat])
  . (KnownXNatTypes xs, FixedDims xs ns, Dimensions ns)
  => XSTFrame (STDataFrame s t ns)

-- | Mutable DataFrame of unknown dimensionality
data SomeSTDataFrame s (t :: Type)
  = forall (ns :: [Nat]) . Dimensions ns => SomeSTDataFrame (STDataFrame s t ns)

-- | Create a new mutable DataFrame.
newDataFrame :: forall t (ns :: [Nat]) s
              . ( PrimBytes t, Dimensions ns)
             => ST s (STDataFrame s t ns)
newDataFrame = coerce (newDataFrame# @t @ns)
{-# INLINE newDataFrame #-}


-- | Create a new mutable DataFrame.
newPinnedDataFrame :: forall t (ns :: [Nat]) s
                    . ( PrimBytes t, Dimensions ns)
                   => ST s (STDataFrame s t ns)
newPinnedDataFrame = coerce (newPinnedDataFrame# @t @ns)
{-# INLINE newPinnedDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyDataFrame :: forall (t :: Type)
                        (b :: Nat) (bi :: Nat) (bd :: Nat)
                        (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
               . ( (b + 1) ~ (bi + bd)
                 , PrimBytes t
                 , PrimBytes (DataFrame t (bd :+ bs))
                 , ConcatList as (b :+ bs) asbs
                 )
              => Idxs (as +: bi) -> DataFrame t (bd :+ bs)
              -> STDataFrame s t asbs -> ST s ()
copyDataFrame = coerce (copyDataFrame# @t @b @bi @bd @as @bs @asbs)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyMutableDataFrame :: forall (t :: Type)
                               (b :: Nat) (bi :: Nat) (bd :: Nat)
                               (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                      . ( (b + 1) ~ (bi + bd)
                        , PrimBytes t
                        , ConcatList as (b :+ bs) asbs
                        )
                     => Idxs (as +: bi) -> STDataFrame s t (bd :+ bs)
                     -> STDataFrame s t asbs -> ST s ()
copyMutableDataFrame = coerce (copyMDataFrame# @t @b @bi @bd @as @bs @asbs)
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
copyDataFrame' = coerce (copyDataFrame'# @t @as @bs @asbs)
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
copyMutableDataFrame' = coerce (copyMDataFrame'# @t @as @bs @asbs)
{-# INLINE copyMutableDataFrame' #-}

-- | Some operations on a mutable DataFrame are allowed if all dimensions
--   are fixed, others can work with @XNat@.
--   This class provides an interface for operations that support both kinds
--   of indices: @Nat@ and @XNat@.
class KnownDimKind k => STDataFrameDimKind (k :: Type) where
    type SubDataFrameViewCtx (b :: k) (bi :: k) (bd :: k) :: Constraint
    type FreezeDataFrameCtx (t :: Type) (ns :: [k]) :: Constraint
    type ThawDataFrameCtx (t :: Type) (ns :: [k]) :: Constraint
    -- | View a part of a DataFrame.
    --
    --   This function does not perform a copy.
    --   All changes to a new DataFrame will be reflected in the original DataFrame as well.
    subDataFrameView ::
         forall (t :: Type) (b :: k) (bi :: k) (bd :: k)
                (as :: [k]) (bs :: [k]) (asbs :: [k]) s
       . ( SubDataFrameViewCtx b bi bd
         , ConcatList as (b :+ bs) asbs
         )
      => Idxs (as +: bi) -> STDataFrame s t asbs -> STDataFrame s t (bd :+ bs)
    -- | View a part of a DataFrame.
    --
    --   This function does not perform a copy.
    --   All changes to a new DataFrame will be reflected in the original DataFrame as well.
    --
    --   This is a simpler version of @subDataFrameView@ that allows to view over one index at a time.
    subDataFrameView' ::
         forall (t :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
       . ConcatList as bs asbs
      => Idxs as -> STDataFrame s t asbs -> STDataFrame s t bs
    -- | Make a mutable DataFrame immutable, without copying.
    unsafeFreezeDataFrame ::
         forall (t :: Type) (ns :: [k]) s
       . FreezeDataFrameCtx t ns
      => STDataFrame s t ns -> ST s (DataFrame t ns)
    -- | Copy content of a mutable DataFrame into a new immutable DataFrame.
    freezeDataFrame ::
         forall (t :: Type) (ns :: [k]) s
       . FreezeDataFrameCtx t ns
      => STDataFrame s t ns -> ST s (DataFrame t ns)
    -- | Create a new mutable DataFrame and copy content of immutable one in there.
    thawDataFrame ::
         forall (t :: Type) (ns :: [k]) s
       . ThawDataFrameCtx t ns
      => DataFrame t ns -> ST s (STDataFrame s t ns)
    -- | Create a new mutable DataFrame and copy content of immutable one in there.
    --   The result array is pinned and aligned.
    thawPinDataFrame ::
         forall (t :: Type) (ns :: [k]) s
       . ThawDataFrameCtx t ns
      => DataFrame t ns -> ST s (STDataFrame s t ns)
    -- | UnsafeCoerces an underlying byte array.
    unsafeThawDataFrame ::
         forall (t :: Type) (ns :: [k]) s
       . ThawDataFrameCtx t ns
      => DataFrame t ns -> ST s (STDataFrame s t ns)
    -- | Write a single element at the specified index
    writeDataFrame ::
         forall t (ns :: [k]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> ST s ()
    -- | Read a single element at the specified index
    readDataFrame ::
         forall (t :: Type) (ns :: [k]) s
       . PrimBytes t
      => STDataFrame s t ns -> Idxs ns
      -> ST s (DataFrame t ('[] :: [Nat]))
    -- | Write a single element at the specified element offset
    writeDataFrameOff ::
         forall (t :: Type) (ns :: [k]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Int -> DataFrame t ('[] :: [Nat]) -> ST s ()
    -- | Read a single element at the specified element offset
    readDataFrameOff ::
         forall (t :: Type) (ns :: [k]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Int -> ST s (DataFrame t ('[] :: [Nat]))
    -- | Check if the byte array wrapped by this DataFrame is pinned,
    --   which means cannot be relocated by GC.
    isDataFramePinned ::
         forall (t :: Type) (ns :: [k]) s
       . STDataFrame s t ns -> Bool
    -- | Get cumulative dimensions @ns@ of an @STDataFrame s t ns@
    getDataFrameSteps ::
         forall (t :: Type) (ns :: [k]) s . STDataFrame s t ns -> CumulDims

instance STDataFrameDimKind Nat where
    type SubDataFrameViewCtx b bi bd
           = ((b + 1) ~ (bi + bd), KnownDim bd)
    type FreezeDataFrameCtx t ns
           = PrimArray t (DataFrame t ns)
    type ThawDataFrameCtx t ns
           = (Dimensions ns, PrimBytes (DataFrame t ns), PrimBytes t)
    subDataFrameView ::
           forall t (b :: Nat) bi bd as bs asbs s
         . ( SubDataFrameViewCtx b bi bd
           , ConcatList as (b :+ bs) asbs
           )
        => Idxs (as +: bi) -> STDataFrame s t asbs -> STDataFrame s t (bd :+ bs)
    subDataFrameView = coerce (subDataFrameView# @t @b @bi @bd @as @bs @asbs)
    subDataFrameView' ::
           forall (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
         . ConcatList as bs asbs
        => Idxs as -> STDataFrame s t asbs -> STDataFrame s t bs
    subDataFrameView' = coerce (subDataFrameView'# @t @as @bs @asbs)
    unsafeFreezeDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                           . PrimArray t (DataFrame t ns)
                          => STDataFrame s t ns -> ST s (DataFrame t ns)
    unsafeFreezeDataFrame = coerce (unsafeFreezeDataFrame# @t @ns )
    {-# INLINE unsafeFreezeDataFrame #-}
    freezeDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                     . PrimArray t (DataFrame t ns)
                    => STDataFrame s t ns -> ST s (DataFrame t ns)
    freezeDataFrame = coerce (freezeDataFrame# @t @ns)
    {-# INLINE freezeDataFrame #-}
    thawDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                   . (Dimensions ns, PrimBytes (DataFrame t ns))
                  => DataFrame t ns -> ST s (STDataFrame s t ns)
    thawDataFrame = coerce (thawDataFrame# @t @ns)
    {-# INLINE thawDataFrame #-}
    thawPinDataFrame :: forall (t :: Type) (ns :: [Nat]) s
                      . (Dimensions ns, PrimBytes (DataFrame t ns))
                     => DataFrame t ns -> ST s (STDataFrame s t ns)
    thawPinDataFrame = coerce (thawPinDataFrame# @t @ns)
    {-# INLINE thawPinDataFrame #-}
    unsafeThawDataFrame ::
         forall (t :: Type) (ns :: [Nat]) s
       . (Dimensions ns, PrimBytes (DataFrame t ns), PrimBytes t)
      => DataFrame t ns -> ST s (STDataFrame s t ns)
    unsafeThawDataFrame = coerce (unsafeThawDataFrame# @t @ns)
    {-# INLINE unsafeThawDataFrame #-}
    writeDataFrame ::
         forall t (ns :: [Nat]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> ST s ()
    writeDataFrame = coerce (writeDataFrame# @t @ns)
    {-# INLINE writeDataFrame #-}
    readDataFrame ::
         forall (t :: Type) (ns :: [Nat]) s
       . PrimBytes t
      => STDataFrame s t ns -> Idxs ns
      -> ST s (DataFrame t ('[] :: [Nat]))
    readDataFrame = coerce (readDataFrame# @t @ns)
    {-# INLINE readDataFrame #-}
    writeDataFrameOff ::
         forall (t :: Type) (ns :: [Nat]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Int -> DataFrame t ('[] :: [Nat]) -> ST s ()
    writeDataFrameOff = coerce (writeDataFrameOff# @t @ns)
    {-# INLINE writeDataFrameOff #-}
    readDataFrameOff ::
         forall (t :: Type) (ns :: [Nat]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Int -> ST s (DataFrame t ('[] :: [Nat]))
    readDataFrameOff = coerce (readDataFrameOff# @t @ns)
    {-# INLINE readDataFrameOff #-}
    isDataFramePinned ::
         forall (t :: Type) (ns :: [Nat]) s
       . STDataFrame s t ns -> Bool
    isDataFramePinned = coerce (isDataFramePinned# @t @ns)
    {-# INLINE isDataFramePinned #-}
    getDataFrameSteps ::
         forall (t :: Type) (ns :: [Nat]) s . STDataFrame s t ns -> CumulDims
    getDataFrameSteps = coerce (getDataFrameSteps# @t @ns)
    {-# INLINE getDataFrameSteps #-}


getSubXDims ::
       forall (asX :: [XNat]) (bsX :: [XNat]) (asbsX :: [XNat])
              (asN :: [Nat]) (bsN :: [Nat]) (asbsN :: [Nat])
     . ( ConcatList asX bsX asbsX
       , KnownXNatTypes asbsX, FixedDims asbsX asbsN
       )
    => Idxs asX -> Dims asbsN
    -> ( Idxs asN
       , Dims bsN
       , Dims bsX
       , Dict ( ConcatList asN bsN asbsN
              , KnownXNatTypes bsX, FixedDims bsX bsN, Dimensions bsN)
       )
getSubXDims U bs@Dims
  | Dict <- unsafeCoerce (Dict @(asN ~ asN)) :: Dict (asN ~ '[])
  , Dict <- unsafeCoerce (Dict @(bsN ~ bsN)) :: Dict (bsN ~ asbsN)
    = (U, bs, XDims bs, Dict)
getSubXDims (i :* (is :: Idxs asX')) ((D :: Dim a) :* (asbs :: Dims asbsN'))
  | (isN, bs, xbs, Dict) <- getSubXDims @asX' @bsX @_ @_ @bsN @asbsN' is asbs
  , Dict <- unsafeCoerce (Dict @(asN ~ asN))
              :: Dict (asN ~ (a ': StripSuffix bsN asbsN'))
    = (Idx (idxToWord i) :* isN, bs, xbs, Dict)

instance STDataFrameDimKind XNat where
    type SubDataFrameViewCtx b bi bd
           = ( (DimBound bi + DimBound bd) ~ (DimBound b + 1)
             , BoundedDim bd
             , bi ~ N (DimBound bi)
             , bd ~ N (DimBound bd)
             )
    type FreezeDataFrameCtx t ns
           = PrimBytes t
    type ThawDataFrameCtx t ns
           = PrimBytes t
    subDataFrameView ::
           forall t (b :: XNat) bi bd as bs asbs s
         . ( SubDataFrameViewCtx b bi bd
           , ConcatList as (b :+ bs) asbs
           )
        => Idxs (as +: bi) -> STDataFrame s t asbs -> STDataFrame s t (bd :+ bs)
    subDataFrameView i (XSTFrame a@STDataFrame {})
      | asbs@(Dims :: Dims asbsN) <- dims `inSpaceOf` a
      , Dict <- Dict @(SnocList as bi _)
      , Snoc (ii :: Idxs as) (bi :: Idx bi) <- i
      , (ii', ((D :: Dim bN) :* (Dims :: Dims bsN)), _, Dict)
          <- getSubXDims @as @(b :+ bs) @asbs @_ @_ @asbsN ii asbs
      , Dict <- unsafeCoerce (Dict @(bN ~ bN)) :: Dict (bN ~ DimBound b)
      , i' <- Snoc ii' (unsafeCoerce bi)
        = XSTFrame @s @t @(bd :+ bs) @(DimBound bd :+ bsN)
            (subDataFrameView @Nat @t @bN @(DimBound bi) @(DimBound bd)
                                      @_ @bsN @asbsN i' a)
      | otherwise
        = error "subDataFrameView: impossible pattern"
    subDataFrameView' ::
           forall (t :: Type) (as :: [XNat]) bs asbs s
         . ConcatList as bs asbs
        => Idxs as -> STDataFrame s t asbs -> STDataFrame s t bs
    subDataFrameView' i (XSTFrame a)
      | asbs@(Dims :: Dims asbsN) <- dims `inSpaceOf` a
      , (i', (_ :: Dims bsN), _, Dict)
          <- getSubXDims @as @bs @asbs @_ @_ @asbsN i asbs
        = XSTFrame @s @t @bs @bsN (subDataFrameView' @_ @t @_ @bsN @asbsN i' a)
      | otherwise
        = error "subDataFrameView': impossible pattern"
    unsafeFreezeDataFrame :: forall (t :: Type) (ns :: [XNat]) s
                           . PrimBytes t
                          => STDataFrame s t ns -> ST s (DataFrame t ns)
    unsafeFreezeDataFrame (XSTFrame (a :: STDataFrame s t as))
      | Dict <- inferKnownBackend @_ @t @as
        = XFrame <$> unsafeFreezeDataFrame a
    {-# INLINE unsafeFreezeDataFrame #-}
    freezeDataFrame :: forall (t :: Type) (ns :: [XNat]) s
                     . PrimBytes t
                    => STDataFrame s t ns -> ST s (DataFrame t ns)
    freezeDataFrame (XSTFrame (a :: STDataFrame s t as))
      | Dict <- inferKnownBackend @_ @t @as
        = XFrame <$> freezeDataFrame a
    {-# INLINE freezeDataFrame #-}
    thawDataFrame :: forall (t :: Type) (ns :: [XNat]) s
                   . PrimBytes t
                  => DataFrame t ns -> ST s (STDataFrame s t ns)
    thawDataFrame (XFrame a) = XSTFrame <$> thawDataFrame a
    {-# INLINE thawDataFrame #-}
    thawPinDataFrame :: forall (t :: Type) (ns :: [XNat]) s
                      . PrimBytes t
                     => DataFrame t ns -> ST s (STDataFrame s t ns)
    thawPinDataFrame (XFrame a) = XSTFrame <$> thawPinDataFrame a
    {-# INLINE thawPinDataFrame #-}
    unsafeThawDataFrame ::
         forall (t :: Type) (ns :: [XNat]) s
       . PrimBytes t
      => DataFrame t ns -> ST s (STDataFrame s t ns)
    unsafeThawDataFrame (XFrame a) = XSTFrame <$> unsafeThawDataFrame a
    {-# INLINE unsafeThawDataFrame #-}
    writeDataFrame ::
         forall t (ns :: [XNat]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> ST s ()
    writeDataFrame (XSTFrame a) ix = writeDataFrame a (unsafeCoerce ix)
    {-# INLINE writeDataFrame #-}
    readDataFrame ::
         forall (t :: Type) (ns :: [XNat]) s
       . PrimBytes t
      => STDataFrame s t ns -> Idxs ns
      -> ST s (DataFrame t ('[] :: [Nat]))
    readDataFrame (XSTFrame a) ix = readDataFrame a (unsafeCoerce ix)
    {-# INLINE readDataFrame #-}
    writeDataFrameOff ::
         forall (t :: Type) (ns :: [XNat]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Int -> DataFrame t ('[] :: [Nat]) -> ST s ()
    writeDataFrameOff (XSTFrame a) = writeDataFrameOff a
    {-# INLINE writeDataFrameOff #-}
    readDataFrameOff ::
         forall (t :: Type) (ns :: [XNat]) s
       . PrimBytes t
      => STDataFrame s t ns
      -> Int -> ST s (DataFrame t ('[] :: [Nat]))
    readDataFrameOff (XSTFrame a) = readDataFrameOff a
    {-# INLINE readDataFrameOff #-}
    isDataFramePinned ::
         forall (t :: Type) (ns :: [XNat]) s
       . STDataFrame s t ns -> Bool
    isDataFramePinned (XSTFrame a) = isDataFramePinned a
    {-# INLINE isDataFramePinned #-}
    getDataFrameSteps ::
         forall (t :: Type) (ns :: [XNat]) s . STDataFrame s t ns -> CumulDims
    getDataFrameSteps (XSTFrame a) = getDataFrameSteps a
    {-# INLINE getDataFrameSteps #-}
