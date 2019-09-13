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
    , getDataFrameSteps
    ) where


import GHC.IO  (IO (..))
import GHC.Ptr (Ptr (..))

import Control.Monad.ST                     (RealWorld)
import Data.Coerce
import Data.Kind
import Numeric.DataFrame.Internal.Mutable
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Unsafe.Coerce                        (unsafeCoerce)


-- | Mutable DataFrame that lives in ST.
--   Internal representation is always a MutableByteArray.
data family IODataFrame (t :: Type) (ns :: [k])

-- | Pure wrapper on a mutable byte array
newtype instance IODataFrame t (ns :: [Nat]) = IODataFrame (MDataFrame RealWorld t (ns :: [Nat]))

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed mutable data frame.
data instance IODataFrame t (xs :: [XNat])
  = forall (ns :: [Nat])
  . (KnownXNatTypes xs, FixedDims xs ns, Dimensions ns)
  => XIOFrame (IODataFrame t ns)

-- | Mutable DataFrame of unknown dimensionality
data SomeIODataFrame (t :: Type)
  = forall (ns :: [Nat]) . Dimensions ns => SomeIODataFrame (IODataFrame t ns)

-- | Create a new mutable DataFrame.
newDataFrame :: forall t (ns :: [Nat])
              . ( PrimBytes t, Dimensions ns)
             => IO (IODataFrame t ns)
newDataFrame = coerce (newDataFrame# @t @ns)
{-# INLINE newDataFrame #-}


-- | Create a new mutable DataFrame.
newPinnedDataFrame :: forall t (ns :: [Nat])
                    . ( PrimBytes t, Dimensions ns)
                   => IO (IODataFrame t ns)
newPinnedDataFrame = coerce (newPinnedDataFrame# @t @ns)
{-# INLINE newPinnedDataFrame #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyDataFrame :: forall (t :: Type)
                        (b :: Nat) (bi :: Nat) (bd :: Nat)
                        (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
               . ( (b + 1) ~ (bi + bd)
                 , PrimBytes t
                 , PrimBytes (DataFrame t (bd :+ bs))
                 , ConcatList as (b :+ bs) asbs
                 )
              => Idxs (as +: bi) -> DataFrame t (bd :+ bs)
              -> IODataFrame t asbs -> IO ()
copyDataFrame = coerce (copyDataFrame# @t @b @bi @bd @as @bs @asbs)
{-# INLINE copyDataFrame #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'@, this function allows to copy over a range of contiguous
--   indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyMutableDataFrame :: forall (t :: Type)
                               (b :: Nat) (bi :: Nat) (bd :: Nat)
                               (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                      . ( (b + 1) ~ (bi + bd)
                        , PrimBytes t
                        , ConcatList as (b :+ bs) asbs
                        )
                     => Idxs (as +: bi) -> IODataFrame t (bd :+ bs)
                     -> IODataFrame t asbs -> IO ()
copyMutableDataFrame = coerce (copyMDataFrame# @t @b @bi @bd @as @bs @asbs)
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
copyDataFrame' = coerce (copyDataFrame'# @t @as @bs @asbs)
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
copyMutableDataFrame' = coerce (copyMDataFrame'# @t @as @bs @asbs)
{-# INLINE copyMutableDataFrame' #-}

-- | Some operations on a mutable DataFrame are allowed if all dimensions
--   are fixed, others can work with @XNat@.
--   This class provides an interface for operations that support both kinds
--   of indices: @Nat@ and @XNat@.
class KnownDimKind k => IODataFrameDimKind (k :: Type) where
    type SubDataFrameViewCtx (b :: k) (bi :: k) (bd :: k) :: Constraint
    type FreezeDataFrameCtx (t :: Type) (ns :: [k]) :: Constraint
    type ThawDataFrameCtx (t :: Type) (ns :: [k]) :: Constraint
    -- | View a part of a DataFrame.
    --
    --   This function does not perform a copy.
    --   All changes to a new DataFrame will be reflected in the original DataFrame as well.
    subDataFrameView ::
         forall (t :: Type) (b :: k) (bi :: k) (bd :: k)
                (as :: [k]) (bs :: [k]) (asbs :: [k])
       . ( SubDataFrameViewCtx b bi bd
         , ConcatList as (b :+ bs) asbs
         )
      => Idxs (as +: bi) -> IODataFrame t asbs -> IODataFrame t (bd :+ bs)
    -- | View a part of a DataFrame.
    --
    --   This function does not perform a copy.
    --   All changes to a new DataFrame will be reflected in the original DataFrame as well.
    --
    --   This is a simpler version of @subDataFrameView@ that allows to view over one index at a time.
    subDataFrameView' ::
         forall (t :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
       . ConcatList as bs asbs
      => Idxs as -> IODataFrame t asbs -> IODataFrame t bs
    -- | Make a mutable DataFrame immutable, without copying.
    unsafeFreezeDataFrame ::
         forall (t :: Type) (ns :: [k])
       . FreezeDataFrameCtx t ns
      => IODataFrame t ns -> IO (DataFrame t ns)
    -- | Copy content of a mutable DataFrame into a new immutable DataFrame.
    freezeDataFrame ::
         forall (t :: Type) (ns :: [k])
       . FreezeDataFrameCtx t ns
      => IODataFrame t ns -> IO (DataFrame t ns)
    -- | Create a new mutable DataFrame and copy content of immutable one in there.
    thawDataFrame ::
         forall (t :: Type) (ns :: [k])
       . ThawDataFrameCtx t ns
      => DataFrame t ns -> IO (IODataFrame t ns)
    -- | Create a new mutable DataFrame and copy content of immutable one in there.
    --   The result array is pinned and aligned.
    thawPinDataFrame ::
         forall (t :: Type) (ns :: [k])
       . ThawDataFrameCtx t ns
      => DataFrame t ns -> IO (IODataFrame t ns)
    -- | UnsafeCoerces an underlying byte array.
    unsafeThawDataFrame ::
         forall (t :: Type) (ns :: [k])
       . ThawDataFrameCtx t ns
      => DataFrame t ns -> IO (IODataFrame t ns)
    -- | Write a single element at the specified index
    writeDataFrame ::
         forall t (ns :: [k])
       . PrimBytes t
      => IODataFrame t ns
      -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> IO ()
    -- | Read a single element at the specified index
    readDataFrame ::
         forall (t :: Type) (ns :: [k])
       . PrimBytes t
      => IODataFrame t ns -> Idxs ns
      -> IO (DataFrame t ('[] :: [Nat]))
    -- | Write a single element at the specified element offset
    writeDataFrameOff ::
         forall (t :: Type) (ns :: [k])
       . PrimBytes t
      => IODataFrame t ns
      -> Int -> DataFrame t ('[] :: [Nat]) -> IO ()
    -- | Read a single element at the specified element offset
    readDataFrameOff ::
         forall (t :: Type) (ns :: [k])
       . PrimBytes t
      => IODataFrame t ns
      -> Int -> IO (DataFrame t ('[] :: [Nat]))
    -- | Check if the byte array wrapped by this DataFrame is pinned,
    --   which means cannot be relocated by GC.
    isDataFramePinned ::
         forall (t :: Type) (ns :: [k])
       . IODataFrame t ns -> Bool
    -- | Get cumulative dimensions @ns@ of an @IODataFrame t ns@
    getDataFrameSteps ::
         forall (t :: Type) (ns :: [k]) . IODataFrame t ns -> CumulDims
    -- | Allow arbitrary IO operations on a pointer to the beginning of the data
    --   keeping the data from garbage collecting until the arg function returns.
    --
    --   Warning: do not let @Ptr t@ leave the scope of the arg function,
    --            the data may be garbage-collected by then.
    --
    --   Warning: use this function on a pinned DataFrame only;
    --            otherwise, the data may be relocated before the arg fun finishes.
    withDataFramePtr ::
         forall (t :: Type) (ns :: [k]) (r :: Type)
       . PrimBytes t
      => IODataFrame t ns -> ( Ptr t -> IO r ) -> IO r

instance IODataFrameDimKind Nat where
    type SubDataFrameViewCtx b bi bd
           = ((b + 1) ~ (bi + bd), KnownDim bd)
    type FreezeDataFrameCtx t ns
           = PrimArray t (DataFrame t ns)
    type ThawDataFrameCtx t ns
           = (Dimensions ns, PrimBytes (DataFrame t ns), PrimBytes t)
    subDataFrameView ::
           forall t (b :: Nat) bi bd as bs asbs
         . ( SubDataFrameViewCtx b bi bd
           , ConcatList as (b :+ bs) asbs
           )
        => Idxs (as +: bi) -> IODataFrame t asbs -> IODataFrame t (bd :+ bs)
    subDataFrameView = coerce (subDataFrameView# @t @b @bi @bd @as @bs @asbs)
    subDataFrameView' ::
           forall (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
         . ConcatList as bs asbs
        => Idxs as -> IODataFrame t asbs -> IODataFrame t bs
    subDataFrameView' = coerce (subDataFrameView'# @t @as @bs @asbs)
    unsafeFreezeDataFrame :: forall (t :: Type) (ns :: [Nat])
                           . PrimArray t (DataFrame t ns)
                          => IODataFrame t ns -> IO (DataFrame t ns)
    unsafeFreezeDataFrame = coerce (unsafeFreezeDataFrame# @t @ns )
    {-# INLINE unsafeFreezeDataFrame #-}
    freezeDataFrame :: forall (t :: Type) (ns :: [Nat])
                     . PrimArray t (DataFrame t ns)
                    => IODataFrame t ns -> IO (DataFrame t ns)
    freezeDataFrame = coerce (freezeDataFrame# @t @ns)
    {-# INLINE freezeDataFrame #-}
    thawDataFrame :: forall (t :: Type) (ns :: [Nat])
                   . (Dimensions ns, PrimBytes (DataFrame t ns))
                  => DataFrame t ns -> IO (IODataFrame t ns)
    thawDataFrame = coerce (thawDataFrame# @t @ns)
    {-# INLINE thawDataFrame #-}
    thawPinDataFrame :: forall (t :: Type) (ns :: [Nat])
                      . (Dimensions ns, PrimBytes (DataFrame t ns))
                     => DataFrame t ns -> IO (IODataFrame t ns)
    thawPinDataFrame = coerce (thawPinDataFrame# @t @ns)
    {-# INLINE thawPinDataFrame #-}
    unsafeThawDataFrame ::
         forall (t :: Type) (ns :: [Nat])
       . (Dimensions ns, PrimBytes (DataFrame t ns), PrimBytes t)
      => DataFrame t ns -> IO (IODataFrame t ns)
    unsafeThawDataFrame = coerce (unsafeThawDataFrame# @t @ns)
    {-# INLINE unsafeThawDataFrame #-}
    writeDataFrame ::
         forall t (ns :: [Nat])
       . PrimBytes t
      => IODataFrame t ns
      -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> IO ()
    writeDataFrame = coerce (writeDataFrame# @t @ns)
    {-# INLINE writeDataFrame #-}
    readDataFrame ::
         forall (t :: Type) (ns :: [Nat])
       . PrimBytes t
      => IODataFrame t ns -> Idxs ns
      -> IO (DataFrame t ('[] :: [Nat]))
    readDataFrame = coerce (readDataFrame# @t @ns)
    {-# INLINE readDataFrame #-}
    writeDataFrameOff ::
         forall (t :: Type) (ns :: [Nat])
       . PrimBytes t
      => IODataFrame t ns
      -> Int -> DataFrame t ('[] :: [Nat]) -> IO ()
    writeDataFrameOff = coerce (writeDataFrameOff# @t @ns)
    {-# INLINE writeDataFrameOff #-}
    readDataFrameOff ::
         forall (t :: Type) (ns :: [Nat])
       . PrimBytes t
      => IODataFrame t ns
      -> Int -> IO (DataFrame t ('[] :: [Nat]))
    readDataFrameOff = coerce (readDataFrameOff# @t @ns)
    {-# INLINE readDataFrameOff #-}
    isDataFramePinned ::
         forall (t :: Type) (ns :: [Nat])
       . IODataFrame t ns -> Bool
    isDataFramePinned = coerce (isDataFramePinned# @t @ns)
    {-# INLINE isDataFramePinned #-}
    getDataFrameSteps ::
         forall (t :: Type) (ns :: [Nat]) . IODataFrame t ns -> CumulDims
    getDataFrameSteps = coerce (getDataFrameSteps# @t @ns)
    {-# INLINE getDataFrameSteps #-}
    withDataFramePtr (IODataFrame x) k
      = IO $ withDataFramePtr# x (\p -> case k (Ptr p) of IO f -> f)
    {-# INLINE withDataFramePtr #-}

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

instance IODataFrameDimKind XNat where
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
           forall t (b :: XNat) bi bd as bs asbs
         . ( SubDataFrameViewCtx b bi bd
           , ConcatList as (b :+ bs) asbs
           )
        => Idxs (as +: bi) -> IODataFrame t asbs -> IODataFrame t (bd :+ bs)
    subDataFrameView i (XIOFrame a@IODataFrame {})
      | asbs@(Dims :: Dims asbsN) <- dims `inSpaceOf` a
      , Dict <- Dict @(SnocList as bi _)
      , Snoc (ii :: Idxs as) (bi :: Idx bi) <- i
      , (ii', ((D :: Dim bN) :* (Dims :: Dims bsN)), _, Dict)
          <- getSubXDims @as @(b :+ bs) @asbs @_ @_ @asbsN ii asbs
      , Dict <- unsafeCoerce (Dict @(bN ~ bN)) :: Dict (bN ~ DimBound b)
      , i' <- Snoc ii' (unsafeCoerce bi)
        = XIOFrame @t @(bd :+ bs) @(DimBound bd :+ bsN)
            (subDataFrameView @Nat @t @bN @(DimBound bi) @(DimBound bd)
                                      @_ @bsN @asbsN i' a)
      | otherwise
        = error "subDataFrameView: impossible pattern"
    subDataFrameView' ::
           forall (t :: Type) (as :: [XNat]) bs asbs
         . ConcatList as bs asbs
        => Idxs as -> IODataFrame t asbs -> IODataFrame t bs
    subDataFrameView' i (XIOFrame a)
      | asbs@(Dims :: Dims asbsN) <- dims `inSpaceOf` a
      , (i', (_ :: Dims bsN), _, Dict)
          <- getSubXDims @as @bs @asbs @_ @_ @asbsN i asbs
        = XIOFrame @t @bs @bsN (subDataFrameView' @_ @t @_ @bsN @asbsN i' a)
      | otherwise
        = error "subDataFrameView': impossible pattern"
    unsafeFreezeDataFrame :: forall (t :: Type) (ns :: [XNat])
                           . PrimBytes t
                          => IODataFrame t ns -> IO (DataFrame t ns)
    unsafeFreezeDataFrame (XIOFrame (a :: IODataFrame t as))
      | Dict <- inferKnownBackend @_ @t @as
        = XFrame <$> unsafeFreezeDataFrame a
    {-# INLINE unsafeFreezeDataFrame #-}
    freezeDataFrame :: forall (t :: Type) (ns :: [XNat])
                     . PrimBytes t
                    => IODataFrame t ns -> IO (DataFrame t ns)
    freezeDataFrame (XIOFrame (a :: IODataFrame t as))
      | Dict <- inferKnownBackend @_ @t @as
        = XFrame <$> freezeDataFrame a
    {-# INLINE freezeDataFrame #-}
    thawDataFrame :: forall (t :: Type) (ns :: [XNat])
                   . PrimBytes t
                  => DataFrame t ns -> IO (IODataFrame t ns)
    thawDataFrame (XFrame a) = XIOFrame <$> thawDataFrame a
    {-# INLINE thawDataFrame #-}
    thawPinDataFrame :: forall (t :: Type) (ns :: [XNat])
                      . PrimBytes t
                     => DataFrame t ns -> IO (IODataFrame t ns)
    thawPinDataFrame (XFrame a) = XIOFrame <$> thawPinDataFrame a
    {-# INLINE thawPinDataFrame #-}
    unsafeThawDataFrame ::
         forall (t :: Type) (ns :: [XNat])
       . PrimBytes t
      => DataFrame t ns -> IO (IODataFrame t ns)
    unsafeThawDataFrame (XFrame a) = XIOFrame <$> unsafeThawDataFrame a
    {-# INLINE unsafeThawDataFrame #-}
    writeDataFrame ::
         forall t (ns :: [XNat])
       . PrimBytes t
      => IODataFrame t ns
      -> Idxs ns -> DataFrame t ('[] :: [Nat]) -> IO ()
    writeDataFrame (XIOFrame a) ix = writeDataFrame a (unsafeCoerce ix)
    {-# INLINE writeDataFrame #-}
    readDataFrame ::
         forall (t :: Type) (ns :: [XNat])
       . PrimBytes t
      => IODataFrame t ns -> Idxs ns
      -> IO (DataFrame t ('[] :: [Nat]))
    readDataFrame (XIOFrame a) ix = readDataFrame a (unsafeCoerce ix)
    {-# INLINE readDataFrame #-}
    writeDataFrameOff ::
         forall (t :: Type) (ns :: [XNat])
       . PrimBytes t
      => IODataFrame t ns
      -> Int -> DataFrame t ('[] :: [Nat]) -> IO ()
    writeDataFrameOff (XIOFrame a) = writeDataFrameOff a
    {-# INLINE writeDataFrameOff #-}
    readDataFrameOff ::
         forall (t :: Type) (ns :: [XNat])
       . PrimBytes t
      => IODataFrame t ns
      -> Int -> IO (DataFrame t ('[] :: [Nat]))
    readDataFrameOff (XIOFrame a) = readDataFrameOff a
    {-# INLINE readDataFrameOff #-}
    isDataFramePinned ::
         forall (t :: Type) (ns :: [XNat])
       . IODataFrame t ns -> Bool
    isDataFramePinned (XIOFrame a) = isDataFramePinned a
    {-# INLINE isDataFramePinned #-}
    getDataFrameSteps ::
         forall (t :: Type) (ns :: [XNat]) . IODataFrame t ns -> CumulDims
    getDataFrameSteps (XIOFrame a) = getDataFrameSteps a
    {-# INLINE getDataFrameSteps #-}
    withDataFramePtr (XIOFrame (IODataFrame x)) k
      = IO $ withDataFramePtr# x (\p -> case k (Ptr p) of IO f -> f)
    {-# INLINE withDataFramePtr #-}
