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
{-# LANGUAGE UnboxedTuples             #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Internal.Mutable
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Interface to perform primitive stateful operations on mutable frames.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Internal.Mutable
    ( MDataFrame ()
    , castDataFrame#
    , newDataFrame#, newPinnedDataFrame#
    , oneMoreDataFrame#
    , subDataFrameView#, subDataFrameView'#
    , copyDataFrame#, copyMDataFrame#
    , copyDataFrame'#, copyMDataFrame'#
    , copyDataFrameOff#, copyMDataFrameOff#
    , freezeDataFrame#, unsafeFreezeDataFrame#
    , thawDataFrame#, thawPinDataFrame#, unsafeThawDataFrame#, withThawDataFrame#
    , writeDataFrame#, writeDataFrameOff#
    , readDataFrame#, readDataFrameOff#
    , withDataFramePtr#, isDataFramePinned#
    , getDataFrameSteps#
    ) where

import GHC.Base
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.PrimBytes

-- | Mutable DataFrame type.
--   Keeps element offset, number of elements, and a mutable byte storage
data MDataFrame s t (ns :: [k])
  = MDataFrame# Int# CumulDims (MutableByteArray# s)

-- | Allow coercing between @XNat@-indexed and @Nat@-indexed Mutable DataFrames.
castDataFrame# ::
       forall (t :: Type) (xns :: [XNat]) (ns :: [Nat]) s
     . FixedDims xns ns
    => MDataFrame s t ns -> MDataFrame s t xns
castDataFrame# (MDataFrame# o c a) = MDataFrame# o c a
{-# INLINE castDataFrame# #-}

-- | Create a new mutable DataFrame.
newDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (PrimBytes t, Dimensions ns)
    => State# s -> (# State# s, MDataFrame s t ns #)
newDataFrame#
    | steps <- cumulDims $ dims @ns
    , n <- cdTotalDim# steps
      = if isTrue# (n ==# 0#)
        then \s0 -> (# s0, error "Empty DataFrame (DF0)" #)
        else \s0 -> case newByteArray# (n *# byteSize @t undefined) s0 of
                          (# s1, mba #) -> (# s1,  MDataFrame# 0# steps mba #)
{-# INLINE newDataFrame# #-}

-- | Create a new mutable DataFrame.
newPinnedDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (PrimBytes t, Dimensions ns)
    => State# s -> (# State# s, MDataFrame s t ns #)
newPinnedDataFrame#
    | steps <- cumulDims $ dims @ns
    , n <- cdTotalDim# steps
      = if isTrue# (n ==# 0#)
        then \s0 -> (# s0, error "Empty DataFrame (DF0)" #)
        else \s0 -> case newAlignedPinnedByteArray#
                          (n *# byteSize @t undefined)
                          (byteAlign @t undefined) s0 of
                          (# s1, mba #) -> (# s1,  MDataFrame# 0# steps mba #)
{-# INLINE newPinnedDataFrame# #-}

-- | Create a new mutable DataFrame of the same size.
oneMoreDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . MDataFrame s t ns -> State# s -> (# State# s, MDataFrame s t ns #)
oneMoreDataFrame# mdf@(MDataFrame# off steps mba) s0
    | 0# <- cdTotalDim# steps = (# s0, mdf #)
    | (# s1, bs #) <- getSizeofMutableByteArray# mba s0
    , (# s2, mba' #) <- newByteArray# (bs -# off) s1
      = (# s2,  MDataFrame# 0# steps mba' #)
{-# INLINE oneMoreDataFrame# #-}

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   If any of the dims in @as@ or @b@ is unknown (@a ~ XN m@),
--   then this function is unsafe and can throw an `OutOfDimBounds` exception.
--   Otherwise, its safety is guaranteed by the type system.
subDataFrameView# ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (SubFrameIndexCtx b bi bd, KnownDim bd, ConcatList as (b :+ bs) asbs)
    => Idxs (as +: bi) -> MDataFrame s t asbs -> MDataFrame s t (bd :+ bs)
subDataFrameView# ei (MDataFrame# offM stepsM arr)
    = MDataFrame# (case offA of I# i -> i) stepsA arr
  where
    (offA, stepsA) = getOffAndStepsSub (I# offM) stepsM ei (dim @bd)

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
subDataFrameView'# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ConcatList as bs asbs
    => Idxs as -> MDataFrame s t asbs -> MDataFrame s t bs
subDataFrameView'# ei (MDataFrame# offM stepsM arr)
    = MDataFrame# (case offA of I# i -> i) stepsA arr
  where
    (offA, stepsA) = getOffAndSteps (I# offM) stepsM ei

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
copyDataFrame# ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( SubFrameIndexCtx b bi bd, KnownDim bd
       , ExactDims bs
       , PrimArray t (DataFrame t (bd :+ bs))
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> DataFrame t (bd :+ bs) -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyDataFrame# ei df (MDataFrame# offM stepsM arrDest)
    | elS <- byteSize @t undefined
    , Just (I# offDest, stepsB)
         <- getOffAndStepsSubM (I# offM) stepsM ei (dim @bd)
    , n <- cdTotalDim# stepsB
    , isTrue# (n ># 0#) -- is there enough space to write anything?
    = withArrayContent
      (\e s -> (# fillArray arrDest offDest n e s, () #))
      (\_ offSrc arrSrc s ->
        (# copyByteArray# arrSrc  (offSrc *# elS)
                          arrDest (offDest *# elS) (n *# elS) s, () #)) df
    | otherwise = \s -> (# s, () #)
{-# INLINE copyDataFrame# #-}

{-# ANN copyMDataFrame# "HLint: ignore Use camelCase" #-}
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
copyMDataFrame# ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( SubFrameIndexCtx b bi bd
       , ExactDims bs
       , PrimBytes t
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> MDataFrame s t (bd :+ bs) -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyMDataFrame# ei (MDataFrame# offA (CumulDims ~(bb:b:_)) arrA)
                   (MDataFrame# offM stepsM arrM)
    | elS <- byteSize @t undefined
    , Just (I# offDest, stepsB)
         <- getOffAndStepsSubM (I# offM) stepsM ei (unsafeCoerce# (quot bb b))
    , n <- cdTotalDim# stepsB
    , isTrue# (n ># 0#) -- is there enough space to write anything?
    = \s -> (# copyMutableByteArray# arrA (offA *# elS)
                                     arrM (offDest *# elS) (n *# elS) s
             , () #)
    | otherwise = \s -> (# s, () #)
{-# INLINE copyMDataFrame# #-}

{-# ANN copyDataFrame'# "HLint: ignore Use camelCase" #-}
-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame@ that allows
--     to copy over one index at a time.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
copyDataFrame'# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( ExactDims bs
       , PrimArray t (DataFrame t bs)
       , ConcatList as bs asbs )
    => Idxs as -> DataFrame t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyDataFrame'# ei df (MDataFrame# offM stepsM arrDest)
    | elS <- byteSize @t undefined
    , Just (I# offDest, stepsA) <- getOffAndStepsM (I# offM) stepsM ei
    , n <- cdTotalDim# stepsA
    = withArrayContent
      (\e s -> (# fillArray arrDest offDest n e s, () #))
      (\_ offSrc arrSrc s ->
        (# copyByteArray# arrSrc  (offSrc *# elS)
                          arrDest (offDest *# elS) (n *# elS) s, () #)) df
    | otherwise = \s -> (# s, () #)
{-# INLINE copyDataFrame'# #-}

{-# ANN copyMDataFrame'# "HLint: ignore Use camelCase" #-}
-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyMutableDataFrame@ that allows
--     to copy over one index at a time.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @as@ is unknown (@a ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
copyMDataFrame'# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Idxs as -> MDataFrame s t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyMDataFrame'# ei (MDataFrame# offA stepsA arrA) (MDataFrame# offM stepsM arrM)
    | elS <- byteSize @t undefined
    , lenA <- cdTotalDim# stepsA
    , Just (I# i) <- cdIxM stepsM ei
    = \s -> (# copyMutableByteArray#
                 arrA (offA *# elS)
                 arrM ((offM +# i) *# elS) (lenA *# elS) s, () #)
    | otherwise = \s -> (# s, () #)
{-# INLINE copyMDataFrame'# #-}

-- | Copy one DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you write beyond the DataFrame bounds.
copyDataFrameOff# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( Dimensions bs
       , PrimArray t (DataFrame t bs)
       , ConcatList as bs asbs )
    => Int -> DataFrame t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyDataFrameOff# (I# off) df (MDataFrame# offM _ arrDest)
    | elS <- byteSize @t undefined
    , offDest <- offM +# off
    = withArrayContent
      (\e s ->
        (# fillArray arrDest offDest
              (case totalDim (dims @bs) of W# n -> word2Int# n) e s, () #))
      (\steps offSrc arrSrc s ->
        (# copyByteArray# arrSrc  (offSrc *# elS)
                          arrDest (offDest *# elS)
                                  (cdTotalDim# steps *# elS) s, () #)) df
{-# INLINE copyDataFrameOff# #-}

-- | Copy one mutable DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you write beyond the DataFrame bounds.
copyMDataFrameOff# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Int -> MDataFrame s t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyMDataFrameOff# (I# off) (MDataFrame# offA stepsA arrA)
                            (MDataFrame# offM _      arrM)
    | elS <- byteSize @t undefined
    , lenA <- cdTotalDim# stepsA
    = \s -> (# copyMutableByteArray#
                 arrA (offA *# elS)
                 arrM ((offM +# off) *# elS) (lenA *# elS) s, () #)
{-# INLINE copyMDataFrameOff# #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => MDataFrame s t ns
    -> State# s -> (# State# s, DataFrame t ns #)
unsafeFreezeDataFrame# (MDataFrame# offM steps arrM) s0
    | 0# <- cdTotalDim# steps
      = (# s0, error "Empty DataFrame (DF0)" #)
    | (# s1, arrA #) <- unsafeFreezeByteArray# arrM s0
      = (# s1, fromElems steps offM arrA #)
{-# INLINE unsafeFreezeDataFrame# #-}

-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => MDataFrame s t ns -> State# s -> (# State# s, DataFrame t ns #)
freezeDataFrame# (MDataFrame# offM steps arrM) s0
    | 0# <- cdTotalDim# steps
      = (# s0, error "Empty DataFrame (DF0)" #)
    | elS  <- byteSize @t undefined
    , n <- cdTotalDim# steps
    , (# s1, mba #) <- newByteArray# (n *# elS) s0
    , s2 <- copyMutableByteArray# arrM (offM *# elS) mba 0# (n *# elS) s1
    , (# s3, arrA #) <- unsafeFreezeByteArray# mba s2
      = (# s3, fromElems steps 0# arrA #)
{-# INLINE freezeDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimArray t (DataFrame t ns))
    => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawDataFrame# df s0
    | nw == 0
      = (# s0, error "Empty DataFrame (DF0)" #)
    | bSize <- case nw of W# w -> byteSize @t undefined *# word2Int# w
    , (# s1, arrM #) <- newByteArray# bSize s0
    , r <- MDataFrame# 0# steps arrM
    , (# s2, _ #) <- copyDataFrameOff# 0 df r s1
      = (# s2, r #)
  where
    nw = cdTotalDim steps
    steps = getSteps (dims @ns) df
{-# INLINE thawDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimArray t (DataFrame t ns))
    => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawPinDataFrame# df s0
    | nw == 0
      = (# s0, error "Empty DataFrame (DF0)" #)
    | bSize <- case nw of W# w -> byteSize @t undefined *# word2Int# w
    , (# s1, arrM #) <- newAlignedPinnedByteArray# bSize (byteAlign @t undefined) s0
    , r <- MDataFrame# 0# steps arrM
    , (# s2, _ #) <- copyDataFrameOff# 0 df r s1
      = (# s2, r #)
  where
    nw = cdTotalDim steps
    steps = getSteps (dims @ns) df
{-# INLINE thawPinDataFrame# #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimArray t (DataFrame t ns))
    => DataFrame t ns
    -> State# s -> (# State# s, MDataFrame s t ns #)
unsafeThawDataFrame# = withArrayContent f g
  where
    f :: t -> State# s -> (# State# s, MDataFrame s t ns #)
    f e s0
      | steps <- cumulDims (dims @ns)
      , n <- cdTotalDim# steps
      , (# s1, arrM #) <- newByteArray# (n *# byteSize @t undefined) s0
      = (# fillArray arrM 0# n e s1, MDataFrame# 0# steps arrM #)
    g :: CumulDims -> Int# -> ByteArray# -> State# s -> (# State# s, MDataFrame s t ns #)
    g steps off ba s0
      = (# s0, MDataFrame# off steps (unsafeCoerce# ba) #)
{-# INLINE unsafeThawDataFrame# #-}

-- | Given two continuations @f@ and @g@.
--   If the input DataFrame is a single broadcast value, use it in @f@.
--   Otherwise, create a new mutable DataFrame and copy content of immutable one
--   in there; then use it in @g@.
--
--   This function is useful when @thawDataFrame@ cannot be used due to
--   @Dimensions ns@ constraint being not available.
withThawDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) (r :: Type) s
     . PrimArray t (DataFrame t ns)
    => (t -> State# s -> (# State# s, r #)) -- ^ f
    -> (MDataFrame s t ns -> State# s -> (# State# s, r #)) -- ^ g
    -> DataFrame t ns
    -> State# s -> (# State# s, r #)
withThawDataFrame# f g = withArrayContent f g'
  where
    g' :: CumulDims -> Int# -> ByteArray# -> State# s -> (# State# s, r #)
    g' steps eOff arrA s0 = case cdTotalDim# steps of
      0# -> g (error "Empty DataFrame (DF0)") s0
      elems
       | elS <- byteSize @t undefined
       , bsize <- elS *# elems
       , (# s1, arrM #) <- newByteArray# bsize s0
       , s2 <- copyByteArray# arrA (eOff *# elS) arrM 0# bsize s1
         -> g (MDataFrame# 0# steps arrM) s2
{-# INLINE withThawDataFrame# #-}

-- | Write a single element at the specified element offset.
--
--   This is a low-level write function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you write beyond the DataFrame bounds.
writeDataFrameOff# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Int -> DataFrame t ('[] :: [k])
    -> State# s -> (# State# s, () #)
writeDataFrameOff# (MDataFrame# off _ mba) (I# i) x s
  = (# writeArray mba (off +# i) x s, () #)
{-# INLINE writeDataFrameOff# #-}

-- | Write a single element at the specified index.
--
--   This function is safe (no `OutOfDimBounds` exception possible).
--   If any of the dims in @ns@ is unknown (@n ~ XN m@),
--   you may happen to write data beyond dataframe bounds.
--   In this case, this function does nothing.
writeDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Idxs ns -> DataFrame t ('[] :: [k])
    -> State# s -> (# State# s, () #)
writeDataFrame# mdf@(MDataFrame# _ st _) ei
  | Just off <- (cdIxM st ei)
  = writeDataFrameOff# mdf off
  | otherwise = const (\s -> (# s, () #))
{-# INLINE writeDataFrame# #-}

-- | Read a single element at the specified element offset.
--
--   This is a low-level read function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
--   You will get an undefined behavior if you read beyond the DataFrame bounds.
readDataFrameOff# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Int
    -> State# s -> (# State# s, DataFrame t ('[] :: [k]) #)
readDataFrameOff# (MDataFrame# off _ mba) (I# i)
  = readArray @(DataFrame t ('[] :: [k])) mba (off +# i)
{-# INLINE readDataFrameOff# #-}

-- | Read a single element at the specified index.
--
--   If any of the dims in @ns@ is unknown (@n ~ XN m@),
--   then this function is unsafe and can throw an `OutOfDimBounds` exception.
--   Otherwise, its safety is guaranteed by the type system.
readDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Idxs ns
    -> State# s -> (# State# s, DataFrame t ('[] :: [k]) #)
readDataFrame# mdf@(MDataFrame# _ st _) ei
  = readDataFrameOff# mdf (cdIx st ei)
{-# INLINE readDataFrame# #-}

-- | Allow arbitrary operations on a pointer to the beginning of the data.
--   Only possible with @RealWord@ state (thus, in @IO@) due to semantics of
--   @touch#@ operation that keeps the data from being garbage collected.
withDataFramePtr# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) (r :: Type)
     . PrimBytes t
    => MDataFrame RealWorld t ns
    -> (Addr# -> State# RealWorld -> (# State# RealWorld, r #))
    -> State# RealWorld -> (# State# RealWorld, r #)
withDataFramePtr# (MDataFrame# off _ mba) k s0
  | (# s1, a #) <- unsafeFreezeByteArray# mba s0
  , (# s2, r #) <- k ( byteArrayContents# a
                       `plusAddr#` (off *# byteSize @t undefined)
                     ) s1
    = (# touch# mba s2, r #)
{-# INLINE withDataFramePtr# #-}

-- | Check if the byte array wrapped by this DataFrame is pinned,
--   which means cannot be relocated by GC.
isDataFramePinned# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s . MDataFrame s t ns -> Bool
isDataFramePinned# (MDataFrame# _ _ mba)
  = isTrue# (isMutableByteArrayPinned# mba)

-- | Get cumulative dimensions @ns@ of a @MDataFrame s t ns@
getDataFrameSteps# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . MDataFrame s t ns -> CumulDims
getDataFrameSteps# (MDataFrame# _ c _) = c
{-# INLINE getDataFrameSteps# #-}

-- | Fill a mutable byte array with the same single element
fillArray :: PrimBytes t
          => MutableByteArray# s
          -> Int# -- ^ Offset in elements
          -> Int# -- ^ Number of elements
          -> t
          -> State# s -> State# s
fillArray mba off n e
  = let lim = off +# n
        go i s | isTrue# (i >=# lim) = s
               | otherwise           = go (i +# 1#) (writeArray mba i e s)
    in  go off
{-# INLINE fillArray #-}
