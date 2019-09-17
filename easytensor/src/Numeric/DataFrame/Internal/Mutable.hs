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
    , thawDataFrame#, thawPinDataFrame#, unsafeThawDataFrame#
    , uncheckedThawDataFrame#
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
castDataFrame# = unsafeCoerce#
{-# INLINE castDataFrame# #-}

-- | Create a new mutable DataFrame.
newDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (PrimBytes t, Dimensions ns)
    => State# s -> (# State# s, MDataFrame s t ns #)
newDataFrame# s0
    | steps <- cumulDims $ dims @ns
    , n <- cdTotalDim# steps
      = if isTrue# (n ==# 0#)
        then (# s0, MDataFrame# 0# steps (error "Empty DataFrame (DF0)") #)
        else case newByteArray# (n *# byteSize @t undefined) s0 of
               (# s1, mba #) -> (# s1,  MDataFrame# 0# steps mba #)
{-# INLINE newDataFrame# #-}

-- | Create a new mutable DataFrame.
newPinnedDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (PrimBytes t, Dimensions ns)
    => State# s -> (# State# s, MDataFrame s t ns #)
newPinnedDataFrame# s0
    | steps <- cumulDims $ dims @ns
    , n <- cdTotalDim# steps
      = if isTrue# (n ==# 0#)
        then (# s0, MDataFrame# 0# steps (error "Empty DataFrame (DF0)") #)
        else case newAlignedPinnedByteArray#
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
subDataFrameView# ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (SubFrameIndexCtx b bi bd, KnownDim bd, ConcatList as (b :+ bs) asbs)
    => Idxs (as +: bi) -> MDataFrame s t asbs -> MDataFrame s t (bd :+ bs)
subDataFrameView# ei (MDataFrame# offM (CumulDims stepsM) arr)
    = MDataFrame# (case offA of W# w -> word2Int# w)
                  (CumulDims (n * dimVal (dim @bd) : stepsA)) arr
  where
    (offA, stepsA@(n:_)) = getOffAndSteps (W# (int2Word# offM)) stepsM (listIdxs ei)

-- | View a part of a DataFrame.
--
--   This function does not perform a copy.
--   All changes to a new DataFrame will be reflected in the original DataFrame as well.
--
--   This is a simpler version of @subDataFrameView#@ that allows
--    to view over one index at a time.
subDataFrameView'# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ConcatList as bs asbs
    => Idxs as -> MDataFrame s t asbs -> MDataFrame s t bs
subDataFrameView'# ei (MDataFrame# offM (CumulDims stepsM) arr)
    = MDataFrame# (case offA of W# w -> word2Int# w) (CumulDims stepsA) arr
  where
    (offA, stepsA) = getOffAndSteps (W# (int2Word# offM)) stepsM (listIdxs ei)

getOffAndSteps :: Word -> [Word] -> [Word] -> (Word, [Word])
getOffAndSteps off steps [] = (off, steps)
getOffAndSteps off ~(_:steps@(s:_)) (i:ixs) = getOffAndSteps (off + i*s) steps ixs

-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyDataFrame'#@, this function allows to copy over a range
--    of contiguous indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
copyDataFrame# ::
       forall (t :: Type) (k :: Type)
              (b :: k) (bi :: k) (bd :: k)
              (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( SubFrameIndexCtx b bi bd
       , ExactDims bs
       , PrimBytes t
       , PrimBytes (DataFrame t (bd :+ bs))
       , ConcatList as (b :+ bs) asbs )
    => Idxs (as +: bi) -> DataFrame t (bd :+ bs) -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyDataFrame# ei df (MDataFrame# off steps mba) s
    | I# i <- cdIx steps ei
    = (# writeBytes mba ((off +# i) *# byteSize @t undefined) df s, () #)
{-# INLINE copyDataFrame# #-}

{-# ANN copyMDataFrame# "HLint: ignore Use camelCase" #-}
-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   In contrast to @copyMDataFrame'#@, this function allows to copy over a range
--    of contiguous indices over a single dimension.
--   For example, you can write a 3x4 matrix into a 7x4 matrix, starting at indices 0..3.
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
copyMDataFrame# ei (MDataFrame# offA stepsA arrA) (MDataFrame# offM stepsM arrM) s
    | elS <- byteSize @t undefined
    , lenA <- cdTotalDim# stepsA
    , I# i <- cdIx stepsM ei
    = (# copyMutableByteArray# arrA (offA *# elS)
                               arrM ((offM +# i) *# elS) (lenA *# elS) s
       , () #)
{-# INLINE copyMDataFrame# #-}

{-# ANN copyDataFrame'# "HLint: ignore Use camelCase" #-}
-- | Copy one DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyDataFrame#@ that allows
--     to copy over one index at a time.
copyDataFrame'# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( ExactDims bs
       , PrimBytes t
       , PrimBytes (DataFrame t bs)
       , ConcatList as bs asbs )
    => Idxs as -> DataFrame t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyDataFrame'# ei df (MDataFrame# off steps mba) s
    | I# i <- cdIx steps ei
    = (# writeBytes mba ((off +# i) *# byteSize @t undefined) df s, () #)
{-# INLINE copyDataFrame'# #-}

{-# ANN copyMDataFrame'# "HLint: ignore Use camelCase" #-}
-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
--
--   This is a simpler version of @copyMDataFrame#@ that allows
--     to copy over one index at a time.
copyMDataFrame'# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Idxs as -> MDataFrame s t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyMDataFrame'# ei (MDataFrame# offA stepsA arrA) (MDataFrame# offM stepsM arrM) s
    | elS <- byteSize @t undefined
    , lenA <- cdTotalDim# stepsA
    , I# i <- cdIx stepsM ei
    = (# copyMutableByteArray# arrA (offA *# elS)
                               arrM ((offM +# i) *# elS) (lenA *# elS) s
       , () #)
{-# INLINE copyMDataFrame'# #-}

-- | Copy one DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
copyDataFrameOff# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . ( ExactDims bs
       , PrimBytes t
       , PrimBytes (DataFrame t bs)
       , ConcatList as bs asbs )
    => Int -> DataFrame t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyDataFrameOff# (I# off) df (MDataFrame# off0 _ mba) s
    = (# writeBytes mba ((off0 +# off) *# byteSize @t undefined) df s, () #)
{-# INLINE copyDataFrameOff# #-}

-- | Copy one mutable DataFrame into another mutable DataFrame by offset in
--   primitive elements.
--
--   This is a low-level copy function; you have to keep in mind the row-major
--   layout of Mutable DataFrames. Offset bounds are not checked.
copyMDataFrameOff# ::
       forall (t :: Type) (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) s
     . (ExactDims bs, PrimBytes t, ConcatList as bs asbs)
    => Int -> MDataFrame s t bs -> MDataFrame s t asbs
    -> State# s -> (# State# s, () #)
copyMDataFrameOff# (I# off) (MDataFrame# offA stepsA arrA)
                            (MDataFrame# offM _      arrM) s
    | elS <- byteSize @t undefined
    , lenA <- cdTotalDim# stepsA
    = (# copyMutableByteArray# arrA (offA *# elS)
                               arrM ((offM +# off) *# elS) (lenA *# elS) s
       , () #)
{-# INLINE copyMDataFrameOff# #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => MDataFrame s t ns
    -> State# s -> (# State# s, DataFrame t ns #)
unsafeFreezeDataFrame# (MDataFrame# offM steps arrM) s0
    | 0# <- cdTotalDim# steps
      = (# s0, fromElems steps offM (unsafeCoerce# arrM) #)
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
      = (# s0, fromElems steps offM (error "Empty DataFrame (DF0)") #)
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
     . (Dimensions ns, PrimBytes (DataFrame t ns))
    => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawDataFrame# df s0
    | 0# <- byteSize df
      = (# s0, MDataFrame# 0# (cumulDims $ dims @ns)
                              (error "Empty DataFrame (DF0)") #)
    | bsize <- byteSize df
    , arrA  <- getBytes df
    , boff  <- byteOffset df
    , (# s1, arrM #) <- newByteArray# bsize s0
    , s2 <- copyByteArray# arrA boff arrM 0# bsize s1
      = (# s2, MDataFrame# 0# (cumulDims $ dims @ns) arrM #)
{-# INLINE thawDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimBytes (DataFrame t ns))
    => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawPinDataFrame# df s0
    | 0# <- byteSize df
      = (# s0, MDataFrame# 0# (cumulDims $ dims @ns)
                              (error "Empty DataFrame (DF0)") #)
    | bsize <- byteSize df
    , arrA  <- getBytes df
    , boff  <- byteOffset df
    , (# s1, arrM #) <- newAlignedPinnedByteArray# bsize (byteAlign df) s0
    , s2 <- copyByteArray# arrA boff arrM 0# bsize s1
      = (# s2, MDataFrame# 0# (cumulDims $ dims @ns) arrM #)
{-# INLINE thawPinDataFrame# #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . (Dimensions ns, PrimBytes (DataFrame t ns), PrimBytes t)
    => DataFrame t ns
    -> State# s -> (# State# s, MDataFrame s t ns #)
unsafeThawDataFrame# df s0
    | elS  <- byteSize @t undefined
    , arrA <- getBytes df
    , boff <- byteOffset df
    , steps <- cumulDims $ dims @ns
    = (# s0
       , MDataFrame# (quotInt# boff elS) steps (unsafeCoerce# arrA)
       #)
{-# INLINE unsafeThawDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   This function is unsafe in that it assumes that the input DataFrame is
--   not @fromScalar@-made (i.e. I assume CumulDims is available).
--   It is only safe to call this function if @uniqueOrCumulDims@ from @PrimArray@
--   returns @Right CumulDims@.
uncheckedThawDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimArray t (DataFrame t ns)
    => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
uncheckedThawDataFrame# df s0
  | (# | (# steps, eOff, arrA #) #) <- arrayContent# df
    = case cdTotalDim# steps of
        0# -> (# s0, MDataFrame# 0# steps (error "Empty DataFrame (DF0)") #)
        elems
         | elS <- byteSize @t undefined
         , bsize <- elS *# elems
         , (# s1, arrM #) <- newByteArray# bsize s0
         , s2 <- copyByteArray# arrA (eOff *# elS) arrM 0# bsize s1
           -> (# s2, MDataFrame# 0# steps arrM #)
  | otherwise = error "unexpected arrayContent# - 'fromScalar'."
{-# INLINE uncheckedThawDataFrame# #-}

-- | Write a single element at the specified element offset
writeDataFrameOff# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Int -> DataFrame t ('[] :: [k])
    -> State# s -> (# State# s, () #)
writeDataFrameOff# (MDataFrame# off _ mba) (I# i) x s
  = (# writeArray mba (off +# i) x s, () #)
{-# INLINE writeDataFrameOff# #-}

-- | Write a single element at the specified index
writeDataFrame# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Idxs ns -> DataFrame t ('[] :: [k])
    -> State# s -> (# State# s, () #)
writeDataFrame# mdf@(MDataFrame# _ st _) ei
  = writeDataFrameOff# mdf (cdIx st ei)
{-# INLINE writeDataFrame# #-}

-- | Read a single element at the specified element offset
readDataFrameOff# ::
       forall (t :: Type) (k :: Type) (ns :: [k]) s
     . PrimBytes (DataFrame t ('[] :: [k]))
    => MDataFrame s t ns -> Int
    -> State# s -> (# State# s, DataFrame t ('[] :: [k]) #)
readDataFrameOff# (MDataFrame# off _ mba) (I# i)
  = readArray @(DataFrame t ('[] :: [k])) mba (off +# i)
{-# INLINE readDataFrameOff# #-}

-- | Read a single element at the specified index
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
