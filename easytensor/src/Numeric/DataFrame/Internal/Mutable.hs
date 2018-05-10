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
{-# LANGUAGE UnboxedTuples             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Internal.Mutable
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Interfrace to perform primitive stateful operations on mutable frames.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Internal.Mutable
    ( MDataFrame ()
    , newDataFrame#, newPinnedDataFrame#
    , copyDataFrame#, copyMDataFrame#
    , freezeDataFrame#, unsafeFreezeDataFrame#
    , thawDataFrame#, thawPinDataFrame#, unsafeThawDataFrame#
    , writeDataFrame#, writeDataFrameOff#
    , readDataFrame#, readDataFrameOff#
    , withDataFramePtr#, isDataFramePinned#
    ) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.PrimBytes


-- | Mutable DataFrame type.
--   Keeps element offset, number of elements, and a mutable byte storage
data MDataFrame s t (ns :: [Nat])
  = MDataFrame# Int# Int# (MutableByteArray# s)


-- | Create a new mutable DataFrame.
newDataFrame# :: forall t (ns :: [Nat]) s
               . ( PrimBytes t, Dimensions ns)
              => State# s -> (# State# s, MDataFrame s t ns #)
newDataFrame# s0
    | W# nw <- totalDim' @ns
    , n <- word2Int# nw
    , (# s1, mba #) <- newByteArray# (n *# byteSize @t undefined) s0
    = (# s1,  MDataFrame# 0# n mba #)
{-# INLINE newDataFrame# #-}

-- | Create a new mutable DataFrame.
newPinnedDataFrame# :: forall t (ns :: [Nat]) s
                     . ( PrimBytes t, Dimensions ns)
                    => State# s -> (# State# s, MDataFrame s t ns #)
newPinnedDataFrame# s0
    | W# nw <- totalDim' @ns
    , n <- word2Int# nw
    , (# s1, mba #)  <- newAlignedPinnedByteArray#
        (n *# byteSize @t undefined)
        (byteAlign @t undefined) s0
    = (# s1,  MDataFrame# 0# n mba #)
{-# INLINE newPinnedDataFrame# #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
copyDataFrame# :: forall (t :: Type) (as :: [Nat]) (b' :: Nat) (b :: Nat)
                         (bs :: [Nat]) (asbs :: [Nat]) s
                . ( PrimBytes t
                  , PrimBytes (DataFrame t (as +: b'))
                  , ConcatList as (b :+ bs) asbs
                  , Dimensions (b :+ bs)
                  )
               => DataFrame t (as +: b') -> Idxs (b :+ bs) -> MDataFrame s t asbs
               -> State# s -> (# State# s, () #)
copyDataFrame# df ei (MDataFrame# off _ mba) s
    | I# i <- fromEnum ei
    = (# writeBytes mba ((off +# i) *# byteSize @t undefined) df s, () #)
{-# INLINE copyDataFrame# #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMDataFrame# :: forall (t :: Type) (as :: [Nat]) (b' :: Nat) (b :: Nat)
                          (bs :: [Nat]) (asbs :: [Nat]) s
                 . ( PrimBytes t
                   , ConcatList as (b :+ bs) asbs
                   , Dimensions (b :+ bs)
                   )
                => MDataFrame s t (as +: b') -> Idxs (b :+ bs) -> MDataFrame s t asbs
                -> State# s -> (# State# s, () #)
copyMDataFrame# (MDataFrame# offA lenA arrA) ei (MDataFrame# offM _ arrM) s
    | elS <- byteSize @t undefined
    , I# i <- fromEnum ei
    = (# copyMutableByteArray# arrA (offA *# elS)
                               arrM ((offM +# i) *# elS) (lenA *# elS) s
       , () #)
{-# INLINE copyMDataFrame# #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                        . PrimArray t (DataFrame t ns)
                       => MDataFrame s t ns
                       -> State# s -> (# State# s, DataFrame t ns #)
unsafeFreezeDataFrame# (MDataFrame# offM lenM arrM) s1
    | (# s2, arrA #) <- unsafeFreezeByteArray# arrM s1
    = (# s2, fromElems offM lenM arrA #)
{-# INLINE unsafeFreezeDataFrame# #-}

-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                  . PrimArray t (DataFrame t ns)
                 => MDataFrame s t ns -> State# s -> (# State# s, DataFrame t ns #)
freezeDataFrame# (MDataFrame# offM n arrM) s0
    | elS  <- byteSize @t undefined
    , (# s1, mba #) <- newByteArray# (n *# elS) s0
    , s2 <- copyMutableByteArray# arrM (offM *# elS) mba 0# (n *# elS) s1
    , (# s3, arrA #) <- unsafeFreezeByteArray# mba s2
    = (# s3, fromElems 0# n arrA #)
{-# INLINE freezeDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                . (PrimBytes (DataFrame t ns), PrimBytes t)
               => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawDataFrame# df s0
    | elS  <- byteSize @t undefined
    , arrA  <- getBytes df
    , boff <- byteOffset df
    , bsize <- byteSize df
    , (# s1, arrM #) <- newByteArray# bsize s0
    , s2 <- copyByteArray# arrA boff arrM 0# bsize s1
    = (# s2, MDataFrame# 0# (quotInt# bsize elS) arrM #)
{-# INLINE thawDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
--   The result array is pinned and aligned.
thawPinDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                . (PrimBytes (DataFrame t ns), PrimBytes t)
               => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawPinDataFrame# df s0
    | elS  <- byteSize @t undefined
    , arrA  <- getBytes df
    , boff <- byteOffset df
    , bsize <- byteSize df
    , (# s1, arrM #) <- newAlignedPinnedByteArray# bsize (byteAlign df) s0
    , s2 <- copyByteArray# arrA boff arrM 0# bsize s1
    = (# s2, MDataFrame# 0# (quotInt# bsize elS) arrM #)
{-# INLINE thawPinDataFrame# #-}

-- | UnsafeCoerces an underlying byte array.
unsafeThawDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                      . (PrimBytes (DataFrame t ns), PrimBytes t)
                     => DataFrame t ns
                     -> State# s -> (# State# s, MDataFrame s t ns #)
unsafeThawDataFrame# df s0
    | elS  <- byteSize @t undefined
    , arrA  <- getBytes df
    , boff <- byteOffset df
    , bsize <- byteSize df
    = (# s0
       , MDataFrame# (quotInt# boff elS) (quotInt# bsize elS) (unsafeCoerce# arrA)
       #)
{-# INLINE unsafeThawDataFrame# #-}


-- | Write a single element at the specified element offset
writeDataFrameOff# :: forall (t :: Type) (ns :: [Nat]) s
                    . PrimBytes t
                   => MDataFrame s t ns -> Int# -> t -> State# s -> (# State# s, () #)
writeDataFrameOff# (MDataFrame# off _ mba) i x s
  = (# writeArray mba (off +# i) x s, () #)
{-# INLINE writeDataFrameOff# #-}

-- | Write a single element at the specified index
writeDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                 . ( PrimBytes t, Dimensions ns )
                => MDataFrame s t ns -> Idxs ns -> t -> State# s -> (# State# s, () #)
writeDataFrame# mdf ei | I# i <- fromEnum ei = writeDataFrameOff# mdf i
{-# INLINE writeDataFrame# #-}

-- | Read a single element at the specified element offset
readDataFrameOff# :: forall (t :: Type) (ns :: [Nat]) s
                   . PrimBytes t
                  => MDataFrame s t ns -> Int# -> State# s -> (# State# s, t #)
readDataFrameOff# (MDataFrame# off _ mba) i = readArray mba (off +# i)
{-# INLINE readDataFrameOff# #-}

-- | Read a single element at the specified index
readDataFrame# :: forall (t :: Type) (ns :: [Nat]) s
                . ( PrimBytes t, Dimensions ns )
               => MDataFrame s t ns -> Idxs ns -> State# s -> (# State# s, t #)
readDataFrame# mdf ei | I# i <- fromEnum ei = readDataFrameOff# mdf i
{-# INLINE readDataFrame# #-}

-- | Allow arbitrary operations on a pointer to the beginning of the data.
--   Only possible with @RealWord@ state (thus, in @IO@) due to semantics of
--   @touch#@ operation that keeps the data from being garbage collected.
withDataFramePtr# :: forall (t :: Type) (ns :: [Nat]) (r :: Type)
                   . PrimBytes t
                  => MDataFrame RealWorld t ns
                  -> ( Addr# -> State# RealWorld -> (# State# RealWorld, r #) )
                  -> State# RealWorld -> (# State# RealWorld, r #)
withDataFramePtr# (MDataFrame# off _ mba) k s0
  | (# s1, a #) <- unsafeFreezeByteArray# mba s0
  , (# s2, r #) <- k ( byteArrayContents# a
                       `plusAddr#` (off *# byteSize @t undefined)
                     ) s1
  = (# touch# mba s2, r #)

-- | Check if the byte array wrapped by this DataFrame is pinned,
--   which means cannot be relocated by GC.
isDataFramePinned# :: forall (t :: Type) (ns :: [Nat]) s
                    . MDataFrame s t ns -> Bool
isDataFramePinned# (MDataFrame# _ _ mba)
  = isTrue# (isMutableByteArrayPinned# mba)
