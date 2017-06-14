{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedTuples             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Mutable
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Interfrace to perform primitive stateful operations on mutable frames.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Mutable
    ( MutableFrame (..), MDataFrame ()
    , newDataFrame#, copyDataFrame#, copyMDataFrame#, unsafeFreezeDataFrame#
    , freezeDataFrame#, thawDataFrame#
    , writeDataFrame#, readDataFrame#
    ) where


import           GHC.Int                (Int16 (..), Int32 (..), Int64 (..),
                                         Int8 (..))
import           GHC.Prim
import           GHC.Types              (Double (..), Float (..), Int (..),
                                         Word (..))
import           GHC.Word               (Word16 (..), Word32 (..), Word64 (..),
                                         Word8 (..))

import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions


-- | Mutable DataFrame type
data MDataFrame s t (ns :: [Nat]) = MDataFrame# Int# Int# (MutableByteArray# s)

-- | Create a new mutable DataFrame.
newDataFrame# :: forall t (ns :: [Nat]) s
               . ( PrimBytes t, Dimensions ns)
              => State# s -> (# State# s, MDataFrame s t ns #)
newDataFrame# s0
    | elS  <- elementByteSize (undefined :: t)
    , I# n <- totalDim (Proxy @ns)
    , (# s1, mba #) <- newByteArray# (n *# elS) s0
    = (# s1,  MDataFrame# 0# n mba #)
{-# INLINE newDataFrame# #-}

-- | Copy one DataFrame into another mutable DataFrame at specified position.
copyDataFrame# :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( PrimBytes (DataFrame t as)
                  , ConcatList as bs asbs
                  , Dimensions bs
                  )
               => DataFrame t as -> Idx bs -> MDataFrame s t asbs -> State# s -> (# State# s, () #)
copyDataFrame# df ei (MDataFrame# offM _ arrM) s
    | (# offA, lenA, arrA #) <- toBytes df
    , elS <- elementByteSize df
    , I# i <- fromEnum ei
    = (# copyByteArray# arrA (offA *# elS) arrM ((offM +# i) *# elS) (lenA *# elS) s, () #)
{-# INLINE copyDataFrame# #-}

-- | Copy one mutable DataFrame into another mutable DataFrame at specified position.
copyMDataFrame# :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) s
                . ( PrimBytes t
                  , ConcatList as bs asbs
                  , Dimensions bs
                  )
               => MDataFrame s t as -> Idx bs -> MDataFrame s t asbs -> State# s -> (# State# s, () #)
copyMDataFrame# (MDataFrame# offA lenA arrA) ei (MDataFrame# offM _ arrM) s
    | elS <- elementByteSize (undefined :: t)
    , I# i <- fromEnum ei
    = (# copyMutableByteArray# arrA (offA *# elS) arrM ((offM +# i) *# elS) (lenA *# elS) s, () #)
{-# INLINE copyMDataFrame# #-}

-- | Make a mutable DataFrame immutable, without copying.
unsafeFreezeDataFrame# :: forall t (ns :: [Nat]) s
                        . PrimBytes (DataFrame t ns)
                       => MDataFrame s t ns -> State# s -> (# State# s, DataFrame t ns #)
unsafeFreezeDataFrame# (MDataFrame# offM lenM arrM) s1
    | (# s2, arrA #) <- unsafeFreezeByteArray# arrM s1
    = (# s2, fromBytes (# offM, lenM, arrA #) #)
{-# INLINE unsafeFreezeDataFrame# #-}

-- | Copy content of a mutable DataFrame into a new immutable DataFrame.
freezeDataFrame# :: forall t (ns :: [Nat]) s
                  . PrimBytes (DataFrame t ns)
                 => MDataFrame s t ns -> State# s -> (# State# s, DataFrame t ns #)
freezeDataFrame# (MDataFrame# offM n arrM) s0
    | elS  <- elementByteSize (undefined :: DataFrame t ns)
    , (# s1, mba #) <- newByteArray# (n *# elS) s0
    , s2 <- copyMutableByteArray# arrM (offM *# elS) mba 0# (n *# elS) s1
    , (# s3, arrA #) <- unsafeFreezeByteArray# mba s2
    = (# s3, fromBytes (# 0#, n, arrA #) #)
{-# INLINE freezeDataFrame# #-}

-- | Create a new mutable DataFrame and copy content of immutable one in there.
thawDataFrame# :: forall t (ns :: [Nat]) s
                . PrimBytes (DataFrame t ns)
               => DataFrame t ns -> State# s -> (# State# s, MDataFrame s t ns #)
thawDataFrame# df s0
    | elS  <- elementByteSize (undefined :: DataFrame t ns)
    , (# offA, n, arrA #) <- toBytes df
    , (# s1, arrM #) <- newByteArray# (n *# elS) s0
    , s2 <- copyByteArray# arrA (offA *# elS) arrM 0# (n *# elS) s1
    = (# s2, MDataFrame# 0# n arrM #)
{-# INLINE thawDataFrame# #-}

-- | Write a single element at the specified index
writeDataFrame# :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => MDataFrame s t ns -> Idx ns -> t -> State# s -> (# State# s, () #)
writeDataFrame# mdf ei x s | I# i <- fromEnum ei = (# writeDataFrameOff# mdf i x s, () #)
{-# INLINE writeDataFrame# #-}

-- | Read a single element at the specified index
readDataFrame# :: forall t (ns :: [Nat]) s
                . ( MutableFrame t ns, Dimensions ns )
               => MDataFrame s t ns -> Idx ns -> State# s -> (# State# s, t #)
readDataFrame# mdf ei | I# i <- fromEnum ei = readDataFrameOff# mdf i
{-# INLINE readDataFrame# #-}

class MutableFrame t (ns :: [Nat]) where
    -- | Write a single element at the specified element offset
    writeDataFrameOff# :: MDataFrame s t ns -> Int# -> t -> State# s -> State# s
    -- | Read a single element at the specified element offset
    readDataFrameOff# :: MDataFrame s t ns -> Int# -> State# s -> (# State# s, t #)

instance MutableFrame Float (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (F# x) = writeFloatArray# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readFloatArray# arr (off +# i) s0 = (# s1, F# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Double (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (D# x) = writeDoubleArray# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readDoubleArray# arr (off +# i) s0 = (# s1, D# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (I# x) = writeIntArray# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readIntArray# arr (off +# i) s0 = (# s1, I# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int8 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (I8# x) = writeInt8Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readInt8Array# arr (off +# i) s0 = (# s1, I8# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int16 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (I16# x) = writeInt16Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readInt16Array# arr (off +# i) s0 = (# s1, I16# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int32 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (I32# x) = writeInt32Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readInt32Array# arr (off +# i) s0 = (# s1, I32# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Int64 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (I64# x) = writeInt64Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readInt64Array# arr (off +# i) s0 = (# s1, I64# r #)
    {-# INLINE readDataFrameOff# #-}


instance MutableFrame Word (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (W# x) = writeWordArray# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readWordArray# arr (off +# i) s0 = (# s1, W# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word8 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (W8# x) = writeWord8Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readWord8Array# arr (off +# i) s0 = (# s1, W8# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word16 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (W16# x) = writeWord16Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readWord16Array# arr (off +# i) s0 = (# s1, W16# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word32 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (W32# x) = writeWord32Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readWord32Array# arr (off +# i) s0 = (# s1, W32# r #)
    {-# INLINE readDataFrameOff# #-}

instance MutableFrame Word64 (ns :: [Nat]) where
    writeDataFrameOff# (MDataFrame# off _ arr) i (W64# x) = writeWord64Array# arr (off +# i) x
    {-# INLINE writeDataFrameOff# #-}
    readDataFrameOff#  (MDataFrame# off _ arr) i s0
      | (# s1, r #) <- readWord64Array# arr (off +# i) s0 = (# s1, W64# r #)
    {-# INLINE readDataFrameOff# #-}
