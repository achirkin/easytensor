{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.NDArray.Class
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
module Numeric.NDArray.Class
  ( Dim (..), totalDim, order
  , Dim# (..), order#
  , Dimensions (..)
  ) where


import GHC.TypeLits
import GHC.Prim
import GHC.Types
import Data.Proxy


-- | Type-level checked dimensionality
data Dim (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Dim '[]
   -- | List-like concatenation of dimensionality
   (:-) :: !Int -> !(Dim ds) -> Dim (n':ds)

-- | Type-level checked dimensionality (unboxed)
data Dim# (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z# :: Dim# '[]
   -- | List-like concatenation of dimensionality
   (:#) :: Int# -> Dim# ds -> Dim# (n':ds)

-- | Support for Dim GADT
class Dimensions (ds :: [Nat]) where
  -- | Dimensionality of a second rank type
  dim :: t ds -> Dim ds
  -- | Dimensionality of a second rank type (unboxed)
  dim# :: t ds -> Dim# ds
  -- | Total number of elements - product of all dimension sizes (unboxed)
  totalDim# :: t ds -> Int#
  -- | Run a primitive loop over all dimensions
  loop#      :: Dim# ds -> (Dim# ds -> State# s -> State# s) -> State# s -> State# s
  -- | Run a primitive loop over all dimensions
  loop       :: Dim  ds -> (Dim  ds -> State# s -> State# s) -> State# s -> State# s
  -- | Get index offset: i1 + i2*n1 + i3*n1*n2 + ...
  ioffset   :: Dim ds -> Int
  -- | Get index offset: i1 + i2*n1 + i3*n1*n2 + ... (unboxed)
  ioffset#  :: Dim# ds -> Int#

-- | Total number of elements - product of all dimension sizes
totalDim :: Dimensions ds => t ds -> Int
totalDim x = I# (totalDim# x)
{-# INLINE totalDim #-}

-- | Number of dimensions
order :: Dim ds -> Int
order Z = 0
order (_:-xs) = 1 + order xs
{-# INLINE order #-}

-- | Number of dimensions (unboxed)
order# :: Dim# ds -> Int#
order# Z# = 0#
order# (_:#xs) = 1# +# order# xs
{-# INLINE order# #-}


instance Dimensions '[] where
  dim _ = Z
  {-# INLINE dim #-}
  dim# _ = Z#
  {-# INLINE dim# #-}
  totalDim# _ = 1#
  {-# INLINE totalDim# #-}
  loop# _ _ s = s
  {-# INLINE loop# #-}
  loop _ _ s = s
  {-# INLINE loop #-}
  ioffset# _ = 0#
  {-# INLINE ioffset# #-}
  ioffset _ = 0
  {-# INLINE ioffset #-}

instance (KnownNat d, Dimensions ds) => Dimensions (d ': ds) where
  dim x = fromIntegral (natVal' (headDim# x)) :- dim (tailDim# x)
  {-# INLINE dim #-}
  dim# x = case fromIntegral (natVal' (headDim# x)) of
             I# n -> n :# dim# (tailDim# x)
  {-# INLINE dim# #-}
  totalDim# x = case fromIntegral (natVal' (headDim# x)) of
             I# n -> n *# totalDim# (tailDim# x)
  {-# INLINE totalDim# #-}
  loop# (n:#Z#) f = loop1# n (\i -> f (i:#Z#))
  loop# (n:#ns) f = loop# ns (\js -> loop1# n (\i -> f (i:#js)))
  {-# INLINE loop# #-}
  loop (n:-Z) f = loop1 n (\i -> f (i:-Z))
  loop (n:-ns) f = loop ns (\js -> loop1 n (\i -> f (i:-js)))
  {-# INLINE loop #-}
  ioffset# (i:#Z#) = i
  ioffset# iis@(i:#is) = case fromIntegral (natVal' (headDim# iis)) of
             I# n -> i +# n *# ioffset# is
  {-# INLINE ioffset# #-}
  ioffset (i:-Z) = i
  ioffset iis@(i:-is) = i + fromIntegral (natVal' (headDim# iis)) * ioffset is
  {-# INLINE ioffset #-}


headDim# :: t (d ': ds :: [Nat]) -> Proxy# d
headDim# _ = proxy#
{-# INLINE headDim# #-}

tailDim# :: t (d ': ds :: [Nat]) -> Proxy ds
tailDim# _ = Proxy
{-# INLINE tailDim# #-}

-- | Do something in a loop for int i from 0 to n-1
loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop1# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1# #-}


-- | Do something in a loop for int i from 1 to n
loop1 :: Int -> (Int -> State# s -> State# s) -> State# s -> State# s
loop1 n f = loop' 0
  where
    loop' i s | i > n = s
              | otherwise = case f i s of s1 -> loop' (i + 1) s1
{-# INLINE loop1 #-}

