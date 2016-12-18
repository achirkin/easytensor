{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vector.Base.FloatXN
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Vector.Base.FloatXN () where

#include "MachDeps.h"
#include "HsBaseConfig.h"

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types
import GHC.TypeLits

import Numeric.Commons
import Numeric.Vector.Class
import Numeric.Vector.Family


instance KnownNat n => Show (VFloatXN n) where
  show x = "{" ++ drop 1
              ( accumVReverse (\a s -> ", " ++ show (F# a) ++ s) x " }"
              )

instance KnownNat n => Eq (VFloatXN n) where
  a == b = accumV2 (\x y r -> r && isTrue# (x `eqFloat#` y)) a b True
  {-# INLINE (==) #-}
  a /= b = accumV2 (\x y r -> r || isTrue# (x `neFloat#` y)) a b False
  {-# INLINE (/=) #-}

-- | Implement partial ordering for `>`, `<`, `>=`, `<=` and lexicographical ordering for `compare`
instance KnownNat n => Ord (VFloatXN n) where
  a > b = accumV2 (\x y r -> r && isTrue# (x `gtFloat#` y)) a b True
  {-# INLINE (>) #-}
  a < b = accumV2 (\x y r -> r && isTrue# (x `ltFloat#` y)) a b True
  {-# INLINE (<) #-}
  a >= b = accumV2 (\x y r -> r && isTrue# (x `geFloat#` y)) a b True
  {-# INLINE (>=) #-}
  a <= b = accumV2 (\x y r -> r && isTrue# (x `leFloat#` y)) a b True
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare a b = accumV2 (\x y r -> r `mappend`
                          if isTrue# (x `gtFloat#` y)
                          then GT
                          else if isTrue# (x `ltFloat#` y)
                               then LT
                               else EQ
                        ) a b EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min = zipV  (\x y -> if isTrue# (x `gtFloat#` y) then y else x)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max = zipV  (\x y -> if isTrue# (x `gtFloat#` y) then x else y)
  {-# INLINE max #-}



instance (KnownNat n, 3 <= n) => Num (VFloatXN n) where
  (+) = zipV plusFloat#
  {-# INLINE (+) #-}
  (-) = zipV minusFloat#
  {-# INLINE (-) #-}
  (*) = zipV timesFloat#
  {-# INLINE (*) #-}
  negate = mapV negateFloat#
  {-# INLINE negate #-}
  abs = mapV (\x -> if isTrue# (x `geFloat#` 0.0#) then x else negateFloat# x)
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (x `gtFloat#` 0.0#) then 1.0# else if isTrue# (x `ltFloat#` 0.0#) then -1.0# else 0.0#)
  {-# INLINE signum #-}
  fromInteger = broadcastVec . fromInteger
  {-# INLINE fromInteger #-}



instance (KnownNat n, 3 <= n) => Fractional (VFloatXN n) where
  (/) = zipV divideFloat#
  {-# INLINE (/) #-}
  recip = mapV (divideFloat# 1.0#)
  {-# INLINE recip #-}
  fromRational = broadcastVec . fromRational
  {-# INLINE fromRational #-}



instance (KnownNat n, 3 <= n) => Floating (VFloatXN n) where
  pi = broadcastVec pi
  {-# INLINE pi #-}
  exp = mapV expFloat#
  {-# INLINE exp #-}
  log = mapV logFloat#
  {-# INLINE log #-}
  sqrt = mapV sqrtFloat#
  {-# INLINE sqrt #-}
  sin = mapV sinFloat#
  {-# INLINE sin #-}
  cos = mapV cosFloat#
  {-# INLINE cos #-}
  tan = mapV tanFloat#
  {-# INLINE tan #-}
  asin = mapV asinFloat#
  {-# INLINE asin #-}
  acos = mapV acosFloat#
  {-# INLINE acos #-}
  atan = mapV atanFloat#
  {-# INLINE atan #-}
  sinh = mapV sinFloat#
  {-# INLINE sinh #-}
  cosh = mapV coshFloat#
  {-# INLINE cosh #-}
  tanh = mapV tanhFloat#
  {-# INLINE tanh #-}
  (**) = zipV powerFloat#
  {-# INLINE (**) #-}

  logBase = zipV (\x y -> logFloat# y `divideFloat#` logFloat# x)
  {-# INLINE logBase #-}
  asinh = mapV (\x -> logFloat# (x `plusFloat#` sqrtFloat# (1.0# `plusFloat#` timesFloat# x x)))
  {-# INLINE asinh #-}
  acosh = mapV (\x ->  case plusFloat# x 1.0# of
                 y -> logFloat# ( x `plusFloat#` timesFloat# y (sqrtFloat# (minusFloat# x 1.0# `divideFloat#` y)))
               )
  {-# INLINE acosh #-}
  atanh = mapV (\x -> 0.5# `timesFloat#` logFloat# (plusFloat# 1.0# x `divideFloat#` minusFloat# 1.0# x))
  {-# INLINE atanh #-}



instance (KnownNat n, 3 <= n) => VectorCalculus Float n (VFloatXN n) where
  broadcastVec (F# x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> writeFloatArray# marr i x s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
    where
      n = dim# (undefined :: VFloatXN n)
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE broadcastVec #-}
  a .*. b = broadcastVec $ dot a b
  {-# INLINE (.*.) #-}
  dot a b = F# (accumV2Float (\x y r -> r `plusFloat#` timesFloat# x y) a b 0.0#)
  {-# INLINE dot #-}
  indexVec (I# i) _x@(VFloatXN arr)
#ifndef UNSAFE_INDICES
      | isTrue# ( (i ># dim# _x)
           `orI#` (i <=# 0#)
          )       = error $ "Bad index " ++ show (I# i) ++ " for " ++ show (dim _x)  ++ "D vector"
      | otherwise
#endif
                  = F# (indexFloatArray# arr (i -# 1#))
  {-# INLINE indexVec #-}
  normL1 v = F# (accumVFloat (\x a -> a `plusFloat#` (if isTrue# (x `geFloat#` 0.0#) then x else negateFloat# x)) v 0.0#)
  {-# INLINE normL1 #-}
  normL2 v = sqrt $ F# (accumVFloat (\x a -> a `plusFloat#` timesFloat# x x) v 0.0#)
  {-# INLINE normL2 #-}
  normLPInf v@(VFloatXN arr) = F# (accumVFloat (\x a -> if isTrue# (x `geFloat#` a) then x else a) v (indexFloatArray# arr 0#))
  {-# INLINE normLPInf #-}
  normLNInf v@(VFloatXN arr) = F# (accumVFloat (\x a -> if isTrue# (x `leFloat#` a) then x else a) v (indexFloatArray# arr 0#))
  {-# INLINE normLNInf #-}
  normLP n v = case realToFrac n of
    F# p -> F# (powerFloat# (divideFloat# 1.0# p) (accumVFloat (\x a -> a `plusFloat#` powerFloat# x p) v 0.0#))
  {-# INLINE normLP #-}
  dim x = I# (dim# x)
  {-# INLINE dim #-}




instance KnownNat n =>  PrimBytes (VFloatXN n) where
  toBytes (VFloatXN a) = a
  {-# INLINE toBytes #-}
  fromBytes = VFloatXN
  {-# INLINE fromBytes #-}
  byteSize x = SIZEOF_HSFLOAT# *# dim# x
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}


instance FloatBytes (VFloatXN n) where
  ixF i (VFloatXN a) = indexFloatArray# a i
  {-# INLINE ixF #-}


instance KnownNat n => ElementWise Int Float (VFloatXN n) where
  ewmap f x@(VFloatXN a) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (I# (i +# 1#)) (F# (indexFloatArray# a i)) of
                 F# r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
    where
      n = dim# x
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE ewmap #-}
  ewgen f = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (I# (i +# 1#)) of
                 F# r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
    where
      n = dim# (undefined :: VFloatXN n)
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE ewgen #-}
  ewfold f v0 x@(VFloatXN a) = fo 0# v0
    where
      n = dim# x
      fo i v | isTrue# (i ==# n) = case f (I# (i +# 1#)) (F# (indexFloatArray# a i)) v of v1 -> fo (i +# 1#) v1
             | otherwise = v
  {-# INLINE ewfold #-}

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


dim# :: KnownNat n => VFloatXN n -> Int#
dim# x = case fromInteger (natVal x) of I# n -> n
{-# INLINE dim# #-}

-- | Do something in a loop for int i from 0 to n-1
loop# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop# #-}


zipV :: KnownNat n => (Float# -> Float# -> Float#) -> VFloatXN n -> VFloatXN n -> VFloatXN n
zipV f x@(VFloatXN a) (VFloatXN b) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (indexFloatArray# a i) (indexFloatArray# b i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
  where
    n = dim# x
    bs = n *# SIZEOF_HSFLOAT#

mapV :: KnownNat n => (Float# -> Float#) -> VFloatXN n -> VFloatXN n
mapV f x@(VFloatXN a) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (indexFloatArray# a i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
  where
    n = dim# x
    bs = n *# SIZEOF_HSFLOAT#


accumVFloat :: KnownNat n => (Float# -> Float# -> Float#) -> VFloatXN n -> Float# -> Float#
accumVFloat f x@(VFloatXN a) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) acc)
    n = dim# x

accumV2 :: KnownNat n => (Float# -> Float# -> a -> a) -> VFloatXN n -> VFloatXN n -> a -> a
accumV2 f x@(VFloatXN a) (VFloatXN b) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) (indexFloatArray# b i) acc)
    n = dim# x


accumV2Float :: KnownNat n => (Float# -> Float# -> Float# -> Float#) -> VFloatXN n -> VFloatXN n -> Float# -> Float#
accumV2Float f x@(VFloatXN a) (VFloatXN b) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) (indexFloatArray# b i) acc)
    n = dim# x


accumVReverse :: KnownNat n => (Float# -> a -> a) -> VFloatXN n -> a -> a
accumVReverse f x@(VFloatXN a) = loop' (n -# 1#)
  where
    loop' i acc | isTrue# (i ==# -1#) = acc
                | otherwise = loop' (i -# 1#) (f (indexFloatArray# a i) acc)
    n = dim# x


