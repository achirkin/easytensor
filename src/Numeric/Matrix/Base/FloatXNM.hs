{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix.Base.FloatXNM
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix.Base.FloatXNM () where

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types
import GHC.TypeLits


import Numeric.Commons
import Numeric.Vector.Class
import Numeric.Vector.Family
import Numeric.Matrix.Class
import Numeric.Matrix.Family




instance (KnownNat n, KnownNat m) => Show (MFloatXNM n m) where
  show x@(MFloatXNM arr) = "{" ++ drop 2 (loop' (n -# 1#) (m -# 1#) " }")
    where
      loop' i j acc | isTrue# (i ==# -1#) = acc
                    | isTrue# (j ==# -1#) = loop' (i -# 1#) (m -# 1#) ('\n':acc)
                    | otherwise           = loop' i (j -# 1#) (", " ++ show (F# (indexFloatArray# arr (i +# n *# j))) ++ acc)
      n = dimN# x
      m = dimM# x



instance (KnownNat n, KnownNat m) => Eq (MFloatXNM n m) where
  a == b = accumV2 (\x y r -> r && isTrue# (x `eqFloat#` y)) a b True
  {-# INLINE (==) #-}
  a /= b = accumV2 (\x y r -> r || isTrue# (x `neFloat#` y)) a b False
  {-# INLINE (/=) #-}




-- | Implement partial ordering for `>`, `<`, `>=`, `<=` and lexicographical ordering for `compare`
instance (KnownNat n, KnownNat m) => Ord (MFloatXNM n m) where
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




instance (KnownNat n, KnownNat m) => Num (MFloatXNM n m) where
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
  fromInteger = broadcastMat . fromInteger
  {-# INLINE fromInteger #-}



instance (KnownNat n, KnownNat m) => Fractional (MFloatXNM n m) where
  (/) = zipV divideFloat#
  {-# INLINE (/) #-}
  recip = mapV (divideFloat# 1.0#)
  {-# INLINE recip #-}
  fromRational = broadcastMat . fromRational
  {-# INLINE fromRational #-}



instance (KnownNat n, KnownNat m) => Floating (MFloatXNM n m) where
  pi = broadcastMat pi
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








instance (KnownNat n, KnownNat m) => MatrixCalculus (MFloatXNM n m) Float n m where
  broadcastMat (F# x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> writeFloatArray# marr i x s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> MFloatXNM r
    where
      n = dimN# (undefined :: MFloatXNM n m) *# dimM# (undefined :: MFloatXNM n m)
      bs = n *# 4#
  {-# INLINE broadcastMat #-}
  indexMat (I# i) (I# j) x@(MFloatXNM arr)
#ifndef UNSAFE_INDICES
      | isTrue# ( (i ># n)
           `orI#` (i <=# 0#)
           `orI#` (j ># _m)
           `orI#` (j <=# 0#)
          )       = error $ "Bad index (" ++ show (I# i) ++ ", " ++ show (I# j) ++ ") for "
                          ++ show (I# n) ++ "x" ++ show (I# _m) ++ "D matrix"
      | otherwise
#endif
                  = F# (indexFloatArray# arr (i -# 1# +# n *# (j -# 1#)))
    where
      n = dimN# x
      _m = dimM# x
  {-# INLINE indexMat #-}
  transpose x@(MFloatXNM arr) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop2# n m
               (\i j s' -> writeFloatArray# marr (i +# n *# j) (indexFloatArray# arr (i *# m +# j)) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n = dimN# x
      m = dimM# x
      bs = n *# m *# 4#
  dimN x = I# (dimN# x)
  {-# INLINE dimN #-}
  dimM x = I# (dimM# x)
  {-# INLINE dimM #-}
  indexCol (I# j) x@(MFloatXNM arr)
#ifndef UNSAFE_INDICES
      | isTrue# ( (j ># dimM# x)
           `orI#` (j <=# 0#)
          )       = error $ "Bad column index " ++ show (I# j) ++ " for "
                          ++ show (I# n) ++ "x" ++ show (dimM x) ++ "D matrix"
      | otherwise
#endif
                  = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case copyByteArray# arr (bs *# (j -# 1#)) marr 0# bs s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n = dimN# x
      bs = n *# 4#
  indexRow (I# i) x@(MFloatXNM arr)
#ifndef UNSAFE_INDICES
      | isTrue# ( (i ># n)
           `orI#` (i <=# 0#)
          )       = error $ "Bad row index " ++ show (I# i) ++ " for "
                          ++ show (I# n) ++ "x" ++ show (I# m) ++ "D matrix"
      | otherwise
#endif
                  = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# m
               (\j s' -> writeFloatArray# marr j (indexFloatArray# arr (i -# 1# +# n *# j)) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n = dimN# x
      m = dimM# x
      bs = m *# 4#


instance (KnownNat n, KnownNat m) => PrimBytes (MFloatXNM n m) where
  toBytes (MFloatXNM a) = a
  fromBytes = MFloatXNM
  byteSize x = 4# *# dimN# x *# dimM# x
  {-# INLINE byteSize #-}
  byteAlign _ = 8#
  {-# INLINE byteAlign #-}


instance (KnownNat n, KnownNat m, KnownNat k)
      => MatrixProduct (MFloatXNM n m) (MFloatXNM m k) (MFloatXNM n k) where
  prod x@(MFloatXNM arrx) y@(MFloatXNM arry) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusFloat#` timesFloat# (indexFloatArray# arrx (i +# n *# l))
                                                                                           (indexFloatArray# arry (l +# m *# j)))
           in case loop2# n k
               (\i j s' -> writeFloatArray# marr (i +# n *# j) (loop' i j 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> MFloatXNM r
    where
      n = dimN# x
      m = dimM# x
      k = dimM# y
      bs = n *# k *# 4#


instance MatrixProduct VFloatX2 Float VFloatX2 where
  prod (VFloatX2 a b) (F# x) = VFloatX2 (x `timesFloat#` a) (x `timesFloat#` b)

instance (VectorCalculus (VFloatXN n) Float n, Num (VFloatXN n))
      => MatrixProduct (VFloatXN n)  Float (VFloatXN n) where
  prod v x = broadcastVec x * v

instance (KnownNat n, KnownNat m)
      => MatrixProduct (MFloatXNM n m) (VFloatXN m) (VFloatXN n) where
  prod x@(MFloatXNM arrx) (VFloatXN arry) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i l r | isTrue# (l ==# m) = r
                           | otherwise = loop' i (l +# 1#) (r `plusFloat#` timesFloat# (indexFloatArray# arrx (i +# n *# l))
                                                                                       (indexFloatArray# arry l))
           in case loop# n
               (\i s' -> writeFloatArray# marr i (loop' i 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
    where
      n = dimN# x
      m = dimM# x
      bs = n *# 4#


instance (KnownNat m, PrimBytes VFloatX2)
      => MatrixProduct (MFloatXNM 2 m) (VFloatXN m) VFloatX2 where
  prod x@(MFloatXNM arrx) (VFloatXN arry) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i l r | isTrue# (l ==# m) = r
                           | otherwise = loop' i (l +# 1#) (r `plusFloat#` timesFloat# (indexFloatArray# arrx (i +# 2# *# l))
                                                                                       (indexFloatArray# arry l))
           in case loop# 2#
               (\i s' -> writeFloatArray# marr i (loop' i 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      m = dimM# x
      bs = 2# *# 4#

instance (KnownNat n, PrimBytes VFloatX2)
      => MatrixProduct (MFloatXNM n 2) VFloatX2 (VFloatXN n) where
  prod x@(MFloatXNM arrx) y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i l r | isTrue# (l ==# 2#) = r
                           | otherwise = loop' i (l +# 1#) (r `plusFloat#` timesFloat# (indexFloatArray# arrx (i +# n *# l))
                                                                                       (indexFloatArray# arry l))
           in case loop# n
               (\i s' -> writeFloatArray# marr i (loop' i 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
    where
      arry = toBytes y
      n = dimN# x
      bs = n *# 4#



instance (PrimBytes VFloatX2)
      => MatrixProduct (MFloatXNM 2 2) VFloatX2 VFloatX2 where
  prod (MFloatXNM arrx) y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i l r | isTrue# (l ==# 2#) = r
                           | otherwise = loop' i (l +# 1#) (r `plusFloat#` timesFloat# (indexFloatArray# arrx (i +# 2# *# l))
                                                                                       (indexFloatArray# arry l))
           in case loop# 2#
               (\i s' -> writeFloatArray# marr i (loop' i 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      arry = toBytes y
      bs = 2# *# 4#


instance MatrixInverse (MFloatXNM n m) where
  inverse = undefined


-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


proxyN# :: MFloatXNM n m -> Proxy# n
proxyN# _ = proxy#

dimN# :: KnownNat n => MFloatXNM n m -> Int#
dimN# x = case fromInteger (natVal' (proxyN# x)) of I# n -> n
{-# INLINE dimN# #-}

dimM# :: KnownNat m => MFloatXNM n m -> Int#
dimM# x = case fromInteger (natVal x) of I# n -> n
{-# INLINE dimM# #-}

-- | Do something in a loop for int i from 0 to n
loop# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop# #-}

-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s) -> State# s -> State# s
loop2# n m f = loop' 0# 0#
  where
    loop' i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop' 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop' (i +# 1#) j s1
{-# INLINE loop2# #-}

zipV :: (KnownNat n, KnownNat m) => (Float# -> Float# -> Float#) -> MFloatXNM n m -> MFloatXNM n m -> MFloatXNM n m
zipV f x@(MFloatXNM a) (MFloatXNM b) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (indexFloatArray# a i) (indexFloatArray# b i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> MFloatXNM r
  where
    n = dimN# x *# dimM# x
    bs = n *# 4#

mapV :: (KnownNat n, KnownNat m) => (Float# -> Float#) -> MFloatXNM n m -> MFloatXNM n m
mapV f x@(MFloatXNM a) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (indexFloatArray# a i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> MFloatXNM r
  where
    n = dimN# x *# dimM# x
    bs = n *# 4#


--accumVFloat :: (KnownNat n, KnownNat m) => (Float# -> Float# -> Float#) -> MFloatXNM n m -> Float# -> Float#
--accumVFloat f x@(MFloatXNM a) = loop' 0#
--  where
--    loop' i acc | isTrue# (i ==# n) = acc
--                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) acc)
--    n = dimN# x *# dimM# x

accumV2 :: (KnownNat n, KnownNat m) => (Float# -> Float# -> a -> a) -> MFloatXNM n m -> MFloatXNM n m -> a -> a
accumV2 f x@(MFloatXNM a) (MFloatXNM b) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) (indexFloatArray# b i) acc)
    n = dimN# x *# dimM# x


--accumV2Float :: (KnownNat n, KnownNat m) => (Float# -> Float# -> Float# -> Float#) -> MFloatXNM n m -> MFloatXNM n m -> Float# -> Float#
--accumV2Float f x@(MFloatXNM a) (MFloatXNM b) = loop' 0#
--  where
--    loop' i acc | isTrue# (i ==# n) = acc
--                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) (indexFloatArray# b i) acc)
--    n = dimN# x *# dimM# x
--
--
--accumVReverse :: (KnownNat n, KnownNat m) => (Float# -> a -> a) -> MFloatXNM n m -> a -> a
--accumVReverse f x@(MFloatXNM a) = loop' (n -# 1#)
--  where
--    loop' i acc | isTrue# (i ==# -1#) = acc
--                | otherwise = loop' (i -# 1#) (f (indexFloatArray# a i) acc)
--    n = dimN# x *# dimM# x
