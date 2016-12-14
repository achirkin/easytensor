{-# LANGUAGE UndecidableInstances #-}
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

#include "MachDeps.h"
#include "HsBaseConfig.h"

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types
import GHC.TypeLits


import Numeric.Commons
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
  -- | Element-wise minimum
  min = zipV  (\x y -> if isTrue# (x `gtFloat#` y) then y else x)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max = zipV  (\x y -> if isTrue# (x `gtFloat#` y) then x else y)
  {-# INLINE max #-}





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








instance (KnownNat n, KnownNat m) => MatrixCalculus Float n m (MFloatXNM n m) where
  broadcastMat (F# x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> writeFloatArray# marr i x s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> MFloatXNM r
    where
      n = dimN# (undefined :: MFloatXNM n m) *# dimM# (undefined :: MFloatXNM n m)
      bs = n *# SIZEOF_HSFLOAT#
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
      bs = n *# m *# SIZEOF_HSFLOAT#
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
      bs = n *# SIZEOF_HSFLOAT#
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
      bs = m *# SIZEOF_HSFLOAT#


instance (KnownNat n, KnownNat m) => PrimBytes (MFloatXNM n m) where
  toBytes (MFloatXNM a) = a
  {-# INLINE toBytes #-}
  fromBytes = MFloatXNM
  {-# INLINE fromBytes #-}
  byteSize x = SIZEOF_HSFLOAT# *# dimN# x *# dimM# x
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}

instance FloatBytes (MFloatXNM n m) where
  ixF i (MFloatXNM a) = indexFloatArray# a i
  {-# INLINE ixF #-}


instance KnownNat n => SquareMatrixCalculus Float n (MFloatXNM n n) where
  eye = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\j s' -> writeFloatArray# marr (j *# n1) 1.0# s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n1 = n +# 1#
      n = dimN# (undefined :: MFloatXNM n n)
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE eye #-}
  diag (F# v) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\j s' -> writeFloatArray# marr (j *# n1) v s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n1 = n +# 1#
      n = dimN# (undefined :: MFloatXNM n n)
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE diag #-}


  det v@(MFloatXNM arr) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, mat #) -> case newByteArray# (n *# SIZEOF_HSFLOAT#) (copyByteArray# arr 0# mat 0# bs s1) of
           (# s2, vec #) ->
              let f i x s | isTrue# (i >=# n) = (# s, x #)
                          | otherwise =
                              let (# s' , j  #) = maxInRowRem# n n i mat s
                                  (# s'', x' #) = if isTrue# (i /=# j) then (# swapCols# n i j vec mat s', negateFloat# x #)
                                                                       else (# s', x #)
                                  (# s''', y #) = clearRowEnd# n n i mat s''
                              in if isTrue# (eqFloat# 0.0# y) then (# s''', 0.0# #)
                                                              else f (i +# 1#) (timesFloat# x' y) s'''
              in f 0# 1.0# s2
     ) of (# _, r #) -> F# r
    where
      n = dimN# v
      bs = n *# n *# SIZEOF_HSFLOAT#

  {-# INLINE det #-}



  trace x@(MFloatXNM a) = F# (loop' 0# 0.0#)
    where
      n1 = n +# 1#
      n = dimN# x
      nn = n *# n
      loop' i acc | isTrue# (i ># nn) = acc
                  | otherwise = loop' (i +# n1) (indexFloatArray# a i `plusFloat#` acc)
  {-# INLINE trace #-}
  fromDiag x@(MFloatXNM a) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\j s' -> writeFloatArray# marr j (indexFloatArray# a (j *# n1)) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n1 = n +# 1#
      n = dimN# x
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE fromDiag #-}
  toDiag x = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\j s' -> writeFloatArray# marr (j *# n1) (indexFloatArray# a j) s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      a = toBytes x
      n1 = n +# 1#
      n = dimN# (undefined :: MFloatXNM n n)
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE toDiag #-}


instance KnownNat n => MatrixInverse (MFloatXNM n n) where
  inverse v@(MFloatXNM arr) = case runRW#
     ( \s0 -> case newByteArray# (bs *# 2#) s0 of
         (# s1, mat #) -> case newByteArray# vs
                -- copy original matrix to the top of an augmented matrix
                (loop# n (\i s -> writeFloatArray# mat (i *# nn +# i +# n) 1.0# (copyByteArray# arr (i *# vs) mat (2# *# i *# vs) vs s))
                         (setByteArray# mat 0# (bs *# 2#) 0# s1)
                ) of
           (# s2, vec #) ->
              let f i s | isTrue# (i >=# n) = s
                        | otherwise =
                            let (# s' , j  #) = maxInRowRem# nn n i mat s
                                s''           = if isTrue# (i /=# j) then swapCols# nn i j vec mat s'
                                                                     else s'
                                (# s''', _ #) = clearRowAll# nn n i mat s''
                            in f (i +# 1#) s'''
              in unsafeFreezeByteArray# mat
                  ( shrinkMutableByteArray# mat bs
                   (-- copy inverse matrix from the augmented part
                    loop# n (\i s -> copyMutableByteArray# mat (2# *# i *# vs +# vs) mat (i *# vs) vs s)
                   (f 0# s2)
                   )
                  )
     ) of (# _, r #) -> MFloatXNM r
    where
      nn = 2# *# n
      n = dimN# v
      vs = n *# SIZEOF_HSFLOAT#
      bs = n *# n *# SIZEOF_HSFLOAT#



instance (KnownNat n, KnownNat m) => ElementWise (Int,Int) Float (MFloatXNM n m) where
  ewmap f x@(MFloatXNM arr) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop2# n m
               (\i j s' -> case f (I# (i +# 1#), I# (j +# 1#)) (F# (indexFloatArray# arr (i *# m +# j))) of
                            F# r -> writeFloatArray# marr (i +# n *# j) r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n = dimN# x
      m = dimM# x
      bs = n *# m *# SIZEOF_HSFLOAT#
  {-# INLINE ewmap #-}
  ewgen f = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop2# n m
               (\i j s' -> case f (I# (i +# 1#), I# (j +# 1#)) of
                            F# r -> writeFloatArray# marr (i +# n *# j) r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes r
    where
      n = dimN# (undefined :: MFloatXNM n m)
      m = dimM# (undefined :: MFloatXNM n m)
      bs = n *# m *# SIZEOF_HSFLOAT#
  {-# INLINE ewgen #-}


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
    bs = n *# SIZEOF_HSFLOAT#

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
    bs = n *# SIZEOF_HSFLOAT#


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



-- | Swap columns i and j. Does not check if i or j is larger than matrix width m
swapCols# :: Int# -- n
          -> Int# -- ith column to swap
          -> Int# -- jth column to swap
          -> MutableByteArray# s -- buffer byte array of length of n elems
          -> MutableByteArray# s -- byte array of matrix
          -> State# s -- previous state
          -> State# s -- next state
swapCols# n i j vec mat s0 =
  -- copy ith column to bugger vec
  case copyMutableByteArray# mat (i *# bs) vec 0# bs s0 of
    s1 -> case copyMutableByteArray# mat (j *# bs) mat (i *# bs) bs s1 of
      s2 -> copyMutableByteArray# vec 0# mat (j *# bs) bs s2
 where
  bs = n *# SIZEOF_HSFLOAT#

-- | Starting from i-th row and i+1-th column, substract a multiple of i-th column from i+1 .. m columns,
--   such that there are only zeroes in i-th row and i+1..m columns elements.
clearRowEnd# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to remove from all others
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Float# #) -- next state and a diagonal element
clearRowEnd# n m i mat s0 = (# loop' (i +# 1#) s1, y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    (# s1, y' #) = readFloatArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0# `divideFloat#` y'
    n' = n -# i -# 1#
    loop' k s | isTrue# (k >=# m) = s
              | otherwise = loop' (k +# 1#)
       ( let x0 = k *# n +# i
             (# s', a' #) = readFloatArray# mat x0 s
             s'' = writeFloatArray# mat x0 0.0# s'
             a  = a' `timesFloat#` yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )

-- | Substract a multiple of i-th column from 0 .. i-1 and i+1 .. m columns,
--   such that there are only zeroes in i-th row and i+1..m columns elements.
--   Assuming that elements 0..i-1 in i-th row are zeroes, so they do not affect other columns.
--   After all columns updated, divide i-th row by its diagonal element
clearRowAll# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to remove from all others
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Float# #) -- next state and a diagonal element
clearRowAll# n m i mat s0 = (# divLoop (i +# 1#) (writeFloatArray# mat ((n +# 1#) *# i) 1.0# (loop' 0# i (loop' (i +# 1#) m s1))), y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    (# s1, y' #) = readFloatArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0# `divideFloat#` y'
    n' = n -# i -# 1#
    loop' k km s | isTrue# (k >=# km) = s
                 | otherwise = loop' (k +# 1#) km
       ( let x0 = k *# n +# i
             (# s', a' #) = readFloatArray# mat x0 s
             s'' = writeFloatArray# mat x0 0.0# s'
             a  = a' `timesFloat#` yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )
    divLoop k s | isTrue# (k >=# n) = s
                | otherwise = divLoop (k +# 1#)
       ( let x0 = n *# i +# k
             (# s', x #) = readFloatArray# mat x0 s
         in writeFloatArray# mat x0 (timesFloat# x yrc) s'
       )


-- | Remove a multiple of one row from another one.
--   do: xi = xi - yi*a
multNRem# :: Int# -- n - nr of elements to go through
          -> Int# -- start idx of y
          -> Int# -- start idx of x
          -> Float# -- multiplier a
          -> MutableByteArray# s -- byte array of matrix
          -> State# s -- previous state
          -> State# s -- next state
multNRem# 0# _ _  _ _ s = s
multNRem# n x0 y0 a mat s = multNRem# (n -# 1#) (x0 +# 1#) (y0 +# 1#) a mat
  ( case readFloatArray# mat y0 s of
     (# s1, y #) -> case readFloatArray# mat x0 s1 of
       (# s2, x #) -> writeFloatArray# mat x0 (x `minusFloat#` timesFloat# y a) s2
  )



-- | Gives index of maximum (absolute) element in i-th row, starting from i-th element only.
--   If i >= m then returns i.
maxInRowRem# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to start to search for and a row to look in
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Int# #) -- next state
maxInRowRem# n m i mat s0 = loop' i (abs# v) i s1
  where
    (# s1, v #) = readFloatArray# mat ((n +# 1#) *# i) s0
    abs# x = if isTrue# (x `geFloat#` 0.0#) then x else negateFloat# x
    loop' ok ov k s | isTrue# (k >=# m) = (# s, ok #)
                    | otherwise = case readFloatArray# mat (n *# k +# i) s of
                        (# s', v' #) -> if isTrue# (abs# v' `gtFloat#` ov)
                                        then loop' k (abs# v') (k +# 1#) s'
                                        else loop' ok ov (k +# 1#) s'




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
