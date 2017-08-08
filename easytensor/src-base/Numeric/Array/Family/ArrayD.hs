{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE BangPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family.ArrayD
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayD () where


import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Double (..), Int (..),
                                            RuntimeRep (..), isTrue#)

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse
import           Numeric.TypeLits
import           Numeric.Matrix.Class


#include "MachDeps.h"
#define ARR_TYPE                 ArrayD
#define ARR_FROMSCALAR           FromScalarD#
#define ARR_CONSTR               ArrayD#
#define EL_TYPE_BOXED            Double
#define EL_TYPE_PRIM             Double#
#define EL_RUNTIME_REP           'DoubleRep
#define EL_CONSTR                D#
#define EL_SIZE                  SIZEOF_HSDOUBLE#
#define EL_ALIGNMENT             ALIGNMENT_HSDOUBLE#
#define EL_ZERO                  0.0##
#define EL_ONE                   1.0##
#define EL_MINUS_ONE             -1.0##
#define INDEX_ARRAY              indexDoubleArray#
#define WRITE_ARRAY              writeDoubleArray#
#define OP_EQ                    (==##)
#define OP_NE                    (/=##)
#define OP_GT                    (>##)
#define OP_GE                    (>=##)
#define OP_LT                    (<##)
#define OP_LE                    (<=##)
#define OP_PLUS                  (+##)
#define OP_MINUS                 (-##)
#define OP_TIMES                 (*##)
#define OP_NEGATE                negateDouble#
#include "Array.h"


instance Num (ArrayD ds) where
  (+) = zipV (+##)
  {-# INLINE (+) #-}
  (-) = zipV (-##)
  {-# INLINE (-) #-}
  (*) = zipV (*##)
  {-# INLINE (*) #-}
  negate = mapV negateDouble#
  {-# INLINE negate #-}
  abs = mapV (\x -> if isTrue# (x >=## 0.0##)
                    then x
                    else negateDouble# x
                )
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (x >## 0.0##)
                       then 1.0##
                       else if isTrue# (x <## 0.0##)
                            then -1.0##
                            else 0.0##
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional (ArrayD ds) where
  (/) = zipV (/##)
  {-# INLINE (/) #-}
  recip = mapV (1.0## /##)
  {-# INLINE recip #-}
  fromRational = broadcastArray . fromRational
  {-# INLINE fromRational #-}


instance Floating (ArrayD ds) where
  pi = broadcastArray pi
  {-# INLINE pi #-}
  exp = mapV expDouble#
  {-# INLINE exp #-}
  log = mapV logDouble#
  {-# INLINE log #-}
  sqrt = mapV sqrtDouble#
  {-# INLINE sqrt #-}
  sin = mapV sinDouble#
  {-# INLINE sin #-}
  cos = mapV cosDouble#
  {-# INLINE cos #-}
  tan = mapV tanDouble#
  {-# INLINE tan #-}
  asin = mapV asinDouble#
  {-# INLINE asin #-}
  acos = mapV acosDouble#
  {-# INLINE acos #-}
  atan = mapV atanDouble#
  {-# INLINE atan #-}
  sinh = mapV sinDouble#
  {-# INLINE sinh #-}
  cosh = mapV coshDouble#
  {-# INLINE cosh #-}
  tanh = mapV tanhDouble#
  {-# INLINE tanh #-}
  (**) = zipV (**##)
  {-# INLINE (**) #-}

  logBase = zipV (\x y -> logDouble# y /## logDouble# x)
  {-# INLINE logBase #-}
  asinh = mapV (\x -> logDouble# (x +##
                                sqrtDouble# (1.0## +## x *## x)))
  {-# INLINE asinh #-}
  acosh = mapV (\x ->  case x +## 1.0## of
                 y -> logDouble# ( x +## y *##
                           sqrtDouble# ((x -## 1.0##) /## y)
                        )
               )
  {-# INLINE acosh #-}
  atanh = mapV (\x -> 0.5## *##
                logDouble# ((1.0## +## x) /## (1.0## -## x)))
  {-# INLINE atanh #-}


instance (KnownNat n, KnownNat m, ArrayD '[n,m] ~ Array Double '[n,m], 2 <= n, 2 <= m)
      => MatrixCalculus Double n m where
  transpose (KnownDataFrame (ArrayD# offs nm arr)) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop2# n m
               (\i j s' -> writeDoubleArray# marr (j +# m *# i)
                              (indexDoubleArray# arr (offs +# j *# n +# i)) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, nm, r #)
    where
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      m = case fromInteger $ natVal (Proxy @m) of I# mp -> mp
      bs = n *# m *# EL_SIZE
  transpose (KnownDataFrame (FromScalarD# x)) = unsafeCoerce# $ FromScalarD# x

instance ( KnownDim n, ArrayD '[n,n] ~ Array Double '[n,n] )
      => SquareMatrixCalculus Double n where
  eye = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\j s' -> writeDoubleArray# marr (j *# n1) 1.0## s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# n,  r #)
    where
      n1 = n +# 1#
      n = case dimVal' @n of I# np -> np
      bs = n *# n *# EL_SIZE
  {-# INLINE eye #-}
  diag (KnownDataFrame (Scalar (D# v))) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\j s' -> writeDoubleArray# marr (j *# n1) v s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# n,  r #)
    where
      n1 = n +# 1#
      n = case dimVal' @n of I# np -> np
      bs = n *# n *# EL_SIZE
  {-# INLINE diag #-}


  det (KnownDataFrame (ArrayD# off nsqr arr)) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
       (# s1, mat #) -> case newByteArray#
                            (n *# EL_SIZE)
                            (copyByteArray# arr offb mat 0# bs s1) of
         (# s2, vec #) ->
            let f i x s | isTrue# (i >=# n) = (# s, x #)
                        | otherwise =
                            let !(# s' , j  #) = maxInRowRem# n n i mat s
                                !(# s'', x' #) = if isTrue# (i /=# j)
                                                then (# swapCols# n i j vec mat s'
                                                               , negateDouble# x #)
                                                else (# s', x #)
                                !(# s''', y #) = clearRowEnd# n n i mat s''
                            in if isTrue# (0.0## ==## y)
                               then (# s''', 0.0## #)
                               else f (i +# 1#) (x' *## y) s'''
            in f 0# 1.0## s2
     ) of (# _, r #) -> KnownDataFrame (Scalar (D# r))
    where
      n = case dimVal' @n of I# np -> np
      offb = off *# EL_SIZE
      bs = nsqr *# EL_SIZE
  det (KnownDataFrame (FromScalarD# _)) = 0
  {-# INLINE det #-}



  trace (KnownDataFrame (ArrayD# off nsqr a)) = KnownDataFrame (Scalar (D# (loop' 0# 0.0##)))
    where
      n1 = n +# 1#
      n = case dimVal' @n of I# np -> np
      loop' i acc | isTrue# (i ># nsqr) = acc
                  | otherwise = loop' (i +# n1)
                         (indexDoubleArray# a (off +# i) +## acc)
  trace (KnownDataFrame (FromScalarD# x)) = KnownDataFrame (Scalar (D# (x *## n)))
    where
      n = case fromIntegral (dimVal' @n) of D# np -> np
  {-# INLINE trace #-}



instance (KnownNat n, ArrayD '[n,n] ~ Array Double '[n,n], 2 <= n) => MatrixInverse Double n where
  inverse (KnownDataFrame (ArrayD# offs nsqr arr)) = case runRW#
     ( \s0 -> case newByteArray# (bs *# 2#) s0 of
         (# s1, mat #) -> case newByteArray# (vs *# 2#)
                -- copy original matrix to the top of an augmented matrix
                (loop1# n (\i s -> writeDoubleArray# mat
                           (i *# nn +# i +# n) 1.0##
                           (copyByteArray# arr (offb +# i *# vs)
                                           mat (2# *# i *# vs) vs s))
                         (setByteArray# mat 0# (bs *# 2#) 0# s1)
                ) of
           (# s2, vec #) ->
              let f i s | isTrue# (i >=# n) = s
                        | otherwise =
                            let !(# s' , j  #) = maxInRowRem# nn n i mat s
                                s''           = if isTrue# (i /=# j) then swapCols# nn i j vec mat s'
                                                                     else s'
                                !(# s''', _ #) = clearRowAll# nn n i mat s''
                            in f (i +# 1#) s'''
              in unsafeFreezeByteArray# mat
                  ( shrinkMutableByteArray# mat bs
                   (-- copy inverse matrix from the augmented part
                    loop1# n (\i s ->
                       copyMutableByteArray# mat
                                             (2# *# i *# vs +# vs)
                                             mat (i *# vs) vs s)
                   (f 0# s2)
                   )
                  )
     ) of (# _, r #) -> KnownDataFrame (ArrayD# 0# nsqr r)
    where
      nn = 2# *# n
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      vs = n *# EL_SIZE
      bs = n *# n *# EL_SIZE
      offb = offs *# EL_SIZE
  inverse (KnownDataFrame (FromScalarD# _)) = error "Cannot take inverse of a degenerate matrix"


-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------

-- #ifndef UNSAFE_INDICES
--       | isTrue# ( (i ># dim# _x)
--            `orI#` (i <=# 0#)
--           )       = error $ "Bad index " ++
--                     show (I# i) ++ " for " ++ show (dim _x)  ++ "D vector"
--       | otherwise
-- #endif


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
  bs = n *# EL_SIZE

-- | Starting from i-th row and i+1-th column, substract a multiple of i-th column from i+1 .. m columns,
--   such that there are only zeroes in i-th row and i+1..m columns elements.
clearRowEnd# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to remove from all others
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Double# #) -- next state and a diagonal element
clearRowEnd# n m i mat s0 = (# loop' (i +# 1#) s1, y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    !(# s1, y' #) = readDoubleArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0## /## y'
    n' = n -# i -# 1#
    loop' k s | isTrue# (k >=# m) = s
              | otherwise = loop' (k +# 1#)
       ( let x0 = k *# n +# i
             !(# s', a' #) = readDoubleArray# mat x0 s
             s'' = writeDoubleArray# mat x0 0.0## s'
             a  = a' *## yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )

-- | Substract a multiple of i-th column from 0 .. i-1 and i+1 .. m columns,
--   such that there are only zeroes in i-th row everywhere except i-th column
--   Assuming that elements in 0..i-1 columnts and in i-th row are zeroes, so they do not affect other columns.
--   After all columns updated, divide i-th row by its diagonal element, so (i,i) element has 1.
clearRowAll# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to remove from all others
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Double# #) -- next state and a diagonal element
clearRowAll# n m i mat s0 = (# divLoop (i +# 1#)
            (writeDoubleArray# mat ((n +# 1#) *# i) 1.0##
            (loop' 0# i (loop' (i +# 1#) m s1))), y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    !(# s1, y' #) = readDoubleArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0## /## y'
    n' = n -# i -# 1#
    loop' k km s | isTrue# (k >=# km) = s
                 | otherwise = loop' (k +# 1#) km
       ( let x0 = k *# n +# i
             !(# s', a' #) = readDoubleArray# mat x0 s
             s'' = writeDoubleArray# mat x0 0.0## s'
             a  = a' *## yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )
    divLoop k s | isTrue# (k >=# n) = s
                | otherwise = divLoop (k +# 1#)
       ( let x0 = n *# i +# k
             !(# s', x #) = readDoubleArray# mat x0 s
         in writeDoubleArray# mat x0 (x *## yrc) s'
       )


-- | Remove a multiple of one row from another one.
--   do: xi = xi - yi*a
multNRem# :: Int# -- n - nr of elements to go through
          -> Int# -- start idx of x (update)
          -> Int# -- start idx of y (read)
          -> Double# -- multiplier a
          -> MutableByteArray# s -- byte array of matrix
          -> State# s -- previous state
          -> State# s -- next state
multNRem# 0# _ _  _ _ s = s
multNRem# n x0 y0 a mat s = multNRem# (n -# 1#) (x0 +# 1#) (y0 +# 1#) a mat
  ( case readDoubleArray# mat y0 s of
     (# s1, y #) -> case readDoubleArray# mat x0 s1 of
       (# s2, x #) -> writeDoubleArray# mat x0 (x -## y *## a) s2
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
    !(# s1, v #) = readDoubleArray# mat ((n +# 1#) *# i) s0
    abs# x = if isTrue# (x >=## 0.0##) then x else negateDouble# x
    loop' ok ov k s | isTrue# (k >=# m) = (# s, ok #)
                    | otherwise = case readDoubleArray# mat (n *# k +# i) s of
                        (# s', v' #) -> if isTrue# (abs# v' >## ov)
                                        then loop' k (abs# v') (k +# 1#) s'
                                        else loop' ok ov (k +# 1#) s'

-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s)
       -> State# s -> State# s
loop2# n m f = loop0 0# 0#
  where
    loop0 i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop0 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop0 (i +# 1#) j s1
{-# INLINE loop2# #-}
