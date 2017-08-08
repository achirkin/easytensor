{-# LANGUAGE BangPatterns          #-}
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
-- Module      :  Numeric.Array.Family.ArrayF
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayF () where



import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Float (..), Int (..),
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
#define ARR_TYPE                 ArrayF
#define ARR_FROMSCALAR           FromScalarF#
#define ARR_CONSTR               ArrayF#
#define EL_TYPE_BOXED            Float
#define EL_TYPE_PRIM             Float#
#define EL_RUNTIME_REP           'FloatRep
#define EL_CONSTR                F#
#define EL_SIZE                  SIZEOF_HSFLOAT#
#define EL_ALIGNMENT             ALIGNMENT_HSFLOAT#
#define EL_ZERO                  0.0#
#define EL_ONE                   1.0#
#define EL_MINUS_ONE             -1.0#
#define INDEX_ARRAY              indexFloatArray#
#define WRITE_ARRAY              writeFloatArray#
#define OP_EQ                    eqFloat#
#define OP_NE                    neFloat#
#define OP_GT                    gtFloat#
#define OP_GE                    geFloat#
#define OP_LT                    ltFloat#
#define OP_LE                    leFloat#
#define OP_PLUS                  plusFloat#
#define OP_MINUS                 minusFloat#
#define OP_TIMES                 timesFloat#
#define OP_NEGATE                negateFloat#
#include "Array.h"


instance Num (ArrayF ds) where
  (+) = zipV plusFloat#
  {-# INLINE (+) #-}
  (-) = zipV minusFloat#
  {-# INLINE (-) #-}
  (*) = zipV timesFloat#
  {-# INLINE (*) #-}
  negate = mapV negateFloat#
  {-# INLINE negate #-}
  abs = mapV (\x -> if isTrue# (geFloat# x 0.0#)
                    then x
                    else negateFloat# x
                )
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (gtFloat# x 0.0#)
                       then 1.0#
                       else if isTrue# (ltFloat# x 0.0#)
                            then -1.0#
                            else 0.0#
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional (ArrayF ds) where
  (/) = zipV divideFloat#
  {-# INLINE (/) #-}
  recip = mapV (divideFloat# 1.0#)
  {-# INLINE recip #-}
  fromRational = broadcastArray . fromRational
  {-# INLINE fromRational #-}



instance Floating (ArrayF ds) where
  pi = broadcastArray pi
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
  asinh = mapV (\x -> logFloat# (x `plusFloat#`
                                sqrtFloat# (1.0# `plusFloat#` timesFloat# x x)))
  {-# INLINE asinh #-}
  acosh = mapV (\x ->  case plusFloat# x 1.0# of
                 y -> logFloat# ( x `plusFloat#` timesFloat# y
                           (sqrtFloat# (minusFloat# x 1.0# `divideFloat#` y))
                        )
               )
  {-# INLINE acosh #-}
  atanh = mapV (\x -> 0.5# `timesFloat#`
                logFloat# (plusFloat# 1.0# x `divideFloat#` minusFloat# 1.0# x))
  {-# INLINE atanh #-}




instance (KnownDim n, KnownDim m, ArrayF '[n,m] ~ Array Float '[n,m], 2 <= n, 2 <= m)
      => MatrixCalculus Float n m where
  transpose (KnownDataFrame (ArrayF# offs nm arr)) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop2# n m
               (\i j s' -> writeFloatArray# marr (j +# m *# i)
                              (indexFloatArray# arr (offs +# j *# n +# i)) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, nm, r #)
    where
      n = case dimVal' @n of I# np -> np
      m = case dimVal' @m of I# mp -> mp
      bs = n *# m *# SIZEOF_HSFLOAT#
  transpose (KnownDataFrame (FromScalarF# x)) = unsafeCoerce# $ FromScalarF# x

instance ( KnownDim n, ArrayF '[n,n] ~ Array Float '[n,n] )
      => SquareMatrixCalculus Float n where
  eye = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\j s' -> writeFloatArray# marr (j *# n1) 1.0# s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# n,  r #)
    where
      n1 = n +# 1#
      n = case dimVal' @n of I# np -> np
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE eye #-}
  diag (KnownDataFrame (Scalar (F# v))) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\j s' -> writeFloatArray# marr (j *# n1) v s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# n,  r #)
    where
      n1 = n +# 1#
      n = case dimVal' @n of I# np -> np
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE diag #-}


  det (KnownDataFrame (ArrayF# off nsqr arr)) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
       (# s1, mat #) -> case newByteArray#
                            (n *# SIZEOF_HSFLOAT#)
                            (copyByteArray# arr offb mat 0# bs s1) of
         (# s2, vec #) ->
            let f i x s | isTrue# (i >=# n) = (# s, x #)
                        | otherwise =
                            let !(# s' , j  #) = maxInRowRem# n n i mat s
                                !(# s'', x' #) = if isTrue# (i /=# j)
                                                then (# swapCols# n i j vec mat s'
                                                               , negateFloat# x #)
                                                else (# s', x #)
                                !(# s''', y #) = clearRowEnd# n n i mat s''
                            in if isTrue# (eqFloat# 0.0# y)
                               then (# s''', 0.0# #)
                               else f (i +# 1#) (timesFloat# x' y) s'''
            in f 0# 1.0# s2
     ) of (# _, r #) -> KnownDataFrame (Scalar (F# r))
    where
      n = case dimVal' @n of I# np -> np
      offb = off *# SIZEOF_HSFLOAT#
      bs = nsqr *# SIZEOF_HSFLOAT#
  det (KnownDataFrame (FromScalarF# _)) = 0
  {-# INLINE det #-}



  trace (KnownDataFrame (ArrayF# off nsqr a)) = KnownDataFrame (Scalar (F# (loop' 0# 0.0#)))
    where
      n1 = n +# 1#
      n = case dimVal' @n of I# np -> np
      loop' i acc | isTrue# (i ># nsqr) = acc
                  | otherwise = loop' (i +# n1)
                         (indexFloatArray# a (off +# i) `plusFloat#` acc)
  trace (KnownDataFrame (FromScalarF# x)) = KnownDataFrame (Scalar (F# (x `timesFloat#` n)))
    where
      n = case fromIntegral (dimVal' @n) of F# np -> np
  {-# INLINE trace #-}



instance (KnownNat n, ArrayF '[n,n] ~ Array Float '[n,n], 2 <= n) => MatrixInverse Float n where
  inverse (KnownDataFrame (ArrayF# offs nsqr arr)) = case runRW#
     ( \s0 -> case newByteArray# (bs *# 2#) s0 of
         (# s1, mat #) -> case newByteArray# (vs *# 2#)
                -- copy original matrix to the top of an augmented matrix
                (loop1# n (\i s -> writeFloatArray# mat
                           (i *# nn +# i +# n) 1.0#
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
     ) of (# _, r #) -> KnownDataFrame (ArrayF# 0# nsqr r)
    where
      nn = 2# *# n
      n = case dimVal' @n of I# np -> np
      vs = n *# SIZEOF_HSFLOAT#
      bs = n *# n *# SIZEOF_HSFLOAT#
      offb = offs *# SIZEOF_HSFLOAT#
  inverse (KnownDataFrame (FromScalarF# _)) = error "Cannot take inverse of a degenerate matrix"


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
    !(# s1, y' #) = readFloatArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0# `divideFloat#` y'
    n' = n -# i -# 1#
    loop' k s | isTrue# (k >=# m) = s
              | otherwise = loop' (k +# 1#)
       ( let x0 = k *# n +# i
             !(# s', a' #) = readFloatArray# mat x0 s
             s'' = writeFloatArray# mat x0 0.0# s'
             a  = a' `timesFloat#` yrc
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
             -> (# State# s, Float# #) -- next state and a diagonal element
clearRowAll# n m i mat s0 = (# divLoop (i +# 1#)
            (writeFloatArray# mat ((n +# 1#) *# i) 1.0#
            (loop' 0# i (loop' (i +# 1#) m s1))), y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    !(# s1, y' #) = readFloatArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0# `divideFloat#` y'
    n' = n -# i -# 1#
    loop' k km s | isTrue# (k >=# km) = s
                 | otherwise = loop' (k +# 1#) km
       ( let x0 = k *# n +# i
             !(# s', a' #) = readFloatArray# mat x0 s
             s'' = writeFloatArray# mat x0 0.0# s'
             a  = a' `timesFloat#` yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )
    divLoop k s | isTrue# (k >=# n) = s
                | otherwise = divLoop (k +# 1#)
       ( let x0 = n *# i +# k
             !(# s', x #) = readFloatArray# mat x0 s
         in writeFloatArray# mat x0 (timesFloat# x yrc) s'
       )


-- | Remove a multiple of one row from another one.
--   do: xi = xi - yi*a
multNRem# :: Int# -- n - nr of elements to go through
          -> Int# -- start idx of x (update)
          -> Int# -- start idx of y (read)
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
    !(# s1, v #) = readFloatArray# mat ((n +# 1#) *# i) s0
    abs# x = if isTrue# (x `geFloat#` 0.0#) then x else negateFloat# x
    loop' ok ov k s | isTrue# (k >=# m) = (# s, ok #)
                    | otherwise = case readFloatArray# mat (n *# k +# i) s of
                        (# s', v' #) -> if isTrue# (abs# v' `gtFloat#` ov)
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
