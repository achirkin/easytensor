{-# LANGUAGE KindSignatures, GADTs #-}
{-# LANGUAGE TypeOperators #-}
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
-- Module      :  Numeric.NDArray.Base.Float
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.NDArray.Base.Float  where


#include "MachDeps.h"
#include "HsBaseConfig.h"

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types
--import GHC.TypeLits



import Numeric.Dimensions
import Numeric.Commons
--import Numeric.NDArray.Class
import Numeric.NDArray.Family



instance ( (Take 2 ds ++ Drop 2 ds) ~ ds
         , Dimensions ds
         , Dimensions (Take 2 ds)
         , Dimensions (Drop 2 ds)
         ) => Show (NDArrayF ds) where
  show x = drop 1 $ foldr loopOuter "" [minBound..maxBound]
    where
      loopInner :: Dim (Drop 2 ds) -> Dim (Take 2 ds) -> String
      loopInner _ Z = "{}"
      loopInner ods (n:-Z) =  ('{' :) . drop 1 $
                                  foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
                                         [1 :- ods .. n :- ods]
      loopInner ods (n:-m:-_) = ('{' :) . drop 2 $
                            foldr (\i ss -> '\n':
                                    foldr (\j s ->
                                             ", " ++ show (x ! (i :- j :- ods)) ++ s
                                          ) ss [1..m]
                                  ) " }" [1..n]
      loopOuter :: Dim (Drop 2 ds) -> String -> String
      loopOuter Z s = "\n" ++ loopInner Z maxBound ++ s
      loopOuter ds s ="\n" ++ show ds ++ ":\n" ++ loopInner ds maxBound ++ s


instance Dimensions ds => Eq (NDArrayF ds) where
  a == b = accumV2 (\x y r -> r && isTrue# (x `eqFloat#` y)) a b True
  {-# INLINE (==) #-}
  a /= b = accumV2 (\x y r -> r || isTrue# (x `neFloat#` y)) a b False
  {-# INLINE (/=) #-}




-- | Implement partial ordering for `>`, `<`, `>=`, `<=` and lexicographical ordering for `compare`
instance Dimensions ds => Ord (NDArrayF ds) where
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





instance Dimensions ds => Num (NDArrayF ds) where
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
  fromInteger = broadcastArrayF undefined . fromInteger
  {-# INLINE fromInteger #-}



instance Dimensions ds => Fractional (NDArrayF ds) where
  (/) = zipV divideFloat#
  {-# INLINE (/) #-}
  recip = mapV (divideFloat# 1.0#)
  {-# INLINE recip #-}
  fromRational = broadcastArrayF undefined . fromRational
  {-# INLINE fromRational #-}



instance Dimensions ds => Floating (NDArrayF ds) where
  pi = broadcastArrayF undefined pi
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



-- | Uses first argument to enforce type (can and should be undefined)
broadcastArrayF :: Dimensions ds => NDArrayF ds -> Float -> NDArrayF ds
broadcastArrayF arr (F# x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> writeFloatArray# marr i x s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> NDArrayF# r
  where
    n = totalDim# arr
    bs = n *# SIZEOF_HSFLOAT#
{-# INLINE broadcastArrayF #-}




instance Dimensions ds => PrimBytes (NDArrayF ds) where
  toBytes (NDArrayF# a) = a
  {-# INLINE toBytes #-}
  fromBytes = NDArrayF#
  {-# INLINE fromBytes #-}
  byteSize x = SIZEOF_HSFLOAT# *# totalDim# x
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}

instance FloatBytes (NDArrayF ds) where
  ixF i (NDArrayF# a) = indexFloatArray# a i
  {-# INLINE ixF #-}



instance Dimensions ds => ElementWise (Dim ds) Float (NDArrayF ds) where
  (!) (NDArrayF# arr) i = case fromEnum i of I# j -> F# (indexFloatArray# arr j)
  {-# INLINE (!) #-}
  ewmap f x@(NDArrayF# arr) = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dim x)
               (\ix s' -> case readMutVar# mi s' of
                           (# s'', I# i #) -> case f ix (F# (indexFloatArray# arr i)) of
                              F# r -> writeMutVar# mi (I# (i +# 1#)) (writeFloatArray# marr i r s'')
               ) s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> NDArrayF# r
    where
      n = totalDim# x
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE ewmap #-}
  ewgen f  = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dim x)
               (\ix s' -> case readMutVar# mi s' of
                           (# s'', I# i #) -> case f ix of
                              F# r -> writeMutVar# mi (I# (i +# 1#)) (writeFloatArray# marr i r s'')
               ) s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> NDArrayF# r
    where
      x = undefined :: NDArrayF ds
      n = totalDim# x
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE ewgen #-}
  ewfold f v0 x@(NDArrayF# arr) = case runRW#
     (\s0 -> case newMutVar# (0,v0) s0 of
         (# s1, miv #) -> case loopS (dim x)
               (\ix s' -> case readMutVar# miv s' of
                           (# s'', (I# i, v) #) -> writeMutVar# miv (I# (i +# 1#), f ix (F# (indexFloatArray# arr i)) v) s''
               ) s1 of
            s2 -> readMutVar# miv s2
     ) of (# _, (_, r) #) -> r
  {-# INLINE ewfold #-}
  indexWise f x@(NDArrayF# arr) = case loopA (dim x) g (AU# 0# (pure (\_ s -> s))) of
        AU# _ f' -> wr <$> f'
    where
      g ds (AU# i f') = AU# ( i +# 1# ) $ (\(F# z) u a s -> writeFloatArray# a i z (u a s)) <$> f ds (F# (indexFloatArray# arr i)) <*> f'
      n = totalDim# x
      bs = n *# SIZEOF_HSFLOAT#
      wr f' = case runRW#
                   ( \s0 -> case newByteArray# bs s0 of
                             (# s1, marr #) ->  case f' marr s1 of
                               s2 -> unsafeFreezeByteArray# marr s2
                   ) of (# _, r #) -> NDArrayF# r
  elementWise f x@(NDArrayF# arr) = wr <$> loop1a# n g (pure (\_ s -> s))
    where
      g i f' = (\(F# z) u a s -> writeFloatArray# a i z (u a s)) <$> f (F# (indexFloatArray# arr i)) <*> f'
      n = totalDim# x
      bs = n *# SIZEOF_HSFLOAT#
      wr f' = case runRW#
                   ( \s0 -> case newByteArray# bs s0 of
                             (# s1, marr #) ->  case f' marr s1 of
                               s2 -> unsafeFreezeByteArray# marr s2
                   ) of (# _, r #) -> NDArrayF# r

data ArrayUpdate# (f :: * -> *) s  = AU# Int# !(f (MutableByteArray# s -> State# s -> State# s))



-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------



zipV :: Dimensions ds => (Float# -> Float# -> Float#) -> NDArrayF ds -> NDArrayF ds -> NDArrayF ds
zipV f x@(NDArrayF# a) (NDArrayF# b) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> case f (indexFloatArray# a i) (indexFloatArray# b i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> NDArrayF# r
  where
    n = totalDim# x
    bs = n *# SIZEOF_HSFLOAT#

mapV :: Dimensions ds => (Float# -> Float#) -> NDArrayF ds-> NDArrayF ds
mapV f x@(NDArrayF# a) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> case f (indexFloatArray# a i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> NDArrayF# r
  where
    n = totalDim# x
    bs = n *# SIZEOF_HSFLOAT#


accumV2 :: Dimensions ds => (Float# -> Float# -> a -> a) -> NDArrayF ds -> NDArrayF ds -> a -> a
accumV2 f x@(NDArrayF# a) (NDArrayF# b) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) (indexFloatArray# b i) acc)
    n = totalDim# x



-- | Do something in a loop for int i from 0 to n
loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop1# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1# #-}

-- | Do something in a loop for int i from 0 to n
loop1a# :: Int# -> (Int# -> a -> a) -> a -> a
loop1a# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1a# #-}
