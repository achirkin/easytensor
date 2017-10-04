{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family.DoubleX2
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.DoubleX2 () where


#include "MachDeps.h"

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Double (..), RuntimeRep (..),
                                            isTrue#)

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions


instance Bounded DoubleX2 where
  maxBound = case infty of D# x -> DoubleX2# x x
  minBound = case negate infty of D# x -> DoubleX2# x x

infty :: Double
infty = read "Infinity"



instance Show DoubleX2 where
  show (DoubleX2# a1 a2) = "{ "     ++ show (D# a1)
                            ++ ", " ++ show (D# a2)
                            ++ " }"



instance Eq DoubleX2 where
  DoubleX2# a1 a2 == DoubleX2# b1 b2 = isTrue# (  (a1 ==## b1)
                                          `andI#` (a2 ==## b2)
                                           )
  {-# INLINE (==) #-}
  DoubleX2# a1 a2 /= DoubleX2# b1 b2 = isTrue# (  (a1 /=## b1)
                                           `orI#` (a2 /=## b2)
                                           )
  {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance Ord DoubleX2 where
  DoubleX2# a1 a2 > DoubleX2# b1 b2 = isTrue# (   (a1 >## b1)
                                          `andI#` (a2 >## b2)
                                           )
  {-# INLINE (>) #-}
  DoubleX2# a1 a2 < DoubleX2# b1 b2 = isTrue# (   (a1 <## b1)
                                          `andI#` (a2 <## b2)
                                           )
  {-# INLINE (<) #-}
  DoubleX2# a1 a2 >= DoubleX2# b1 b2 = isTrue# (  (a1 >=## b1)
                                          `andI#` (a2 >=## b2)
                                           )
  {-# INLINE (>=) #-}
  DoubleX2# a1 a2 <= DoubleX2# b1 b2 = isTrue# (  (a1 <=## b1)
                                          `andI#` (a2 <=## b2)
                                           )
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare (DoubleX2# a1 a2) (DoubleX2# b1 b2)
    | isTrue# (a1 >## b1) = GT
    | isTrue# (a1 <## b1) = LT
    | isTrue# (a2 >## b2) = GT
    | isTrue# (a2 <## b2) = LT
    | otherwise = EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min (DoubleX2# a1 a2) (DoubleX2# b1 b2) =
      DoubleX2# (if isTrue# (a1 >## b1) then b1 else a1)
                (if isTrue# (a2 >## b2) then b2 else a2)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max (DoubleX2# a1 a2) (DoubleX2# b1 b2) =
      DoubleX2# (if isTrue# (a1 >## b1) then a1 else b1)
                (if isTrue# (a2 >## b2) then a2 else b2)
  {-# INLINE max #-}



-- | element-wise operations for vectors
instance Num DoubleX2 where
  DoubleX2# a1 a2 + DoubleX2# b1 b2
    = DoubleX2# ((+##) a1 b1) ((+##) a2 b2)
  {-# INLINE (+) #-}
  DoubleX2# a1 a2 - DoubleX2# b1 b2
    = DoubleX2# ((-##) a1 b1) ((-##) a2 b2)
  {-# INLINE (-) #-}
  DoubleX2# a1 a2 * DoubleX2# b1 b2
    = DoubleX2# ((*##) a1 b1) ((*##) a2 b2)
  {-# INLINE (*) #-}
  negate (DoubleX2# a1 a2)
    = DoubleX2# (negateDouble# a1) (negateDouble# a2)
  {-# INLINE negate #-}
  abs (DoubleX2# a1 a2)
    = DoubleX2# (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1)
                (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
  {-# INLINE abs #-}
  signum (DoubleX2# a1 a2)
    = DoubleX2# (if isTrue# (a1 >## 0.0##)
                then 1.0##
                else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
               (if isTrue# (a2 >## 0.0##)
                then 1.0##
                else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## )
  {-# INLINE signum #-}
  fromInteger n = case fromInteger n of D# x -> DoubleX2# x x
  {-# INLINE fromInteger #-}



instance Fractional DoubleX2 where
  DoubleX2# a1 a2 / DoubleX2# b1 b2 = DoubleX2# ((/##) a1 b1)
                                                ((/##) a2 b2)
  {-# INLINE (/) #-}
  recip (DoubleX2# a1 a2) = DoubleX2# ((/##) 1.0## a1)
                                      ((/##) 1.0## a2)
  {-# INLINE recip #-}
  fromRational r = case fromRational r of D# x -> DoubleX2# x x
  {-# INLINE fromRational #-}



instance Floating DoubleX2 where
  pi = DoubleX2# 3.141592653589793238## 3.141592653589793238##
  {-# INLINE pi #-}
  exp (DoubleX2# a1 a2) = DoubleX2# (expDouble# a1)
                                    (expDouble# a2)
  {-# INLINE exp #-}
  log (DoubleX2# a1 a2) = DoubleX2# (logDouble# a1)
                                    (logDouble# a2)
  {-# INLINE log #-}
  sqrt (DoubleX2# a1 a2) = DoubleX2# (sqrtDouble# a1)
                                     (sqrtDouble# a2)
  {-# INLINE sqrt #-}
  sin (DoubleX2# a1 a2) = DoubleX2# (sinDouble# a1)
                                    (sinDouble# a2)
  {-# INLINE sin #-}
  cos (DoubleX2# a1 a2) = DoubleX2# (cosDouble# a1)
                                    (cosDouble# a2)
  {-# INLINE cos #-}
  tan (DoubleX2# a1 a2) = DoubleX2# (tanDouble# a1)
                                    (tanDouble# a2)
  {-# INLINE tan #-}
  asin (DoubleX2# a1 a2) = DoubleX2# (asinDouble# a1)
                                     (asinDouble# a2)
  {-# INLINE asin #-}
  acos (DoubleX2# a1 a2) = DoubleX2# (acosDouble# a1)
                                     (acosDouble# a2)
  {-# INLINE acos #-}
  atan (DoubleX2# a1 a2) = DoubleX2# (atanDouble# a1)
                                     (atanDouble# a2)
  {-# INLINE atan #-}
  sinh (DoubleX2# a1 a2) = DoubleX2# (sinDouble# a1)
                                     (sinDouble# a2)
  {-# INLINE sinh #-}
  cosh (DoubleX2# a1 a2) = DoubleX2# (coshDouble# a1)
                                     (coshDouble# a2)
  {-# INLINE cosh #-}
  tanh (DoubleX2# a1 a2) = DoubleX2# (tanhDouble# a1)
                                     (tanhDouble# a2)
  {-# INLINE tanh #-}
  DoubleX2# a1 a2 ** DoubleX2# b1 b2 = DoubleX2# ((**##) a1 b1)
                                                 ((**##) a2 b2)
  {-# INLINE (**) #-}

  logBase x y         =  log y / log x
  {-# INLINE logBase #-}
  asinh x = log (x + sqrt (1.0+x*x))
  {-# INLINE asinh #-}
  acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
  {-# INLINE acosh #-}
  atanh x = 0.5 * log ((1.0+x) / (1.0-x))
  {-# INLINE atanh #-}



type instance ElemRep DoubleX2 = 'DoubleRep
type instance ElemPrim DoubleX2 = Double#
instance PrimBytes DoubleX2 where
  toBytes (DoubleX2# a1 a2) = case runRW#
     ( \s0 -> case newByteArray# (SIZEOF_HSFLOAT# *# 2#) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
           s2 -> case writeDoubleArray# marr 1# a2 s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, a #) -> (# 0#, 2#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = DoubleX2#
    (indexDoubleArray# arr off)
    (indexDoubleArray# arr (off +# 1#))
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSFLOAT# *# 2#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE elementByteSize #-}
  ix 0# (DoubleX2# a1 _) = a1
  ix 1# (DoubleX2# _ a2) = a2
  ix _ _                = undefined
  {-# INLINE ix #-}


instance ElementWise (Idx '[2]) Double DoubleX2 where
  indexOffset# (DoubleX2# a1 _) 0# = D# a1
  indexOffset# (DoubleX2# _ a2) 1# = D# a2
  indexOffset# _               _  = undefined
  {-# INLINE indexOffset# #-}

  (!) (DoubleX2# a1 _) ( 1 :! Z) = D# a1
  (!) (DoubleX2# _ a2) ( 2 :! Z) = D# a2
  (!) _               ( _ :! Z) = undefined
  {-# INLINE (!) #-}

  broadcast (D# x) = DoubleX2# x x
  {-# INLINE broadcast #-}

  ewmap f (DoubleX2# x y) = case (f (1:!Z) (D# x), f (2:!Z) (D# y)) of
                              (D# r1, D# r2) -> DoubleX2# r1 r2
  {-# INLINE ewmap #-}

  ewgen f = case (f (1:!Z), f (2:!Z)) of (D# r1, D# r2) -> DoubleX2# r1 r2
  {-# INLINE ewgen #-}

  ewgenA f = (\(D# r1) (D# r2) -> DoubleX2# r1 r2) <$> f (1:!Z) <*> f (2:!Z)
  {-# INLINE ewgenA #-}

  ewfoldl f x0 (DoubleX2# x y) = f (2:!Z) (f (1:!Z) x0 (D# x)) (D# y)
  {-# INLINE ewfoldl #-}

  ewfoldr f x0 (DoubleX2# x y) = f (1:!Z) (D# x) (f (2:!Z) (D# y) x0)
  {-# INLINE ewfoldr #-}

  elementWise f (DoubleX2# x y) = (\(D# a) (D# b) -> DoubleX2# a b)
                               <$> f (D# x) <*> f (D# y)
  {-# INLINE elementWise #-}

  indexWise f (DoubleX2# x y) = (\(D# a) (D# b) -> DoubleX2# a b)
                             <$> f (1:!Z) (D# x) <*> f (2:!Z) (D# y)
  {-# INLINE indexWise #-}

  update (1 :! Z) (D# q) (DoubleX2# _ y) = DoubleX2# q y
  update (2 :! Z) (D# q) (DoubleX2# x _) = DoubleX2# x q
  update (_ :! Z) _ x = x
  {-# INLINE update #-}
