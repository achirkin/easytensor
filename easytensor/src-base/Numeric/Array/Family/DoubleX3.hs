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
-- Module      :  Numeric.Array.Family.DoubleX3
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.DoubleX3 () where


#include "MachDeps.h"

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Double (..), RuntimeRep (..),
                                            isTrue#)

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions


instance Bounded DoubleX3 where
  maxBound = case infty of D# x -> DoubleX3# x x x
  minBound = case negate infty of D# x -> DoubleX3# x x x

infty :: Double
infty = read "Infinity"


instance Show DoubleX3 where
  show (DoubleX3# a1 a2 a3) = "{ "     ++ show (D# a1)
                              ++ ", " ++ show (D# a2)
                              ++ ", " ++ show (D# a3)
                              ++ " }"



instance Eq DoubleX3 where
  DoubleX3# a1 a2 a3 == DoubleX3# b1 b2 b3 = isTrue# (  (a1 ==## b1)
                                              `andI#` (a2 ==## b2)
                                              `andI#` (a3 ==## b3)
                                              )
  {-# INLINE (==) #-}
  DoubleX3# a1 a2 a3 /= DoubleX3# b1 b2 b3 = isTrue# (  (a1 /=## b1)
                                               `orI#` (a2 /=## b2)
                                               `orI#` (a3 /=## b3)
                                               )
  {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance Ord DoubleX3 where
  DoubleX3# a1 a2 a3 > DoubleX3# b1 b2 b3 = isTrue# (   (a1 >## b1)
                                              `andI#` (a2 >## b2)
                                              `andI#` (a3 >## b3)
                                              )
  {-# INLINE (>) #-}
  DoubleX3# a1 a2 a3 < DoubleX3# b1 b2 b3 = isTrue# (   (a1 <## b1)
                                              `andI#` (a2 <## b2)
                                              `andI#` (a3 <## b3)
                                              )
  {-# INLINE (<) #-}
  DoubleX3# a1 a2 a3 >= DoubleX3# b1 b2 b3 = isTrue# (  (a1 >=## b1)
                                              `andI#` (a2 >=## b2)
                                              `andI#` (a3 >=## b3)
                                              )
  {-# INLINE (>=) #-}
  DoubleX3# a1 a2 a3 <= DoubleX3# b1 b2 b3 = isTrue# (  (a1 <=## b1)
                                              `andI#` (a2 <=## b2)
                                              `andI#` (a3 <=## b3)
                                              )
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare (DoubleX3# a1 a2 a3) (DoubleX3# b1 b2 b3)
    | isTrue# (a1 >## b1) = GT
    | isTrue# (a1 <## b1) = LT
    | isTrue# (a2 >## b2) = GT
    | isTrue# (a2 <## b2) = LT
    | isTrue# (a3 >## b3) = GT
    | isTrue# (a3 <## b3) = LT
    | otherwise = EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min (DoubleX3# a1 a2 a3) (DoubleX3# b1 b2 b3) =
      DoubleX3# (if isTrue# (a1 >## b1) then b1 else a1)
               (if isTrue# (a2 >## b2) then b2 else a2)
               (if isTrue# (a3 >## b3) then b3 else a3)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max (DoubleX3# a1 a2 a3) (DoubleX3# b1 b2 b3) =
      DoubleX3# (if isTrue# (a1 >## b1) then a1 else b1)
               (if isTrue# (a2 >## b2) then a2 else b2)
               (if isTrue# (a3 >## b3) then a3 else b3)
  {-# INLINE max #-}



-- | element-wise operations for vectors
instance Num DoubleX3 where
  DoubleX3# a1 a2 a3 + DoubleX3# b1 b2 b3
    = DoubleX3# ((+##) a1 b1) ((+##) a2 b2) ((+##) a3 b3)
  {-# INLINE (+) #-}
  DoubleX3# a1 a2 a3 - DoubleX3# b1 b2 b3
    = DoubleX3# ((-##) a1 b1) ((-##) a2 b2) ((-##) a3 b3)
  {-# INLINE (-) #-}
  DoubleX3# a1 a2 a3 * DoubleX3# b1 b2 b3
    = DoubleX3# ((*##) a1 b1) ((*##) a2 b2) ((*##) a3 b3)
  {-# INLINE (*) #-}
  negate (DoubleX3# a1 a2 a3)
    = DoubleX3# (negateDouble# a1) (negateDouble# a2) (negateDouble# a3)
  {-# INLINE negate #-}
  abs (DoubleX3# a1 a2 a3)
    = DoubleX3# (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1)
               (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
               (if isTrue# (a3 >=## 0.0##) then a3 else negateDouble# a3)
  {-# INLINE abs #-}
  signum (DoubleX3# a1 a2 a3)
    = DoubleX3# (if isTrue# (a1 >## 0.0##)
                then 1.0##
                else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
               (if isTrue# (a2 >## 0.0##)
                then 1.0##
                else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## )
               (if isTrue# (a3 >## 0.0##)
                then 1.0##
                else if isTrue# (a3 <## 0.0##) then -1.0## else 0.0## )
  {-# INLINE signum #-}
  fromInteger n = case fromInteger n of D# x -> DoubleX3# x x x
  {-# INLINE fromInteger #-}



instance Fractional DoubleX3 where
  DoubleX3# a1 a2 a3 / DoubleX3# b1 b2 b3  = DoubleX3# ((/##) a1 b1)
                                                    ((/##) a2 b2)
                                                    ((/##) a3 b3)
  {-# INLINE (/) #-}
  recip (DoubleX3# a1 a2 a3) = DoubleX3# ((/##) 1.0## a1)
                                       ((/##) 1.0## a2)
                                       ((/##) 1.0## a3)
  {-# INLINE recip #-}
  fromRational r = case fromRational r of D# x -> DoubleX3# x x x
  {-# INLINE fromRational #-}



instance Floating DoubleX3 where
  pi = DoubleX3# 3.141592653589793238## 3.141592653589793238## 3.141592653589793238##
  {-# INLINE pi #-}
  exp (DoubleX3# a1 a2 a3) = DoubleX3# (expDouble# a1)
                                     (expDouble# a2)
                                     (expDouble# a3)
  {-# INLINE exp #-}
  log (DoubleX3# a1 a2 a3) = DoubleX3# (logDouble# a1)
                                     (logDouble# a2)
                                     (logDouble# a3)
  {-# INLINE log #-}
  sqrt (DoubleX3# a1 a2 a3) = DoubleX3# (sqrtDouble# a1)
                                      (sqrtDouble# a2)
                                      (sqrtDouble# a3)
  {-# INLINE sqrt #-}
  sin (DoubleX3# a1 a2 a3) = DoubleX3# (sinDouble# a1)
                                     (sinDouble# a2)
                                     (sinDouble# a3)
  {-# INLINE sin #-}
  cos (DoubleX3# a1 a2 a3) = DoubleX3# (cosDouble# a1)
                                     (cosDouble# a2)
                                     (cosDouble# a3)
  {-# INLINE cos #-}
  tan (DoubleX3# a1 a2 a3) = DoubleX3# (tanDouble# a1)
                                     (tanDouble# a2)
                                     (tanDouble# a3)
  {-# INLINE tan #-}
  asin (DoubleX3# a1 a2 a3) = DoubleX3# (asinDouble# a1)
                                      (asinDouble# a2)
                                      (asinDouble# a3)
  {-# INLINE asin #-}
  acos (DoubleX3# a1 a2 a3) = DoubleX3# (acosDouble# a1)
                                      (acosDouble# a2)
                                      (acosDouble# a3)
  {-# INLINE acos #-}
  atan (DoubleX3# a1 a2 a3) = DoubleX3# (atanDouble# a1)
                                      (atanDouble# a2)
                                      (atanDouble# a3)
  {-# INLINE atan #-}
  sinh (DoubleX3# a1 a2 a3) = DoubleX3# (sinDouble# a1)
                                      (sinDouble# a2)
                                      (sinDouble# a3)
  {-# INLINE sinh #-}
  cosh (DoubleX3# a1 a2 a3) = DoubleX3# (coshDouble# a1)
                                      (coshDouble# a2)
                                      (coshDouble# a3)
  {-# INLINE cosh #-}
  tanh (DoubleX3# a1 a2 a3) = DoubleX3# (tanhDouble# a1)
                                      (tanhDouble# a2)
                                      (tanhDouble# a3)
  {-# INLINE tanh #-}
  DoubleX3# a1 a2 a3 ** DoubleX3# b1 b2 b3 = DoubleX3# ((**##) a1 b1)
                                                    ((**##) a2 b2)
                                                    ((**##) a3 b3)
  {-# INLINE (**) #-}

  logBase x y         =  log y / log x
  {-# INLINE logBase #-}
  asinh x = log (x + sqrt (1.0+x*x))
  {-# INLINE asinh #-}
  acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
  {-# INLINE acosh #-}
  atanh x = 0.5 * log ((1.0+x) / (1.0-x))
  {-# INLINE atanh #-}



type instance ElemRep DoubleX3 = 'DoubleRep
type instance ElemPrim DoubleX3 = Double#
instance PrimBytes DoubleX3 where
  toBytes (DoubleX3# a1 a2 a3) = case runRW#
     ( \s0 -> case newByteArray# (SIZEOF_HSDOUBLE# *# 3#) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
           s2 -> case writeDoubleArray# marr 1# a2 s2 of
             s3 -> case writeDoubleArray# marr 2# a3 s3 of
               s4 -> unsafeFreezeByteArray# marr s4
     ) of (# _, a #) -> (# 0#, 3#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = DoubleX3#
    (indexDoubleArray# arr off)
    (indexDoubleArray# arr (off +# 1#))
    (indexDoubleArray# arr (off +# 2#))
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSDOUBLE# *# 3#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSDOUBLE#
  {-# INLINE byteAlign #-}
  elementByteSize _ = SIZEOF_HSDOUBLE#
  {-# INLINE elementByteSize #-}
  ix 0# (DoubleX3# a1 _ _) = a1
  ix 1# (DoubleX3# _ a2 _) = a2
  ix 2# (DoubleX3# _ _ a3) = a3
  ix _ _                  = undefined
  {-# INLINE ix #-}


instance ElementWise (Idx '[3]) Double DoubleX3 where
  indexOffset# (DoubleX3# a1 _ _) 0# = D# a1
  indexOffset# (DoubleX3# _ a2 _) 1# = D# a2
  indexOffset# (DoubleX3# _ _ a3) 2# = D# a3
  indexOffset# _                   _  = undefined
  {-# INLINE indexOffset# #-}

  (!) (DoubleX3# a1 _ _) ( 1 :! Z) = D# a1
  (!) (DoubleX3# _ a2 _) ( 2 :! Z) = D# a2
  (!) (DoubleX3# _ _ a3) ( 3 :! Z) = D# a3
  (!) _               ( _ :! Z)   = undefined
  {-# INLINE (!) #-}

  broadcast (D# x) = DoubleX3# x x x
  {-# INLINE broadcast #-}

  ewmap f (DoubleX3# x y z) = case (f (1:!Z) (D# x), f (2:!Z) (D# y), f (3:!Z) (D# z)) of
                              (D# r1, D# r2, D# r3) -> DoubleX3# r1 r2 r3
  {-# INLINE ewmap #-}

  ewgen f = case (f (1:!Z), f (2:!Z), f (3:!Z)) of (D# r1, D# r2, D# r3) -> DoubleX3# r1 r2 r3
  {-# INLINE ewgen #-}

  ewgenA f = (\(D# r1) (D# r2) (D# r3) -> DoubleX3# r1 r2 r3)
          <$> f (1:!Z) <*> f (2:!Z) <*> f (3:!Z)
  {-# INLINE ewgenA #-}

  ewfoldl f x0 (DoubleX3# x y z) = f (3:!Z) (f (2:!Z) (f (1:!Z) x0 (D# x)) (D# y)) (D# z)
  {-# INLINE ewfoldl #-}

  ewfoldr f x0 (DoubleX3# x y z) = f (1:!Z) (D# x) (f (2:!Z) (D# y) (f (3:!Z) (D# z) x0))
  {-# INLINE ewfoldr #-}

  elementWise f (DoubleX3# x y z) = (\(D# a) (D# b) (D# c) -> DoubleX3# a b c)
                                 <$> f (D# x) <*> f (D# y) <*> f (D# z)
  {-# INLINE elementWise #-}

  indexWise f (DoubleX3# x y z) = (\(D# a) (D# b) (D# c) -> DoubleX3# a b c)
                             <$> f (1:!Z) (D# x) <*> f (2:!Z) (D# y) <*> f (3:!Z) (D# z)
  {-# INLINE indexWise #-}

  update (1 :! Z) (D# q) (DoubleX3# _ y z) = DoubleX3# q y z
  update (2 :! Z) (D# q) (DoubleX3# x _ z) = DoubleX3# x q z
  update (3 :! Z) (D# q) (DoubleX3# x y _) = DoubleX3# x y q
  update (_ :! Z) _ x = x
  {-# INLINE update #-}
