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
-- Module      :  Numeric.Array.Family.DoubleX4
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.DoubleX4 () where


#include "MachDeps.h"

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Double (..), RuntimeRep (..),
                                            isTrue#)

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions


instance Bounded DoubleX4 where
  maxBound = case infty of D# x -> DoubleX4# x x x x
  minBound = case negate infty of D# x -> DoubleX4# x x x x

infty :: Double
infty = read "Infinity"


instance Show DoubleX4 where
  show (DoubleX4# a1 a2 a3 a4) = "{ "     ++ show (D# a1)
                              ++ ", " ++ show (D# a2)
                              ++ ", " ++ show (D# a3)
                              ++ ", " ++ show (D# a4)
                              ++ " }"



instance Eq DoubleX4 where
  DoubleX4# a1 a2 a3 a4 == DoubleX4# b1 b2 b3 b4 = isTrue# (  (a1 ==## b1)
                                              `andI#` (a2 ==## b2)
                                              `andI#` (a3 ==## b3)
                                              `andI#` (a4 ==## b4)
                                              )
  {-# INLINE (==) #-}
  DoubleX4# a1 a2 a3 a4 /= DoubleX4# b1 b2 b3 b4 = isTrue# (  (a1 /=## b1)
                                               `orI#` (a2 /=## b2)
                                               `orI#` (a3 /=## b3)
                                               `orI#` (a4 /=## b4)
                                               )
  {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance Ord DoubleX4 where
  DoubleX4# a1 a2 a3 a4 > DoubleX4# b1 b2 b3 b4 = isTrue# (   (a1 >## b1)
                                              `andI#` (a2 >## b2)
                                              `andI#` (a3 >## b3)
                                              `andI#` (a4 >## b4)
                                              )
  {-# INLINE (>) #-}
  DoubleX4# a1 a2 a3 a4 < DoubleX4# b1 b2 b3 b4 = isTrue# (   (a1 <## b1)
                                              `andI#` (a2 <## b2)
                                              `andI#` (a3 <## b3)
                                              `andI#` (a4 <## b4)
                                              )
  {-# INLINE (<) #-}
  DoubleX4# a1 a2 a3 a4 >= DoubleX4# b1 b2 b3 b4 = isTrue# (  (a1 >=## b1)
                                              `andI#` (a2 >=## b2)
                                              `andI#` (a3 >=## b3)
                                              `andI#` (a4 >=## b4)
                                              )
  {-# INLINE (>=) #-}
  DoubleX4# a1 a2 a3 a4 <= DoubleX4# b1 b2 b3 b4 = isTrue# (  (a1 <=## b1)
                                              `andI#` (a2 <=## b2)
                                              `andI#` (a3 <=## b3)
                                              `andI#` (a4 <=## b4)
                                              )
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare (DoubleX4# a1 a2 a3 a4) (DoubleX4# b1 b2 b3 b4)
    | isTrue# (a1 >## b1) = GT
    | isTrue# (a1 <## b1) = LT
    | isTrue# (a2 >## b2) = GT
    | isTrue# (a2 <## b2) = LT
    | isTrue# (a3 >## b3) = GT
    | isTrue# (a3 <## b3) = LT
    | isTrue# (a4 >## b4) = GT
    | isTrue# (a4 <## b4) = LT
    | otherwise = EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min (DoubleX4# a1 a2 a3 a4) (DoubleX4# b1 b2 b3 b4) =
      DoubleX4# (if isTrue# (a1 >## b1) then b1 else a1)
               (if isTrue# (a2 >## b2) then b2 else a2)
               (if isTrue# (a3 >## b3) then b3 else a3)
               (if isTrue# (a4 >## b4) then b4 else a4)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max (DoubleX4# a1 a2 a3 a4) (DoubleX4# b1 b2 b3 b4) =
      DoubleX4# (if isTrue# (a1 >## b1) then a1 else b1)
               (if isTrue# (a2 >## b2) then a2 else b2)
               (if isTrue# (a3 >## b3) then a3 else b3)
               (if isTrue# (a4 >## b4) then a4 else b4)
  {-# INLINE max #-}



-- | element-wise operations for vectors
instance Num DoubleX4 where
  DoubleX4# a1 a2 a3 a4 + DoubleX4# b1 b2 b3 b4
    = DoubleX4# ((+##) a1 b1) ((+##) a2 b2) ((+##) a3 b3) ((+##) a4 b4)
  {-# INLINE (+) #-}
  DoubleX4# a1 a2 a3 a4 - DoubleX4# b1 b2 b3 b4
    = DoubleX4# ((-##) a1 b1) ((-##) a2 b2) ((-##) a3 b3) ((-##) a4 b4)
  {-# INLINE (-) #-}
  DoubleX4# a1 a2 a3 a4 * DoubleX4# b1 b2 b3 b4
    = DoubleX4# ((*##) a1 b1) ((*##) a2 b2) ((*##) a3 b3) ((*##) a4 b4)
  {-# INLINE (*) #-}
  negate (DoubleX4# a1 a2 a3 a4)
    = DoubleX4# (negateDouble# a1) (negateDouble# a2) (negateDouble# a3) (negateDouble# a4)
  {-# INLINE negate #-}
  abs (DoubleX4# a1 a2 a3 a4)
    = DoubleX4# (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1)
               (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
               (if isTrue# (a3 >=## 0.0##) then a3 else negateDouble# a3)
               (if isTrue# (a4 >=## 0.0##) then a4 else negateDouble# a4)
  {-# INLINE abs #-}
  signum (DoubleX4# a1 a2 a3 a4)
    = DoubleX4# (if isTrue# (a1 >## 0.0##)
                then 1.0##
                else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
               (if isTrue# (a2 >## 0.0##)
                then 1.0##
                else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## )
               (if isTrue# (a3 >## 0.0##)
                then 1.0##
                else if isTrue# (a3 <## 0.0##) then -1.0## else 0.0## )
               (if isTrue# (a4 >## 0.0##)
                then 1.0##
                else if isTrue# (a4 <## 0.0##) then -1.0## else 0.0## )
  {-# INLINE signum #-}
  fromInteger n = case fromInteger n of D# x -> DoubleX4# x x x x
  {-# INLINE fromInteger #-}



instance Fractional DoubleX4 where
  DoubleX4# a1 a2 a3 a4 / DoubleX4# b1 b2 b3 b4  = DoubleX4# ((/##) a1 b1)
                                                    ((/##) a2 b2)
                                                    ((/##) a3 b3)
                                                    ((/##) a4 b4)
  {-# INLINE (/) #-}
  recip (DoubleX4# a1 a2 a3 a4) = DoubleX4# ((/##) 1.0## a1)
                                       ((/##) 1.0## a2)
                                       ((/##) 1.0## a3)
                                       ((/##) 1.0## a4)
  {-# INLINE recip #-}
  fromRational r = case fromRational r of D# x -> DoubleX4# x x x x
  {-# INLINE fromRational #-}



instance Floating DoubleX4 where
  pi = DoubleX4# 3.141592653589793238## 3.141592653589793238## 3.141592653589793238## 3.141592653589793238##
  {-# INLINE pi #-}
  exp (DoubleX4# a1 a2 a3 a4) = DoubleX4# (expDouble# a1)
                                     (expDouble# a2)
                                     (expDouble# a3)
                                     (expDouble# a4)
  {-# INLINE exp #-}
  log (DoubleX4# a1 a2 a3 a4) = DoubleX4# (logDouble# a1)
                                     (logDouble# a2)
                                     (logDouble# a3)
                                     (logDouble# a4)
  {-# INLINE log #-}
  sqrt (DoubleX4# a1 a2 a3 a4) = DoubleX4# (sqrtDouble# a1)
                                      (sqrtDouble# a2)
                                      (sqrtDouble# a3)
                                      (sqrtDouble# a4)
  {-# INLINE sqrt #-}
  sin (DoubleX4# a1 a2 a3 a4) = DoubleX4# (sinDouble# a1)
                                     (sinDouble# a2)
                                     (sinDouble# a3)
                                     (sinDouble# a4)
  {-# INLINE sin #-}
  cos (DoubleX4# a1 a2 a3 a4) = DoubleX4# (cosDouble# a1)
                                     (cosDouble# a2)
                                     (cosDouble# a3)
                                     (cosDouble# a4)
  {-# INLINE cos #-}
  tan (DoubleX4# a1 a2 a3 a4) = DoubleX4# (tanDouble# a1)
                                     (tanDouble# a2)
                                     (tanDouble# a3)
                                     (tanDouble# a4)
  {-# INLINE tan #-}
  asin (DoubleX4# a1 a2 a3 a4) = DoubleX4# (asinDouble# a1)
                                      (asinDouble# a2)
                                      (asinDouble# a3)
                                      (asinDouble# a4)
  {-# INLINE asin #-}
  acos (DoubleX4# a1 a2 a3 a4) = DoubleX4# (acosDouble# a1)
                                      (acosDouble# a2)
                                      (acosDouble# a3)
                                      (acosDouble# a4)
  {-# INLINE acos #-}
  atan (DoubleX4# a1 a2 a3 a4) = DoubleX4# (atanDouble# a1)
                                      (atanDouble# a2)
                                      (atanDouble# a3)
                                      (atanDouble# a4)
  {-# INLINE atan #-}
  sinh (DoubleX4# a1 a2 a3 a4) = DoubleX4# (sinDouble# a1)
                                      (sinDouble# a2)
                                      (sinDouble# a3)
                                      (sinDouble# a4)
  {-# INLINE sinh #-}
  cosh (DoubleX4# a1 a2 a3 a4) = DoubleX4# (coshDouble# a1)
                                      (coshDouble# a2)
                                      (coshDouble# a3)
                                      (coshDouble# a4)
  {-# INLINE cosh #-}
  tanh (DoubleX4# a1 a2 a3 a4) = DoubleX4# (tanhDouble# a1)
                                      (tanhDouble# a2)
                                      (tanhDouble# a3)
                                      (tanhDouble# a4)
  {-# INLINE tanh #-}
  DoubleX4# a1 a2 a3 a4 ** DoubleX4# b1 b2 b3 b4 = DoubleX4# ((**##) a1 b1)
                                                    ((**##) a2 b2)
                                                    ((**##) a3 b3)
                                                    ((**##) a4 b4)
  {-# INLINE (**) #-}

  logBase x y         =  log y / log x
  {-# INLINE logBase #-}
  asinh x = log (x + sqrt (1.0+x*x))
  {-# INLINE asinh #-}
  acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
  {-# INLINE acosh #-}
  atanh x = 0.5 * log ((1.0+x) / (1.0-x))
  {-# INLINE atanh #-}



type instance ElemRep DoubleX4 = 'DoubleRep
type instance ElemPrim DoubleX4 = Double#
instance PrimBytes DoubleX4 where
  toBytes (DoubleX4# a1 a2 a3 a4) = case runRW#
     ( \s0 -> case newByteArray# (SIZEOF_HSFLOAT# *# 3#) s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
           s2 -> case writeDoubleArray# marr 1# a2 s2 of
             s3 -> case writeDoubleArray# marr 2# a3 s3 of
               s4 -> case writeDoubleArray# marr 3# a4 s4 of
                 s5 -> unsafeFreezeByteArray# marr s5
     ) of (# _, a #) -> (# 0#, 4#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = DoubleX4#
    (indexDoubleArray# arr off)
    (indexDoubleArray# arr (off +# 1#))
    (indexDoubleArray# arr (off +# 2#))
    (indexDoubleArray# arr (off +# 3#))
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSFLOAT# *# 4#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE elementByteSize #-}
  ix 0# (DoubleX4# a1 _ _ _) = a1
  ix 1# (DoubleX4# _ a2 _ _) = a2
  ix 2# (DoubleX4# _ _ a3 _) = a3
  ix 3# (DoubleX4# _ _ _ a4) = a4
  ix _ _                    = undefined
  {-# INLINE ix #-}


instance ElementWise (Idx '[4]) Double DoubleX4 where
  indexOffset# (DoubleX4# a1 _ _ _) 0# = D# a1
  indexOffset# (DoubleX4# _ a2 _ _) 1# = D# a2
  indexOffset# (DoubleX4# _ _ a3 _) 2# = D# a3
  indexOffset# (DoubleX4# _ _ _ a4) 3# = D# a4
  indexOffset# _                   _  = undefined
  {-# INLINE indexOffset# #-}

  (!) (DoubleX4# a1 _ _ _) ( 1 :! Z) = D# a1
  (!) (DoubleX4# _ a2 _ _) ( 2 :! Z) = D# a2
  (!) (DoubleX4# _ _ a3 _) ( 3 :! Z) = D# a3
  (!) (DoubleX4# _ _ _ a4) ( 4 :! Z) = D# a4
  (!) _                   ( _ :! Z) = undefined
  {-# INLINE (!) #-}

  broadcast (D# x) = DoubleX4# x x x x
  {-# INLINE broadcast #-}

  ewmap f (DoubleX4# x y z w) = case (f (1:!Z) (D# x), f (2:!Z) (D# y), f (3:!Z) (D# z), f (3:!Z) (D# w)) of
                              (D# r1, D# r2, D# r3, D# r4) -> DoubleX4# r1 r2 r3 r4
  {-# INLINE ewmap #-}

  ewgen f = case (f (1:!Z), f (2:!Z), f (3:!Z), f (4:!Z)) of (D# r1, D# r2, D# r3, D# r4) -> DoubleX4# r1 r2 r3 r4
  {-# INLINE ewgen #-}

  ewgenA f = (\(D# a) (D# b) (D# c) (D# d) -> DoubleX4# a b c d)
          <$> f (1:!Z) <*> f (2:!Z) <*> f (3:!Z) <*> f (4:!Z)
  {-# INLINE ewgenA #-}

  ewfoldl f x0 (DoubleX4# x y z w) = f (4:!Z) (f (3:!Z) (f (2:!Z) (f (1:!Z) x0 (D# x)) (D# y)) (D# z)) (D# w)
  {-# INLINE ewfoldl #-}

  ewfoldr f x0 (DoubleX4# x y z w) = f (1:!Z) (D# x) (f (2:!Z) (D# y) (f (3:!Z) (D# z) (f (4:!Z) (D# w) x0)))
  {-# INLINE ewfoldr #-}

  elementWise f (DoubleX4# x y z w) = (\(D# a) (D# b) (D# c) (D# d) -> DoubleX4# a b c d)
                                 <$> f (D# x) <*> f (D# y) <*> f (D# z) <*> f (D# w)
  {-# INLINE elementWise #-}

  indexWise f (DoubleX4# x y z w) = (\(D# a) (D# b) (D# c) (D# d) -> DoubleX4# a b c d)
                             <$> f (1:!Z) (D# x) <*> f (2:!Z) (D# y) <*> f (3:!Z) (D# z) <*> f (4:!Z) (D# w)
  {-# INLINE indexWise #-}

  update (1 :! Z) (D# q) (DoubleX4# _ y z w) = DoubleX4# q y z w
  update (2 :! Z) (D# q) (DoubleX4# x _ z w) = DoubleX4# x q z w
  update (3 :! Z) (D# q) (DoubleX4# x y _ w) = DoubleX4# x y q w
  update (4 :! Z) (D# q) (DoubleX4# x y z _) = DoubleX4# x y z q
  update (_ :! Z) _ x = x
  {-# INLINE update #-}
