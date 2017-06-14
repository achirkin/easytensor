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
-- Module      :  Numeric.Array.Family.FloatX4
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.FloatX4 () where


#include "MachDeps.h"

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Float (..), RuntimeRep (..),
                                            isTrue#)

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions





instance Show FloatX4 where
  show (FloatX4# a1 a2 a3 a4) = "{ "     ++ show (F# a1)
                              ++ ", " ++ show (F# a2)
                              ++ ", " ++ show (F# a3)
                              ++ ", " ++ show (F# a4)
                              ++ " }"



instance Eq FloatX4 where
  FloatX4# a1 a2 a3 a4 == FloatX4# b1 b2 b3 b4 = isTrue# (  (a1 `eqFloat#` b1)
                                              `andI#` (a2 `eqFloat#` b2)
                                              `andI#` (a3 `eqFloat#` b3)
                                              `andI#` (a4 `eqFloat#` b4)
                                              )
  {-# INLINE (==) #-}
  FloatX4# a1 a2 a3 a4 /= FloatX4# b1 b2 b3 b4 = isTrue# (  (a1 `neFloat#` b1)
                                               `orI#` (a2 `neFloat#` b2)
                                               `orI#` (a3 `neFloat#` b3)
                                               `orI#` (a4 `neFloat#` b4)
                                               )
  {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance Ord FloatX4 where
  FloatX4# a1 a2 a3 a4 > FloatX4# b1 b2 b3 b4 = isTrue# (   (a1 `gtFloat#` b1)
                                              `andI#` (a2 `gtFloat#` b2)
                                              `andI#` (a3 `gtFloat#` b3)
                                              `andI#` (a4 `gtFloat#` b4)
                                              )
  {-# INLINE (>) #-}
  FloatX4# a1 a2 a3 a4 < FloatX4# b1 b2 b3 b4 = isTrue# (   (a1 `ltFloat#` b1)
                                              `andI#` (a2 `ltFloat#` b2)
                                              `andI#` (a3 `ltFloat#` b3)
                                              `andI#` (a4 `ltFloat#` b4)
                                              )
  {-# INLINE (<) #-}
  FloatX4# a1 a2 a3 a4 >= FloatX4# b1 b2 b3 b4 = isTrue# (  (a1 `geFloat#` b1)
                                              `andI#` (a2 `geFloat#` b2)
                                              `andI#` (a3 `geFloat#` b3)
                                              `andI#` (a4 `geFloat#` b4)
                                              )
  {-# INLINE (>=) #-}
  FloatX4# a1 a2 a3 a4 <= FloatX4# b1 b2 b3 b4 = isTrue# (  (a1 `leFloat#` b1)
                                              `andI#` (a2 `leFloat#` b2)
                                              `andI#` (a3 `leFloat#` b3)
                                              `andI#` (a4 `leFloat#` b4)
                                              )
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare (FloatX4# a1 a2 a3 a4) (FloatX4# b1 b2 b3 b4)
    | isTrue# (a1 `gtFloat#` b1) = GT
    | isTrue# (a1 `ltFloat#` b1) = LT
    | isTrue# (a2 `gtFloat#` b2) = GT
    | isTrue# (a2 `ltFloat#` b2) = LT
    | isTrue# (a3 `gtFloat#` b3) = GT
    | isTrue# (a3 `ltFloat#` b3) = LT
    | isTrue# (a4 `gtFloat#` b4) = GT
    | isTrue# (a4 `ltFloat#` b4) = LT
    | otherwise = EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min (FloatX4# a1 a2 a3 a4) (FloatX4# b1 b2 b3 b4) =
      FloatX4# (if isTrue# (a1 `gtFloat#` b1) then b1 else a1)
               (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
               (if isTrue# (a3 `gtFloat#` b3) then b3 else a3)
               (if isTrue# (a4 `gtFloat#` b4) then b4 else a4)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max (FloatX4# a1 a2 a3 a4) (FloatX4# b1 b2 b3 b4) =
      FloatX4# (if isTrue# (a1 `gtFloat#` b1) then a1 else b1)
               (if isTrue# (a2 `gtFloat#` b2) then a2 else b2)
               (if isTrue# (a3 `gtFloat#` b3) then a3 else b3)
               (if isTrue# (a4 `gtFloat#` b4) then a4 else b4)
  {-# INLINE max #-}



-- | element-wise operations for vectors
instance Num FloatX4 where
  FloatX4# a1 a2 a3 a4 + FloatX4# b1 b2 b3 b4
    = FloatX4# (plusFloat# a1 b1) (plusFloat# a2 b2) (plusFloat# a3 b3) (plusFloat# a4 b4)
  {-# INLINE (+) #-}
  FloatX4# a1 a2 a3 a4 - FloatX4# b1 b2 b3 b4
    = FloatX4# (minusFloat# a1 b1) (minusFloat# a2 b2) (minusFloat# a3 b3) (minusFloat# a4 b4)
  {-# INLINE (-) #-}
  FloatX4# a1 a2 a3 a4 * FloatX4# b1 b2 b3 b4
    = FloatX4# (timesFloat# a1 b1) (timesFloat# a2 b2) (timesFloat# a3 b3) (timesFloat# a4 b4)
  {-# INLINE (*) #-}
  negate (FloatX4# a1 a2 a3 a4)
    = FloatX4# (negateFloat# a1) (negateFloat# a2) (negateFloat# a3) (negateFloat# a4)
  {-# INLINE negate #-}
  abs (FloatX4# a1 a2 a3 a4)
    = FloatX4# (if isTrue# (a1 `geFloat#` 0.0#) then a1 else negateFloat# a1)
               (if isTrue# (a2 `geFloat#` 0.0#) then a2 else negateFloat# a2)
               (if isTrue# (a3 `geFloat#` 0.0#) then a3 else negateFloat# a3)
               (if isTrue# (a4 `geFloat#` 0.0#) then a4 else negateFloat# a4)
  {-# INLINE abs #-}
  signum (FloatX4# a1 a2 a3 a4)
    = FloatX4# (if isTrue# (a1 `gtFloat#` 0.0#)
                then 1.0#
                else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
               (if isTrue# (a2 `gtFloat#` 0.0#)
                then 1.0#
                else if isTrue# (a2 `ltFloat#` 0.0#) then -1.0# else 0.0# )
               (if isTrue# (a3 `gtFloat#` 0.0#)
                then 1.0#
                else if isTrue# (a3 `ltFloat#` 0.0#) then -1.0# else 0.0# )
               (if isTrue# (a4 `gtFloat#` 0.0#)
                then 1.0#
                else if isTrue# (a4 `ltFloat#` 0.0#) then -1.0# else 0.0# )
  {-# INLINE signum #-}
  fromInteger n = case fromInteger n of F# x -> FloatX4# x x x x
  {-# INLINE fromInteger #-}



instance Fractional FloatX4 where
  FloatX4# a1 a2 a3 a4 / FloatX4# b1 b2 b3 b4  = FloatX4# (divideFloat# a1 b1)
                                                    (divideFloat# a2 b2)
                                                    (divideFloat# a3 b3)
                                                    (divideFloat# a4 b4)
  {-# INLINE (/) #-}
  recip (FloatX4# a1 a2 a3 a4) = FloatX4# (divideFloat# 1.0# a1)
                                       (divideFloat# 1.0# a2)
                                       (divideFloat# 1.0# a3)
                                       (divideFloat# 1.0# a4)
  {-# INLINE recip #-}
  fromRational r = case fromRational r of F# x -> FloatX4# x x x x
  {-# INLINE fromRational #-}



instance Floating FloatX4 where
  pi = FloatX4# 3.141592653589793238# 3.141592653589793238# 3.141592653589793238# 3.141592653589793238#
  {-# INLINE pi #-}
  exp (FloatX4# a1 a2 a3 a4) = FloatX4# (expFloat# a1)
                                     (expFloat# a2)
                                     (expFloat# a3)
                                     (expFloat# a4)
  {-# INLINE exp #-}
  log (FloatX4# a1 a2 a3 a4) = FloatX4# (logFloat# a1)
                                     (logFloat# a2)
                                     (logFloat# a3)
                                     (logFloat# a4)
  {-# INLINE log #-}
  sqrt (FloatX4# a1 a2 a3 a4) = FloatX4# (sqrtFloat# a1)
                                      (sqrtFloat# a2)
                                      (sqrtFloat# a3)
                                      (sqrtFloat# a4)
  {-# INLINE sqrt #-}
  sin (FloatX4# a1 a2 a3 a4) = FloatX4# (sinFloat# a1)
                                     (sinFloat# a2)
                                     (sinFloat# a3)
                                     (sinFloat# a4)
  {-# INLINE sin #-}
  cos (FloatX4# a1 a2 a3 a4) = FloatX4# (cosFloat# a1)
                                     (cosFloat# a2)
                                     (cosFloat# a3)
                                     (cosFloat# a4)
  {-# INLINE cos #-}
  tan (FloatX4# a1 a2 a3 a4) = FloatX4# (tanFloat# a1)
                                     (tanFloat# a2)
                                     (tanFloat# a3)
                                     (tanFloat# a4)
  {-# INLINE tan #-}
  asin (FloatX4# a1 a2 a3 a4) = FloatX4# (asinFloat# a1)
                                      (asinFloat# a2)
                                      (asinFloat# a3)
                                      (asinFloat# a4)
  {-# INLINE asin #-}
  acos (FloatX4# a1 a2 a3 a4) = FloatX4# (acosFloat# a1)
                                      (acosFloat# a2)
                                      (acosFloat# a3)
                                      (acosFloat# a4)
  {-# INLINE acos #-}
  atan (FloatX4# a1 a2 a3 a4) = FloatX4# (atanFloat# a1)
                                      (atanFloat# a2)
                                      (atanFloat# a3)
                                      (atanFloat# a4)
  {-# INLINE atan #-}
  sinh (FloatX4# a1 a2 a3 a4) = FloatX4# (sinFloat# a1)
                                      (sinFloat# a2)
                                      (sinFloat# a3)
                                      (sinFloat# a4)
  {-# INLINE sinh #-}
  cosh (FloatX4# a1 a2 a3 a4) = FloatX4# (coshFloat# a1)
                                      (coshFloat# a2)
                                      (coshFloat# a3)
                                      (coshFloat# a4)
  {-# INLINE cosh #-}
  tanh (FloatX4# a1 a2 a3 a4) = FloatX4# (tanhFloat# a1)
                                      (tanhFloat# a2)
                                      (tanhFloat# a3)
                                      (tanhFloat# a4)
  {-# INLINE tanh #-}
  FloatX4# a1 a2 a3 a4 ** FloatX4# b1 b2 b3 b4 = FloatX4# (powerFloat# a1 b1)
                                                    (powerFloat# a2 b2)
                                                    (powerFloat# a3 b3)
                                                    (powerFloat# a4 b4)
  {-# INLINE (**) #-}

  logBase x y         =  log y / log x
  {-# INLINE logBase #-}
  asinh x = log (x + sqrt (1.0+x*x))
  {-# INLINE asinh #-}
  acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
  {-# INLINE acosh #-}
  atanh x = 0.5 * log ((1.0+x) / (1.0-x))
  {-# INLINE atanh #-}



type instance ElemRep FloatX4 = 'FloatRep
instance PrimBytes FloatX4 where
  type ElemPrim FloatX4 = Float#
  toBytes (FloatX4# a1 a2 a3 a4) = case runRW#
     ( \s0 -> case newByteArray# (SIZEOF_HSFLOAT# *# 3#) s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# a1 s1 of
           s2 -> case writeFloatArray# marr 1# a2 s2 of
             s3 -> case writeFloatArray# marr 2# a3 s3 of
               s4 -> case writeFloatArray# marr 3# a4 s4 of
                 s5 -> unsafeFreezeByteArray# marr s5
     ) of (# _, a #) -> (# 0#, 4#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = FloatX4#
    (indexFloatArray# arr off)
    (indexFloatArray# arr (off +# 1#))
    (indexFloatArray# arr (off +# 2#))
    (indexFloatArray# arr (off +# 3#))
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSFLOAT# *# 4#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE elementByteSize #-}
  ix 0# (FloatX4# a1 _ _ _) = a1
  ix 1# (FloatX4# _ a2 _ _) = a2
  ix 2# (FloatX4# _ _ a3 _) = a3
  ix 3# (FloatX4# _ _ _ a4) = a4
  ix _ _                    = undefined
  {-# INLINE ix #-}


instance ElementWise (Idx '[4]) Float FloatX4 where

  (!) (FloatX4# a1 _ _ _) ( 1 :! Z) = F# a1
  (!) (FloatX4# _ a2 _ _) ( 2 :! Z) = F# a2
  (!) (FloatX4# _ _ a3 _) ( 3 :! Z) = F# a3
  (!) (FloatX4# _ _ _ a4) ( 4 :! Z) = F# a4
  (!) _                   ( _ :! Z) = undefined
  {-# INLINE (!) #-}

  broadcast (F# x) = FloatX4# x x x x
  {-# INLINE broadcast #-}

  ewmap f (FloatX4# x y z w) = case (f (1:!Z) (F# x), f (2:!Z) (F# y), f (3:!Z) (F# z), f (3:!Z) (F# w)) of
                              (F# r1, F# r2, F# r3, F# r4) -> FloatX4# r1 r2 r3 r4
  {-# INLINE ewmap #-}

  ewgen f = case (f (1:!Z), f (2:!Z), f (3:!Z), f (4:!Z)) of (F# r1, F# r2, F# r3, F# r4) -> FloatX4# r1 r2 r3 r4
  {-# INLINE ewgen #-}

  ewgenA f = (\(F# a) (F# b) (F# c) (F# d) -> FloatX4# a b c d)
          <$> f (1:!Z) <*> f (2:!Z) <*> f (3:!Z) <*> f (4:!Z)
  {-# INLINE ewgenA #-}

  ewfold f x0 (FloatX4# x y z w) = f (4:!Z) (F# w) (f (3:!Z) (F# z) (f (2:!Z) (F# y) (f (1:!Z) (F# x) x0)))
  {-# INLINE ewfold #-}

  elementWise f (FloatX4# x y z w) = (\(F# a) (F# b) (F# c) (F# d) -> FloatX4# a b c d)
                                 <$> f (F# x) <*> f (F# y) <*> f (F# z) <*> f (F# w)
  {-# INLINE elementWise #-}

  indexWise f (FloatX4# x y z w) = (\(F# a) (F# b) (F# c) (F# d) -> FloatX4# a b c d)
                             <$> f (1:!Z) (F# x) <*> f (2:!Z) (F# y) <*> f (3:!Z) (F# z) <*> f (4:!Z) (F# w)
  {-# INLINE indexWise #-}

  update (1 :! Z) (F# q) (FloatX4# _ y z w) = FloatX4# q y z w
  update (2 :! Z) (F# q) (FloatX4# x _ z w) = FloatX4# x q z w
  update (3 :! Z) (F# q) (FloatX4# x y _ w) = FloatX4# x y q w
  update (4 :! Z) (F# q) (FloatX4# x y z _) = FloatX4# x y z q
  update (_ :! Z) _ x = x
  {-# INLINE update #-}
