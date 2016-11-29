{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vector.Base.FloatX2
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Vector.Base.FloatX2 () where

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types

import Numeric.Commons
import Numeric.Vector.Class
import Numeric.Vector.Family




instance Show VFloatX2 where
  show (VFloatX2 a1 a2) = "{ "     ++ show (F# a1)
                          ++ ", " ++ show (F# a2)
                          ++ " }"



instance Eq VFloatX2 where
  VFloatX2 a1 a2 == VFloatX2 b1 b2 = isTrue# (  (a1 `eqFloat#` b1)
                                        `andI#` (a2 `eqFloat#` b2)
                                         )
  {-# INLINE (==) #-}
  VFloatX2 a1 a2 /= VFloatX2 b1 b2 = isTrue# (  (a1 `neFloat#` b1)
                                         `orI#` (a2 `neFloat#` b2)
                                         )
  {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=` and lexicographical ordering for `compare`
instance Ord VFloatX2 where
  VFloatX2 a1 a2 > VFloatX2 b1 b2 = isTrue# (   (a1 `gtFloat#` b1)
                                        `andI#` (a2 `gtFloat#` b2)
                                         )
  {-# INLINE (>) #-}
  VFloatX2 a1 a2 < VFloatX2 b1 b2 = isTrue# (   (a1 `ltFloat#` b1)
                                        `andI#` (a2 `ltFloat#` b2)
                                         )
  {-# INLINE (<) #-}
  VFloatX2 a1 a2 >= VFloatX2 b1 b2 = isTrue# (  (a1 `geFloat#` b1)
                                        `andI#` (a2 `geFloat#` b2)
                                         )
  {-# INLINE (>=) #-}
  VFloatX2 a1 a2 <= VFloatX2 b1 b2 = isTrue# (  (a1 `leFloat#` b1)
                                        `andI#` (a2 `leFloat#` b2)
                                         )
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare (VFloatX2 a1 a2) (VFloatX2 b1 b2)
    | isTrue# (a1 `gtFloat#` b1) = GT
    | isTrue# (a1 `ltFloat#` b1) = LT
    | isTrue# (a2 `gtFloat#` b2) = GT
    | isTrue# (a2 `ltFloat#` b2) = LT
    | otherwise = EQ
  {-# INLINE compare #-}



-- | element-wise operations for vectors
instance Num VFloatX2 where
  VFloatX2 a1 a2 + VFloatX2 b1 b2 = VFloatX2 (plusFloat# a1 b1) (plusFloat# a2 b2)
  {-# INLINE (+) #-}
  VFloatX2 a1 a2 - VFloatX2 b1 b2 = VFloatX2 (minusFloat# a1 b1) (minusFloat# a2 b2)
  {-# INLINE (-) #-}
  VFloatX2 a1 a2 * VFloatX2 b1 b2 = VFloatX2 (timesFloat# a1 b1) (timesFloat# a2 b2)
  {-# INLINE (*) #-}
  negate (VFloatX2 a1 a2) = VFloatX2 (negateFloat# a1) (negateFloat# a2)
  {-# INLINE negate #-}
  abs (VFloatX2 a1 a2) = VFloatX2 (if isTrue# (a1 `geFloat#` 0.0#) then a1 else negateFloat# a1)
                                  (if isTrue# (a2 `geFloat#` 0.0#) then a2 else negateFloat# a2)
  {-# INLINE abs #-}
  signum (VFloatX2 a1 a2) = VFloatX2 (if isTrue# (a1 `gtFloat#` 0.0#) then 1.0# else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
                                     (if isTrue# (a2 `gtFloat#` 0.0#) then 1.0# else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
  {-# INLINE signum #-}
  fromInteger n = case fromInteger n of F# x -> VFloatX2 x x
  {-# INLINE fromInteger #-}



instance Fractional VFloatX2 where
  VFloatX2 a1 a2 / VFloatX2 b1 b2 = VFloatX2 (divideFloat# a1 b1)
                                             (divideFloat# a2 b2)
  {-# INLINE (/) #-}
  recip (VFloatX2 a1 a2) = VFloatX2 (divideFloat# 1.0# a1)
                                    (divideFloat# 1.0# a2)
  {-# INLINE recip #-}
  fromRational r = case fromRational r of F# x -> VFloatX2 x x
  {-# INLINE fromRational #-}



instance Floating VFloatX2 where
  pi = VFloatX2 3.141592653589793238# 3.141592653589793238#
  {-# INLINE pi #-}
  exp (VFloatX2 a1 a2) = VFloatX2 (expFloat# a1)
                                  (expFloat# a2)
  {-# INLINE exp #-}
  log (VFloatX2 a1 a2) = VFloatX2 (logFloat# a1)
                                  (logFloat# a2)
  {-# INLINE log #-}
  sqrt (VFloatX2 a1 a2) = VFloatX2 (sqrtFloat# a1)
                                   (sqrtFloat# a2)
  {-# INLINE sqrt #-}
  sin (VFloatX2 a1 a2) = VFloatX2 (sinFloat# a1)
                                  (sinFloat# a2)
  {-# INLINE sin #-}
  cos (VFloatX2 a1 a2) = VFloatX2 (cosFloat# a1)
                                  (cosFloat# a2)
  {-# INLINE cos #-}
  tan (VFloatX2 a1 a2) = VFloatX2 (tanFloat# a1)
                                  (tanFloat# a2)
  {-# INLINE tan #-}
  asin (VFloatX2 a1 a2) = VFloatX2 (asinFloat# a1)
                                   (asinFloat# a2)
  {-# INLINE asin #-}
  acos (VFloatX2 a1 a2) = VFloatX2 (acosFloat# a1)
                                   (acosFloat# a2)
  {-# INLINE acos #-}
  atan (VFloatX2 a1 a2) = VFloatX2 (atanFloat# a1)
                                   (atanFloat# a2)
  {-# INLINE atan #-}
  sinh (VFloatX2 a1 a2) = VFloatX2 (sinFloat# a1)
                                   (sinFloat# a2)
  {-# INLINE sinh #-}
  cosh (VFloatX2 a1 a2) = VFloatX2 (coshFloat# a1)
                                   (coshFloat# a2)
  {-# INLINE cosh #-}
  tanh (VFloatX2 a1 a2) = VFloatX2 (tanhFloat# a1)
                                   (tanhFloat# a2)
  {-# INLINE tanh #-}
  VFloatX2 a1 a2 ** VFloatX2 b1 b2 = VFloatX2 (powerFloat# a1 b1)
                                              (powerFloat# a2 b2)
  {-# INLINE (**) #-}

  logBase x y         =  log y / log x
  {-# INLINE logBase #-}
  asinh x = log (x + sqrt (1.0+x*x))
  {-# INLINE asinh #-}
  acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
  {-# INLINE acosh #-}
  atanh x = 0.5 * log ((1.0+x) / (1.0-x))
  {-# INLINE atanh #-}



--  log1p (VFloatX2 a1 a2) = case ( log1p (F# a1), log1p (F# a2) ) of
--    (F# x1, F# x2) -> VFloatX2 x1 x2
--  expm1 (VFloatX2 a1 a2) = case ( expm1 (F# a1), expm1 (F# a2) ) of
--    (F# x1, F# x2) -> VFloatX2 x1 x2
--
--  log1mexp a
--      | a <= log 2 = log (negate (expm1Float a))
--      | otherwise  = log1p (negate (exp a))
--  {-# INLINE log1mexp #-}
--  log1pexp a
--      | a <= 18   = log1p (exp a)
--      | a <= 100  = a + exp (negate a)
--      | otherwise = a
--  {-# INLINE log1pexp #-}



instance VectorCalculus VFloatX2 Float 2 where
  broadcastVec (F# x) = VFloatX2 x x
  {-# INLINE broadcastVec #-}
  VFloatX2 a1 a2 .*. VFloatX2 b1 b2 = case timesFloat# a1 b1
                              `plusFloat#` timesFloat# a2 b2 of
    x -> VFloatX2 x x
  {-# INLINE (.*.) #-}
  VFloatX2 a1 a2 `dot` VFloatX2 b1 b2 = F# ( timesFloat# a1 b1
                                `plusFloat#` timesFloat# a2 b2
                                )
  {-# INLINE dot #-}
  indexVec 1 (VFloatX2 a1 _) = F# a1
  indexVec 2 (VFloatX2 _ a2) = F# a2
  indexVec i _ = error $ "Bad index " ++ show i ++ " for 2D vector"
  {-# INLINE indexVec #-}
  normL1 v = case abs v of
      VFloatX2 a1 a2 -> F# (a1 `plusFloat#` a2)
  {-# INLINE normL1 #-}
  normL2 v = sqrt $ dot v v
  {-# INLINE normL2 #-}
  normLPInf (VFloatX2 a1 a2) = F# (if isTrue# (a1 `gtFloat#` a2) then a1 else a2)
  {-# INLINE normLPInf #-}
  normLNInf (VFloatX2 a1 a2) = F# (if isTrue# (a1 `gtFloat#` a2) then a2 else a1)
  {-# INLINE normLNInf #-}
  normLP n (VFloatX2 a1 a2) = case realToFrac n of
    F# x -> F# ( powerFloat# (divideFloat# 1.0# x)
                 (            powerFloat# a1 x
                 `plusFloat#` powerFloat# a2 x
                 )
               )
  {-# INLINE normLP #-}
  dim _ = 2
  {-# INLINE dim #-}


instance Vector2D Float where
  vec2 (F# x) (F# y) = VFloatX2 x y
  {-# INLINE vec2 #-}
  det2 (VFloatX2 a1 a2)  (VFloatX2 b1 b2) = F# (timesFloat# a1 b2 `minusFloat#` timesFloat# a2 b1)
  {-# INLINE det2 #-}


instance PrimBytes VFloatX2 where
  toBytes (VFloatX2 a1 a2) = case runRW#
     ( \s0 -> case newByteArray# 8# s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# a1 s1 of
           s2 -> case writeFloatArray# marr 1# a2 s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, a #) -> a
  fromBytes arr = VFloatX2
    (indexFloatArray# arr 0#)
    (indexFloatArray# arr 1#)
  byteSize _ = 8#
  {-# INLINE byteSize #-}
  byteAlign _ = 8#
  {-# INLINE byteAlign #-}









