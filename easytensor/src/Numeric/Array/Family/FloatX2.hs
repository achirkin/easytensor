{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family.FloatX2
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.FloatX2 () where


#include "MachDeps.h"

import           GHC.Base             (runRW#)
import           GHC.Prim
-- import           GHC.TypeLits
import           GHC.Types
-- import           Data.Proxy

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions





instance Show FloatX2 where
  show (FloatX2# a1 a2) = "{ "     ++ show (F# a1)
                            ++ ", " ++ show (F# a2)
                            ++ " }"



instance Eq FloatX2 where
  FloatX2# a1 a2 == FloatX2# b1 b2 = isTrue# (  (a1 `eqFloat#` b1)
                                          `andI#` (a2 `eqFloat#` b2)
                                           )
  {-# INLINE (==) #-}
  FloatX2# a1 a2 /= FloatX2# b1 b2 = isTrue# (  (a1 `neFloat#` b1)
                                           `orI#` (a2 `neFloat#` b2)
                                           )
  {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance Ord FloatX2 where
  FloatX2# a1 a2 > FloatX2# b1 b2 = isTrue# (   (a1 `gtFloat#` b1)
                                          `andI#` (a2 `gtFloat#` b2)
                                           )
  {-# INLINE (>) #-}
  FloatX2# a1 a2 < FloatX2# b1 b2 = isTrue# (   (a1 `ltFloat#` b1)
                                          `andI#` (a2 `ltFloat#` b2)
                                           )
  {-# INLINE (<) #-}
  FloatX2# a1 a2 >= FloatX2# b1 b2 = isTrue# (  (a1 `geFloat#` b1)
                                          `andI#` (a2 `geFloat#` b2)
                                           )
  {-# INLINE (>=) #-}
  FloatX2# a1 a2 <= FloatX2# b1 b2 = isTrue# (  (a1 `leFloat#` b1)
                                          `andI#` (a2 `leFloat#` b2)
                                           )
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare (FloatX2# a1 a2) (FloatX2# b1 b2)
    | isTrue# (a1 `gtFloat#` b1) = GT
    | isTrue# (a1 `ltFloat#` b1) = LT
    | isTrue# (a2 `gtFloat#` b2) = GT
    | isTrue# (a2 `ltFloat#` b2) = LT
    | otherwise = EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min (FloatX2# a1 a2) (FloatX2# b1 b2) =
      FloatX2# (if isTrue# (a1 `gtFloat#` b1) then b1 else a1)
                (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max (FloatX2# a1 a2) (FloatX2# b1 b2) =
      FloatX2# (if isTrue# (a1 `gtFloat#` b1) then a1 else b1)
                (if isTrue# (a2 `gtFloat#` b2) then a2 else b2)
  {-# INLINE max #-}



-- | element-wise operations for vectors
instance Num FloatX2 where
  FloatX2# a1 a2 + FloatX2# b1 b2
    = FloatX2# (plusFloat# a1 b1) (plusFloat# a2 b2)
  {-# INLINE (+) #-}
  FloatX2# a1 a2 - FloatX2# b1 b2
    = FloatX2# (minusFloat# a1 b1) (minusFloat# a2 b2)
  {-# INLINE (-) #-}
  FloatX2# a1 a2 * FloatX2# b1 b2
    = FloatX2# (timesFloat# a1 b1) (timesFloat# a2 b2)
  {-# INLINE (*) #-}
  negate (FloatX2# a1 a2)
    = FloatX2# (negateFloat# a1) (negateFloat# a2)
  {-# INLINE negate #-}
  abs (FloatX2# a1 a2)
    = FloatX2# (if isTrue# (a1 `geFloat#` 0.0#) then a1 else negateFloat# a1)
                (if isTrue# (a2 `geFloat#` 0.0#) then a2 else negateFloat# a2)
  {-# INLINE abs #-}
  signum (FloatX2# a1 a2)
    = FloatX2# (if isTrue# (a1 `gtFloat#` 0.0#)
                 then 1.0#
                 else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
                (if isTrue# (a2 `gtFloat#` 0.0#)
                 then 1.0#
                 else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
  {-# INLINE signum #-}
  fromInteger n = case fromInteger n of F# x -> FloatX2# x x
  {-# INLINE fromInteger #-}



instance Fractional FloatX2 where
  FloatX2# a1 a2 / FloatX2# b1 b2 = FloatX2# (divideFloat# a1 b1)
                                                (divideFloat# a2 b2)
  {-# INLINE (/) #-}
  recip (FloatX2# a1 a2) = FloatX2# (divideFloat# 1.0# a1)
                                      (divideFloat# 1.0# a2)
  {-# INLINE recip #-}
  fromRational r = case fromRational r of F# x -> FloatX2# x x
  {-# INLINE fromRational #-}



instance Floating FloatX2 where
  pi = FloatX2# 3.141592653589793238# 3.141592653589793238#
  {-# INLINE pi #-}
  exp (FloatX2# a1 a2) = FloatX2# (expFloat# a1)
                                    (expFloat# a2)
  {-# INLINE exp #-}
  log (FloatX2# a1 a2) = FloatX2# (logFloat# a1)
                                    (logFloat# a2)
  {-# INLINE log #-}
  sqrt (FloatX2# a1 a2) = FloatX2# (sqrtFloat# a1)
                                     (sqrtFloat# a2)
  {-# INLINE sqrt #-}
  sin (FloatX2# a1 a2) = FloatX2# (sinFloat# a1)
                                    (sinFloat# a2)
  {-# INLINE sin #-}
  cos (FloatX2# a1 a2) = FloatX2# (cosFloat# a1)
                                    (cosFloat# a2)
  {-# INLINE cos #-}
  tan (FloatX2# a1 a2) = FloatX2# (tanFloat# a1)
                                    (tanFloat# a2)
  {-# INLINE tan #-}
  asin (FloatX2# a1 a2) = FloatX2# (asinFloat# a1)
                                     (asinFloat# a2)
  {-# INLINE asin #-}
  acos (FloatX2# a1 a2) = FloatX2# (acosFloat# a1)
                                     (acosFloat# a2)
  {-# INLINE acos #-}
  atan (FloatX2# a1 a2) = FloatX2# (atanFloat# a1)
                                     (atanFloat# a2)
  {-# INLINE atan #-}
  sinh (FloatX2# a1 a2) = FloatX2# (sinFloat# a1)
                                     (sinFloat# a2)
  {-# INLINE sinh #-}
  cosh (FloatX2# a1 a2) = FloatX2# (coshFloat# a1)
                                     (coshFloat# a2)
  {-# INLINE cosh #-}
  tanh (FloatX2# a1 a2) = FloatX2# (tanhFloat# a1)
                                     (tanhFloat# a2)
  {-# INLINE tanh #-}
  FloatX2# a1 a2 ** FloatX2# b1 b2 = FloatX2# (powerFloat# a1 b1)
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



--  log1p (FloatX2# a1 a2) = case ( log1p (F# a1), log1p (F# a2) ) of
--    (F# x1, F# x2) -> FloatX2# x1 x2
--  expm1 (FloatX2# a1 a2) = case ( expm1 (F# a1), expm1 (F# a2) ) of
--    (F# x1, F# x2) -> FloatX2# x1 x2
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



-- instance VectorCalculus Float 2 FloatX2 where
--   broadcastVec (F# x) = FloatX2# x x
--   {-# INLINE broadcastVec #-}
--   FloatX2# a1 a2 .*. FloatX2# b1 b2 = case timesFloat# a1 b1
--                                 `plusFloat#` timesFloat# a2 b2 of
--     x -> FloatX2# x x
--   {-# INLINE (.*.) #-}
--   FloatX2# a1 a2 `dot` FloatX2# b1 b2 = F# ( timesFloat# a1 b1
--                                   `plusFloat#` timesFloat# a2 b2
--                                   )
--   {-# INLINE dot #-}
--   indexVec 1 (FloatX2# a1 _) = F# a1
--   indexVec 2 (FloatX2# _ a2) = F# a2
--   indexVec i _ = error $ "Bad index " ++ show i ++ " for 2D vector"
--   {-# INLINE indexVec #-}
--   normL1 v = case abs v of
--       FloatX2# a1 a2 -> F# (a1 `plusFloat#` a2)
--   {-# INLINE normL1 #-}
--   normL2 v = sqrt $ dot v v
--   {-# INLINE normL2 #-}
--   normLPInf (FloatX2# a1 a2)
--     = F# (if isTrue# (a1 `gtFloat#` a2) then a1 else a2)
--   {-# INLINE normLPInf #-}
--   normLNInf (FloatX2# a1 a2)
--     = F# (if isTrue# (a1 `gtFloat#` a2) then a2 else a1)
--   {-# INLINE normLNInf #-}
--   normLP n (FloatX2# a1 a2) = case realToFrac n of
--     F# x -> F# ( powerFloat# (divideFloat# 1.0# x)
--                  (            powerFloat# a1 x
--                  `plusFloat#` powerFloat# a2 x
--                  )
--                )
--   {-# INLINE normLP #-}
--   dim _ = 2
--   {-# INLINE dim #-}
--
--
--
--
-- instance Vector2D Float where
--   vec2 (F# x) (F# y) = FloatX2# x y
--   {-# INLINE vec2 #-}
--   det2 (FloatX2# a1 a2)  (FloatX2# b1 b2)
--     = F# (timesFloat# a1 b2 `minusFloat#` timesFloat# a2 b1)
--   {-# INLINE det2 #-}

type instance ElemRep FloatX2 = 'FloatRep
instance PrimBytes FloatX2 where
  type ElemPrim FloatX2 = Float#
  toBytes (FloatX2# a1 a2) = case runRW#
     ( \s0 -> case newByteArray# (SIZEOF_HSFLOAT# *# 2#) s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# a1 s1 of
           s2 -> case writeFloatArray# marr 1# a2 s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, a #) -> (# 0#, 2#, a #)
  {-# INLINE toBytes #-}
  fromBytes (# off, _, arr #) = FloatX2#
    (indexFloatArray# arr off)
    (indexFloatArray# arr (off +# 1#))
  {-# INLINE fromBytes #-}
  byteSize _ = SIZEOF_HSFLOAT# *# 2#
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE elementByteSize #-}
  ix 0# (FloatX2# a1 _) = a1
  ix 1# (FloatX2# _ a2) = a2
  ix _ _ = undefined
  {-# INLINE ix #-}


instance ElementWise (Idx '[2]) Float FloatX2 where

  (!) (FloatX2# a1 _) ( 1 :! Z) = F# a1
  (!) (FloatX2# _ a2) ( 2 :! Z) = F# a2
  (!) _               ( _ :! Z) = undefined
  {-# INLINE (!) #-}

  broadcast (F# x) = FloatX2# x x
  {-# INLINE broadcast #-}

  ewmap f (FloatX2# x y) = case (f (1:!Z) (F# x), f (2:!Z) (F# y)) of
                              (F# r1, F# r2) -> FloatX2# r1 r2
  {-# INLINE ewmap #-}

  ewgen f = case (f (1:!Z), f (2:!Z)) of (F# r1, F# r2) -> FloatX2# r1 r2
  {-# INLINE ewgen #-}

  ewfold f x0 (FloatX2# x y) = f (2:!Z) (F# y) (f (1:!Z) (F# x) x0)
  {-# INLINE ewfold #-}

  elementWise f (FloatX2# x y) = (\(F# a) (F# b) -> FloatX2# a b)
                               <$> f (F# x) <*> f (F# y)
  {-# INLINE elementWise #-}

  indexWise f (FloatX2# x y) = (\(F# a) (F# b) -> FloatX2# a b)
                             <$> f (1:!Z) (F# x) <*> f (2:!Z) (F# y)
  {-# INLINE indexWise #-}
