{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vector.Base.FloatXN
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Vector.Base.FloatXN () where

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types
import GHC.TypeLits

import Numeric.Commons
import Numeric.Vector.Class
import Numeric.Vector.Family


instance KnownNat n => Show (VFloatXN n) where
  show x = "{" ++ drop 1
              ( accumVReverse (\a s -> ", " ++ show (F# a) ++ s) x " }"
              )

instance KnownNat n => Eq (VFloatXN n) where
  a == b = accumV2 (\x y r -> r && isTrue# (x `eqFloat#` y)) a b True
  {-# INLINE (==) #-}
  a /= b = accumV2 (\x y r -> r || isTrue# (x `neFloat#` y)) a b False
  {-# INLINE (/=) #-}

-- | Implement partial ordering for `>`, `<`, `>=`, `<=` and lexicographical ordering for `compare`
instance KnownNat n => Ord (VFloatXN n) where
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



instance KnownNat n => Num (VFloatXN n) where
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
  fromInteger n = broadcastVec (fromInteger n)
  {-# INLINE fromInteger #-}





instance KnownNat n => VectorCalculus (VFloatXN n) Float n where
  broadcastVec (F# x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> writeFloatArray# marr i x s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
    where
      n = dim# (undefined :: VFloatXN n)
      bs = n *# 4#
  {-# INLINE broadcastVec #-}
--  VFloatX2 a1 a2 .*. VFloatX2 b1 b2 = case timesFloat# a1 b1
--                              `plusFloat#` timesFloat# a2 b2 of
--    x -> VFloatX2 x x
--  {-# INLINE (.*.) #-}
--  VFloatX2 a1 a2 `dot` VFloatX2 b1 b2 = F# ( timesFloat# a1 b1
--                                `plusFloat#` timesFloat# a2 b2
--                                )
--  {-# INLINE dot #-}
--  indexVec 1 (VFloatX2 a1 _) = F# a1
--  indexVec 2 (VFloatX2 _ a2) = F# a2
--  indexVec i _ = error $ "Bad index " ++ show i ++ " for 2D vector"
--  {-# INLINE indexVec #-}
--  normL1 v = case abs v of
--      VFloatX2 a1 a2 -> F# (a1 `plusFloat#` a2)
--  {-# INLINE normL1 #-}
--  normL2 v = sqrt $ dot v v
--  {-# INLINE normL2 #-}
--  normLPInf (VFloatX2 a1 a2) = F# (if isTrue# (a1 `gtFloat#` a2) then a1 else a2)
--  {-# INLINE normLPInf #-}
--  normLNInf (VFloatX2 a1 a2) = F# (if isTrue# (a1 `gtFloat#` a2) then a2 else a1)
--  {-# INLINE normLNInf #-}
--  normLP n (VFloatX2 a1 a2) = case realToFrac n of
--    F# x -> F# ( powerFloat# (divideFloat# 1.0# x)
--                 (            powerFloat# a1 x
--                 `plusFloat#` powerFloat# a2 x
--                 )
--               )
--  {-# INLINE normLP #-}
  dim x = I# (dim# x)
  {-# INLINE dim #-}



instance KnownNat n =>  PrimBytes (VFloatXN n) where
  toBytes (VFloatXN a) = a
  fromBytes = VFloatXN
  byteSize x = 4# *# dim# x
  {-# INLINE byteSize #-}
  byteAlign _ = 8#
  {-# INLINE byteAlign #-}

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


dim# :: KnownNat n => VFloatXN n -> Int#
dim# x = case fromInteger (natVal x) of I# n -> n
{-# INLINE dim# #-}

-- | Do something in a loop for int i from 0 to n
loop# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop# #-}


zipV :: KnownNat n => (Float# -> Float# -> Float#) -> VFloatXN n -> VFloatXN n -> VFloatXN n
zipV f x@(VFloatXN a) (VFloatXN b) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (indexFloatArray# a i) (indexFloatArray# b i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
  where
    n = dim# x
    bs = n *# 4#

mapV :: KnownNat n => (Float# -> Float#) -> VFloatXN n -> VFloatXN n
mapV f x@(VFloatXN a) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop# n
               (\i s' -> case f (indexFloatArray# a i) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> VFloatXN r
  where
    n = dim# x
    bs = n *# 4#

accumV :: KnownNat n => (Float# -> a -> a) -> VFloatXN n -> a -> a
accumV f x@(VFloatXN a) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) acc)
    n = dim# x

accumV2 :: KnownNat n => (Float# -> Float# -> a -> a) -> VFloatXN n -> VFloatXN n -> a -> a
accumV2 f x@(VFloatXN a) (VFloatXN b) = loop' 0#
  where
    loop' i acc | isTrue# (i ==# n) = acc
                | otherwise = loop' (i +# 1#) (f (indexFloatArray# a i) (indexFloatArray# b i) acc)
    n = dim# x


accumVReverse :: KnownNat n => (Float# -> a -> a) -> VFloatXN n -> a -> a
accumVReverse f x@(VFloatXN a) = loop' (n -# 1#)
  where
    loop' i acc | isTrue# (i ==# -1#) = acc
                | otherwise = loop' (i -# 1#) (f (indexFloatArray# a i) acc)
    n = dim# x
