{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
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

module Numeric.Array.Base.ArrayF () where


#include "MachDeps.h"
#include "HsBaseConfig.h"

import           GHC.Base             (runRW#)
import           GHC.Prim
-- import           GHC.TypeLits
import           GHC.Types



import           Numeric.Array.Family
-- import           Numeric.Commons
-- import           Numeric.Dimensions



-- instance ( (Take 2 ds ++ Drop 2 ds) ~ ds
--          , Dimensions ds
--          , Dimensions (Take 2 ds)
--          , Dimensions (Drop 2 ds)
--          ) => Show (ArrayF ds) where
--   show x = drop 1 $ foldr loopOuter "" [minBound..maxBound]
--     where
--       loopInner :: Dim (Drop 2 ds) -> Dim (Take 2 ds) -> String
--       loopInner _ Z = "{}"
--       loopInner ods (n:-Z) =  ('{' :) . drop 1 $
--                                   foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
--                                          [1 :- ods .. n :- ods]
--       loopInner ods (n:-m:-_) = ('{' :) . drop 2 $
--                       foldr (\i ss -> '\n':
--                               foldr (\j s ->
--                                        ", " ++ show (x ! (i :- j :- ods)) ++ s
--                                     ) ss [1..m]
--                             ) " }" [1..n]
--       loopOuter :: Dim (Drop 2 ds) -> String -> String
--       loopOuter Z s  = "\n" ++ loopInner Z maxBound ++ s
--       loopOuter ds s ="\n" ++ show ds ++ ":\n" ++ loopInner ds maxBound ++ s


instance Eq (ArrayF ds) where
  a == b = accumV2 (\x y r -> r && isTrue# (x `eqFloat#` y)) a b True
  {-# INLINE (==) #-}
  a /= b = accumV2 (\x y r -> r || isTrue# (x `neFloat#` y)) a b False
  {-# INLINE (/=) #-}




-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--     and lexicographical ordering for `compare`
instance Ord (ArrayF ds) where
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





instance Num (ArrayF ds) where
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
  signum = mapV (\x -> if isTrue# (x `gtFloat#` 0.0#)
                      then 1.0#
                      else if isTrue# (x `ltFloat#` 0.0#)
                           then -1.0#
                           else 0.0#
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArrayF . fromInteger
  {-# INLINE fromInteger #-}



instance Fractional (ArrayF ds) where
  (/) = zipV divideFloat#
  {-# INLINE (/) #-}
  recip = mapV (divideFloat# 1.0#)
  {-# INLINE recip #-}
  fromRational = broadcastArrayF . fromRational
  {-# INLINE fromRational #-}



instance Floating (ArrayF ds) where
  pi = broadcastArrayF pi
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



-- | Uses first argument to enforce type (can and should be undefined)
broadcastArrayF :: Float -> ArrayF ds
broadcastArrayF (F# x) = FromScalarF# x
{-# INLINE broadcastArrayF #-}




-- instance Dimensions ds => PrimBytes (ArrayF ds) where
--   toBytes (ArrayF# a) = a
--   {-# INLINE toBytes #-}
--   fromBytes = ArrayF#
--   {-# INLINE fromBytes #-}
--   byteSize x = SIZEOF_HSFLOAT# *# totalDim# x
--   {-# INLINE byteSize #-}
--   byteAlign _ = ALIGNMENT_HSFLOAT#
--   {-# INLINE byteAlign #-}
--
-- instance FloatBytes (ArrayF ds) where
--   ixF i (ArrayF# a) = indexFloatArray# a i
--   {-# INLINE ixF #-}






--
-- instance Dimensions ds => ElementWise (Dim ds) Float (ArrayF ds) where
--   (!) (ArrayF# offset _ _ arr) i
--        = case fromEnum i of I# j -> F# (indexFloatArray# arr (offset +# j))
--   {-# INLINE (!) #-}
--   ewmap f x@(ArrayF# offset n dims arr) = case runRW#
--      (\s0 -> case newByteArray# bs s0 of
--        (# s1, marr #) -> case newMutVar# 0 s1 of
--          (# s2, mi #) -> case loopS (dim x)
--              (\ix s' -> case readMutVar# mi s' of
--                (# s'', I# i #) ->
--                  case f ix (F# (indexFloatArray# arr (offset +# i))) of
--                   F# r -> writeMutVar# mi (I# (i +# 1#))
--                                           (writeFloatArray# marr i r s'')
--              ) s2 of
--            s3 -> unsafeFreezeByteArray# marr s3
--      ) of (# _, r #) -> ArrayF# 0# n dims r
--     where
--       bs = n *# SIZEOF_HSFLOAT#
--   {-# INLINE ewmap #-}
--   ewgen f  = case runRW#
--      (\s0 -> case newByteArray# bs s0 of
--        (# s1, marr #) -> case newMutVar# 0 s1 of
--          (# s2, mi #) -> case loopS (dim x)
--              (\ix s' -> case readMutVar# mi s' of
--                (# s'', I# i #) -> case f ix of
--                   F# r -> writeMutVar# mi (I# (i +# 1#))
--                                           (writeFloatArray# marr i r s'')
--              ) s2 of
--            s3 -> unsafeFreezeByteArray# marr s3
--      ) of (# _, r #) -> ArrayF# 0# n undefined r
--     where
--       x = undefined :: ArrayF ds
--       n = totalDim# x
--       bs = n *# SIZEOF_HSFLOAT#
--   {-# INLINE ewgen #-}
--   ewfold f v0 x@(ArrayF# offset _ _ arr) = case runRW#
--      (\s0 -> case newMutVar# (0,v0) s0 of
--          (# s1, miv #) -> case loopS (dim x)
--                (\ix s' -> case readMutVar# miv s' of
--                  (# s'', (I# i, v) #) -> writeMutVar# miv
--                         ( I# (i +# 1#)
--                         , f ix (F# (indexFloatArray# arr (offset +# i))) v
--                         ) s''
--                ) s1 of
--             s2 -> readMutVar# miv s2
--      ) of (# _, (_, r) #) -> r
--   {-# INLINE ewfold #-}
--   indexWise f x@(ArrayF# offset n dims arr)
--       = case loopA (dim x) g (AU# 0# (pure (\_ s -> s))) of
--         AU# _ f' -> wr <$> f'
--     where
--       g ds (AU# i f') = AU# ( i +# 1# )
--                           $ (\(F# z) u a s -> writeFloatArray# a i z (u a s))
--                            <$> f ds (F# (indexFloatArray# arr (offset +# i))) <*> f'
--       bs = n *# SIZEOF_HSFLOAT#
--       wr f' = case runRW#
--                    ( \s0 -> case newByteArray# bs s0 of
--                              (# s1, marr #) ->  case f' marr s1 of
--                                s2 -> unsafeFreezeByteArray# marr s2
--                    ) of (# _, r #) -> ArrayF# 0# n dims r
--   elementWise f x@(ArrayF# offset n dims arr) =
--       wr <$> loop1a# n g (pure (\_ s -> s))
--     where
--       g i f' = (\(F# z) u a s -> writeFloatArray# a i z (u a s))
--                       <$> f (F# (indexFloatArray# arr i)) <*> f'
--       bs = n *# SIZEOF_HSFLOAT#
--       wr f' = case runRW#
--                    ( \s0 -> case newByteArray# bs s0 of
--                              (# s1, marr #) ->  case f' marr s1 of
--                                s2 -> unsafeFreezeByteArray# marr s2
--                    ) of (# _, r #) -> ArrayF# 0# n dims r
--
-- data ArrayUpdate# (f :: * -> *) s
--   = AU# Int# !(f (MutableByteArray# s -> State# s -> State# s))



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

zipV :: (Float# -> Float# -> Float#)
     -> ArrayF ds -> ArrayF ds -> ArrayF ds
zipV f (FromScalarF# a) (FromScalarF# b) = FromScalarF# (f a b)
zipV f x (FromScalarF# b) = mapV (`f` b) x
zipV f (FromScalarF# a) y = mapV (f a) y
zipV f (ArrayF# offsetA n dims a) (ArrayF# offsetB _ _ b) = case runRW#
     ( \s0 -> case newByteArray# (n *# SIZEOF_HSFLOAT#) s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> case f (indexFloatArray# a (offsetA +# i))
                                (indexFloatArray# b (offsetB +# i)) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ArrayF# 0# n dims r


mapV :: (Float# -> Float#) -> ArrayF ds-> ArrayF ds
mapV f (FromScalarF# x) = FromScalarF# (f x)
mapV f (ArrayF# offset n dims a) = case runRW#
     ( \s0 -> case newByteArray# (n *# SIZEOF_HSFLOAT#) s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> case f (indexFloatArray# a (offset +# i)) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ArrayF# 0# n dims r

-- | Accumulates only idempotent operations!
--   Being applied to FromScalars, executes only once!
accumV2 :: (Float# -> Float# -> a -> a)
        -> ArrayF ds -> ArrayF ds -> a -> a
accumV2 f (FromScalarF# a) (FromScalarF# b) = f a b
accumV2 f (ArrayF# offset n _ a) (FromScalarF# b) = loop1a# n
    (\i -> f (indexFloatArray# a (offset +# i)) b)
accumV2 f (FromScalarF# a) (ArrayF# offset n _ b) = loop1a# n
    (\i -> f a (indexFloatArray# b (offset +# i)))
accumV2 f (ArrayF# offsetA n _ a) (ArrayF# offsetB _ _ b) = loop1a# n
    (\i -> f (indexFloatArray# a (offsetA +# i))
             (indexFloatArray# b (offsetB +# i))
    )

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
