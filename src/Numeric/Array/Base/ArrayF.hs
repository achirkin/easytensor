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

import           Data.Proxy
import           Data.Type.Equality
import           GHC.Base             (runRW#)
import           GHC.Prim
import           GHC.TypeLits
import           GHC.Types
import           Unsafe.Coerce

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions



-- instance ( Dimensions ds
--          ) => Show (ArrayF (ds :: [Nat])) where
--   show x = drop 1 $ foldr loopOuter "" [minBound..maxBound]
--     where
--       loopInner :: Proxy ds
--                -> Idx (Drop 2 ds) -> Idx (Take 2 ds) -> String
--       loopInner _ _ Z = "{}"
--       loopInner p ods ids@(n:!Z) = drop 1 $
--                       foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
--                               [1 :! ods .. n :! ods]
--       loopInner p ods ids@(n:!m:!_) = ('{' :) . drop 2 $
--                       foldr (\i ss -> '\n':
--                               foldr (\j s ->
--                                        ", " ++ show (x ! (i :! j :! ods)) ++ s
--                                     ) ss [1..m]
--                             ) " }" [1..n]
--       loopOuter :: Idx (Drop 2 ds) -> String -> String
--       loopOuter Z s  = "\n" ++ loopInner (unsafeCoerce Refl) Z maxBound ++ s
--       loopOuter ds s = "\n" ++ show ds ++ ":\n"
--                             ++ loopInner (unsafeCoerce Refl) ds maxBound ++ s
--       proof1 :: Proxy ns -> Idx '[n] -> Idx (Drop 2 ns)
--              -> ns :~: (n :+ Drop 2 ns)
--       proof1 _ _ _ = unsafeCoerce Refl
--       proof2 :: Proxy ns -> Idx (n ': m ': mns) -> Idx (Drop 2 ns)
--              -> ns :~: (n :+ (m :+ Drop 2 ns))
--       proof2 _ _ _ = unsafeCoerce Refl

instance Show (ArrayF '[]) where
  show x = "{ " ++ show (x ! Z) ++ " }"
instance Dimensions '[n] => Show (ArrayF '[n]) where
  show x = ('{' :) . drop 1 $
                foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
                        [minBound .. maxBound]
instance ( Dimensions (n :+ m :+ ds)
         ) => Show (ArrayF (n :+ m :+ ds)) where
  show x = drop 1 $ foldr loopOuter "" [minBound..maxBound]
    where
      loopInner :: Idx ds -> Idx '[n,m] -> String
      loopInner ods ids@(n:!m:!_) = ('{' :) . drop 2 $
                      foldr (\i ss -> '\n':
                              foldr (\j s ->
                                       ", " ++ show (x ! (i :! j :! ods)) ++ s
                                    ) ss [1..m]
                            ) " }" [1..n]
      loopOuter :: Idx ds -> String -> String
      loopOuter Z s  = "\n" ++ loopInner Z maxBound ++ s
      loopOuter ds s = "\n" ++ show ds ++ ":\n"
                            ++ loopInner ds maxBound ++ s



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


instance Dimensions ds => PrimBytes (ArrayF ds) where
  toBytes (ArrayF# off size a) = (# off, size, a #)
  toBytes (FromScalarF# x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> writeFloatArray# marr i x s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> (# 0#, n, r #)
    where
      n = case totalDim (undefined :: ArrayF ds) of I# d -> d
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE toBytes #-}
  fromBytes (# off, size, a #) = ArrayF# off size a
  {-# INLINE fromBytes #-}
  byteSize x = case totalDim x of
     I# d -> SIZEOF_HSFLOAT# *# d
  {-# INLINE byteSize #-}
  byteAlign _ = ALIGNMENT_HSFLOAT#
  {-# INLINE byteAlign #-}
  elementByteSize _ = SIZEOF_HSFLOAT#
  {-# INLINE elementByteSize #-}

instance FloatBytes (ArrayF ds) where
  ixF i (ArrayF# off _ a) = indexFloatArray# a (off +# i)
  ixF _ (FromScalarF# x)  = x
  {-# INLINE ixF #-}







instance Dimensions ds => ElementWise (Idx ds) Float (ArrayF (ds :: [Nat])) where

  (!) (ArrayF# off _ a) i
       = case fromEnum i of I# j -> F# (indexFloatArray# a (off +# j))
  (!) (FromScalarF# x) _ = F# x
  {-# INLINE (!) #-}

  ewmap f x@(ArrayF# offset n arr) = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dimMax `inSpaceOf` x)
             (\ix s' -> case readMutVar# mi s' of
               (# s'', I# i #) ->
                 case f ix (F# (indexFloatArray# arr (offset +# i))) of
                  F# r -> writeMutVar# mi (I# (i +# 1#))
                                          (writeFloatArray# marr i r s'')
             ) s2 of
           s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> ArrayF# 0# n r
    where
      bs = n *# SIZEOF_HSFLOAT#
  ewmap f x@(FromScalarF# scalVal) = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dimMax `inSpaceOf` x)
             (\ix s' -> case readMutVar# mi s' of
               (# s'', I# i #) ->
                 case f ix (F# scalVal) of
                  F# r -> writeMutVar# mi (I# (i +# 1#))
                                          (writeFloatArray# marr i r s'')
             ) s2 of
           s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> ArrayF# 0# n r
    where
      n = case totalDim x of I# d -> d
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE ewmap #-}

  ewgen f = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dimMax `inSpaceOf` x)
             (\ix s' -> case readMutVar# mi s' of
               (# s'', I# i #) -> case f ix of
                  F# r -> writeMutVar# mi (I# (i +# 1#))
                                          (writeFloatArray# marr i r s'')
             ) s2 of
           s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> ArrayF# 0# n r
    where
      x = undefined :: ArrayF ds
      n = case totalDim x of I# d -> d
      bs = n *# SIZEOF_HSFLOAT#
  {-# INLINE ewgen #-}

  ewfold f v0 x@(ArrayF# offset _ arr) = case runRW#
    (\s0 -> case newMutVar# (0,v0) s0 of
       (# s1, miv #) -> case loopS (dimMax `inSpaceOf` x)
             (\ix s' -> case readMutVar# miv s' of
               (# s'', (I# i, v) #) -> writeMutVar# miv
                      ( I# (i +# 1#)
                      , f ix (F# (indexFloatArray# arr (offset +# i))) v
                      ) s''
             ) s1 of
          s2 -> readMutVar# miv s2
    ) of (# _, (_, r) #) -> r
  ewfold f v0 x@(FromScalarF# scalVal) = case runRW#
    (\s0 -> case newMutVar# v0 s0 of
        (# s1, miv #) -> case loopS (dimMax `inSpaceOf` x)
              (\ix s' -> case readMutVar# miv s' of
                (# s'', v #) -> writeMutVar# miv
                       ( f ix (F# scalVal) v
                       ) s''
              ) s1 of
           s2 -> readMutVar# miv s2
    ) of (# _, r #) -> r
  {-# INLINE ewfold #-}

  indexWise f x@(ArrayF# offset n arr)
      = case loopA (dimMax `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
        AU# _ f' -> wr x bs n <$> f'
    where
      g ds (AU# i f') = AU# ( i +# 1# )
                          $ (\(F# z) u a s -> writeFloatArray# a i z (u a s))
                           <$> f ds (F# (indexFloatArray# arr (offset +# i))) <*> f'
      bs = n *# SIZEOF_HSFLOAT#

  indexWise f x@(FromScalarF# scalVal)
      = case loopA (dimMax `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
        AU# _ f' -> wr x bs n <$> f'
    where
      n = case totalDim x of I# d -> d
      g ds (AU# i f') = AU# ( i +# 1# )
                          $ (\(F# z) u a s -> writeFloatArray# a i z (u a s))
                           <$> f ds (F# scalVal) <*> f'
      bs = n *# SIZEOF_HSFLOAT#


  elementWise f x@(ArrayF# offset n arr) =
      wr x bs n <$> loop1a# n g (pure (\_ s -> s))
    where
      g i f' = (\(F# z) u a s -> writeFloatArray# a i z (u a s))
                      <$> f (F# (indexFloatArray# arr (offset +# i))) <*> f'
      bs = n *# SIZEOF_HSFLOAT#
  elementWise f x@(FromScalarF# scalVal) =
      wr x bs n <$> loop1a# n g (pure (\_ s -> s))
    where
      fa = f (F# scalVal)
      n = case totalDim x of I# d -> d
      g i f' = (\(F# z) u a s -> writeFloatArray# a i z (u a s))
                      <$> fa <*> f'
      bs = n *# SIZEOF_HSFLOAT#

wr :: ArrayF (ds :: [Nat]) -> Int# -> Int#
   -> (MutableByteArray# RealWorld -> State# RealWorld -> State# RealWorld)
   -> ArrayF ds
wr _ bs n f' = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
               (# s1, marr #) ->  case f' marr s1 of
                 s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ArrayF# 0# n r
{-# INLINE wr #-}

data ArrayUpdate# (f :: * -> *) s
  = AU# Int# !(f (MutableByteArray# s -> State# s -> State# s))



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
zipV f (ArrayF# offsetA n a) (ArrayF# offsetB _ b) = case runRW#
     ( \s0 -> case newByteArray# (n *# SIZEOF_HSFLOAT#) s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> case f (indexFloatArray# a (offsetA +# i))
                                (indexFloatArray# b (offsetB +# i)) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ArrayF# 0# n r


mapV :: (Float# -> Float#) -> ArrayF ds -> ArrayF ds
mapV f (FromScalarF# x) = FromScalarF# (f x)
mapV f (ArrayF# offset n a) = case runRW#
     ( \s0 -> case newByteArray# (n *# SIZEOF_HSFLOAT#) s0 of
         (# s1, marr #) -> case loop1# n
               (\i s' -> case f (indexFloatArray# a (offset +# i)) of
                 r -> writeFloatArray# marr i r s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ArrayF# 0# n r

-- | Accumulates only idempotent operations!
--   Being applied to FromScalars, executes only once!
accumV2 :: (Float# -> Float# -> a -> a)
        -> ArrayF ds -> ArrayF ds -> a -> a
accumV2 f (FromScalarF# a) (FromScalarF# b) = f a b
accumV2 f (ArrayF# offset n a) (FromScalarF# b) = loop1a# n
    (\i -> f (indexFloatArray# a (offset +# i)) b)
accumV2 f (FromScalarF# a) (ArrayF# offset n b) = loop1a# n
    (\i -> f a (indexFloatArray# b (offset +# i)))
accumV2 f (ArrayF# offsetA n a) (ArrayF# offsetB _ b) = loop1a# n
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
