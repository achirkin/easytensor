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
{-# LANGUAGE TypeApplications  #-}
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
import           GHC.TypeLits
import           GHC.Types
import           Data.Proxy

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions

import           Numeric.Matrix.Class

instance Show (ArrayF '[]) where
  show x = "{ " ++ show (x ! Z) ++ " }"
instance KnownNat n => Show (ArrayF '[n]) where
  show x = ('{' :) . drop 1 $
                foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
                        [minBound .. maxBound]
instance Dimensions (n :+ m :+ ds)
      => Show (ArrayF (n :+ m :+ ds)) where
  show x = drop 1 $ foldr loopOuter "" [minBound..maxBound]
    where
      loopInner :: Idx ds -> Idx '[n,m] -> String
      loopInner ods (n:!m:!_) = ('{' :) . drop 2 $
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

  broadcast (F# x) = FromScalarF# x
  {-# INLINE broadcast #-}

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





instance (KnownNat n, KnownNat m) => MatrixCalculus Float n m (ArrayF '[n,m]) where
  transpose (ArrayF# offs nm arr) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop2# n m
               (\i j s' -> writeFloatArray# marr (i +# n *# j)
                              (indexFloatArray# arr (offs +# i *# m +# j)) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, nm, r #)
    where
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      m = case fromInteger $ natVal (Proxy @m) of I# mp -> mp
      bs = n *# m *# SIZEOF_HSFLOAT#
  transpose (FromScalarF# x) = unsafeCoerce# $ FromScalarF# x

instance Dimensions '[n,n]
      => SquareMatrixCalculus Float n (ArrayF '[n,n]) where
  eye = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\j s' -> writeFloatArray# marr (j *# n1) 1.0# s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# n,  r #)
    where
      n1 = n +# 1#
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE eye #-}
  diag (F# v) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\j s' -> writeFloatArray# marr (j *# n1) v s'
               ) (setByteArray# marr 0# bs 0# s1) of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# n,  r #)
    where
      n1 = n +# 1#
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      bs = n *# n *# SIZEOF_HSFLOAT#
  {-# INLINE diag #-}


  det (ArrayF# off nsqr arr) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
       (# s1, mat #) -> case newByteArray#
                            (n *# SIZEOF_HSFLOAT#)
                            (copyByteArray# arr offb mat 0# bs s1) of
         (# s2, vec #) ->
            let f i x s | isTrue# (i >=# n) = (# s, x #)
                        | otherwise =
                            let (# s' , j  #) = maxInRowRem# n n i mat s
                                (# s'', x' #) = if isTrue# (i /=# j)
                                                then (# swapCols# n i j vec mat s'
                                                               , negateFloat# x #)
                                                else (# s', x #)
                                (# s''', y #) = clearRowEnd# n n i mat s''
                            in if isTrue# (eqFloat# 0.0# y)
                               then (# s''', 0.0# #)
                               else f (i +# 1#) (timesFloat# x' y) s'''
            in f 0# 1.0# s2
     ) of (# _, r #) -> F# r
    where
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      offb = off *# SIZEOF_HSFLOAT#
      bs = nsqr *# SIZEOF_HSFLOAT#
  det (FromScalarF# _) = 0
  {-# INLINE det #-}



  trace (ArrayF# off nsqr a) = F# (loop' 0# 0.0#)
    where
      n1 = n +# 1#
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      loop' i acc | isTrue# (i ># nsqr) = acc
                  | otherwise = loop' (i +# n1)
                         (indexFloatArray# a (off +# i) `plusFloat#` acc)
  trace (FromScalarF# x) = F# (x `timesFloat#` n)
    where
      n = case fromInteger $ natVal (Proxy @n) of F# np -> np
  {-# INLINE trace #-}


instance KnownNat n => MatrixInverse (ArrayF '[n,n]) where
  inverse (ArrayF# offs nsqr arr) = case runRW#
     ( \s0 -> case newByteArray# (bs *# 2#) s0 of
         (# s1, mat #) -> case newByteArray# (vs *# 2#)
                -- copy original matrix to the top of an augmented matrix
                (loop1# n (\i s -> writeFloatArray# mat
                           (i *# nn +# i +# n) 1.0#
                           (copyByteArray# arr (offb +# i *# vs)
                                           mat (2# *# i *# vs) vs s))
                         (setByteArray# mat 0# (bs *# 2#) 0# s1)
                ) of
           (# s2, vec #) ->
              let f i s | isTrue# (i >=# n) = s
                        | otherwise =
                            let (# s' , j  #) = maxInRowRem# nn n i mat s
                                s''           = if isTrue# (i /=# j) then swapCols# nn i j vec mat s'
                                                                     else s'
                                (# s''', _ #) = clearRowAll# nn n i mat s''
                            in f (i +# 1#) s'''
              in unsafeFreezeByteArray# mat
                  ( shrinkMutableByteArray# mat bs
                   (-- copy inverse matrix from the augmented part
                    loop1# n (\i s ->
                       copyMutableByteArray# mat
                                             (2# *# i *# vs +# vs)
                                             mat (i *# vs) vs s)
                   (f 0# s2)
                   )
                  )
     ) of (# _, r #) -> ArrayF# 0# nsqr r
    where
      nn = 2# *# n
      n = case fromInteger $ natVal (Proxy @n) of I# np -> np
      vs = n *# SIZEOF_HSFLOAT#
      bs = n *# n *# SIZEOF_HSFLOAT#
      offb = offs *# SIZEOF_HSFLOAT#
  inverse (FromScalarF# _) = error "Cannot take inverse of a degenerate matrix"


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


-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s)
       -> State# s -> State# s
loop2# n m f = loop' 0# 0#
  where
    loop' i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop' 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop' (i +# 1#) j s1
{-# INLINE loop2# #-}

-- | Do something in a loop for int i from 0 to n
loop1a# :: Int# -> (Int# -> a -> a) -> a -> a
loop1a# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1a# #-}


-- | Swap columns i and j. Does not check if i or j is larger than matrix width m
swapCols# :: Int# -- n
          -> Int# -- ith column to swap
          -> Int# -- jth column to swap
          -> MutableByteArray# s -- buffer byte array of length of n elems
          -> MutableByteArray# s -- byte array of matrix
          -> State# s -- previous state
          -> State# s -- next state
swapCols# n i j vec mat s0 =
  -- copy ith column to bugger vec
  case copyMutableByteArray# mat (i *# bs) vec 0# bs s0 of
    s1 -> case copyMutableByteArray# mat (j *# bs) mat (i *# bs) bs s1 of
      s2 -> copyMutableByteArray# vec 0# mat (j *# bs) bs s2
 where
  bs = n *# SIZEOF_HSFLOAT#

-- | Starting from i-th row and i+1-th column, substract a multiple of i-th column from i+1 .. m columns,
--   such that there are only zeroes in i-th row and i+1..m columns elements.
clearRowEnd# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to remove from all others
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Float# #) -- next state and a diagonal element
clearRowEnd# n m i mat s0 = (# loop' (i +# 1#) s1, y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    (# s1, y' #) = readFloatArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0# `divideFloat#` y'
    n' = n -# i -# 1#
    loop' k s | isTrue# (k >=# m) = s
              | otherwise = loop' (k +# 1#)
       ( let x0 = k *# n +# i
             (# s', a' #) = readFloatArray# mat x0 s
             s'' = writeFloatArray# mat x0 0.0# s'
             a  = a' `timesFloat#` yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )

-- | Substract a multiple of i-th column from 0 .. i-1 and i+1 .. m columns,
--   such that there are only zeroes in i-th row everywhere except i-th column
--   Assuming that elements in 0..i-1 columnts and in i-th row are zeroes, so they do not affect other columns.
--   After all columns updated, divide i-th row by its diagonal element, so (i,i) element has 1.
clearRowAll# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to remove from all others
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Float# #) -- next state and a diagonal element
clearRowAll# n m i mat s0 = (# divLoop (i +# 1#)
            (writeFloatArray# mat ((n +# 1#) *# i) 1.0#
            (loop' 0# i (loop' (i +# 1#) m s1))), y' #)
  where
    y0 = (n +# 1#) *# i +# 1# -- first element in source column
    (# s1, y' #) = readFloatArray# mat ((n +# 1#) *# i) s0 -- diagonal element, must be non-zero
    yrc = 1.0# `divideFloat#` y'
    n' = n -# i -# 1#
    loop' k km s | isTrue# (k >=# km) = s
                 | otherwise = loop' (k +# 1#) km
       ( let x0 = k *# n +# i
             (# s', a' #) = readFloatArray# mat x0 s
             s'' = writeFloatArray# mat x0 0.0# s'
             a  = a' `timesFloat#` yrc
         in multNRem# n' (x0 +# 1#) y0 a mat s''
       )
    divLoop k s | isTrue# (k >=# n) = s
                | otherwise = divLoop (k +# 1#)
       ( let x0 = n *# i +# k
             (# s', x #) = readFloatArray# mat x0 s
         in writeFloatArray# mat x0 (timesFloat# x yrc) s'
       )


-- | Remove a multiple of one row from another one.
--   do: xi = xi - yi*a
multNRem# :: Int# -- n - nr of elements to go through
          -> Int# -- start idx of x (update)
          -> Int# -- start idx of y (read)
          -> Float# -- multiplier a
          -> MutableByteArray# s -- byte array of matrix
          -> State# s -- previous state
          -> State# s -- next state
multNRem# 0# _ _  _ _ s = s
multNRem# n x0 y0 a mat s = multNRem# (n -# 1#) (x0 +# 1#) (y0 +# 1#) a mat
  ( case readFloatArray# mat y0 s of
     (# s1, y #) -> case readFloatArray# mat x0 s1 of
       (# s2, x #) -> writeFloatArray# mat x0 (x `minusFloat#` timesFloat# y a) s2
  )



-- | Gives index of maximum (absolute) element in i-th row, starting from i-th element only.
--   If i >= m then returns i.
maxInRowRem# :: Int# -- n
             -> Int# -- m
             -> Int# -- ith column to start to search for and a row to look in
             -> MutableByteArray# s -- byte array of matrix
             -> State# s -- previous state
             -> (# State# s, Int# #) -- next state
maxInRowRem# n m i mat s0 = loop' i (abs# v) i s1
  where
    (# s1, v #) = readFloatArray# mat ((n +# 1#) *# i) s0
    abs# x = if isTrue# (x `geFloat#` 0.0#) then x else negateFloat# x
    loop' ok ov k s | isTrue# (k >=# m) = (# s, ok #)
                    | otherwise = case readFloatArray# mat (n *# k +# i) s of
                        (# s', v' #) -> if isTrue# (abs# v' `gtFloat#` ov)
                                        then loop' k (abs# v') (k +# 1#) s'
                                        else loop' ok ov (k +# 1#) s'
