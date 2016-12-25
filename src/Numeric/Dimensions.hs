{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs, PolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type Dim that enumerates through multiple dimensions (type-checked).
-- Lower indices go first, i.e. assumed enumeration is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( Dim (..), totalDim, order, tailDim, headDim
  , Dim# (..), order#
  , Dimensions (..)
  , (++), Reverse, Take, Drop, Length, IsPrefixOf, IsSuffixOf
  ) where



import GHC.TypeLits
import GHC.Prim
import GHC.Types
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool

import Unsafe.Coerce

-- | Type-level checked dimensionality
data Dim (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Dim '[]
   -- | List-like concatenation of dimensionality
   (:-) :: !Int -> !(Dim ds) -> Dim (n':ds)
infixr 5 :-

instance (IsList (Dim ds), Item (Dim ds) ~ Int) => Show (Dim ds) where
  show Z = "Dim Ø"
  show xs = "Dim" ++ foldr (\i s -> " " ++ show i ++ s) "" (toList xs)

instance Eq (Dim ds) where
  Z == Z = True
  (a:-as) == (b:-bs) = a == b && as == bs
  Z /= Z = False
  (a:-as) /= (b:-bs) = a /= b || as /= bs

instance Ord (Dim ds) where
  compare Z Z = EQ
  compare (a:-as) (b:-bs) = compare as bs `mappend` compare a b

instance Dimensions ds => Bounded (Dim ds) where
  maxBound = dimMax
  {-# INLINE maxBound #-}
  minBound = dimMin
  {-# INLINE minBound #-}

instance Dimensions ds => Enum (Dim ds) where
  succ = succDim
  {-# INLINE succ #-}
  pred = predDim
  {-# INLINE pred #-}
  toEnum = toDim
  {-# INLINE toEnum #-}
  fromEnum = fromDim
  {-# INLINE fromEnum #-}
  enumFrom x = take (diffDim maxBound x + 1) $ iterate succ x
  {-# INLINE enumFrom #-}
  enumFromTo x y | x >= y    = take (diffDim x y + 1) $ iterate pred x
                 | otherwise = take (diffDim y x + 1) $ iterate succ x
  {-# INLINE enumFromTo #-}
  enumFromThen x x' = take n $ iterate (stepDim dn) x
    where
      dn = diffDim x' x
      n  = 1 + if dn == 0 then 0
                          else if dn > 0 then diffDim maxBound x `div` dn
                                         else diffDim x minBound `div` negate dn
  {-# INLINE enumFromThen #-}
  enumFromThenTo x x' y = take n $ iterate (stepDim dn) x
    where
      dn = diffDim x' x
      n  = 1 + if dn == 0 then 0
                          else diffDim y x `div` dn
  {-# INLINE enumFromThenTo #-}

-- | Type-level checked dimensionality (unboxed)
data Dim# (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z# :: Dim# '[]
   -- | List-like concatenation of dimensionality
   (:#) :: Int# -> Dim# ds -> Dim# (n':ds)
infixr 5 :#

-- | Support for Dim GADT
class Dimensions (ds :: [Nat]) where
  -- | Dimensionality of a second rank type
  dim :: t ds -> Dim ds
  -- | Dimensionality of a second rank type (unboxed)
  dim# :: t ds -> Dim# ds
  -- | Total number of elements - product of all dimension sizes (unboxed)
  totalDim# :: t ds -> Int#
  -- | Run a primitive loop over all dimensions (0..n-1)
  loopS#      :: Dim# ds -> (Dim# ds -> State# s -> State# s) -> State# s -> State# s
  -- | Run a primitive loop over all dimensions (1..n)
  loopS       :: Dim  ds -> (Dim  ds -> State# s -> State# s) -> State# s -> State# s
  -- | Run a loop over all dimensions keeping a boxed accumulator (1..n)
  loopA       :: Dim  ds -> (Dim  ds -> a -> a) -> a -> a
  -- | Run a loop in a reverse order n..1
  loopReverse :: Dim ds -> (Dim  ds -> a -> a) -> a -> a
  -- | Get index offset: i1 + i2*n1 + i3*n1*n2 + ...
  ioffset   :: Dim ds -> Int
  -- | Get index offset: i1 + i2*n1 + i3*n1*n2 + ... (unboxed)
  ioffset#  :: Dim# ds -> Int#
  -- | Drop a number of dimensions
  dropDims  :: KnownNat n => Proxy n -> Dim ds -> Dim (Drop n ds)
  -- | Take a number of dimensions
  takeDims  :: KnownNat n => Proxy n -> Dim ds -> Dim (Take n ds)
  -- | Maximum values of all dimensions
  dimMax    :: Dim ds
  -- | Minimum values -- ones
  dimMin    :: Dim ds
  -- | Minimum prim values -- zeroes
  dimZero#  :: Dim# ds
  -- | For Enum
  succDim   :: Dim ds -> Dim ds
  -- | For Enum
  predDim   :: Dim ds -> Dim ds
  -- | For Enum
  fromDim   :: Dim ds -> Int
  -- | For Enum
  toDim     :: Int -> Dim ds
  -- | For Enum -- step dimension index by an Integer offset
  stepDim   :: Int -> Dim ds -> Dim ds
  -- | For Enum -- difference in offsets between two Dim values (a `diffDim` b) = a - b
  diffDim   :: Dim ds -> Dim ds -> Int


instance IsList (Dim ds) => IsList (Dim (d:ds)) where
  type Item (Dim (d:ds)) = Int
  fromList (x:xs) = x :- fromList (unsafeCoerce xs)
  fromList [] = undefined
  toList (x :- xs) = x : unsafeCoerce (toList xs)

instance IsList (Dim '[]) where
  type Item (Dim '[]) = Int
  fromList _ = Z
  toList _   = []

-- | Get all but first dimensions
tailDim :: Dim ds -> Dim (Drop 1 ds)
tailDim Z = Z
tailDim (_:-xs) = xs

-- | Get the first dimension
headDim :: t (d ': ds :: [Nat]) -> Proxy d
headDim _ = Proxy

-- | Total number of elements - product of all dimension sizes
totalDim :: Dimensions ds => t ds -> Int
totalDim x = I# (totalDim# x)
{-# INLINE totalDim #-}

-- | Number of dimensions
order :: Dim ds -> Int
order Z = 0
order (_:-xs) = 1 + order xs
{-# INLINE order #-}

-- | Number of dimensions (unboxed)
order# :: Dim# ds -> Int#
order# Z# = 0#
order# (_:#xs) = 1# +# order# xs
{-# INLINE order# #-}


instance Dimensions '[] where
  dim _ = Z
  {-# INLINE dim #-}
  dim# _ = Z#
  {-# INLINE dim# #-}
  totalDim# _ = 1#
  {-# INLINE totalDim# #-}
  loopS# _ f = f Z#
  {-# INLINE loopS# #-}
  loopS _ f = f Z
  {-# INLINE loopS #-}
  loopA _ f = f Z
  {-# INLINE loopA #-}
  loopReverse _ f = f Z
  {-# INLINE loopReverse #-}
  ioffset# _ = 0#
  {-# INLINE ioffset# #-}
  ioffset _ = 0
  {-# INLINE ioffset #-}
  dropDims _ Z = Z
  {-# INLINE dropDims #-}
  takeDims _ Z = Z
  {-# INLINE takeDims #-}
  dimMax = Z
  {-# INLINE dimMax #-}
  dimMin = Z
  {-# INLINE dimMin #-}
  dimZero# = Z#
  {-# INLINE dimZero# #-}
  succDim = id
  {-# INLINE succDim #-}
  predDim = id
  {-# INLINE predDim #-}
  fromDim _ = 0
  {-# INLINE fromDim #-}
  toDim _ = Z
  {-# INLINE toDim #-}
  stepDim _ = id
  {-# INLINE stepDim #-}
  diffDim _ _ = 0
  {-# INLINE diffDim #-}


instance (KnownNat d, Dimensions ds) => Dimensions (d ': ds) where
  dim x = fromIntegral (natVal' (headDim# x)) :- dim (tailDim# x)
  {-# INLINE dim #-}
  dim# x = case fromIntegral (natVal' (headDim# x)) of
             I# n -> n :# dim# (tailDim# x)
  {-# INLINE dim# #-}
  totalDim# x = case fromIntegral (natVal' (headDim# x)) of
             I# n -> n *# totalDim# (tailDim# x)
  {-# INLINE totalDim# #-}
  loopS# (n:#Z#) f = loop1# n (\i -> f (i:#Z#))
  loopS# (n:#ns) f = loopS# ns (\js -> loop1# n (\i -> f (i:#js)))
  {-# INLINE loopS# #-}
  loopS (n:-Z) f = loop1 n (\i -> f (i:-Z))
  loopS (n:-ns) f = loopS ns (\js -> loop1 n (\i -> f (i:-js)))
  {-# INLINE loopS #-}
  loopA (n:-Z) f = loopA1 n (f . (:-Z))
  loopA (n:-ns) f = loopA ns (\js -> loopA1 n (f . (:-js)))
  {-# INLINE loopA #-}
  loopReverse (n:-Z) f = loopReverse1 n (f . (:-Z))
  loopReverse (n:-ns) f = loopReverse ns (\js -> loopReverse1 n (f . (:-js)))
  {-# INLINE loopReverse #-}
  ioffset# (i:#Z#) = i
  ioffset# iis@(i:#is) = case fromIntegral (natVal' (headDim# iis)) of
             I# n -> i +# n *# ioffset# is
  {-# INLINE ioffset# #-}
  ioffset (i:-Z) = i
  ioffset iis@(i:-is) = i + fromIntegral (natVal' (headDim# iis)) * ioffset is
  {-# INLINE ioffset #-}
  dropDims p ds = case (fromInteger (natVal p), order ds) of
          (0, _) -> unsafeCoerce ds
          (n, k) -> if n >= k then unsafeCoerce Z
                              else f n ds
    where
      f 0 ds' = unsafeCoerce ds'
      f i (_:-ds') = unsafeCoerce (f (i-1) $ unsafeCoerce ds')
      f _ Z = unsafeCoerce Z
  {-# INLINE dropDims #-}
  takeDims p ds = case (fromInteger (natVal p), order ds) of
          (0, _) -> unsafeCoerce Z
          (n, k) -> if n >= k then unsafeCoerce ds
                              else f n ds
    where
      f 0 _ = unsafeCoerce Z
      f i (d:-ds') = unsafeCoerce $ d :- unsafeCoerce (f (i-1) $ unsafeCoerce ds')
      f _ Z = unsafeCoerce Z
  {-# INLINE takeDims #-}
  dimMax = ds
    where
      ds = fromInteger (natVal $ headDim ds) :- dimMax
  {-# INLINE dimMax #-}
  dimMin = 1 :- dimMin
  {-# INLINE dimMin #-}
  dimZero# = 0# :# dimZero#
  {-# INLINE dimZero# #-}
  succDim ds@(i:-is) = case fromInteger (natVal' (headDim# ds)) of
                         n -> if i == n then 1 :- succDim is
                                        else succ i :- is
  {-# INLINE succDim #-}
  predDim ds@(i:-is) = if i == 1 then fromInteger (natVal' (headDim# ds)) :- predDim is
                                 else pred i :- is
  {-# INLINE predDim #-}
  fromDim ds@(i:-is) = pred i + fromInteger (natVal' (headDim# ds)) * fromDim is
  {-# INLINE fromDim #-}
  toDim j = r
    where
      r = case divMod j $ fromInteger (natVal' (headDim# r)) of
            (j', i) -> succ i :- toDim j'
  {-# INLINE toDim #-}
  stepDim di ds@(i:-is) = case divMod (di + i - 1) $ fromInteger (natVal' (headDim# ds)) of
                           (0  , i') -> succ i' :- is
                           (di', i') -> succ i' :- stepDim di' is
  {-# INLINE stepDim #-}
  diffDim ds@(i1:-is1) (i2:-is2) = i1 - i2 + fromInteger (natVal' (headDim# ds)) * diffDim is1 is2
  {-# INLINE diffDim #-}



headDim# :: t (d ': ds :: [Nat]) -> Proxy# d
headDim# _ = proxy#
{-# INLINE headDim# #-}

tailDim# :: t (d ': ds :: [Nat]) -> Proxy ds
tailDim# _ = Proxy
{-# INLINE tailDim# #-}

-- | Do something in a loop for int i from 0 to n-1
loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop1# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1# #-}


-- | Do something in a loop for int i from 1 to n
loop1 :: Int -> (Int -> State# s -> State# s) -> State# s -> State# s
loop1 n f = loop' 1
  where
    loop' i s | i > n = s
              | otherwise = case f i s of s1 -> loop' (i + 1) s1
{-# INLINE loop1 #-}

-- | Do something in a loop for int i from 1 to n
loopA1 :: Int -> (Int -> a -> a) -> a -> a
loopA1 n f = loop' 1
  where
    loop' i s | i > n = s
              | otherwise = case f i s of s1 -> loop' (i + 1) s1
{-# INLINE loopA1 #-}

-- | Do something in a loop for int i from n to 1
loopReverse1 :: Int -> (Int -> a -> a) -> a -> a
loopReverse1 n f = loop' n
  where
    loop' i s | i == 0 = s
              | otherwise = case f i s of s1 -> loop' (i - 1) s1
{-# INLINE loopReverse1 #-}




type family (as :: [k]) ++ (bs :: [k]) = (rs :: [k]) where
  '[]        ++ bs = bs
  (a ': as)  ++ bs = a ': (as ++ bs)
infixr 5 ++


type family Reverse (as :: [k]) ::[k] where -- = (rs :: [k]) | rs -> as
  Reverse  '[]        = '[]
  Reverse (a ': as) = Reverse as ++ '[a]

type family IsPrefixOf (as :: [k]) (bs :: [k]) :: Bool where
  IsPrefixOf '[] _ = 'True
  IsPrefixOf (a ': as)  (b ': bs) = a == b && IsPrefixOf as bs
  IsPrefixOf _ _ = 'False

type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + Length as

type family Take (n::Nat) (as :: [k]) :: [k] where
  Take _ '[] = '[]
  Take 0 _   = '[]
  Take n (a ': as) = a ': Take (n-1) as

type family Drop (n::Nat) (as :: [k]) :: [k] where
  Drop _ '[] = '[]
  Drop 0 as  = as
  Drop n (a ': as) = Drop (n-1) as

type family IsSuffixOf (as :: [k]) (bs :: [k]) :: Bool where
  IsSuffixOf '[] _ = 'True
  IsSuffixOf as bs = If (CmpNat (Length as) (Length bs) == 'GT)
                         'False
                         (as == Drop (Length bs - Length as) bs)

