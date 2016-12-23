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
--
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( Dim (..), totalDim, order
  , Dim# (..), order#
  , Dimensions (..), SubDimensions (..)
  , (++), Reverse, Take, Drop, Length, IsPrefixOf, IsSuffixOf
  ) where



import GHC.TypeLits
import GHC.Prim
import GHC.Types
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

instance Show (Dim ds) where
  show Z = "}"
  show (i :- xs) = show i ++ ", " ++ show xs



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



class SubDimensions (n :: Nat) (ds :: [Nat]) where
  dropDims  :: Proxy n -> Dim ds -> Dim (Drop n ds)
  takeDims  :: Proxy n -> Dim ds -> Dim (Take n ds)

instance SubDimensions n '[] where
  dropDims _ Z = Z
  {-# INLINE dropDims #-}
  takeDims _ _ = Z
  {-# INLINE takeDims #-}

instance SubDimensions 0 (d ': ds) where
  dropDims _ ds = ds
  {-# INLINE dropDims #-}
  takeDims _ _ = Z
  {-# INLINE takeDims #-}


instance (SubDimensions (n-1) ds, 1 <= n) => SubDimensions n (d ': ds) where
  dropDims _ (_ :- ds) = unsafeCoerce (dropDims (Proxy :: Proxy (n-1)) ds) :: Dim (Drop n (d ': ds))
  {-# INLINE dropDims #-}
  takeDims _ (d :- ds) = unsafeCoerce (d :- dropDims (Proxy :: Proxy (n-1)) ds) :: Dim (Take n (d ': ds))
  {-# INLINE takeDims #-}

--  takeDims _ _ = Z
--  {-# INLINE takeDims #-}

--dropDims :: Proxy n -> Dim ds -> Dim (Drop n ds)
--dropDims _ Z = Z
--dropDims p (d :- ds) = case natVal p of
--     0 -> d :- ds
--     _ -> dropDims p' ds :: Dim (Drop (n-1) ds)
--  where
--    f :: Proxy n -> Proxy (n-1)
--    f _ = Proxy
--    p' = f p

--takeDims :: Dimensions ds => Proxy n -> Dim ds -> Dim (Take n ds)
--takeDims _ Z = Z
--takeDims p (d:-ds) = case natVal p of
--     0 -> Z
--     _ -> d :- unsafeCoerce (takeDims p' ds)
--  where
--    f :: Proxy n -> Proxy (n-1)
--    f _ = Proxy
--    p' = f p

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

--  takeDims p (d:-ds) = d :- (takeDims p' ds)
--    where
--      f :: Proxy n -> Proxy (n-1)
--      f _ = Proxy
--      p' = f p
----      g :: Proxy k -> Dim ds -> Dim (Take k ds)
----      g = takeDims
--  {-# INLINE takeDims #-}


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




type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]        ++ bs = bs
  (a ': as)  ++ bs = a ': (as ++ bs)
infixr 5 ++


type family Reverse (as :: [k]) :: [k] where
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

