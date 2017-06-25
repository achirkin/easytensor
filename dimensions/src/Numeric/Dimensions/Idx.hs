{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Idx
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type Idx that enumerates through multiple dimensions.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
-- 
-----------------------------------------------------------------------------

module Numeric.Dimensions.Idx
  ( -- * Data types
    Idx (..)
  , appendIdx, splitIdx
  ) where


import           Control.Arrow           (first)
import           GHC.Exts                (IsList (..))
import           Unsafe.Coerce           (unsafeCoerce)

import           Numeric.Dimensions.Dim
import           Numeric.Dimensions.List



-- | Type-level dimensional indexing with arbitrary Int values inside
data Idx (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Idx '[]
   -- | List-like concatenation of indices
   (:!) :: {-# UNPACK #-} !Int -> !(Idx ds) -> Idx (d ': ds)
infixr 5 :!

idxToList :: Idx ds -> [Int]
idxToList Z         = []
idxToList (x :! xs) = x : idxToList xs

idxFromList :: [Int] -> Idx ds
idxFromList []     = unsafeCoerce Z
idxFromList (x:xs) = unsafeCoerce $ x :! unsafeCoerce (idxFromList xs)

succIdx :: Dim xs -> Idx xs -> Idx xs
succIdx _ Z = Z
succIdx ((Dn :: Dim d) :* ds) (i :! is) | i >= dimVal' @d = 1 :! succIdx ds is
                                        | otherwise       = succ i :! is
{-# INLINE succIdx #-}

predIdx :: Dim xs -> Idx xs -> Idx xs
predIdx _ Z = Z
predIdx ((Dn :: Dim d) :* ds) (i :! is) | i <= 1    = dimVal' @d :! predIdx ds is
                                        | otherwise = pred i :! is
{-# INLINE predIdx #-}

-- | Convert zero-based offset into Idx in a given space
toIdx :: Dim xs -> Int -> Idx xs
toIdx D _ = Z
toIdx ((Dn :: Dim d) :* ds) off = case divMod off (dimVal' @d) of
      (off', i) -> i+1 :! toIdx ds off'

-- | Get zero-based offset of current index
fromIdx :: Dim xs -> Idx xs -> Int
fromIdx _ Z                             = 0
fromIdx ((Dn :: Dim d) :* ds) (i :! is) = i - 1 + dimVal' @d * fromIdx ds is

-- | Offset difference of two indices (first idx - second idx)
diffIdx :: Dim xs -> Idx xs -> Idx xs -> Int
diffIdx _ Z _ = 0
diffIdx ((Dn :: Dim d) :* ds) (i1:!is1) (i2:!is2) = i1 - i2
          + dimVal' @d * diffIdx ds is1 is2

-- | Step dimension index by an Integer offset
stepIdx :: Dim ds -> Int -> Idx ds -> Idx ds
stepIdx _ _ Z = Z
stepIdx ((Dn :: Dim d) :* ds) di (i:!is)
      = case divMod (di + i - 1) (dimVal' @d) of
         (0  , i') -> i'+1 :! is
         (di', i') -> i'+1 :! stepIdx ds di' is
{-# INLINE stepIdx #-}

-- | Append index dimension
appendIdx :: forall (as :: [Nat]) (b :: Nat)
           . Idx as -> Int -> Idx (as +: b)
appendIdx Z i = i :! Z
appendIdx (j :! js) i = unsafeCoerce $ j :! (unsafeCoerce (appendIdx js i) :: Idx (Tail (as +: b)))
{-# INLINE appendIdx #-}

-- | Split index into prefix and suffix dimensioned indices
splitIdx :: forall (as :: [Nat]) (bs :: [Nat])
          . FiniteList as => Idx (as ++ bs) -> (Idx as, Idx bs)
splitIdx = splitN (order @_ @as)
  where
    splitN :: Int -> Idx (as ++ bs) -> (Idx as, Idx bs)
    splitN 0 js = unsafeCoerce (Z, js)
    splitN n (j :! js) = first (unsafeCoerce . (j :!))
                       $ splitN (n-1) (unsafeCoerce js)
    splitN _ Z  = unsafeCoerce (Z, Z)
{-# INLINE splitIdx #-}


instance Show (Idx ds) where
    show Z  = "Idx Ø"
    show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (idxToList xs)

instance Eq (Idx ds) where
    Z == Z = True
    (a:!as) == (b:!bs) = a == b && as == bs
    Z /= Z = False
    (a:!as) /= (b:!bs) = a /= b || as /= bs


-- | With this instance we can slightly reduce indexing expressions
--   e.g. x ! (1 :! 2 :! 4) == x ! (1 :! 2 :! 4 :! Z)
instance Num (Idx '[n]) where
    (a:!Z) + (b:!Z) = (a+b) :! Z
    (a:!Z) - (b:!Z) = (a-b) :! Z
    (a:!Z) * (b:!Z) = (a*b) :! Z
    signum (a:!Z)   = signum a :! Z
    abs (a:!Z)      = abs a :! Z
    fromInteger i   = fromInteger i :! Z


instance Ord (Idx ds) where
    compare Z Z             = EQ
    compare (a:!as) (b:!bs) = compare as bs `mappend` compare a b

instance Dimensions ds => Bounded (Idx ds) where
    maxBound = f (dim @ds)
      where
        f :: forall ns . Dim ns -> Idx ns
        f D                     = Z
        f ((Dn :: Dim n) :* ds) = dimVal' @n :! f ds
    {-# INLINE maxBound #-}
    minBound = f (dim @ds)
      where
        f :: forall (ns :: [Nat]) . Dim ns -> Idx ns
        f D          = Z
        f (Dn :* ds) = 1 :! f ds
    {-# INLINE minBound #-}

instance IsList (Idx ds) where
    type Item (Idx ds) = Int
    -- | Very unsafe way to convert Haskell list into Idx.
    --   If the length of a list is not equal to the length of type-level
    --   dimensions, behavior is undefined (going to crash likely).
    fromList = idxFromList
    toList = idxToList

instance Dimensions ds => Enum (Idx ds) where
    succ = succIdx (dim @ds)
    {-# INLINE succ #-}
    pred = predIdx (dim @ds)
    {-# INLINE pred #-}
    toEnum = toIdx (dim @ds)
    {-# INLINE toEnum #-}
    fromEnum = fromIdx (dim @ds)
    {-# INLINE fromEnum #-}
    enumFrom x = take (diffIdx ds maxBound x + 1) $ iterate (succIdx ds) x
      where
        ds = dim @ds
    {-# INLINE enumFrom #-}
    enumFromTo x y | x >= y    = take (diffIdx ds x y + 1) $ iterate (predIdx ds) x
                   | otherwise = take (diffIdx ds y x + 1) $ iterate (succIdx ds) x
      where
        ds = dim @ds
    {-# INLINE enumFromTo #-}
    enumFromThen x x' = take n $ iterate (stepIdx ds dn) x
      where
        ds = dim @ds
        dn = diffIdx ds x' x
        n  = 1 + if dn == 0 then 0
                            else if dn > 0 then diffIdx ds maxBound x `div` dn
                                           else diffIdx ds x minBound `div` negate dn
    {-# INLINE enumFromThen #-}
    enumFromThenTo x x' y = take n $ iterate (stepIdx ds dn) x
      where
        ds = dim @ds
        dn = diffIdx ds x' x
        n  = 1 + if dn == 0 then 0
                            else diffIdx ds y x `div` dn
    {-# INLINE enumFromThenTo #-}
