{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Numeric.Dimensions.Plugin.AtLeast
  ( AtLeast (..)
  , cons, mergeDesc, flattenDesc, toList
  , Nat (..), None, One, Two
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable       (toList)


data Nat = Nil | S Nat
type None = 'Nil
type One  = 'S None
type Two  = 'S One

data AtLeast n a where
  L    :: [a] -> AtLeast None a
  (:|) :: a -> AtLeast n a -> AtLeast ('S n) a

infixr 5 :|

cons :: a -> AtLeast n a -> AtLeast n a
cons x (L xs)    = L (x:xs)
cons x (a :| as) = x :| cons a as

-- | @O(n + m)@ Merge two decreasing lists into one.
mergeDesc :: Ord a => AtLeast n a -> AtLeast n a -> AtLeast n a
mergeDesc xs (L []) = xs
mergeDesc (L []) xs = xs
mergeDesc (L (a:as)) (L (b:bs)) = case compare a b of
  GT -> cons a $ mergeDesc (L as) (L (b:bs))
  EQ -> cons a $ mergeDesc (L as) (L bs)
  LT -> cons b $ mergeDesc (L (a:as)) (L bs)
mergeDesc (a :| as) (b :| bs) = case compare a b of
  GT -> a :| mergeDesc as (cons b bs)
  EQ -> a :| mergeDesc as bs
  LT -> b :| mergeDesc (cons a as) bs

flattenDesc :: Ord a => AtLeast n (AtLeast n a) -> AtLeast n a
flattenDesc (L []) = L []
flattenDesc l      = case l of
    L []       -> L []
    L (x : xs) -> go (x :| L xs)
    (x :| xs)  -> go (x :| L (toList xs))
  where
    go :: Ord a => AtLeast One (AtLeast n a) -> AtLeast n a
    go (x  :| L [])      = x
    go (x1 :| L (x2:xs)) = go (mergeDesc x1 x2 :| L xs)

deriving instance Eq a => Eq (AtLeast k a)
deriving instance Ord a => Ord (AtLeast k a)
deriving instance Show a => Show (AtLeast k a)

instance Functor (AtLeast n) where
  fmap f (L xs)    = L $ fmap f xs
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative (AtLeast None) where
  pure a = L [a]
  (L f) <*> (L a) = L (f <*> a)
  liftA2 = liftM2

instance Monad (AtLeast None) where
  L a >>= f = L $ a >>= (\x -> case f x of L cs -> cs)

instance Applicative (AtLeast One) where
  pure a = a :| L []
  (<*>) = ap
  liftA2 = liftM2

instance Monad (AtLeast One)  where
  (a :| L as) >>= f = b :| L (bs ++ bs')
    where
      b :| L bs = f a
      bs' = as >>= (\x -> case f x of (c :| L cs) -> c : cs)

instance Foldable (AtLeast n) where
  foldr k x (L as)    = foldr k x as
  foldr k x (a :| as) = k a (foldr k x as)
  foldl k x (L as)    = foldl k x as
  foldl k x (a :| as) = foldl k (k x a) as
  toList (L xs)    = xs
  toList (x :| xs) = x : toList xs

instance Traversable (AtLeast n) where
  traverse k (L as)    = L <$> traverse k as
  traverse k (a :| as) = liftA2 (:|) (k a) (traverse k as)

instance Semigroup (AtLeast n a) where
  L as <> L bs = L (as <> bs)
  (a :| as) <> (b :| bs) = a :| as <> cons b bs

instance Monoid (AtLeast None a) where
  mempty = L []
