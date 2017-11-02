{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Semigroup
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Re-export most of "Data.Semigroup" with a few changes
--  and new definitions.
--
-- The main initiative behind this module is to provide more strict
-- alternatives to widely used semigroups.
-- For example, 'Data.Semigroup.Option' has lazy @(\<\>)@ implementation,
-- which causes memory leaks in large foldMaps.
--
-----------------------------------------------------------------------------
module Numeric.Semigroup
    ( Semigroup(..)
    , stimesMonoid
    , stimesIdempotent
    , stimesIdempotentMonoid
    , mtimesDefault
    , foldMap'
    -- * Semigroups
    , Min(..)
    , Max(..)
    , First(..)
    , Last(..)
    , WrappedMonoid(..)
    -- * Re-exported monoids from Data.Monoid
    , Monoid(..)
    , Dual(..)
    , Endo(..)
    , All(..)
    , Any(..)
    , Sum(..)
    , Product(..)
    -- * A better monoid for Maybe
    , Option (..)
    , option, fromOption, toOption
    -- * Difference lists of a semigroup
    , diff
    , cycle1
    -- * ArgMin, ArgMax
    , Arg(..)
    , ArgMin
    , ArgMax
    , MinMax (..)
    , minMax, mmDiff, mmAvg
    ) where

import Data.Semigroup hiding (Option (..), option)
import Data.Foldable (foldl')
import Data.Data
import Control.Applicative
import Control.Monad.Fix
import GHC.Generics

-- | 'Option' is effectively 'Maybe' with a better instance of
-- 'Monoid', built off of an underlying 'Semigroup' instead of an
-- underlying 'Monoid'.
--
-- This version of 'Option' data type is more strict than the one from
-- "Data.Semigroup".
newtype Option a = Option { getOption :: Maybe a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1
           , Functor, Alternative, Applicative, Monad, MonadFix)

-- | Fold an 'Option' case-wise, just like 'maybe'.
--   Eagerly evaluates the value before returning!
option :: b -> (a -> b) -> Option a -> b
option !b _ (Option Nothing)   = b
option _  f (Option (Just !a)) = f a

-- | Get value from 'Option' with default value.
--   Eagerly evaluates the value before returning!
fromOption :: a -> Option a -> a
fromOption !a (Option Nothing)  = a
fromOption _ (Option (Just !a)) = a

-- | Wrap a value into 'Option' container.
--   Eagerly evaluates the value before wrapping!
toOption :: a -> Option a
toOption !a = Option (Just a)


instance Foldable Option where
  foldMap _ (Option Nothing)   = mempty
  foldMap f (Option (Just !a)) = f a

instance Traversable Option where
  traverse _ (Option Nothing)    = pure (Option Nothing)
  traverse f (Option (Just !a))  = (\ !b -> Option (Just b)) <$> f a

instance Semigroup a => Semigroup (Option a) where
  Option Nothing   <> Option Nothing   = Option Nothing
  Option Nothing   <> Option (Just !a) = Option (Just a)
  Option (Just !a) <> Option Nothing   = Option (Just a)
  Option (Just !a) <> Option (Just !b) = Option (Just $! a <> b)

  stimes _ (Option Nothing) = Option Nothing
  stimes n (Option (Just a)) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Option, negative multiplier"
    EQ -> Option Nothing
    GT -> Option (Just $! stimes n a)

instance Semigroup a => Monoid (Option a) where
  mappend = (<>)
  mempty  = Option Nothing



-- | Evaluate minimum and maximum at the same time.
--   Arithmetics and semigroup operations are eager,
--   functorial operations are lazy.
--
--   This data type is especially useful for calculating bounds
--   of foldable containers with numeric data using @foldMap minMax@.
data MinMax a = MinMax a a
  deriving (Eq, Read, Show, Data, Typeable, Generic, Generic1)

minMax :: a -> MinMax a
minMax !a = MinMax a a

mmDiff :: Num a => MinMax a -> a
mmDiff (MinMax !x !y) = y - x

mmAvg :: Fractional a => MinMax a -> a
mmAvg (MinMax !x !y) = 0.5 * (x+y)

strictMinMax :: a -> a -> MinMax a
strictMinMax !a !b = MinMax a b

instance Ord a => Semigroup (MinMax a) where
  (MinMax !x1 !y1) <> (MinMax !x2 !y2) = strictMinMax (min x1 x2) (max y1 y2)

  -- 'MinMax' is idempotent.
  -- Also we don't care if @n@ is not positive.
  stimes _ (MinMax !x !y) = MinMax x y



instance (Ord a, Bounded a) => Monoid (MinMax a) where
  -- | Empty instance of minmax is an invalid value @min >= max@.
  --   However, this gives a good monoid append behavior.
  mempty = strictMinMax maxBound minBound
  mappend = (<>)


instance Functor MinMax where
  fmap f (MinMax a b) = MinMax (f a) (f b)

instance Applicative MinMax where
  pure a = MinMax a a
  MinMax f g <*> MinMax a b = MinMax (f a) (g b)

instance Monad MinMax where
  return = pure
  MinMax a b >>= m = case (m a, m b) of
      (MinMax x _, MinMax _ y) -> MinMax x y

instance MonadFix MinMax where
  mfix mf = let MinMax x _ = mf x
                MinMax _ y = mf y
            in MinMax x y

instance Bounded a => Bounded (MinMax a) where
  minBound = strictMinMax minBound minBound
  maxBound = strictMinMax maxBound maxBound

-- | MinMax checks whether bounds overlap.
--
--    * Strict inequality means that intervals do not overlap.
--    * Non-strict inequality means non-strict inequality in both constructor arguments.
--    * `EQ` means intervals overlap
instance Ord a => Ord (MinMax a) where
  MinMax _ amax < MinMax bmin _ = amax < bmin
  MinMax amin _ > MinMax _ bmax = amin > bmax
  MinMax amin amax <= MinMax bmin bmax = amin <= bmin && amax <= bmax
  MinMax amin amax >= MinMax bmin bmax = amin >= bmin && amax >= bmax
  -- |  A contraversal decision was made here: implementing `compare` operation.
  --    `compare` returns GT or LT only if bounds do not overlap and returns EQ otherwise.
  compare (MinMax amin amax) (MinMax bmin bmax)
    | amin > bmax = GT
    | bmin > amax = LT
    | otherwise   = EQ
  min (MinMax amin amax) (MinMax bmin bmax) = strictMinMax (min amin bmin) (min amax bmax)
  max (MinMax amin amax) (MinMax bmin bmax) = strictMinMax (max amin bmin) (max amax bmax)

instance (Num a, Ord a) => Num (MinMax a) where
  (MinMax !x1 !y1) + (MinMax !x2 !y2) = strictMinMax (x1+x2) (y1+y2)
  (MinMax !x1 !y1) - (MinMax !x2 !y2) = strictMinMax (x1-y2) (y1-x2)
  (MinMax !x1 !y1) * (MinMax !x2 !y2) = strictMinMax (x1*x2) (y1*y2)
  abs (MinMax !x !y) = case (abs x, abs y) of
      (!ax, !ay) -> strictMinMax (min ax ay) (max ax ay)
  negate (MinMax !x !y) = strictMinMax (negate y) (negate x)
  signum (MinMax !x !y) = strictMinMax (signum x) (signum y)
  fromInteger = minMax . fromInteger


-- | Map each element of the structure to a monoid,
--   and combine the results.
--
--   This function differs from @Data.Foldable.foldMap@ in that uses @foldl'@
--   instead of @foldr@ inside.
--   This makes this function suitable for Monoids with strict `mappend` operation.
--   For example,
--
--   > foldMap' Sum $ take 1000000000 ([1..] :: [Int])
--
--   runs in constant memory, whereas normal @foldMap@ would cause a memory leak there.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldl' (flip $ mappend . f) mempty
{-# INLINE foldMap' #-}
