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
    ) where

import Data.Semigroup hiding (Option (..), option)
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
