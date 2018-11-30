{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fplugin Numeric.DataFrame.TcPlugin #-}

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..), pattern DF
  , SomeDataFrame (..)
  , DFBackend (..)
  , Nat
  ) where


import           Data.Semigroup
import           GHC.Base

import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.Dimensions




pattern DF :: Backend (t :: Type) (n :: Nat) -> DataFrame t n
pattern DF x = (DataFrame (DFBackend x))

data SomeDataFrame (t :: Type) where
  SomeDataFrame :: (KnownDim (n :: Nat), KnownBackend (DataFrame t n))
                => DataFrame t n -> SomeDataFrame t

-- | Single frame
newtype instance DataFrame (t :: Type) (n :: Nat)
  = DataFrame { _getDF :: DFBackend t n (Backend t n) }
type instance DataElemType (DataFrame t n) = t
type instance DataDims (DataFrame t n) = n

-- I need two layers of wrappers to provide default overlappable instances to
-- all type classes using KnownBackend mechanics.
-- Type arguments are redundant here;
-- nevertheless, they improve readability of error messages.
newtype DFBackend (t :: Type) (n :: Nat) (backend :: Type)
  = DFBackend { _getBackend :: backend }
type instance DataElemType (DFBackend t _ _) = t
type instance DataDims (DFBackend _  n _) = n

-- Note, deriving KnownBackend goes in a not intuitive way:
-- DFBackend t n b ==> DataFrame t n ==> Backend t n;
-- this way, I may be able to not expose DFBackend in user error messages.
instance KnownBackend (DataFrame t n) => KnownBackend (DFBackend t n b) where
    bSing = unsafeCoerce# (bSing :: BackendSing (DataFrame t n))
    {-# INLINE bSing #-}

-- this should be generated automatically using a compiler plugin.
deriving instance {-# OVERLAPPING #-} Eq t => Eq (DFBackend t 0 (UnitBase t))
deriving instance {-# OVERLAPPING #-} Eq t => Eq (DFBackend t 1 (ScalarBase t))
deriving instance {-# OVERLAPPING #-} Eq t => Eq (DFBackend t 2 (Vec2Base t))
deriving instance {-# INCOHERENT #-} Eq t => Eq (DFBackend t n (ListBase t n))
deriving instance {-# OVERLAPPING #-} Ord t => Ord (DFBackend t 0 (UnitBase t))
deriving instance {-# OVERLAPPING #-} Ord t => Ord (DFBackend t 1 (ScalarBase t))
deriving instance {-# OVERLAPPING #-} Ord t => Ord (DFBackend t 2 (Vec2Base t))
deriving instance {-# INCOHERENT #-} Ord t => Ord (DFBackend t n (ListBase t n))
deriving instance {-# OVERLAPPING #-} Show t => Show (DFBackend t 0 (UnitBase t))
deriving instance {-# OVERLAPPING #-} Show t => Show (DFBackend t 1 (ScalarBase t))
deriving instance {-# OVERLAPPING #-} Show t => Show (DFBackend t 2 (Vec2Base t))
deriving instance {-# INCOHERENT #-} Show t => Show (DFBackend t n (ListBase t n))
deriving instance {-# OVERLAPPING #-} Num t => Semigroup (DFBackend t 0 (UnitBase t))
deriving instance {-# OVERLAPPING #-} Num t => Semigroup (DFBackend t 1 (ScalarBase t))
deriving instance {-# OVERLAPPING #-} Num t => Semigroup (DFBackend t 2 (Vec2Base t))
deriving instance {-# INCOHERENT #-} Num t => Semigroup (DFBackend t n (ListBase t n))
deriving instance {-# OVERLAPPING #-} Num t => Monoid (DFBackend t 0 (UnitBase t))
deriving instance {-# OVERLAPPING #-} Num t => Monoid (DFBackend t 1 (ScalarBase t))
deriving instance {-# OVERLAPPING #-} Num t => Monoid (DFBackend t 2 (Vec2Base t))
deriving instance {-# INCOHERENT #-} (Num t, KnownDim n) => Monoid (DFBackend t n (ListBase t n))
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), KnownBackend b, Eq t ) => Eq (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), KnownBackend b, Ord t ) => Ord (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), KnownBackend b, Show t ) => Show (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), KnownBackend b, Num t ) => Semigroup (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), KnownBackend b, Num t, KnownDim n) => Monoid (DFBackend t n b)
deriving instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Eq t ) => Eq (DFBackend t n b)
deriving instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Ord t ) => Ord (DFBackend t n b)
deriving instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Show t ) => Show (DFBackend t n b)
deriving instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Num t ) => Semigroup (DFBackend t n b)
deriving instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Num t, KnownDim n) => Monoid (DFBackend t n b)

instance KnownBackend (Backend t n) => KnownBackend (DataFrame t n) where
  bSing = unsafeCoerce# (bSing :: BackendSing (Backend t n))
  {-# INLINE bSing #-}
deriving instance Eq (DFBackend t n (Backend t n)) => Eq (DataFrame t n)
deriving instance Ord (DFBackend t n (Backend t n)) => Ord (DataFrame t n)
deriving instance Show (DFBackend t n (Backend t n)) => Show (DataFrame t n)
deriving instance Semigroup (DFBackend t n (Backend t n)) => Semigroup (DataFrame t n)
deriving instance Monoid (DFBackend t n (Backend t n)) => Monoid (DataFrame t n)
