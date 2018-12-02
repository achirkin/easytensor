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
{-# LANGUAGE RoleAnnotations       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

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
import           Numeric.DataFrame.DFBackend



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


instance KnownBackend (Backend t n) => KnownBackend (DataFrame t n) where
  bSing = unsafeCoerce# (bSing :: BackendSing (Backend t n))
  {-# INLINE bSing #-}
deriving instance Eq (DFBackend t n (Backend t n)) => Eq (DataFrame t n)
deriving instance Ord (DFBackend t n (Backend t n)) => Ord (DataFrame t n)
deriving instance Show (DFBackend t n (Backend t n)) => Show (DataFrame t n)
deriving instance Semigroup (DFBackend t n (Backend t n)) => Semigroup (DataFrame t n)
deriving instance Monoid (DFBackend t n (Backend t n)) => Monoid (DataFrame t n)
