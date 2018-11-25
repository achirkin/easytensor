{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fplugin Numeric.DataFrame.TcPlugin #-}

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..)
  , SomeDataFrame (..)
  ) where


import           Data.Semigroup
import           GHC.Base

import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.Dimensions


data SomeDataFrame (t :: Type) where
  SomeDataFrame :: (KnownDim n, ArraySingleton t n)
                => DataFrame t n -> SomeDataFrame t

-- | Single frame
newtype DataFrame (t :: Type) (n :: Nat)
  = DataFrame { _getDF :: Array t n }

deriving instance ( Eq t0, ArraySingleton t0 n) => Eq (DataFrame t0 n)
deriving instance ( Ord z, ArraySingleton z k)  => Ord (DataFrame z k)
deriving instance ( Show t, ArraySingleton t n) => Show (DataFrame t n)
deriving instance ( Num t, ArraySingleton t f)  => Semigroup (DataFrame t f)
deriving instance ( Num t, KnownDim n, ArraySingleton t n)
                                                => Monoid (DataFrame t n)
