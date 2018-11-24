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

deriving instance ( Eq t, ArraySingleton t n)   => Eq (DataFrame t n)
deriving instance ( Ord t, ArraySingleton t n)  => Ord (DataFrame t n)
deriving instance ( Show t, ArraySingleton t n) => Show (DataFrame t n)
deriving instance ( Num t, ArraySingleton t n)  => Semigroup (DataFrame t n)
deriving instance ( Num t, KnownDim n, ArraySingleton t n)
                                                => Monoid (DataFrame t n)
