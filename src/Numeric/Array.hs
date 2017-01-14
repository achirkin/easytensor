{-# LANGUAGE GADTs, TypeInType #-}
{-# LANGUAGE KindSignatures, DataKinds, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnboxedTuples    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array
  ( Array
  ) where

import GHC.TypeLits (Nat)
import Numeric.Commons
import Numeric.Dimensions
import Numeric.Array.Base.ArrayF ()
import Numeric.Array.Family

-- | A wrapper on top of ArrayType type family
--   to eliminate any possible ambiguity.
newtype Array t (ds :: [Nat]) = Array {_unArray :: ArrayType t ds }

instance Show t => Show (Array t '[]) where
  show (Array t) = show t
instance ( Dimensions (d ': ds)
         , Dimensions (Take 2 (d ': ds))
         , Dimensions (Drop 2 (d ': ds))
         ) => Show (Array Float (d ': ds :: [Nat])) where
  show (Array t) = show t


deriving instance Bounded (ArrayType t ds) => Bounded (Array t ds)
deriving instance Enum (ArrayType t ds) => Enum (Array t ds)

deriving instance {-# OVERLAPPABLE #-} Eq (ArrayType t ds) => Eq (Array t ds)
deriving instance {-# OVERLAPPING #-} Eq t => Eq (Array t '[])
deriving instance {-# OVERLAPPING #-} Eq (Array Float (d ': ds))


deriving instance Integral (ArrayType t ds) => Integral (Array t ds)
deriving instance Num (ArrayType t ds) => Num (Array t ds)
deriving instance Fractional (ArrayType t ds) => Fractional (Array t ds)
deriving instance Floating (ArrayType t ds) => Floating (Array t ds)
deriving instance Ord (ArrayType t ds) => Ord (Array t ds)
deriving instance Read (ArrayType t ds) => Read (Array t ds)
deriving instance Real (ArrayType t ds) => Real (Array t ds)
deriving instance RealFrac (ArrayType t ds) => RealFrac (Array t ds)
deriving instance RealFloat (ArrayType t ds) => RealFloat (Array t ds)
deriving instance PrimBytes (ArrayType t ds) => PrimBytes (Array t ds)
deriving instance FloatBytes (ArrayType t ds) => FloatBytes (Array t ds)
deriving instance DoubleBytes (ArrayType t ds) => DoubleBytes (Array t ds)
deriving instance IntBytes (ArrayType t ds) => IntBytes (Array t ds)
deriving instance WordBytes (ArrayType t ds) => WordBytes (Array t ds)
