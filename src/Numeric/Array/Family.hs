{-# LANGUAGE GADTs, TypeInType #-}
{-# LANGUAGE KindSignatures, DataKinds, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family
  ( Array
  , ArrayF (..)
  ) where

-- import GHC.TypeLits
import GHC.Prim
import Numeric.Commons

-- | A wrapper on top of ArrayType type family
--   to eliminate any possible ambiguity.
newtype Array t (ds :: [k]) = Array (ArrayType t ds)

instance Show (ArrayType t ds) => Show (Array t ds) where
  show (Array t) = show t

deriving instance Bounded (ArrayType t ds) => Bounded (Array t ds)
deriving instance Enum (ArrayType t ds) => Enum (Array t ds)
deriving instance Eq (ArrayType t ds) => Eq (Array t ds)
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


type family ArrayType t (ds :: [k]) = v | v -> t ds where
  ArrayType t     ('[] :: [k]) = Scalar k t
  ArrayType Float  ds          = ArrayF ds


newtype Scalar k t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes, FloatBytes, DoubleBytes, IntBytes, WordBytes)
instance Show t => Show (Scalar k t) where
  show (Scalar t) = "{ " ++ show t ++ " }"


-- * Array implementations.
--   All array implementations have the same structure:
--   Array[Type] (element offset :: Int#) (element length :: Int#)
--                 (content :: ByteArray#)
--   All types can also be instantiated with only a single scalar.

-- | N-Dimensional arrays based on Float# type
data ArrayF (ds :: [k]) = ArrayF# Int# Int# ByteArray#
                        | FromScalarF# Float#
