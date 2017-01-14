{-# LANGUAGE DataKinds, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
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
  ( ArrayType
  , ArrayF (..)
  ) where

import GHC.TypeLits (Nat)
import GHC.Prim
import Numeric.Commons


type family ArrayType t (ds :: [Nat]) = v | v -> t ds where
  ArrayType t     '[] = Scalar t
  ArrayType Float (d ': ds)    = ArrayF (d ': ds)


-- | Specialize scalar type without any arrays
newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes, FloatBytes, DoubleBytes, IntBytes, WordBytes)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"


-- * Array implementations.
--   All array implementations have the same structure:
--   Array[Type] (element offset :: Int#) (element length :: Int#)
--                 (content :: ByteArray#)
--   All types can also be instantiated with only a single scalar.

-- | N-Dimensional arrays based on Float# type
data ArrayF (ds :: [Nat]) = ArrayF# Int# Int# ByteArray#
                        | FromScalarF# Float#
