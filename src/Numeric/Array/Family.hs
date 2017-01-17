
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilyDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Numeric.Dimensions

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

-- | Indexing over scalars is trivial...
instance ElementWise (Idx ('[] :: [Nat])) t (Scalar t) where
  (!) x _ = _unScalar x
  {-# INLINE (!) #-}
  ewmap f = Scalar . f Z . _unScalar
  {-# INLINE ewmap #-}
  ewgen f = Scalar $ f Z
  {-# INLINE ewgen #-}
  ewfold f x0 x = f Z (_unScalar x) x0
  {-# INLINE ewfold #-}
  elementWise f = fmap Scalar . f . _unScalar
  {-# INLINE elementWise #-}
  indexWise f = fmap Scalar . f Z . _unScalar
  {-# INLINE indexWise #-}

-- * Array implementations.
--   All array implementations have the same structure:
--   Array[Type] (element offset :: Int#) (element length :: Int#)
--                 (content :: ByteArray#)
--   All types can also be instantiated with only a single scalar.

-- | N-Dimensional arrays based on Float# type
data ArrayF (ds :: [Nat]) = ArrayF# Int# Int# ByteArray#
                        | FromScalarF# Float#


_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
