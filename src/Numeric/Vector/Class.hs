{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vector.Class
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Vector.Class
  ( VectorCalculus (..)
  , Vector2D (..)
  , Vector3D (..)
  , Vector4D (..)
  ) where


import GHC.TypeLits

import Numeric.Vector.Family (Vector)

class VectorCalculus v t (n :: Nat) | v -> t, v -> n, t n -> v where
    -- | Fill Vec with the same value
    broadcastVec :: t -> v
    -- | Scalar product -- sum of Vecs' components products, propagated into whole Vec
    infixl 7 .*.
    (.*.) :: v -> v -> v
    -- | Scalar product -- sum of Vecs' components products -- a scalar
    dot :: v -> v -> t
    -- | Get element by its index
    indexVec :: Int -> v -> t
    -- | Sum of absolute values
    normL1 :: v -> t
    -- | hypot function (square root of squares)
    normL2 :: v -> t
    -- | Maximum of absolute values
    normLPInf :: v -> t
    -- | Minimum of absolute values
    normLNInf :: v -> t
    -- | Norm in Lp space
    normLP :: Int -> v -> t
    -- | Dimensionality of a vector
    dim :: v -> Int

{-# RULES
"normLP/L1" normLP 1 = normL1
"normLP/L2" normLP 2 = normL2
  #-}

class Vector2D t where
  -- | Compose a 2D vector
  vec2 :: t -> t -> Vector t 2
  -- | Take a determinant of a matrix composed from two 2D vectors.
  --   Like a cross product in 2D.
  det2 :: Vector t 2 -> Vector t 2 -> t


class Vector3D t where
  -- | Compose a 3D vector
  vec3 :: t -> t -> t -> Vector t 3
  -- | Cross product
  cross :: Vector t 3 -> Vector t 3 -> Vector t 3

class Vector4D t where
  -- | Compose a 4D vector
  vec4 :: t -> t -> t -> t -> Vector t 4
