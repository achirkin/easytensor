{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vector
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Vector
  ( Vector
  -- * Type classes
  , VectorCalculus (..), Vector2D (..), Vector3D (..), Vector4D (..)
  -- * Type abbreviations
  , Vec2f, Vec3f, Vec4f
  -- * Common functions on vectors
  , (<:>)
  ) where

import GHC.Base (runRW#)
import GHC.Prim
import GHC.TypeLits

import Numeric.Commons
import Numeric.Vector.Class (VectorCalculus(..), Vector2D (..), Vector3D (..), Vector4D (..))
-- import Numeric.Vector.Family (Vector)
import Numeric.DataFrame


-- Import instances
-- import Numeric.Vector.Base.FloatX2 ()
-- import Numeric.Vector.Base.FloatXN ()


-- Type abbreviations
type Vector t (n :: Nat) = DataFrame t '[n]

type Vec2f = Vector Float 2
type Vec3f = Vector Float 3
type Vec4f = Vector Float 4
