{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
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
import Numeric.Vector.Family (Vector)


-- Import instances
import Numeric.Vector.Base.FloatX2 ()
import Numeric.Vector.Base.FloatXN ()


-- Type abbreviations

type Vec2f = Vector Float 2
type Vec3f = Vector Float 3
type Vec4f = Vector Float 4


-- | Append one vector to another, adding up their dimensionality
(<:>) :: ( PrimBytes (Vector t n)
         , PrimBytes (Vector t m)
         , PrimBytes (Vector t (n+m))
         )
      => Vector t n -> Vector t m -> Vector t (n + m)
a <:> b = case (# toBytes a, toBytes b, byteSize a, byteSize b #) of
  (# arr1, arr2, n, m #) -> case runRW#
     ( \s0 -> case newByteArray# (n +# m) s0 of
         (# s1, marr #) -> case copyByteArray# arr1 0# marr 0# n s1 of
           s2 -> case copyByteArray# arr2 0# marr n m s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> fromBytes r
