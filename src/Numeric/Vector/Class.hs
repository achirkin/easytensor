{-# LANGUAGE MultiParamTypeClasses #-}
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
  ) where


import Numeric.Vector.Family (Vector)

class VectorCalculus t n where
    -- | Fill Vec with the same value
    broadcastVec :: t -> Vector t n
    -- | Scalar product -- sum of Vecs' components products, propagated into whole Vec
    infixl 7 .*.
    (.*.) :: Vector t n -> Vector t n -> Vector t n
    -- | Scalar product -- sum of Vecs' components products -- a scalar
    dot :: Vector t n -> Vector t n -> t
    -- | Get element by its index
    indexVec :: Int -> Vector t n -> t
    -- | Sum of absolute values
    normL1 :: Vector t n -> t
    -- | hypot function (square root of squares)
    normL2 :: Vector t n -> t
    -- | Maximum of absolute values
    normLPInf :: Vector t n -> t
    -- | Minimum of absolute values
    normLNInf :: Vector t n -> t
    -- | Norm in Lp space
    normLP :: Int -> Vector t n -> t
    -- | Dimensionality of a vector
    dim :: Vector t n -> Int
