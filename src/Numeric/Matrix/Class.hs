{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix.Class
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix.Class
  ( MatrixCalculus (..)
  , SquareMatrixCalculus (..)
  , Matrix2x2 (..)
  , MatrixProduct (..)
  ) where


import GHC.TypeLits

import Numeric.Commons
import Numeric.Vector.Class
import Numeric.Vector.Family (Vector)
import Numeric.Matrix.Family (Matrix)


class MatrixCalculus v t (n :: Nat) (m :: Nat) | v -> t, v -> n, v -> m, t n m -> v where
    -- | Fill Mat with the same value
    broadcastMat :: t -> v
    -- | Get element by its index
    indexMat :: Int -> Int -> v -> t
    -- | Transpose Mat
    transpose :: (MatrixCalculus w t m n, PrimBytes w) => v -> w
    -- | First dimension size of a matrix
    dimN :: v -> Int
    -- | Second dimension size of a matrix
    dimM :: v -> Int
    -- | Get vector column by its index
    indexCol :: (VectorCalculus w t n, PrimBytes w) => Int -> v -> w
    -- | Get vector row by its index
    indexRow :: (VectorCalculus w t m, PrimBytes w) => Int -> v -> w

class SquareMatrixCalculus v t (n :: Nat) | v -> t, v -> n, t n -> v where
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: v
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: t -> v
    -- | Determinant of  Mat
    det :: v -> t
    -- | Sum of diagonal elements
    trace :: v -> t
    -- | Get the diagonal elements from Mat into Vec
    fromDiag :: VectorCalculus w t n => v -> w
    -- | Set Vec values into the diagonal elements of Mat
    toDiag :: VectorCalculus w t n => w -> v


class Matrix2x2 t where
  -- | Compose a 2x2D matrix
  mat22 :: Vector t 2 -> Vector t 2 -> Matrix t 2 2
  rowsOfM22 :: Matrix t 2 2 -> (Vector t 2, Vector t 2)
  colsOfM22 :: Matrix t 2 2 -> (Vector t 2, Vector t 2)



class MatrixProduct a b c | b c -> a where
  -- | matrix-matrix or matrix-vector product
  prod :: a -> b -> c
