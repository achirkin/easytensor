{-# LANGUAGE MagicHash, DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix.Type
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix.Type
  ( MatrixCalculus (..)
  , SquareMatrixCalculus (..)
  , MatrixInverse (..)
  , Matrix
  ) where

import GHC.Types

import Numeric.Commons
import Numeric.DataFrame.Type
import Numeric.Scalar


type Matrix t (n :: Nat) (m :: Nat) = DataFrame t '[n,m]

class MatrixCalculus t (n :: Nat) (m :: Nat) where
    -- -- | Fill Mat with the same value
    -- broadcastMat :: t -> v
    -- -- | Get element by its index
    -- indexMat :: Int -> Int -> v -> t
    -- | Transpose Mat
    transpose :: (MatrixCalculus t m n, PrimBytes (Matrix t m n)) => Matrix t n m -> Matrix t m n
    -- -- | First dimension size of a matrix
    -- dimN :: v -> Int
    -- -- | Second dimension size of a matrix
    -- dimM :: v -> Int
    -- -- | Get vector column by its index
    -- indexCol :: (VectorCalculus t n w, PrimBytes w) => Int -> v -> w
    -- -- | Get vector row by its index
    -- indexRow :: (VectorCalculus t m w, PrimBytes w) => Int -> v -> w

class SquareMatrixCalculus t (n :: Nat) where
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: Matrix t n n
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: Scalar t -> Matrix t n n
    -- | Determinant of  Mat
    det :: Matrix t n n -> t
    -- | Sum of diagonal elements
    trace :: Matrix t n n -> Scalar t
    -- -- | Get the diagonal elements from Mat into Vec
    -- fromDiag :: (VectorCalculus t n w, PrimBytes w) => v -> w
    -- -- | Set Vec values into the diagonal elements of Mat
    -- toDiag :: (VectorCalculus t n w, PrimBytes w) => w -> v

class MatrixInverse t (n :: Nat) where
  inverse :: DataFrame t '[n,n] -> DataFrame t '[n,n]
