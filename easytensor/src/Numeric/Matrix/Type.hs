{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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

import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions     (Nat)
import           Numeric.Scalar

-- | Alias for DataFrames of rank 2
type Matrix t (n :: Nat) (m :: Nat) = DataFrame t '[n,m]

class MatrixCalculus t (n :: Nat) (m :: Nat) where
    -- | Transpose Mat
    transpose :: (MatrixCalculus t m n, PrimBytes (Matrix t m n)) => Matrix t n m -> Matrix t m n


class SquareMatrixCalculus t (n :: Nat) where
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: Matrix t n n
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: Scalar t -> Matrix t n n
    -- | Determinant of  Mat
    det :: Matrix t n n -> Scalar t
    -- | Sum of diagonal elements
    trace :: Matrix t n n -> Scalar t

class MatrixInverse t (n :: Nat) where
  -- | Matrix inverse
  inverse :: DataFrame t '[n,n] -> DataFrame t '[n,n]
