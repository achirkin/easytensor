{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Numeric.Matrix.QR
  ( QR (..), LQ (..), MatrixQR (..)
  ) where


import Control.Monad
import Control.Monad.ST
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
import Numeric.Scalar.Internal
import Numeric.Subroutine.Householder

-- | Result of QR factorization
--   \( A = QR \).
data QR t n m
  = QR
  { qrQ    :: Matrix t n n
    -- ^ Orthogonal matrix \( Q \)
  , qrQDet :: Scalar t
    -- ^ A shortcut for evaluating a determinant of \( |Q| = \pm 1 \)
  , qrR    :: Matrix t n m
    -- ^ Upper-triangular matrix \( R \)
  }

-- | Result of LQ factorization
--   \( A = QR \).
data LQ t n m
  = LQ
  { lqL    :: Matrix t n m
      -- ^ Lower-triangular matrix \( L \)
  , lqQ    :: Matrix t m m
    -- ^ Orthogonal matrix \( Q \)
  , lqQDet :: Scalar t
    -- ^ A shortcut for evaluating a determinant of \( |Q| = \pm 1 \)
  }

deriving instance ( Eq t, PrimBytes t
                  , KnownDim n, KnownDim m) => Eq (QR t n m)
deriving instance ( Show t, PrimBytes t
                  , KnownDim n, KnownDim m) => Show (QR t n m)
deriving instance ( Eq t, PrimBytes t
                  , KnownDim n, KnownDim m) => Eq (LQ t n m)
deriving instance ( Show t, PrimBytes t
                  , KnownDim n, KnownDim m) => Show (LQ t n m)


class (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
      => MatrixQR t (n :: Nat) (m :: Nat) where
    -- | Compute QR factorization
    qr :: Matrix t n m -> QR t n m
    -- | Compute LQ factorization
    lq :: Matrix t n m -> LQ t n m

instance (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
      => MatrixQR t (n :: Nat) (m :: Nat) where
    qr a
      | lim == 0 = QR undefined undefined 1
      | otherwise = runST $ do
        uPtr <- newDataFrame
        pPtr <- unsafeThawDataFrame eye
        rPtr <- thawDataFrame a
        detNegative <-
          let f x i = (x /=) <$> householderReflectionInplaceL
                                   uPtr pPtr rPtr (Idx i :* Idx i :* U)
          in  foldM f False [0..lim - 1]
        qrR <- unsafeFreezeDataFrame rPtr
        qrQ <- unsafeFreezeDataFrame pPtr
        let qrQDet = if detNegative then -1 else 1
        return QR {..}
      where
        n = dimVal' @n
        m = dimVal' @m
        lim = min n m
    lq a
      | lim == 0 = LQ undefined undefined 1
      | otherwise = runST $ do
        uPtr <- newDataFrame
        pPtr <- unsafeThawDataFrame eye
        lPtr <- thawDataFrame a
        detNegative <-
          let f x i = (x /=) <$> householderReflectionInplaceR
                                   uPtr pPtr lPtr (Idx i :* Idx i :* U)
          in  foldM f False [0..lim - 1]
        lqL <- unsafeFreezeDataFrame lPtr
        -- WARNING! todo: get rid of transpose.
        lqQ <- transpose <$> unsafeFreezeDataFrame pPtr
        let lqQDet = if detNegative then -1 else 1
        return LQ {..}
      where
        n = dimVal' @n
        m = dimVal' @m
        lim = min n m

-- {- |
-- Given a QR decomposition of matrix \( A : n \times m \),
-- solve a system of linear equations \( Ax = b \).
--
-- If \( n > m \) this function solves linear least squares problem.
--  -}
-- qrSolveR :: MatrixQR t n m => QR t n m -> Vector t m -> Vector t n
