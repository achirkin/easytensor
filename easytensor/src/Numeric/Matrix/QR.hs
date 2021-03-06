{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.Matrix.QR
  ( QR (..), LQ (..), MatrixQR (..)
  , detViaQR, inverseViaQR
  , qrSolveR, qrSolveL
  ) where


import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.Basics
import Numeric.DataFrame.ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
import Numeric.Scalar.Internal
import Numeric.Subroutine.Householder
import Numeric.Subroutine.SolveTriangular

import Unsafe.Coerce (unsafeCoerce)

-- | Result of QR factorization
--   \( A = QR \).
data QR (t :: Type) (n :: Nat) (m :: Nat)
  = QR
  { qrQ    :: Matrix t n n
    -- ^ Orthogonal matrix \( Q \)
  , qrQDet :: Scalar t
    -- ^ A shortcut for evaluating a determinant of \( |Q| = \pm 1 \)
  , qrR    :: Matrix t n m
    -- ^ Upper-triangular matrix \( R \)
  }

-- | Result of LQ factorization
--   \( A = LQ \).
data LQ (t :: Type) (n :: Nat) (m :: Nat)
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


class (PrimBytes t, Ord t, Epsilon t, KnownDim n, KnownDim m)
      => MatrixQR t (n :: Nat) (m :: Nat) where
    -- | Compute QR factorization
    qr :: Matrix t n m -> QR t n m
    -- | Compute LQ factorization
    lq :: Matrix t n m -> LQ t n m

instance (PrimBytes t, Ord t, Epsilon t, KnownDim n, KnownDim m)
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

-- | Calculate determinant of a matrix via QR decomposition
detViaQR :: forall t n . MatrixQR t n n => Matrix t n n -> Scalar t
detViaQR m = foldl (\x off -> scalar (ixOff off qrR) * x) qrQDet [0,n+1..n*n]
  where
    n = fromIntegral (dimVal' @n) :: Int
    QR {..} = qr m

-- | Calculate inverse of a matrix via QR decomposition
inverseViaQR :: forall t n . MatrixQR t n n => Matrix t n n -> Matrix t n n
inverseViaQR = (`qrSolveR` eye)

{- |
Compute a QR or LQ decomposition of matrix \( A : n \times m \),
and solve a system of linear equations \( Ax = b \).

If \( n >= m \) QR decomposition is used;
if \( n > m \) this function solves linear least squares problem.
If \( n < m \) (underdetermined system) LQ decomposition is used
  to yield a minimum norm solution.
 -}
qrSolveR ::
       forall t (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (MatrixQR t n m, Dimensions ds)
    => Matrix t n m -> DataFrame t (n :+ ds) -> DataFrame t (m :+ ds)
qrSolveR a b = case compareDim dn dm of
  SEQ | Dict <- (unsafeCoerce (Dict @(m ~ m)) :: Dict (m ~ n))
        -> runST $ do
    let QR {..} = qr a
    xPtr <- thawDataFrame (transpose qrQ %* b) -- NB: make a stateful product for transposed mat
    solveUpperTriangularR qrR xPtr
    unsafeFreezeDataFrame xPtr
  SGT | Dict <- unsafeCoerce (Dict @(m <= m)) :: Dict (m <= n)
      , Dict <- unsafeCoerce (Dict @(n ~ n)) :: Dict ((((n - m) + 1) + m) ~ (n + 1) )
      , D <- minusDim dn dm `plusDim` D1
        -> runST $ do
    let QR {..} = qr a
        i0 :: Idxs '[n - m + 1]
        i0 = Idx 0 :* U
    xPtr <- thawDataFrame ( slice i0 $ transpose qrQ %* b
                          ) -- NB: make a stateful product for transposed mat
    solveUpperTriangularR qrR xPtr
    unsafeFreezeDataFrame xPtr
  SLT | Dict <- unsafeCoerce (Dict @(m <= m)) :: Dict (n <= m)
      , Dict <- unsafeCoerce (Dict @(n ~ n)) :: Dict ((((m - n) + 1) + n) ~ (m + 1))
      , Dict <- unsafeCoerce (Dict @(n ~ n)) :: Dict (((n + 1) + (m - n)) ~ (m + 1))
      , dd@D <- minusDim dm dn
      , D <- dd `plusDim` D1
      , D <- dn `plusDim` D1
        -> runST $ do
    let LQ {..} = lq a
        i0 :: Idxs '[m - n + 1]
        i0 = Idx 0 :* U
        iz :: Idxs '[n + 1]
        iz = maxBound :* U
    xPtr <- newDataFrame
    copyDataFrame i0 b xPtr
    copyDataFrame iz (0 :: DataFrame t ((m - n) ': ds)) xPtr
    solveLowerTriangularR lqL (subDataFrameView i0 xPtr)
    (transpose lqQ %*) <$> unsafeFreezeDataFrame xPtr
      -- NB: make a stateful product for transposed mat
  _ -> error "qrSolveR: impossible pattern"
  where
    dn = dim @n
    dm = dim @m


{- |
Compute a QR or LQ decomposition of matrix \( A : n \times m \),
and solve a system of linear equations \( xA = b \).

If \( n <= m \) LQ decomposition is used;
if \( n < m \) this function solves linear least squares problem.
If \( n > m \) (underdetermined system) QR decomposition is used
  to yield a minimum norm solution.
 -}
qrSolveL ::
       forall t (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (MatrixQR t n m, Dimensions ds)
    => Matrix t n m -> DataFrame t (ds +: m) -> DataFrame t (ds +: n)
qrSolveL a b
  | Dims <- Snoc (dims @ds) dn
  , Dims <- Snoc (dims @ds) dm
  , Dict <- Dict @(SnocList ds n _) -- remind GHC that we have (ds +: n)
  , Dict <- Dict @(SnocList ds m _) -- remind GHC that we have (ds +: m)
    = case compareDim dn dm of
  SEQ | Dict <- (unsafeCoerce (Dict @(m ~ m)) :: Dict (m ~ n))
        -> runST $ do
    let LQ {..} = lq a
    xPtr <- thawDataFrame (b %* transpose lqQ) -- NB: make a stateful product for transposed mat
    solveLowerTriangularL xPtr lqL
    unsafeFreezeDataFrame xPtr
  SGT | Dict <- unsafeCoerce (Dict @(m <= m)) :: Dict (m <= n)
      , Dict <- unsafeCoerce (Dict @(n ~ n)) :: Dict ((m + (n - m)) ~ n)
      , D <- minusDim dn dm
        -> runST $ do
    let QR {..} = qr a
    xPtr <- thawDataFrame b
    solveUpperTriangularL xPtr qrR
    ewmap @t @ds @'[n] (\x -> qrQ %* appendDF x (0 :: DataFrame t '[n - m]))
      <$> unsafeFreezeDataFrame xPtr
  SLT | Dict <- unsafeCoerce (Dict @(m <= m)) :: Dict (n <= m)
      , Dict <- unsafeCoerce (Dict @(n ~ n)) :: Dict ((((m - n) + 1) + n) ~ (m + 1))
      , D <- minusDim dm dn `plusDim` D1
        -> runST $ do
    let LQ {..} = lq a
        i0 :: Idx (m - n + 1)
        i0 = Idx 0
    xPtr <- thawDataFrame (b %* transpose lqQ) -- NB: make a stateful product for transposed mat
    solveLowerTriangularL xPtr lqL
    ewmap @t @ds @'[n] (sslice i0)
      <$> unsafeFreezeDataFrame xPtr
  _ -> error "qrSolveL/compareDim: impossible pattern"
  | otherwise = error "qrSolveL: impossible pattern"
  where
    dn = dim @n
    dm = dim @m
