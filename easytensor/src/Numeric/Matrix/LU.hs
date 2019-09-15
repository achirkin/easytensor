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
module Numeric.Matrix.LU
  ( MatrixLU (..), LU (..)
  , luSolveR, luSolveL
  , detViaLU, inverseViaLU
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.DataFrame.Contraction        ((%*))
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
import Numeric.Scalar.Internal
import Numeric.Subroutine.SolveTriangular


-- | Result of LU factorization with Partial Pivoting
--   \( PA = LU \).
data LU t n
  = LU
  { luLower   :: Matrix t n n
    -- ^ Unit lower triangular matrix \(L\).
    --   All elements on the diagonal of @L@ equal @1@.
    --   The rest of the elements satisfy \(|l_{ij}| \leq 1\).
  , luUpper   :: Matrix t n n
    -- ^ Upper triangular matrix \(U\)
  , luPerm    :: Matrix t n n
    -- ^ Row permutation matrix \(P\)
  , luPermDet :: Scalar t
    -- ^ Sign of permutation @luPermDet == det . luPerm@; \(|P| = \pm 1\).
  }

deriving instance (Show t, PrimBytes t, KnownDim n) => Show (LU t n)
deriving instance (Eq (Matrix t n n), Eq t) => Eq (LU t n)

class (KnownDim n, Ord t, Fractional t, PrimBytes t, KnownBackend t '[n,n])
      => MatrixLU t (n :: Nat) where
    -- | Compute LU factorization with Partial Pivoting
    lu :: Matrix t n n -> LU t n


instance (KnownDim n, Ord t, Fractional t, PrimBytes t, KnownBackend t '[n,n])
         => MatrixLU t n where
    lu a = runST $ do
        pPtr <- unsafeThawDataFrame $ iwgen  @_ @'[n] @'[] (\(Idx i :* U) -> S i)
        uPtr <- thawDataFrame a
        lPtr <- newDataFrame
        temp <- newDataFrame
        detPositive <- luInplace temp pPtr uPtr
        p <- unsafeFreezeDataFrame pPtr
        -- split U and L
        forM_ [0..n-1] $ \i -> do
          let ni = n*i
          forM_ [0..n-1] $ \j -> case compare i j of
              GT -> do
                lij <- readDataFrameOff uPtr (ni + j)
                writeDataFrameOff uPtr (ni + j) 0
                writeDataFrameOff lPtr (ni + j) lij
              EQ -> writeDataFrameOff lPtr (ni + j) 1
              LT -> writeDataFrameOff lPtr (ni + j) 0
        luLower <- unsafeFreezeDataFrame lPtr
        luUpper <- unsafeFreezeDataFrame uPtr
        let luPermDet = if detPositive then 1 else -1
            luPerm = iwgen @_ @'[n,n] @'[]
              (\(Idx i :* Idx j :* U) -> if S j == p ! i then 1 else 0)
        return LU {..}
      where
        n = fromIntegral (dimVal' @n) :: Int


-- | Solve @Ax = b@ problem given LU decomposition of A.
luSolveR ::
       forall t (n :: Nat) (ds :: [Nat])
     . (MatrixLU t n, Dimensions ds)
    => LU t n -> DataFrame t (n :+ ds) -> DataFrame t (n :+ ds)
luSolveR LU {..} b = runST $ do
    xPtr <- thawDataFrame (luPerm %* b) -- NB: wasting resources!
    solveLowerTriangularR luLower xPtr
    solveUpperTriangularR luUpper xPtr
    unsafeFreezeDataFrame xPtr

-- | Solve @xA = b@ problem given LU decomposition of A.
luSolveL ::
       forall t (n :: Nat) (ds :: [Nat])
     . (MatrixLU t n, Dimensions ds)
    => LU t n -> DataFrame t (ds +: n) -> DataFrame t (ds +: n)
luSolveL LU {..} b
  | dn  <- dim @n
  , dds <- dims @ds
  , Dims <- Snoc dds dn
  , Dict <- Dict @(SnocList ds n _)
  = runST $ do
    xPtr <- thawDataFrame b
    solveUpperTriangularL xPtr luUpper
    solveLowerTriangularL xPtr luLower
    (%* luPerm) <$> unsafeFreezeDataFrame xPtr
luSolveL _ _ = error "luSolveL: impossible pattern"

-- | Calculate inverse of a matrix via LU decomposition
inverseViaLU :: forall (t :: Type) (n :: Nat)
              . MatrixLU t n => Matrix t n n -> Matrix t n n
inverseViaLU a = runST $ do
    xPtr <- unsafeThawDataFrame luPerm -- luPerm is only ever used once
    solveLowerTriangularR luLower xPtr
    solveUpperTriangularR luUpper xPtr
    unsafeFreezeDataFrame xPtr
  where
    LU {..} = lu a
-- perfectly correct, but slightly slower versions:
-- inverseViaLU a = luSolveR (lu a) eye
-- inverseViaLU a = luSolveL (lu a) eye

-- | Calculate determinant of a matrix via LU decomposition
detViaLU :: forall (t :: Type) (n :: Nat)
          . MatrixLU t n => Matrix t n n -> Scalar t
detViaLU m = foldl (\x off -> scalar (ixOff off luUpper) * x) luPermDet [0,n+1..n*n]
  where
    n = fromIntegral (dimVal' @n) :: Int
    LU {..} = lu m

{- |
Run LU decomposition with partial pivoting inplace, such that
the upper triangular part of matrix \(A\) becomes \(U\) and
the lower triangular part (without diagonal) of matrix  \(A\) becomes \(L\).

\(U\) is upper triangular.
\(L\) is unit lower triangular; all diagonal elements of \(L\) are implicit and
equal to 1; the rest of the elements a smaller than one \(|l_{ij}| \leq 1\).

Pivoting is represented as a permutation vector \(p\);
returned value is the sign of the permutation (positive if @True@, negative otherwise).

NB: Initialize \(p\) with indices @0..n-1@.

Reference: Algorithm 3.4.1 on p.128
       of "Matrix Computations" 4th edition by G. H. Golub and C. F. Van Loan.
 -}
luInplace ::
       forall (s :: Type) (t :: Type) (n :: Nat)
     . (PrimBytes t, Fractional t, Ord t, KnownDim n)
    => STDataFrame s t '[n]    -- ^ Temporary buffer
    -> STDataFrame s Word '[n] -- ^ Current state of permutation \(p\)
    -> STDataFrame s t '[n,n]  -- ^ Current state of \(A\)
    -> ST s Bool
luInplace temp pPtr aPtr = foldM (\b -> fmap (b /=) . go) True [0..n-2]
  where
    n = fromIntegral (dimVal' @n) :: Int

    -- Runs an iteration of the algorithm;
    --- returns whether there was a swap of rows.
    go :: Int -> ST s Bool
    go k = do
      mu <- findPivot  k
      let swapped = k /= mu
      when swapped $ swapRows k mu
      akk <- readDataFrameOff aPtr (k*(n+1))
      when (akk /= 0) $ do
        let rakk = recip akk
        forM_ [k+1..n-1] $ \i -> do
          let ni = n*i
          aik <- (rakk *) <$> readDataFrameOff aPtr (ni + k)
          writeDataFrameOff aPtr (ni + k) aik
          forM_ [k+1..n-1] $ \j -> do
            akj <- readDataFrameOff aPtr (n*k + j)
            aij <- readDataFrameOff aPtr (ni + j)
            writeDataFrameOff aPtr (ni + j) (aij - aik*akj)
      return swapped

    findPivot :: Int -> ST s Int
    findPivot k = snd <$> foldM findPivotF (0, k) [k..n-1]
      where
        findPivotF :: (Scalar t, Int) -> Int -> ST s (Scalar t, Int)
        findPivotF aj@(a, _) i = do
          x <- abs <$> readDataFrameOff aPtr (n*i + k)
          return (if x > a then (x, i) else aj)

    swapRows :: Int -> Int -> ST s ()
    swapRows i j = do
      let iPtr = subDataFrameView' (fromIntegral i :* U) aPtr
          jPtr = subDataFrameView' (fromIntegral j :* U) aPtr
      copyMutableDataFrame' U iPtr temp
      copyMutableDataFrame' U jPtr iPtr
      copyMutableDataFrame' U temp jPtr
      t <- readDataFrameOff pPtr i
      readDataFrameOff pPtr j >>= writeDataFrameOff pPtr i
      writeDataFrameOff pPtr j t
