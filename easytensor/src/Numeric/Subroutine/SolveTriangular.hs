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
{- |
A few ways to solve a system of linear equations in ST monad.
The tesult is always computed inplace.
 -}
module Numeric.Subroutine.SolveTriangular
  ( solveUpperTriangularR
  , solveUpperTriangularL
  , solveLowerTriangularR
  , solveLowerTriangularL
  ) where


import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Scalar.Internal


{- |
Solve a system of linear equations \( Rx = b \)
   or a linear least squares problem \( \min {|| Rx - b ||}^2 \),
where \( R \) is an upper-triangular matrix.

DataFrame \( b \) is modified in-place; by the end of the process \( b_m = x \).

NB: you can use `subDataFrameView` to truncate @b@ without performing a copy.
 -}
solveUpperTriangularR ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (PrimBytes t, Fractional t, Eq t, KnownDim m, m <= n)
    => DataFrame t '[n,m]       -- ^ \(R\)
    -> STDataFrame s t  (m :+ ds) -- ^ Current state of \(b_m\)
                                  --   (first @m@ rows of @b@)
    -> ST s ()
solveUpperTriangularR r bPtr | Dict <- Dict @(m <= n) = mapM_ go [m-1,m-2..0]
  where
    m = fromIntegral $ dimVal' @m :: Int
    k = fromIntegral kw :: Int
    CumulDims (_:kw:_) = getDataFrameSteps bPtr
    go :: Int -> ST s ()
    go i | rii == 0  = forM_ [0..k-1] $ \j -> writeDataFrameOff bPtr (ki + j) 0
         | otherwise = forM_ [0..k-1] $ \j -> do
            bij <- (rrii*) <$> readDataFrameOff bPtr (ki + j)
            writeDataFrameOff bPtr (ki + j) bij
            forM_ [0..i-1] $ \t -> do
              let rti = scalar (ixOff (m*t + i) r)
                  ix = k*t + j
              btj <- readDataFrameOff bPtr ix
              writeDataFrameOff bPtr ix (btj - rti*bij)
      where
        mi = m*i
        ki = k*i
        rii = scalar (ixOff (mi + i) r)
        rrii = recip rii


{- |
Solve a system of linear equations \( xR = b \),
where \( R \) is an upper-triangular matrix.

DataFrame \( b \) is modified in-place; by the end of the process \( b = x_m \).
The \( (n - m) \) rows of \(R\) are not used.
Pad each dimension of \(x\) with \( (n - m) \) zeros if you want to get the full
solution.
 -}
solveUpperTriangularL ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (PrimBytes t, Fractional t, Eq t, KnownDim m, m <= n)
    => STDataFrame s t  (ds +: m) -- ^ Current state of \(b\)
                                  --   (first @m@ "columns" of x)
    -> DataFrame t '[n,m]         -- ^ \(R\)
    -> ST s ()
solveUpperTriangularL bPtr r | Dict <- Dict @(m <= n) = mapM_ go [0..m-1]
  where
    m = fromIntegral $ dimVal' @m :: Int
    k = fromIntegral mkw `quot` m
    CumulDims (mkw:_) = getDataFrameSteps bPtr
    go :: Int -> ST s ()
    go i | rii == 0  = forM_ [0..k-1] $ \j -> writeDataFrameOff bPtr (m*j + i) 0
         | otherwise = forM_ [0..k-1] $ \j -> do
            let mj = m*j
            bji <- (rrii*) <$> readDataFrameOff bPtr (mj + i)
            writeDataFrameOff bPtr (mj + i) bji
            forM_ [i+1..m-1] $ \t -> do
              let rit = scalar (ixOff (mi + t) r)
              bjt <- readDataFrameOff bPtr (mj + t)
              writeDataFrameOff bPtr (mj + t) (bjt - rit*bji)
      where
        mi = m*i
        rii = scalar (ixOff (mi + i) r)
        rrii = recip rii

{- |
Solve a system of linear equations \( Lx = b \),
where \( L \) is a lower-triangular matrix.

DataFrame \( b \) is modified in-place; by the end of the process \( b = x_n \).
The \( (m - n) \) columns of \(L\) are not used.
Pad \(x\) with \( (m - n) \) zero elements if you want to get the full solution.
 -}
solveLowerTriangularR ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (PrimBytes t, Fractional t, Eq t, KnownDim n, KnownDim m, n <= m)
    => DataFrame t '[n,m]         -- ^ \(L\)
    -> STDataFrame s t  (n :+ ds) -- ^ Current state of \(b\)
                                  --   (first @n@ elements of x)
    -> ST s ()
solveLowerTriangularR l bPtr | Dict <- Dict @(n <= m) = mapM_ go [0..n-1]
  where
    m = fromIntegral $ dimVal' @m :: Int
    n = fromIntegral $ dimVal' @n :: Int
    k = fromIntegral kw :: Int
    CumulDims (_:kw:_) = getDataFrameSteps bPtr
    go :: Int -> ST s ()
    go i | lii == 0  = forM_ [0..k-1] $ \j -> writeDataFrameOff bPtr (ki + j) 0
         | otherwise = forM_ [0..k-1] $ \j -> do
            bij <- (rlii*) <$> readDataFrameOff bPtr (ki + j)
            writeDataFrameOff bPtr (ki + j) bij
            forM_ [i+1..n-1] $ \t -> do
              let rti = scalar (ixOff (m*t + i) l)
                  ix = k*t + j
              btj <- readDataFrameOff bPtr ix
              writeDataFrameOff bPtr ix (btj - rti*bij)
      where
        mi = m*i
        ki = k*i
        lii = scalar (ixOff (mi + i) l)
        rlii = recip lii

{- |
Solve a system of linear equations \( xL = b \)
   or a linear least squares problem \( \min {|| xL - b ||}^2 \),
where \( L \) is a lower-triangular matrix.

DataFrame \( b \) is modified in-place; by the end of the process \( b_n = x \).
The last \( (m - n) \) columns of \(L\) and \(b\) and are not touched.
 -}
solveLowerTriangularL ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (PrimBytes t, Fractional t, Eq t, KnownDim n, KnownDim m, n <= m)
    => STDataFrame s t  (ds +: m) -- ^ Current state of \(b\)
    -> DataFrame t '[n,m]         -- ^ \(L\)
    -> ST s ()
solveLowerTriangularL bPtr l | Dict <- Dict @(n <= m) = mapM_ go [n-1,n-2..0]
  where
    m = fromIntegral $ dimVal' @m :: Int
    n = fromIntegral $ dimVal' @n :: Int
    k = fromIntegral kmw `quot` m
    CumulDims (kmw:_) = getDataFrameSteps bPtr
    go :: Int -> ST s ()
    go i | lii == 0  = forM_ [0..k-1] $ \j -> writeDataFrameOff bPtr (m*j + i) 0
         | otherwise = forM_ [0..k-1] $ \j -> do
            let mj = m*j
            bji <- (rlii*) <$> readDataFrameOff bPtr (mj + i)
            writeDataFrameOff bPtr (mj + i) bji
            forM_ [0..i-1] $ \t -> do
              let lit = scalar (ixOff (mi + t) l)
                  ix = m*j + t
              bjt <- readDataFrameOff bPtr ix
              writeDataFrameOff bPtr ix (bjt - lit*bji)
      where
        mi = m*i
        lii = scalar (ixOff (mi + i) l)
        rlii = recip lii
