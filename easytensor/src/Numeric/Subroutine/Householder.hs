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

module Numeric.Subroutine.Householder
  ( householderReflectionInplaceR
  , householderReflectionInplaceL
  ) where


import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Scalar.Internal


{- | Run a Householder transformation inplace.

     Given some orthongonal matrix \(P\), some matrix \(R\) and index \((k,l)\),
     reflects \(R\) along some hyperplane, such that all elements of \(R\)
     below index \( (k, l) \) become zeros,
     then updates \(P\) with the inverse of the same transform as \(R\).

     Notes and invariants:

       1. The transformation happens inplace for both matrices \(P\) and \(R\);
          if \( R = P^\intercal A \), then \( R' = P^*PR = P'^\intercal A \), where
           \( P' \) and \( R' \) are the updated versions of the input matrices,
           \( P^* \) and \( A \) are implicit matrices.

       2. All elements below and to the left of index \(k,l\) in \(R\)
          are assumed (and not checked) to be zeros;
          these are not touched by the subroutine to save flops.

       3. A logical starting value for \(P\) is an identity matrix.
          The subroutine can be used for a QR decomposition:
            \( Q = P \).

 -}
householderReflectionInplaceL ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat)
     . (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
    => STDataFrame s t '[n] -- ^ Temporary buffer for a Householder axis vector
    -> STDataFrame s t '[n,n]  -- ^ Current state of \(P^\intercal\)
    -> STDataFrame s t '[n,m]  -- ^ Current state of \(R\)
    -> Idxs '[n,m] -- ^ Pivot element
    -> ST s ()
householderReflectionInplaceL u p r (Idx i :* Idx j :* U)
    = householderReflectionInplaceL' u p r
      (fromIntegral $ dimVal' @n)
      (fromIntegral $ dimVal' @m)
      (fromIntegral i)
      (fromIntegral j)

householderReflectionInplaceL' ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat)
     . (PrimBytes t, Floating t, Ord t)
    => STDataFrame s t '[n] -- ^ Temporary buffer for a Householder axis vector
    -> STDataFrame s t '[n,n]  -- ^ \(P^\intercal\)
    -> STDataFrame s t '[n,m]  -- ^ \(R\)
    -> Int -- ^ \(n\)
    -> Int -- ^ \(m\)
    -> Int -- ^ \( 0 \leq k < n \)
    -> Int -- ^ \( 0 \leq l < m \)
    -> ST s ()
householderReflectionInplaceL' uPtr pPtr rPtr n m k l = do
    -- pivot element (k,l) of new R
    alpha <- getAlphaAndUpdateU
    u2 <- getU2
    -- u2 == 0 means the column is already zeroed
    when (u2 /= 0) $ do
      let c = 2 / u2 -- a mult constant for updating matrices
      -- update R
      updateRl alpha
      forM_ [l+1..m-1] $ updateRi c
      -- update P
      forM_ [0..n-1] $ updatePi c
  where
    n' = n - k -- remaining rows
    rOff0 = k*m + l -- offset of element (k,l) in matrix R

    -- u = Rk - alpha*ek
    getAlphaAndUpdateU :: ST s (Scalar t)
    getAlphaAndUpdateU = do
      alpha' <- sqrt . fst <$> nTimesM n'
        (\(r, off) -> (\x -> (r + x*x, off + m)) <$> readDataFrameOff rPtr off) (0, rOff0)
      x0 <- readDataFrameOff rPtr rOff0
      let alpha = if x0 >= 0 then negate alpha' else alpha'
      -- update (lower part of) u
      writeDataFrameOff uPtr k (x0 - alpha)
      when (n' >= 1) $ void $ nTimesM (n' - 1)
        (\(i, off) -> (i+1, off+m) <$
          (readDataFrameOff rPtr off >>= writeDataFrameOff uPtr i)
        ) (k+1, rOff0+m)
      return alpha

    -- l-th column of R zeroes below pivot
    updateRl :: Scalar t -> ST s ()
    updateRl alpha = do
      writeDataFrameOff rPtr rOff0 alpha
      when (n' >= 1) $ void $ nTimesM (n' - 1)
        (\off -> (off+m) <$ writeDataFrameOff rPtr off 0) (rOff0+m)

    -- update i-th column of R
    updateRi :: Scalar t -> Int -> ST s ()
    updateRi c i = do
      -- dot product of u and Ri
      uRi <- fmap fst . flip (nTimesM n') (0, (k, k*m+i)) $ \(r, (j, off)) -> do
        ju  <- readDataFrameOff uPtr j
        jiR <- readDataFrameOff rPtr off
        return (r + ju * jiR, (j+1, off+m))
      let c' = c * uRi
      -- update each element
      void $ flip (nTimesM n') (k, k*m+i) $ \(j, off) -> do
        ju  <- readDataFrameOff uPtr j
        jiR <- readDataFrameOff rPtr off
        writeDataFrameOff rPtr off $ jiR - c'*ju
        return (j+1, off+m)

    -- update i-th row of P
    updatePi :: Scalar t -> Int -> ST s ()
    updatePi c i = do
      let off0 = i*n
      -- dot product of u and Pi
      uPi <- fmap fst . flip (nTimesM n') (0, k) $ \(r, j) -> do
        ju  <- readDataFrameOff uPtr  j
        ijP <- readDataFrameOff pPtr (off0 + j)
        return (r + ju * ijP, j+1)
      let c' = c * uPi
      -- update each element
      forM_ [k..n-1] $ \j -> do
        ju  <- readDataFrameOff uPtr j
        ijP <- readDataFrameOff pPtr (off0 + j)
        writeDataFrameOff pPtr (off0 + j) $ ijP - c'*ju

    -- get module squared of u (for Q = I - 2 u*uT / |u|^2 )
    getU2 :: ST s (Scalar t)
    getU2 = fst <$> nTimesM n'
      (\(r, off) -> (\x -> (r + x*x, off + 1)) <$> readDataFrameOff uPtr off) (0, k)

{- | Run a Householder transformation inplace.

  Similar to `householderReflectionInplaceR`, but works from right to left
   - use to zero elements to the right from the pivot.
 -}
householderReflectionInplaceR ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat)
     . (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
    => STDataFrame s t '[m] -- ^ Temporary buffer for a Householder axis vector
    -> STDataFrame s t '[m,m]  -- ^ Current state of \(P^\intercal\)
    -> STDataFrame s t '[n,m]  -- ^ Current state of \(R\)
    -> Idxs '[n,m] -- ^ Pivot element
    -> ST s ()
householderReflectionInplaceR u p r (Idx i :* Idx j :* U)
    = householderReflectionInplaceR' u p r
      (fromIntegral $ dimVal' @n)
      (fromIntegral $ dimVal' @m)
      (fromIntegral i)
      (fromIntegral j)

householderReflectionInplaceR' ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat)
     . (PrimBytes t, Floating t, Ord t)
    => STDataFrame s t '[m] -- ^ Temporary buffer for a Householder axis vector
    -> STDataFrame s t '[m,m]  -- ^ \(P^\intercal\)
    -> STDataFrame s t '[n,m]  -- ^ \(R\)
    -> Int -- ^ \(n\)
    -> Int -- ^ \(m\)
    -> Int -- ^ \( 0 \leq k < n \)
    -> Int -- ^ \( 0 \leq l < m \)
    -> ST s ()
householderReflectionInplaceR' uPtr pPtr rPtr n m k l = do
    -- pivot element (k,l) of new R
    alpha <- getAlphaAndUpdateU
    u2 <- getU2
    -- u2 == 0 means the column is already zeroed
    when (u2 /= 0) $ do
      let c = 2 / u2 -- a mult constant for updating matrices
      -- update R
      updateRk alpha
      forM_ [k+1..n-1] $ updateRi c
      -- update P
      forM_ [0..m-1] $ updatePi c
  where
    m' = m - l -- remaining cols
    rOff0 = k*m + l -- offset of element (k,l) in matrix R

    -- u = Rl - alpha*el
    getAlphaAndUpdateU :: ST s (Scalar t)
    getAlphaAndUpdateU = do
      alpha' <- sqrt . fst <$> nTimesM m'
        (\(r, off) -> (\x -> (r + x*x, off + 1)) <$> readDataFrameOff rPtr off) (0, rOff0)
      x0 <- readDataFrameOff rPtr rOff0
      let alpha = if x0 >= 0 then negate alpha' else alpha'
      -- update (lower part of) u
      writeDataFrameOff uPtr l (x0 - alpha)
      forM_ [1..m'-1] $ \i ->
        readDataFrameOff rPtr (rOff0 + i) >>= writeDataFrameOff uPtr (l + i)
      return alpha

    -- k-th row of R zeroes below pivot
    updateRk :: Scalar t -> ST s ()
    updateRk alpha = do
      writeDataFrameOff rPtr rOff0 alpha
      forM_ [rOff0+1..rOff0+m'-1] $ flip (writeDataFrameOff rPtr) 0

    -- update i-th row of R
    updateRi :: Scalar t -> Int -> ST s ()
    updateRi c i = do
      let off0 = i*m
      -- dot product of u and Ri
      uRi <- fmap fst . flip (nTimesM m') (0, l) $ \(r, j) -> do
        ju  <- readDataFrameOff uPtr  j
        jiR <- readDataFrameOff rPtr (off0 + j)
        return (r + ju * jiR, j+1)
      let c' = c * uRi
      -- update each element
      forM_ [l..m-1] $ \j -> do
        ju  <- readDataFrameOff uPtr j
        jiR <- readDataFrameOff rPtr (off0 + j)
        writeDataFrameOff rPtr (off0 + j) $ jiR - c'*ju

    -- update i-th row of P
    updatePi :: Scalar t -> Int -> ST s ()
    updatePi c i = do
      let off0 = i*m
      -- dot product of u and Pi
      uPi <- fmap fst . flip (nTimesM m') (0, l) $ \(r, j) -> do
        ju  <- readDataFrameOff uPtr  j
        ijP <- readDataFrameOff pPtr (off0 + j)
        return (r + ju * ijP, j+1)
      let c' = c * uPi
      -- update each element
      forM_ [l..m-1] $ \j -> do
        ju  <- readDataFrameOff uPtr j
        ijP <- readDataFrameOff pPtr (off0 + j)
        writeDataFrameOff pPtr (off0 + j) $ ijP - c'*ju

    -- get module squared of u (for Q = I - 2 u*uT / |u|^2 )
    getU2 :: ST s (Scalar t)
    getU2 = fst <$> nTimesM m'
      (\(r, off) -> (\x -> (r + x*x, off + 1)) <$> readDataFrameOff uPtr off) (0, l)


nTimesM :: Monad m => Int -> (a -> m a) -> a -> m a
nTimesM 0 _ x = pure x
nTimesM n m x = m x >>= nTimesM (n - 1) m
