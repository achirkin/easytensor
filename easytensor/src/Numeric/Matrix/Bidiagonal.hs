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

module Numeric.Matrix.Bidiagonal
  ( BiDiag (..), biDiag, bidiagonalHouseholder
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
import Numeric.Vector.Internal


-- | Put two vectors on the main and first upper diagonal.
biDiag :: forall (t :: Type) (n :: Nat) (m :: Nat)
        . (PrimBytes t, Num t)
       => Dims '[n,m]
       -> Vector t (Min n m)
       -> Vector t (Min n m)
       -> Matrix t n m
biDiag (dn@D :* dm@D :* U) a b = runST $ do
    dnm@D <- pure $ minDim dn dm
    rPtr <- thawDataFrame 0
    forM_ [0 .. dimVal dnm - 1] $ \i -> do
      writeDataFrame rPtr (Idx i :* Idx i :* U) $ a ! i
      when (i+1 < dimVal dm) $
        writeDataFrame rPtr (Idx i :* Idx (i+1) :* U) $ b ! i
    unsafeFreezeDataFrame rPtr

-- | Decomposition of a matrix \( A = U B V^\intercal \) such that
--   \( U \) and \( V \) are orthogonal and \( B \) is bidiagonal.
data BiDiag (t :: Type) (n :: Nat)  (m :: Nat)
  = BiDiag
  { bdU     :: Matrix t n n
    -- ^ \( U \) left orthogonal matrix
  , bdUDet  :: Scalar t
    -- ^ A shortcut for evaluating a determinant of \( |U| = \pm 1 \)
  , bdAlpha :: Vector t (Min n m)
    -- ^ Main diagonal of \( B \)
  , bdBeta  :: Vector t (Min n m)
    -- ^ First upper diagonal of \( B \);
    --   its last element equals zero if \( n \geq m \)
  , bdV     :: Matrix t m m
    -- ^ \( B \) left orthogonal matrix
  , bdVDet  :: Scalar t
    -- ^ A shortcut for evaluating a determinant of \( |V| = \pm 1 \)
  }

deriving instance ( Show t, PrimBytes t
                  , KnownDim n, KnownDim m, KnownDim (Min n m))
                  => Show (BiDiag t n m)
deriving instance ( Eq t, PrimBytes t
                  , KnownDim n, KnownDim m, KnownDim (Min n m)
                  , KnownBackend t '[Min n m])
                  => Eq (BiDiag t n m)

{- |
Decompose a matrix \( A = U B V^\intercal \) such that
\( U \) and \( V \) are orthogonal and \( B \) is bidiagonal.

The first returned number
 -}
bidiagonalHouseholder ::
       forall (t :: Type) (n :: Nat) (m :: Nat)
     . (PrimBytes t, Ord t, Epsilon t, KnownDim n, KnownDim m)
    => Matrix t n m
    -> BiDiag t n m
bidiagonalHouseholder a = runST $ do
      D <- pure $ minDim (dim @n) (dim @m)
      tmpNPtr <- newDataFrame
      tmpMPtr <- newDataFrame
      uPtr <- thawDataFrame eye
      bPtr <- thawDataFrame a
      vPtr <- thawDataFrame eye
      (ud, vd) <-
        let f (ud, vd) i = do
              ud' <- householderReflectionInplaceL tmpNPtr uPtr bPtr
                     (Idx (i - 1) :* Idx (i - 1) :* U)
              vd' <- householderReflectionInplaceR tmpMPtr vPtr bPtr
                     (Idx (i - 1) :* Idx i :* U)
              return (ud /= ud', vd /= vd')
        in foldM f (False, False) [1 .. lim - 1]

      udn <- householderReflectionInplaceL tmpNPtr uPtr bPtr
                     (Idx (lim - 1) :* Idx (lim - 1) :* U)
      vdn <- if (m > lim)
             then householderReflectionInplaceR tmpMPtr vPtr bPtr
                     (Idx (lim - 1) :* Idx lim :* U)
             else pure False
      bdU <- unsafeFreezeDataFrame uPtr
      bdV <- unsafeFreezeDataFrame vPtr
      b <- unsafeFreezeDataFrame bPtr
      let bdAlpha = iwgen @t @'[Min n m]
            (\(Idx i :* U) -> index (Idx i :* Idx i :* U) b)
          bdBeta  = iwgen @t @'[Min n m]
            (\(Idx i :* U) -> if i+1 < m then index (Idx i :* Idx (i+1) :* U) b else 0)
          bdUDet = if ud /= udn then -1 else 1
          bdVDet = if vd /= vdn then -1 else 1
      return BiDiag {..}
  where
    n = dimVal' @n
    m = dimVal' @m
    lim = max 1 (min n m)
