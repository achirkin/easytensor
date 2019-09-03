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
  ( QR (..), MatrixQR (..)
  ) where


import Control.Monad
import Control.Monad.ST
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
import Numeric.Subroutine.Householder

-- | Result of QR factorization
--   \( A = QR \).
data QR t n m
  = QR
  { qrQ :: Matrix t n n
    -- ^ Orthogonal matrix \( Q \)
  , qrR :: Matrix t n m
    -- ^ Upper-triangular matrix \( R \)
  }

deriving instance (Show (Matrix t n n), Show (Matrix t n m)) => Show (QR t n m)
deriving instance (Eq (Matrix t n n), Eq (Matrix t n m)) => Eq (QR t n m)


class (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
      => MatrixQR t (n :: Nat) (m :: Nat) where
    -- | Compute QR factorization
    qr :: Matrix t n m -> QR t n m

instance (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
      => MatrixQR t (n :: Nat) (m :: Nat) where
    qr a
      | lim == 0 = QR undefined undefined
      | otherwise = runST $ do
        uPtr <- newDataFrame
        pPtr <- unsafeThawDataFrame eye
        rPtr <- thawDataFrame a
        forM_ [0..lim-1] $ \i ->
          householderReflectionInplaceL uPtr pPtr rPtr (Idx i :* Idx i :* U)
        qrR <- unsafeFreezeDataFrame rPtr
        qrQ <- unsafeFreezeDataFrame pPtr
        return QR {..}
      where
        n = dimVal' @n
        m = dimVal' @m
        lim = min n m
