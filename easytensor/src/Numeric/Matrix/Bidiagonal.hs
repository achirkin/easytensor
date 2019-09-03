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
  ( bidiagonalHouseholder
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal

import Numeric.Subroutine.Householder


bidiagonalHouseholder ::
       forall (t :: Type) (n :: Nat) (m :: Nat)
     . (PrimBytes t, Floating t, Ord t, KnownDim n, KnownDim m)
    => Matrix t n m
    -> (Matrix t n n, Matrix t n m, Matrix t m m)
bidiagonalHouseholder a = runST $ do
      tmpNPtr <- newDataFrame
      tmpMPtr <- newDataFrame
      uPtr <- thawDataFrame eye
      bPtr <- thawDataFrame a
      vPtr <- thawDataFrame eye
      forM_ [1 .. lim - 1] $ \i -> do
        householderReflectionInplaceL tmpNPtr uPtr bPtr (Idx (i - 1) :* Idx (i - 1) :* U)
        householderReflectionInplaceR tmpMPtr vPtr bPtr (Idx (i - 1) :* Idx i :* U)
      householderReflectionInplaceL tmpNPtr uPtr bPtr (Idx (lim - 1) :* Idx (lim - 1) :* U)
      when (m > lim) $
        householderReflectionInplaceR tmpMPtr vPtr bPtr (Idx (lim - 1) :* Idx lim :* U)
      (,,)
        <$> unsafeFreezeDataFrame uPtr
        <*> unsafeFreezeDataFrame bPtr
        <*> unsafeFreezeDataFrame vPtr
  where
    n = dimVal' @n
    m = dimVal' @m
    lim = max 1 (min n m)
