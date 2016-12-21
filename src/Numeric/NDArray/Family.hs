{-# LANGUAGE KindSignatures, DataKinds, TypeFamilyDependencies, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.NDArray.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.NDArray.Family
  ( NDArray
  , NDArrayF (..)
  ) where

import GHC.TypeLits
import GHC.Prim

--import Numeric.Vector.Family
--import Numeric.Matrix.Family



type family NDArray t (ds :: [Nat]) = v | v -> t ds where
  NDArray Float ds = NDArrayF ds


-- | N-Dimensional arrays based on Float# type
data NDArrayF (ds :: [Nat]) = NDArrayF# ByteArray#

