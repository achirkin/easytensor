{-# LANGUAGE KindSignatures, DataKinds, TypeFamilyDependencies, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix.Family
  ( Matrix
  , MFloatXNM (..)
  ) where

import GHC.TypeLits
import GHC.Prim



-- | Family of all matrix types, specialized on low-dimensional matrices
type family Matrix t (n :: Nat) (m :: Nat) = v | v -> t n m where
  Matrix Float n m = MFloatXNM n m


-- | NxM D matrix
data MFloatXNM (n::Nat) (m::Nat) = MFloatXNM ByteArray#
