{-# LANGUAGE KindSignatures, DataKinds, TypeFamilyDependencies, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Vector.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Vector.Family
  ( VFloatX2 (..)
  ) where

-- import GHC.TypeLits
import GHC.Prim

-- import Numeric.Array.Family

-- -- | Family of all vector types, specialized on low-dimensional vectors
-- type family Vector t (n :: Nat) = v | v -> t n where
--   Vector Float 2 = VFloatX2
--   Vector Float n = ArrayF '[n]


-- | 2D Float vector
data VFloatX2 = VFloatX2# Float# Float#

-- -- | ND vector
-- data VFloatXN (n::Nat) = VFloatXN ByteArray#
