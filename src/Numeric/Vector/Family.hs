{-# LANGUAGE KindSignatures, DataKinds, TypeFamilyDependencies #-}
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
  ( Vector
  ) where

import GHC.TypeLits
import {-# SOURCE #-} Numeric.Vector.FloatX2

data VectorN t (n::Nat)

type family Vector t (n :: Nat) = v | v -> t n where
  Vector Float 2 = VFloatX2
  Vector t n = VectorN t n
