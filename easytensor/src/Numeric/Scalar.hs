{-# LANGUAGE KindSignatures, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Scalar
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Scalar
    ( Scalar, unScalar, scalar
    , Scf, Scd
    ) where

import GHC.TypeLits (Nat)

import qualified Numeric.Array.Family as AFam
import           Numeric.DataFrame.Type

-- | Alias for zero-dimensional DataFrame
type Scalar t = DataFrame t ('[] :: [Nat])
type Scf   = Scalar Float
type Scd   = Scalar Double

-- | Convert scalar back to ordinary type
unScalar :: Scalar t -> t
unScalar = AFam._unScalar . _getDF

-- | Convert any type to scalar wrapper
scalar :: t -> Scalar t
scalar = KnownDataFrame . AFam.Scalar
