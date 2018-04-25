{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
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


import qualified Numeric.DataFrame.Internal.Array.Family   as AFam
import           Numeric.DataFrame.Type
import           Numeric.Dimensions     (Nat)

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
