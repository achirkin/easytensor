-- | Scalar is an alias to zero-dimensional DataFrame
module Numeric.Scalar
    ( Scalar, unScalar, scalar, fromScalar
    , Scf, Scd, Sci, Scw
    ) where

import Numeric.DataFrame.Internal.Backend ()
import Numeric.Scalar.Internal
