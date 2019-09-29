{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE PatternSynonyms  #-}
-- | Scalar is an alias to zero-dimensional DataFrame
module Numeric.Scalar.Internal
    ( Scalar, unScalar, scalar, fromScalar
    , Scf, Scd, Sci, Scw
    , pattern S
    ) where

import Numeric.DataFrame.Internal.PrimArray (broadcast)
import Numeric.DataFrame.Type

-- | Alias for zero-dimensional DataFrame
type Scalar t = DataFrame t ('[] :: [Nat])
type Scf   = Scalar Float
type Scd   = Scalar Double
type Sci   = Scalar Int
type Scw   = Scalar Word

-- | Broadcast scalar value onto a whole data frame
fromScalar :: PrimArray t (DataFrame t ds)
           => Scalar t -> DataFrame t (ds :: [Nat])
fromScalar = broadcast . unScalar
{-# INLINE fromScalar #-}
