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


import Data.Type.Lits             (Nat)
import GHC.Base                   (unsafeCoerce#)
import Numeric.DataFrame.SubSpace (SubSpace (ewgen))
import Numeric.DataFrame.Type     (DataFrame, pattern S)

-- | Alias for zero-dimensional DataFrame
type Scalar t = DataFrame t ('[] :: [Nat])
type Scf   = Scalar Float
type Scd   = Scalar Double
type Sci   = Scalar Int
type Scw   = Scalar Word

-- | Convert scalar back to ordinary type
unScalar :: Scalar t -> t
-- rely on that Scalar is just two times newtype alias to t
unScalar = unsafeCoerce#
{-# INLINE unScalar #-}

-- | Convert any type to scalar wrapper
scalar :: t -> Scalar t
-- rely on that Scalar is just two times newtype alias to t
scalar = unsafeCoerce#
{-# INLINE scalar #-}

-- | Broadcast scalar value onto a whole data frame
fromScalar :: SubSpace t ds '[] ds
           => Scalar t -> DataFrame t ds
fromScalar = ewgen
{-# INLINE fromScalar #-}
