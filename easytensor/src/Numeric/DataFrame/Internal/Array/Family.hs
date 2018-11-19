{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Internal.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Internal.Array.Family
  ( Array
  ) where


import           GHC.Base                                           (Nat, Type)
import           Numeric.DataFrame.Internal.Array.Family.ArrayBase  (ArrayBase)
import           Numeric.DataFrame.Internal.Array.Family.DoubleX2   (DoubleX2)
import           Numeric.DataFrame.Internal.Array.Family.DoubleX3   (DoubleX3)
import           Numeric.DataFrame.Internal.Array.Family.DoubleX4   (DoubleX4)
import           Numeric.DataFrame.Internal.Array.Family.FloatX2    (FloatX2)
import           Numeric.DataFrame.Internal.Array.Family.FloatX3    (FloatX3)
import           Numeric.DataFrame.Internal.Array.Family.FloatX4    (FloatX4)
import           Numeric.DataFrame.Internal.Array.Family.ScalarBase (ScalarBase)


-- | This type family aggregates all types used for arrays with different
--   dimensionality.
--   The family is injective; thus, it is possible to get type family instance
--   given the data constructor (and vice versa).
--   If GHC knows the dimensionality of an array at compile time, it chooses
--   a more efficient specialized instance of Array, e.g. Scalar newtype wrapper.
--   Otherwise, it falls back to the generic ArrayBase implementation.
--
--   Data family would not work here, because it would give overlapping instances.
type family Array (t :: Type) (ds :: [Nat]) = (v :: Type) | v -> t ds where
    Array t      '[]    = ScalarBase t
    Array Float  '[2]   = FloatX2
    Array Float  '[3]   = FloatX3
    Array Float  '[4]   = FloatX4
    Array Double '[2]   = DoubleX2
    Array Double '[3]   = DoubleX3
    Array Double '[4]   = DoubleX4
    Array t       ds    = ArrayBase t ds
