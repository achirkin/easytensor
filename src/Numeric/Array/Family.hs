{-# LANGUAGE KindSignatures, DataKinds, PolyKinds, TypeFamilyDependencies, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family
  ( Array
  , ArrayF (..)
  ) where

-- import GHC.TypeLits
import GHC.Prim

import Numeric.Dimensions
--import Numeric.Vector.Family
--import Numeric.Matrix.Family



type family Array t (ds :: [k]) = v | v -> t ds where
  Array Float ds = ArrayF ds


-- * Array implementations.
--   All array implementations have the same structure:
--   Array[Type] (element offset :: Int#) (element length :: Int#)
--                 (dimension sizes left-to-right :: ByteArray#)
--                 (content :: ByteArray#)
--   All types can also be instantiated with only a single scalar.

-- | N-Dimensional arrays based on Float# type
data ArrayF (ds :: [k]) = ArrayF# Int# Int# ByteArray# ByteArray#
                           | FromScalarF# Float#
