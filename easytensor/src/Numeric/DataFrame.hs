{-# LANGUAGE PatternSynonyms #-}

module Numeric.DataFrame
  ( DataFrame (..), SomeDataFrame (..)
  , pattern (:*:), pattern Z, ArraySingletons
    -- * Simplified type aliases
  , module Numeric.Scalar
  , module Numeric.Vector
  , module Numeric.Matrix
    -- * Functionality
  , module Numeric.DataFrame.SubSpace
  , module Numeric.DataFrame.Contraction
  , module Numeric.DataFrame.Shape
  ) where

import           Numeric.DataFrame.Internal.Array ()

import           Numeric.DataFrame.Contraction
import           Numeric.DataFrame.Shape
import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Type

import           Numeric.Matrix
import           Numeric.Scalar
import           Numeric.Vector
