-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame
  ( DataFrame (..), SomeDataFrame (..)
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
