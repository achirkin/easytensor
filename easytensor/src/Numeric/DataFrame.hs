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
  ( DataFrame (SomeDataFrame)
  , NumericFrame
    -- * Utility type families and constraints
  , FPFRame, IntegralFrame, NumericVariantFrame, CommonOpFrame
    -- * Simplified type aliases
  , module Numeric.Scalar
  , module Numeric.Vector
  , module Numeric.Matrix
    -- * Functionality
  , module Numeric.DataFrame.SubSpace
  , module Numeric.DataFrame.Contraction
  , module Numeric.DataFrame.Inference
  , module Numeric.DataFrame.Shape
  , ElementWise ()
  , ArrayInstanceEvidence, ArrayInstance (..), getArrayInstance, inferArrayInstance
  , ArraySizeInference (..), ArraySize (..)
  , ElemTypeInference (..), ElemType (..)
  ) where

import           Numeric.Array
import           Numeric.Array.ElementWise

import           Numeric.DataFrame.Contraction
import           Numeric.DataFrame.Inference
import           Numeric.DataFrame.Shape
import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Type

import           Numeric.Matrix
import           Numeric.Scalar
import           Numeric.Vector
