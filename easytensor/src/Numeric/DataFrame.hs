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
  ( DataFrame
  , module Numeric.Scalar
  , module Numeric.Vector
  , module Numeric.Matrix
  , module Numeric.DataFrame.SubSpace
  , module Numeric.DataFrame.Contraction
  , module Numeric.DataFrame.Inference
  , module Numeric.DataFrame.Shape
  ) where

import Numeric.Array ()

import Numeric.DataFrame.Type
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Contraction
import Numeric.DataFrame.Inference
import Numeric.DataFrame.Shape

import Numeric.Scalar
import Numeric.Vector
import Numeric.Matrix
