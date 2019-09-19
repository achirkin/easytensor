module Numeric.DataFrame
  ( module Numeric.DataFrame.Type
    -- * Simplified type aliases
  , module Numeric.Scalar
  , module Numeric.Vector
  , module Numeric.Matrix
    -- * Functionality
  , module Numeric.DataFrame.SubSpace
  , module Numeric.DataFrame.Contraction
  , module Numeric.Basics
  , module Numeric.Subroutine.Sort
  ) where

import Numeric.DataFrame.Internal.Backend ()

import Numeric.DataFrame.Contraction
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type

import Numeric.Matrix
import Numeric.Scalar
import Numeric.Vector

import Numeric.Basics

import Numeric.Subroutine.Sort (sort, sortBy)
