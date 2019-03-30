-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a set of data types to define and traverse through multiple dimensions.
-- The core types are `Dims ds` and `Idxs ds`, which fix dimension sizes at compile time.
--
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( module Numeric.Dim
  , module Numeric.Dimensions.Dims
  , module Numeric.Dimensions.Idxs
  , module Numeric.Dimensions.Fold
  , module Numeric.Type.List
  , module Data.Constraint
  ) where

import           Data.Constraint         (Dict (..))
import           Numeric.Dim
import           Numeric.Dimensions.Dims
import           Numeric.Dimensions.Fold
import           Numeric.Dimensions.Idxs
import           Numeric.Type.List
