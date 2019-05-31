-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Provides a set of data types to define and traverse through multiple dimensions.
-- The core types are `Dims ds` and `Idxs ds`,
--   which fix dimension sizes at compile time.
--
-- Higher indices go first, i.e. assumed enumeration
--          is i = i1*n1*n2*...*n(k-1) + ... + i(k-2)*n1*n2 + i(k-1)*n1 + ik
-- This corresponds to row-first layout of matrices and multidimenional arrays.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( module Numeric.Dimensions.Dim
  , module Numeric.Dimensions.Idx
  , module Data.Type.List
  , module Data.Constraint
  ) where

import Data.Constraint         ((:-) (..), Dict (..), mapDict, (\\))
import Data.Type.List
import Numeric.Dimensions.Dim
import Numeric.Dimensions.Idx
