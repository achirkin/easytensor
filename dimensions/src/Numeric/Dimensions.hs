-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a set of data types to define and traverse through multiple dimensions.
-- The core types are `Dim ds` and `Idx ds`, which fix dimension sizes at compile time.
-- 
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( module Numeric.Dimensions.List
  , module Numeric.Dimensions.Dim
  , module Numeric.Dimensions.Idx
  ) where

import Numeric.Dimensions.List
import Numeric.Dimensions.Dim
import Numeric.Dimensions.Idx
