{-# LANGUAGE MagicHash #-}
module Numeric.Vector.FloatX2
  ( VFloatX2
  ) where

import GHC.Prim

data VFloatX2 = VFloatX2 Float# Float#
