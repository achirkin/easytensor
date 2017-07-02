-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Low-level implementations of data frames
--
-----------------------------------------------------------------------------

module Numeric.Array
  ( module Numeric.Array.Family
  ) where

import           Numeric.Array.Family
import           Numeric.Array.Family.ArrayD    ()
import           Numeric.Array.Family.ArrayF    ()
import           Numeric.Array.Family.ArrayI    ()
import           Numeric.Array.Family.ArrayI8   ()
import           Numeric.Array.Family.ArrayI16  ()
import           Numeric.Array.Family.ArrayI32  ()
import           Numeric.Array.Family.ArrayI64  ()
import           Numeric.Array.Family.ArrayW    ()
import           Numeric.Array.Family.ArrayW8   ()
import           Numeric.Array.Family.ArrayW16  ()
import           Numeric.Array.Family.ArrayW32  ()
import           Numeric.Array.Family.ArrayW64  ()

import           Numeric.Array.Family.FloatX2   ()
import           Numeric.Array.Family.FloatX3   ()
import           Numeric.Array.Family.FloatX4   ()
