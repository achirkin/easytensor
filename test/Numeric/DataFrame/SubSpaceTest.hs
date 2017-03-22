-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpaceTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where


import           Test.QuickCheck

import           Numeric.DataFrame
import           Numeric.Dimensions


return []
runTests :: IO Bool
runTests = $quickCheckAll
