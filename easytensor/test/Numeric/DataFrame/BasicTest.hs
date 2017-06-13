-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.BasicTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- A set of basic validity tests for DataFrame type.
-- Num, Ord, Fractional, Floating, etc
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.DataFrame.BasicTest (runTests) where

import           Numeric.DataFrame.Arbitraries
import           Test.QuickCheck




{-# ANN prop_Comparisons "HLint: ignore" #-}
prop_Comparisons :: SomeSimpleDFPair -> Bool
prop_Comparisons (SSDFP (SDF x) (SDF y))
  = and
    [ abs x >= abs x / 2
    , abs x <= abs x + abs y
    , x <= x, x <= x
    , x >= x, y >= y
    , x == x, y == y
    , x < x + 1, x > x - 1
    , abs x >= x, abs (-x) >= (-x)
    , x > y            ===> x >= y
    , x < y            ===> x <= y
    , not (x >= y)     ===> not (x > y)
    , not (x <= y)     ===> not (x < y)
    , x == y           ===> and [ not (x > y), not (y < x), x >= y, y <= x ]
    , x >= y && x <= y ===> x == y
    ]
  where
    a ===> b = not a || b
    infix 2 ===>


prop_Numeric :: SomeSimpleDFPair -> Bool
prop_Numeric (SSDFP (SDF x) (SDF y))
  = and
    [ x + x == 2 * x
    , x + y == y + x
    , x + y == max x y + min x y
    , abs x * signum x == x
    , x / 2 + x / 2 == x
    , x * y == y * x
    , x * 0 + y == y
    ]


prop_Floating :: SomeSimpleDFPair -> Bool
prop_Floating (SSDFP (SDF x) (SDF y))
  = all ((eps >=) . abs)
    [ sin x * sin x + cos x * cos x - 1
    , exp lx * exp ly  / exp (lx + ly) - 1
    , exp (log $ 1 + abs x) / (1 + abs x) - 1
    , sin (asin (sin y)) - sin y
    , cos (acos (cos x)) - cos x
    ]
  where
    lx = log (0.01 + abs x)
    ly = log (0.01 + abs y)
    eps = 0.001


return []
runTests :: IO Bool
runTests = $quickCheckAll
