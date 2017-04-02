-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.BasicTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- A set of basic validity tests for DataFrame type.
-- Num, Ord, Fractional, Floating, etc
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}

module Numeric.DataFrame.BasicTest (runTests) where

import           Test.QuickCheck


import Numeric.DataFrame.Arbitraries




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
    , x / 2 + x / 2 == x
    , x * y == y * x
    , x * 0 + y == y
    , abs (sin x * sin x + cos x * cos x - 1) <= 0.0001
    ]


prop_Floating :: SomeSimpleDFPair -> Bool
prop_Floating (SSDFP (SDF x) (SDF y))
  = and
    [ abs (sin x * sin x + cos x * cos x - 1) <= eps
    , abs (exp lx * exp ly  / exp (lx + ly) - 1) <= eps
    , abs (exp (log $ 1 + abs x) / (1 + abs x) - 1) <= eps
    , abs (sin (asin (sin y)) - sin y) <= eps
    , abs (cos (acos (cos x)) - cos x) <= eps
    ]
  where
    lx = log (0.001 + abs x)
    ly = log (0.001 + abs y)
    eps = 0.0001


return []
runTests :: IO Bool
runTests = $quickCheckAll
