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
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Numeric.DataFrame.BasicTest (runTests) where

import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries ()
import           Numeric.Dimensions
import           Test.QuickCheck


prop_Comparisons :: SomeDataFrame '[Float, Float] -> Bool
prop_Comparisons (SomeDataFrame (x :*: y :*: Z))
  | E <- inferOrd x
  , E <- inferFractional x
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

prop_Numeric :: SomeDataFrame '[Int, Int] -> Bool
prop_Numeric (SomeDataFrame (x :*: y :*: Z))
  | E <- inferOrd x
  , E <- inferNum x
  = and
    [ x + x == 2 * x
    , x + y == y + x
    , x + y == max x y + min x y
    , abs x * signum x == x
    , x * y == y * x
    , x * 0 + y == y
    ]


prop_Floating :: SomeDataFrame '[Double, Double] -> Bool
prop_Floating (SomeDataFrame (x :*: y :*: Z))
  | E <- inferOrd x
  , E <- inferFloating x
  , lx <- log (0.01 + abs x)
  , ly <- log (0.01 + abs y)
  , eps <- 0.001
  = all ((eps >=) . abs)
    [ sin x * sin x + cos x * cos x - 1
    , exp lx * exp ly  / exp (lx + ly) - 1
    , exp (log $ 1 + abs x) / (1 + abs x) - 1
    , sin (asin (sin y)) - sin y
    , cos (acos (cos x)) - cos x
    ]


return []
runTests :: IO Bool
runTests = $quickCheckAll
