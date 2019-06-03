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
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Numeric.DataFrame.BasicTest (runTests) where

import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries    ()
import qualified Numeric.ProductOrd.NonTransitive as NonTransitive
import qualified Numeric.ProductOrd.Partial       as Partial
import           Test.QuickCheck

{-# ANN module "HLint: ignore Use <" #-}
{-# ANN module "HLint: ignore Use >" #-}
{-# ANN module "HLint: ignore Use >=" #-}
{-# ANN module "HLint: ignore Use <=" #-}
{-# ANN module "HLint: ignore Redundant compare" #-}

(===>) :: Bool -> Bool -> Bool
a ===> b = not a || b
infix 2 ===>

prop_vanillaOrdTransitivity :: SomeDataFrame '[Double, Double, Double] -> Property
prop_vanillaOrdTransitivity (SomeDataFrame (x :*: y :*: z :*: Z))
  = property $ (x <= y) && (y <= z) ===> x <= z

vanillaOrdComparisons :: Ord t => t -> t -> Property
vanillaOrdComparisons x y
  = conjoin
    [ counterexample "(x < y) == (compare x y == LT)"
          $ (x < y) == (compare x y == LT)
    , counterexample "(x > y) == (compare x y == GT)"
          $ (x > y) == (compare x y == GT)
    , counterexample "(x == y) == (compare x y == EQ)"
          $ (x == y) == (compare x y == EQ)
    , counterexample "min x y == if x <= y then x else y"
          $ min x y == if x <= y then x else y
    , counterexample "max x y == if x >= y then x else y"
          $ max x y == if x >= y then x else y
    ]


comparisons :: (Ord t, Num t) => t -> t -> Property
comparisons x y
  = conjoin
    [ counterexample "abs x <= abs x + abs y" $ abs x <= abs x + abs y
    , counterexample "x <= x" $ x <= x
    , counterexample "y <= y" $ y <= y
    , counterexample "x >= x" $ x >= x
    , counterexample "y >= y" $ y >= y
    , counterexample "x == x" $ x == x
    , counterexample "y == y" $ y == y
    , counterexample "(x > y) == (y < x)"
                              $ (x > y) == (y < x)
    , counterexample "(x >= y) == (y <= x)"
                              $ (x >= y) == (y <= x)
    , counterexample "(x < y) == (x <= y && x /= y)"
                              $ (x < y) == (x <= y && x /= y)
    , counterexample "(x > y) == (x >= y && x /= y)"
                              $ (x > y) == (x >= y && x /= y)
    , counterexample "x < x + 1" $ abs x < 10000000 ===> x < x + 1
    , counterexample "x > x - 1" $ abs x < 10000000 ===> x > x - 1
    , counterexample "abs x >= x" $ abs x >= x
    , counterexample "abs (-x) >= (-x)" $ abs (-x) >= (-x)
    , counterexample "x > y ==> x >= y"
      $ x > y            ===> x >= y
    , counterexample "x < y ==> x <= y"
      $ x < y            ===> x <= y
    , counterexample "not (x >= y) ==> not (x > y)"
      $ not (x >= y)     ===> not (x > y)
    , counterexample "not (x <= y) ==> not (x < y)"
      $ not (x <= y)     ===> not (x < y)
    , counterexample "x == y ==> not GT or LT, but GE and LE"
      $ x == y           ===> and [ not (x > y), not (y < x), x >= y, y <= x ]
    , counterexample "x >= y && x <= y ==> x == y"
      $ x >= y && x <= y ===> x == y
    ]

prop_comparisonsVanillaOrdFloat :: SomeDataFrame '[Float, Float] -> Property
prop_comparisonsVanillaOrdFloat (SomeDataFrame (x :*: y :*: Z)) =
  comparisons x y .&&. vanillaOrdComparisons x y
prop_comparisonsVanillaOrdDouble :: SomeDataFrame '[Double, Double] -> Property
prop_comparisonsVanillaOrdDouble (SomeDataFrame (x :*: y :*: Z)) =
  comparisons x y .&&. vanillaOrdComparisons x y
prop_comparisonsVanillaOrdInt :: SomeDataFrame '[Int, Int] -> Property
prop_comparisonsVanillaOrdInt (SomeDataFrame (x :*: y :*: Z)) =
  comparisons x y .&&. vanillaOrdComparisons x y


prop_comparisonsPPOrdFloat :: SomeDataFrame '[Float, Float] -> Property
prop_comparisonsPPOrdFloat (SomeDataFrame (x :*: y :*: Z))
  = comparisons (Partial.ProductOrd x) (Partial.ProductOrd y)
prop_comparisonsPPOrdDouble :: SomeDataFrame '[Double, Double] -> Property
prop_comparisonsPPOrdDouble (SomeDataFrame (x :*: y :*: Z))
  = comparisons (Partial.ProductOrd x) (Partial.ProductOrd y)
prop_comparisonsPPOrdInt :: SomeDataFrame '[Int, Int] -> Property
prop_comparisonsPPOrdInt (SomeDataFrame (x :*: y :*: Z))
  = comparisons (Partial.ProductOrd x) (Partial.ProductOrd y)


prop_comparisonsNTPOrdFloat :: SomeDataFrame '[Float, Float] -> Property
prop_comparisonsNTPOrdFloat (SomeDataFrame (x :*: y :*: Z))
  = comparisons (NonTransitive.ProductOrd x) (NonTransitive.ProductOrd y)
prop_comparisonsNTPOrdDouble :: SomeDataFrame '[Double, Double] -> Property
prop_comparisonsNTPOrdDouble (SomeDataFrame (x :*: y :*: Z))
  = comparisons (NonTransitive.ProductOrd x) (NonTransitive.ProductOrd y)
prop_comparisonsNTPOrdInt :: SomeDataFrame '[Int, Int] -> Property
prop_comparisonsNTPOrdInt (SomeDataFrame (x :*: y :*: Z))
  = comparisons (NonTransitive.ProductOrd x) (NonTransitive.ProductOrd y)


prop_Numeric :: SomeDataFrame '[Int, Int] -> Bool
prop_Numeric (SomeDataFrame (x :*: y :*: Z))
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
  | lx <- log (0.01 + abs x)
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
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
