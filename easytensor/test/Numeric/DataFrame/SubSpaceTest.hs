-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpaceTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where

import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries
import           Numeric.Dimensions
import           Test.QuickCheck




prop_Dims :: SomeSimpleDF -> SomeSimpleDF -> Bool
prop_Dims (SSDF (SDF (x :: DataFrame Float xs))) (SSDF (SDF (y :: DataFrame Float ys)))
    | Evidence <- inferConcatDimensions @xs @ys
    , Evidence <- inferConcatFiniteList @xs @ys
    = order @_ @(xs ++ ys) == order @_ @xs + order @_ @ys
      && totalDim (Proxy @(xs ++ ys)) == totalDim x * totalDim y

prop_Eye :: SomeSimpleDFNonScalar -> Bool
prop_Eye (SSDFN (SDF (x :: DataFrame Float ds)))
  | Just Evidence <- sumEvs <$> inferUnConsDimensions @ds
                            <*> inferUnSnocDimensions @ds
    = eye %* x == x && x == x %* eye
  | otherwise = False


prop_IndexDimMax :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_IndexDimMax (SDF x) (SDF y) =
   ((maxBound `inSpaceOf` y) !. z) == x
  where
    z = ewgen x :: DataFrame Float '[2,5,4,3,7]

prop_IndexCustom1 :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_IndexCustom1 (SDF x) (SDF _) = (1:!3 !. z) == x
  where
    z = ewgen x :: DataFrame Float '[2,5,4,3,7]


prop_IndexCustom2 :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_IndexCustom2 (SDF x) (SDF _) = (2:!2 !. z) %* eye == x
  where
    z = ewgen x :: DataFrame Float '[2,5,4,3,7]


prop_Foldlr :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_Foldlr (SDF x) (SDF _) =
   abs (ewfoldl (+) 10 z - ewfoldr @_ @'[2,5,4] (+) 0 z - 10) <= fromScalar (zmax * 0.0001)
  where
    z = ewgen x :: DataFrame Float '[2,5,4,3,7]
    zmax = ewfoldl @Float @'[] @'[2,5,4,3,7] (max . abs) 0.001 z

prop_Ewmap :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_Ewmap (SDF _) (SDF y) =
   y * 2 == ewmap @_ @'[3] (*2) y


return []
runTests :: IO Bool
runTests = $quickCheckAll
