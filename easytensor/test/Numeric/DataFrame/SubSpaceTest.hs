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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where

import           Test.QuickCheck
import           Numeric.DataFrame.Arbitraries
import           Data.Proxy

import           Numeric.DataFrame
import           Numeric.Dimensions




prop_Dims :: SomeSimpleDF -> SomeSimpleDF -> Bool
prop_Dims (SSDF (SDF (x :: DataFrame Float xs))) (SSDF (SDF (y :: DataFrame Float ys)))
    | DimensionsEvidence <- inferConcatDimensions x y
    = order (Proxy @(xs ++ ys)) == order x + order y
      && totalDim (Proxy @(xs ++ ys)) == totalDim x * totalDim y


prop_Eye :: SomeSimpleDFNonScalar -> Bool
prop_Eye (SSDFN (SDF (x :: DataFrame Float (d ': ds))))
  | DimensionsEvidence <- inferTailDimensions x
  , DimensionsEvidence <- inferInitDimensions x
  , NonEmptyListEvidence <- inferNonEmptyList @d @ds
  , ValidDim <- inferLastValidDim @d @ds
  = eye %* x == x && x == x %* eye


prop_IndexDimMax :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_IndexDimMax (SDF x) (SDF y) =
   ((dimMax `inSpaceOf` y) !. z) == x
  where
    z = ewgen y x :: DataFrame Float '[2,5,4,3,7]

prop_IndexCustom1 :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_IndexCustom1 (SDF x) (SDF y) = (1:!3 !. z) == x
  where
    z = ewgen y x :: DataFrame Float '[2,5,4,3,7]

prop_IndexCustom2 :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_IndexCustom2 (SDF x) (SDF y) = (2:!2 !. z) %* eye == x
  where
    z = ewgen y x :: DataFrame Float '[2,5,4,3,7]

prop_Foldlr :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_Foldlr (SDF x) (SDF y) =
   abs (ewfoldl y (+) 10 z - ewfoldr y (+) 0 z - 10) < ewgen x (zmax * 0.0001)
  where
    z = ewgen y x :: DataFrame Float '[2,5,4,3,7]
    zmax = ewfoldl z (max . abs) 0 z

prop_Ewmap :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
prop_Ewmap (SDF _) (SDF y) =
   y * 2 == ewmap (dim @'[7]) (*2) y


return []
runTests :: IO Bool
runTests = $quickCheckAll
