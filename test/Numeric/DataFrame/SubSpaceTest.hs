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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where

import           Test.QuickCheck
import           Data.Type.Equality
import           Numeric.DataFrame.Arbitraries
--import           Data.Proxy

import           Numeric.DataFrame
import           Numeric.Dimensions




-- prop_Dims :: SomeSimpleDF -> SomeSimpleDF -> Bool
-- prop_Dims (SSDF (SDF x)) (SSDF (SDF y))
--   | ConcatEvidence <- concatEvidence x y
--   = order dimXY == order x + order y
--  && totalDim dimXY == totalDim x * totalDim y


prop_Eye :: SomeSimpleDFNonScalar -> Bool
prop_Eye (SSDFN (SDF (x :: DataFrame Float (d ': ds))))
  = case ( unsafeEqProof :: Prefix ds (d ': ds) :~: '[d]
         , unsafeEqProof :: IsSuffix ds (d ': ds) :~: 'True
         , unsafeEqProof :: Suffix '[d] (d ': ds) :~: ds
         , unsafeEqProof :: Concat '[d] ds :~: d ': ds
         ) of
    (Refl, Refl, Refl, Refl) -> eye %* x == x

--
-- prop_VariousFixed :: SimpleDF '[2,5,4] -> SimpleDF '[3,7] -> Bool
-- prop_VariousFixed (SDF x) (SDF y) = and
--    [ ((dimMax `inSpaceOf` y) !. z) == x
--    , (1:!3 !. z) == x
--    , (2:!2 !. z) %* eye == x
--    , ewfoldl y (+) 10 z == ewfoldr y (+) 0 z + 10
--    , y * 2 == ewmap (dim @'[7]) (*2) y
--    ]
--   where
--     z = ewgen y x :: DataFrame Float '[2,5,4,3,7]


return []
runTests :: IO Bool
runTests = $quickCheckAll
