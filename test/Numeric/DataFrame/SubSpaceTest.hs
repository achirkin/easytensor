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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where


import           Test.QuickCheck
import           Data.Type.Equality
import           Numeric.DataFrame.Arbitraries
--import           Data.Proxy

import           Numeric.DataFrame
import           Numeric.Dimensions




prop_Dims :: SomeSimpleDF -> SomeSimpleDF -> Bool
prop_Dims (SSDF (SDF x)) (SSDF (SDF y))
  | ConcatEvidence dimXY <- concatEvidence x y
  = order dimXY == order x + order y
 && totalDim dimXY == totalDim x * totalDim y


prop_Eye :: SomeSimpleDFNonScalar -> Bool
prop_Eye (SSDFN (SDF (x :: DataFrame Float (d ': ds))))
  = case ( unsafeEqProof :: Prefix ds (d ': ds) :~: '[d]
         , unsafeEqProof :: IsSuffix ds (d ': ds) :~: 'True
         , unsafeEqProof :: IsPrefix '[d] (d ': ds) :~: 'True
         , unsafeEqProof :: Suffix '[d] (d ': ds) :~: ds
         , unsafeEqProof :: Concat '[d] ds :~: d ': ds
         ) of
    (Refl, Refl, Refl, Refl, Refl) -> eye %* x == x

-- prop_Gen :: SomeSimpleDF -> SomeSimpleDF -> Bool
-- prop_Gen (SSDF (SDF x)) (SSDF (SDF y))
--   | ConcatEvidence dimXY <- concatEvidence x y
--   = ((dimMax `inSpaceOf` y) !. ewgen y x) == x




return []
runTests :: IO Bool
runTests = $quickCheckAll
