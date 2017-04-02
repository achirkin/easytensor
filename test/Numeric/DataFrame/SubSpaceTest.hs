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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where


import           Test.QuickCheck
import           Data.Type.Equality
import           Numeric.DataFrame.Arbitraries
import           Data.Proxy

import           Numeric.DataFrame
import           Numeric.Dimensions




prop_Dims :: SomeSimpleDF -> SomeSimpleDF -> Bool
prop_Dims (SSDF (SDF x)) (SSDF (SDF y))
  | ConcatEvidence dimXY <- concatEvidence x y
  = order dimXY == order x + order y
 && totalDim dimXY == totalDim x * totalDim y


prop_Eye :: SomeSimpleDFNonScalar -> Bool
prop_Eye (SSDFN (SDF (x :: DataFrame Float (d ': ds))))
  = case ( unsafeEqProof :: SimplifyList ('Prefix (ToList ds) ('Cons d (ToList ds)))
                        :~: 'Cons d 'Empty
         , unsafeEqProof :: SimplifyList ('Suffix ('Cons d 'Empty) ('Cons d (ToList ds)))
                        :~: ToList ds
         , listProof Proxy :: ListProof ds
         ) of
    (Refl, Refl, ListProof _) ->  eye %* x == x

-- prop_Gen :: SomeSimpleDF -> SomeSimpleDF -> Bool
-- prop_Gen (SSDF (SDF x)) (SSDF (SDF y))
--   | ConcatEvidence dimXY <- concatEvidence x y
--   = ((dimMax `inSpaceOf` y) !. ewgen y x) == x




return []
runTests :: IO Bool
runTests = $quickCheckAll
