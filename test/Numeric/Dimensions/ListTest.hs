{-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
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
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.ListTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Testing type-level Lists and the inference plugin
--
-----------------------------------------------------------------------------


module Numeric.Dimensions.ListTest (runTests) where

import Data.Proxy
import GHC.TypeLits
import Test.QuickCheck

import Numeric.Dimensions



natSum :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Proxy (a+b)
natSum _ _ = Proxy
natMul :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Proxy (a*b)
natMul _ _ = Proxy
natPow :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Proxy (a^b)
natPow _ _ = Proxy
natRem :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Proxy (a-b)
natRem _ _ = Proxy
natSucc :: Proxy a -> Proxy (a + 1)
natSucc _ = Proxy
natPred :: Proxy a -> Proxy (a - 1)
natPred _ = Proxy

prop_KnownNats :: Integer -> Integer -> Bool
prop_KnownNats a b
  | x <- max (abs a) (abs b)
  , y <- min (abs a) (abs b)
  , z <- y `mod` 50
  , Just (SomeNat px) <- someNatVal x
  , Just (SomeNat py) <- someNatVal y
  , Just (SomeNat px1) <- someNatVal (x+1)
  , Just (SomeNat pz) <- someNatVal z
  = and
    [ x + y == natVal (natSum px py)
    , x * y == natVal (natMul px py)
    , (x+1) ^ z == natVal (natPow px1 pz)
    , x - y == natVal (natRem px py)
    , x == natVal (natPred px1)
    , y + 1 == natVal (natSucc py)
    ]
prop_KnownNats _ _ = True

prop_KnownList :: Integer -> [Int] -> Bool
prop_KnownList a xs'
  | n <- (abs a)
  , xs <- (2+) . abs <$> xs'
  , Just (SomeNat pn) <- someNatVal n
  = case ( withRuntimeDim xs $ \pxs -> and
          [ order pxs == length xs
          , case inferTakeNFiniteList pn pxs of
              fle@FiniteListEvidence ->
                order fle == length (take (fromInteger n) xs)
          ]
         ) of
      Left _ -> True
      Right b -> b
prop_KnownList _ _ = True



return []
runTests :: IO Bool
runTests = $quickCheckAll
