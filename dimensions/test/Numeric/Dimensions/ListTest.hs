{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MagicHash                 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.ListTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Testing type-level Lists and the inference plugin
--
-----------------------------------------------------------------------------


module Numeric.Dimensions.ListTest (runTests) where

import           Test.QuickCheck    (quickCheckAll)

import           Numeric.TypeLits
import           Numeric.Dimensions

-- * Test simple binary nat ops

natSum :: Dim a -> Dim b -> Proxy (a+b)
natSum _ _ = Proxy
natMul :: Dim a -> Dim b -> Proxy (a*b)
natMul _ _ = Proxy
natRem :: Dim a -> Dim b -> Proxy (a-b)
natRem _ _ = Proxy
natSucc :: Dim a -> Proxy (a + 1)
natSucc _ = Proxy
natPred :: Dim a -> Proxy (a - 1)
natPred _ = Proxy

prop_KnownNats :: Int -> Int -> Bool
prop_KnownNats a b
  | x <- max (abs a) (abs b)
  , y <- min (abs a) (abs b)
  , z <- y `mod` 50
  , Just (SomeDim (px@Dn :: Dim x)) <- someDimVal x
  , Just (SomeDim (py@Dn :: Dim y)) <- someDimVal y
  , Just (SomeDim (px1@Dn :: Dim x1)) <- someDimVal (x+1)
  , Just (SomeDim (Dn :: Dim z)) <- someDimVal z
  , Just Evidence <- (\e1 e2 e3 -> e1 `sumEvs` e2 `sumEvs` e3)
        <$> inferMinusKnownDimM @x @y
        <*> inferMinusKnownDimM @x1 @1
        <*> Just ( inferPlusKnownDim @x @y
          `sumEvs` inferTimesKnownDim @x @y
          `sumEvs` inferPlusKnownDim @y @1
         )
  = and
    [ x + y == intNatVal (natSum px py)
    , x * y == intNatVal (natMul px py)
    , x - y == intNatVal (natRem px py)
    , x == intNatVal (natPred px1)
    , y + 1 == intNatVal (natSucc py)
    ]
prop_KnownNats _ _ = True



-- * Test props on a single type-level list

prop_FiniteList :: Int -> [Int] -> Bool
prop_FiniteList a xs'
  | n <- (abs a)
  , xs <- (2+) . abs <$> xs'
  , Just (SomeDim (Dn :: Dim n)) <- someDimVal n
  , Just (SomeDims (pxs :: Dim xs)) <- someDimsVal xs
  , Evidence <- reifyDimensions pxs
  , Evidence <- inferDimFiniteList @xs
  = and [ order @_ @xs == length xs
        , case inferTakeNFiniteList @n @xs of
            Evidence -> order @_ @(Take n xs) == length (take n xs)
        , case inferDropNFiniteList @n @xs of
            Evidence -> order @_ @(Drop n xs) == length (drop n xs)
        , case inferReverseFiniteList @xs of
            Evidence -> order @_ @(Reverse xs) == length (reverse xs)
        ]
prop_FiniteList _ _ = False




-- * Inference properties

prop_ListInference :: [Int] -> [Int] -> Bool
prop_ListInference xs' ys'
  | xs <- (2+) . abs <$> xs'
  , ys <- (2+) . abs <$> ys'
  , Just (SomeDims (dxs :: Dim xs)) <- someDimsVal xs
  , Just (SomeDims (dys :: Dim ys)) <- someDimsVal ys
  , Evidence <- reifyDimensions dxs
  , Evidence <- reifyDimensions dys
  , Evidence <- inferDimFiniteList @xs
       `sumEvs` inferDimFiniteList @ys
  = and [ case inferConcatFiniteList @xs @ys of
            Evidence -> order @_ @(xs ++ ys) == length xs + length ys
        , case inferConcat @xs @ys `sumEvs`
               inferConcatFiniteList @xs @ys of
            Evidence -> case inferPrefixFiniteList @ys @(xs ++ ys) of
              Evidence -> order @_ @(Prefix ys (xs ++ ys)) == length xs
        , case inferConcat @xs @ys `sumEvs`
               inferConcatFiniteList @xs @ys of
            Evidence -> case inferSuffixFiniteList @xs @(xs ++ ys) of
              Evidence -> order @_ @(Suffix xs (xs ++ ys)) == length ys
        ]
prop_ListInference _ _ = False

return []
runTests :: IO Bool
runTests = $quickCheckAll
