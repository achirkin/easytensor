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

natSum :: Proxy# a -> Proxy# b -> Proxy# (a+b)
natSum _ _ = proxy#
natMul :: Proxy# a -> Proxy# b -> Proxy# (a*b)
natMul _ _ = proxy#
natRem :: Proxy# a -> Proxy# b -> Proxy# (a-b)
natRem _ _ = proxy#
natSucc :: Proxy# a -> Proxy# (a + 1)
natSucc _ = proxy#
natPred :: Proxy# a -> Proxy# (a - 1)
natPred _ = proxy#

prop_KnownNats :: Int -> Int -> Bool
prop_KnownNats a b
  | x <- max (abs a) (abs b)
  , y <- min (abs a) (abs b)
  , z <- y `mod` 50
  , Just (SomeDim (px :: Proxy# x)) <- someDimVal x
  , Just (SomeDim (py :: Proxy# y)) <- someDimVal y
  , Just (SomeDim (px1 :: Proxy# x1)) <- someDimVal (x+1)
  , Just (SomeDim (_ :: Proxy# z)) <- someDimVal z
  , Just Evidence <- (\e1 e2 e3 -> e1 `sumEvs` e2 `sumEvs` e3)
        <$> inferMinusKnownDimM @x @y
        <*> inferMinusKnownDimM @x1 @1
        <*> Just ( inferPlusKnownDim @x @y
          `sumEvs` inferTimesKnownDim @x @y
          `sumEvs` inferPlusKnownDim @y @1
         )
  = and
    [ x + y == dimVal# (natSum px py)
    , x * y == dimVal# (natMul px py)
    , x - y == dimVal# (natRem px py)
    , x == dimVal# (natPred px1)
    , y + 1 == dimVal# (natSucc py)
    ]
prop_KnownNats _ _ = True



-- * Test props on a single type-level list

prop_FiniteList :: Int -> [Int] -> Bool
prop_FiniteList a xs'
  | n <- (abs a)
  , xs <- (2+) . abs <$> xs'
  , Just (SomeDim (_ :: Proxy# n)) <- someDimVal n
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
