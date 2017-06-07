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
-- License     :  BSD3
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


-- * Test simple binary nat ops

natSum :: Proxy a -> Proxy b -> Proxy (a+b)
natSum _ _ = Proxy
natMul :: Proxy a -> Proxy b -> Proxy (a*b)
natMul _ _ = Proxy
natPow :: Proxy a -> Proxy b -> Proxy (a^b)
natPow _ _ = Proxy
natRem :: Proxy a -> Proxy b -> Proxy (a-b)
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

-- * Test composite nat binary ops

natF31 :: Proxy a -> Proxy b -> Proxy c -> Proxy (a + b*c)
natF31 _ _ _ = Proxy
natF32 :: Proxy a -> Proxy b -> Proxy c -> Proxy ((a + b^2)*c + 5)
natF32 _ _ _ = Proxy
natF33 :: Proxy a -> Proxy b -> Proxy c -> Proxy ((a*a^2 + b)*(c+1) - a^3)
natF33 _ _ _ = Proxy

prop_3KnownNats :: Integer -> Integer -> Integer -> Bool
prop_3KnownNats x y z
  | a <- abs x, b <- abs y, c <- abs z
  , Just (SomeNat pa) <- someNatVal a
  , Just (SomeNat pb) <- someNatVal b
  , Just (SomeNat pc) <- someNatVal c
  = and
    [ a + b*c                       == natVal (natF31 pa pb pc)
    , (a + b^sq)*c + 5              == natVal (natF32 pa pb pc)
    , (a*a^sq + b)*(c+1) - a^(sq+1) == natVal (natF33 pa pb pc)
    ]
  where
    sq = 2 :: Integer
prop_3KnownNats _ _ _ = True


-- * Test props on a single type-level list

prop_FiniteList :: Integer -> [Int] -> Bool
prop_FiniteList a xs'
  | n <- (abs a)
  , xs <- (2+) . abs <$> xs'
  , Just (SomeNat pn) <- someNatVal n
  , Just (SomeDim pxs) <- someDimVal xs
  = and [ order pxs == length xs
        , case inferTakeNFiniteList pn pxs of
            fle@FiniteListEvidence ->
              order fle == length (take (fromInteger n) xs)
        , case inferDropNFiniteList pn pxs of
            fle@FiniteListEvidence ->
              order fle == length (drop (fromInteger n) xs)
        , case inferReverseFiniteList pxs of
            fle@FiniteListEvidence ->
              order fle == length (reverse xs)
        ]
prop_FiniteList _ _ = False

-- * Test KnownNat propagation for lists of different length

listShift1 :: FiniteList (x1 ': x2 ': x3 ': xs :: [Nat])
           => Proxy (x1 ': x2 ': x3 ': xs)
           -> Proxy '[y1,y2,y3]
           -> Proxy (Length (y1 ': y2 ': y3 ': xs))
listShift1 _ _ = Proxy
listShift2 :: FiniteList (x1 ': x2 ': x3 ': xs :: [Nat])
           => Proxy (x1 ': x2 ': x3 ': xs)
           -> Proxy '[y1,y2,y3,y4,y5]
           -> Proxy (Length (y1 ': y2 ': y3 ': y4 ': y5 ': xs))
listShift2 _ _ = Proxy
listShift3 :: FiniteList (x1 ': x2 ': x3 ': x4 ': x5 ': xs :: [Nat])
           => Proxy (x1 ': x2 ': x3 ': x4 ': x5 ': xs)
           -> Proxy '[y1,y2]
           -> Proxy (Length (y1 ': y2 ': xs) * 2 - 1)
listShift3 _ _ = Proxy
-- | result is of the same length
listShiftApp1 :: FiniteList (x1 ': x2 ': x3 ': xs :: [Nat])
              => Proxy (x1 ': x2 ': x3 ': xs)
              -> Integer
listShiftApp1 pxs = natVal (listShift1 pxs (Proxy @'[1,2,3]))
-- | result is longer by 2
listShiftApp2 :: FiniteList (x1 ': x2 ': x3 ': xs :: [Nat])
              => Proxy (x1 ': x2 ': x3 ': xs)
              -> Integer
listShiftApp2 pxs = natVal (listShift2 pxs (Proxy @'[1,2,3,4,5]))
-- | result is shorter by 3, but multiplied by 2 and -1
listShiftApp3 :: FiniteList (x1 ': x2 ': x3 ': x4 ': x5 ': xs :: [Nat])
              => Proxy (x1 ': x2 ': x3 ': x4 ': x5 ': xs)
              -> Integer
listShiftApp3 pxs = natVal (listShift3 pxs (Proxy @'[1,2]))


prop_ListShift :: [Int] -> Bool
prop_ListShift xs'
  | xs <- (2+) . abs <$> xs'
  , n0 <- fromIntegral (length xs) :: Integer
  , Just (SomeDim (_ :: Dim xs)) <- someDimVal xs
  , ys <- Proxy @(1 ': 2 ': 3 ': 4 ': 5 ': 6 ': 7 ': xs)
  = and [ listShiftApp1 ys == n0 + 7
        , listShiftApp2 ys == n0 + 7 + 2
        , listShiftApp3 ys == (n0 + 7 - 3)*2 - 1
        ]
prop_ListShift _ = False

-- * Inference properties

prop_ListInference :: [Int] -> [Int] -> Bool
prop_ListInference xs' ys'
  | xs <- (2+) . abs <$> xs'
  , ys <- (2+) . abs <$> ys'
  , Just (SomeDim (dxs :: Dim xs)) <- someDimVal xs
  , Just (SomeDim (dys :: Dim ys)) <- someDimVal ys
  = and [ case inferConcatFiniteList dxs dys of
            fle@FiniteListEvidence ->
              order fle == length xs + length ys
        , case (inferConcat dxs dys, inferConcatFiniteList dxs dys) of
            (ConcatEvidence, FiniteListEvidence) ->
              case inferPrefixFiniteList dys (Proxy @(xs ++ ys)) of
                fle@FiniteListEvidence ->
                  order fle == length xs
        , case (inferConcat dxs dys, inferConcatFiniteList dxs dys) of
            (ConcatEvidence, FiniteListEvidence) ->
              case inferSuffixFiniteList dxs (Proxy @(xs ++ ys)) of
                fle@FiniteListEvidence ->
                  order fle == length ys
        ]
prop_ListInference _ _ = False

return []
runTests :: IO Bool
runTests = $quickCheckAll
