-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.BasicTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- A set of basic validity tests for DataFrame type.
-- Num, Ord, Fractional, Floating, etc
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}

module Numeric.DataFrame.BasicTest (runTests) where

import           GHC.TypeLits
import           Test.QuickCheck

import           Numeric.DataFrame
import           Numeric.Dimensions


maxDims :: Int
maxDims = 5

maxDimSize :: Int
maxDimSize = 7

-- | Generating random DataFrames
newtype SimpleDF (ds :: [Nat] ) = SDF { getDF :: DataFrame Float ds}
data SomeSimpleDF = forall (ds :: [Nat])
                  . (Dimensions ds, FPDataFrame Float ds)
                 => SSDF !(SimpleDF ds)
data SomeSimpleDFPair = forall (ds :: [Nat])
                      . (Dimensions ds, FPDataFrame Float ds)
                     => SSDFP !(SimpleDF ds) !(SimpleDF ds)

instance ( Dimensions ds
         , FPDataFrame Float ds
         ) => Arbitrary (SimpleDF (ds :: [Nat])) where
  arbitrary = SDF <$> elementWise (dim @ds) f 0
    where
      f :: Scalar Float -> Gen (Scalar Float)
      f _ = scalar <$> choose (-10000,100000)
  shrink sdf = SDF <$> elementWise (dim @ds) f (getDF sdf)
    where
      f :: Scalar Float -> [Scalar Float]
      f = fmap scalar . shrink . unScalar


instance Arbitrary SomeSimpleDF where
  arbitrary = do
    dimN <- choose (0, maxDims) :: Gen Int
    dims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Int) [1..dimN]
    let eGen = withRuntimeDim dims $
          \(_ :: Dim ds) -> inferFloating (undefined :: DataFrame Float ds) $
            \_ -> SSDF <$> (arbitrary :: Gen (SimpleDF ds))
    case eGen of
      Left s -> error $ "Cannot generate arbitrary SomeSimpleDF: " ++ s
      Right v -> v
  shrink (SSDF x) = SSDF <$> shrink x

instance Arbitrary SomeSimpleDFPair where
  arbitrary = do
    dimN <- choose (0, maxDims) :: Gen Int
    dims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Int) [1..dimN]
    let eGen = withRuntimeDim dims $
          \(_ :: Dim ds) -> inferFloating (undefined :: DataFrame Float ds) $
            \_ -> SSDFP <$> (arbitrary :: Gen (SimpleDF ds)) <*> (arbitrary :: Gen (SimpleDF ds))
    case eGen of
      Left s -> error $ "Cannot generate arbitrary SomeSimpleDF: " ++ s
      Right v -> v
  shrink (SSDFP x y) = SSDFP <$> shrink x <*> shrink y

instance Show (DataFrame Float ds) => Show (SimpleDF ds) where
  show (SDF sdf) = show sdf
instance Show SomeSimpleDF where
  show (SSDF sdf) = show sdf
instance Show SomeSimpleDFPair where
  show (SSDFP x y) = "Pair:\n" ++ show (x,y)


{-# ANN prop_Comparisons "HLint: ignore" #-}
prop_Comparisons :: SomeSimpleDFPair -> Bool
prop_Comparisons (SSDFP (SDF x) (SDF y))
  = and
    [ abs x >= abs x / 2
    , abs x <= abs x + abs y
    , x <= x, x <= x
    , x >= x, y >= y
    , x == x, y == y
    , x < x + 1, x > x - 1
    , abs x >= x, abs (-x) >= (-x)
    , x > y            ===> x >= y
    , x < y            ===> x <= y
    , not (x >= y)     ===> not (x > y)
    , not (x <= y)     ===> not (x < y)
    , x == y           ===> and [ not (x > y), not (y < x), x >= y, y <= x ]
    , x >= y && x <= y ===> x == y
    ]
  where
    a ===> b = not a || b
    infix 2 ===>


prop_Numeric :: SomeSimpleDFPair -> Bool
prop_Numeric (SSDFP (SDF x) (SDF y))
  = and
    [ x + x == 2 * x
    , x + y == y + x
    , x / 2 + x / 2 == x
    , x * y == y * x
    , x * 0 + y == y
    , abs (sin x * sin x + cos x * cos x - 1) <= 0.0001
    ]


prop_Floating :: SomeSimpleDFPair -> Bool
prop_Floating (SSDFP (SDF x) (SDF y))
  = and
    [ abs (sin x * sin x + cos x * cos x - 1) <= eps
    , abs (exp lx * exp ly  / exp (lx + ly) - 1) <= eps
    , abs (exp (log $ 1 + abs x) / (1 + abs x) - 1) <= eps
    , abs (sin (asin (sin y)) - sin y) <= eps
    , abs (cos (acos (cos x)) - cos x) <= eps
    ]
  where
    lx = log (0.001 + abs x)
    ly = log (0.001 + abs y)
    eps = 0.0001


return []
runTests :: IO Bool
runTests = $quickCheckAll
