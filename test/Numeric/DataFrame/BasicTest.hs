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
import           Data.Maybe

import           Numeric.DataFrame
import           Numeric.Dimensions


-- | Generating random DataFrames
newtype SimpleDF (ds :: [Nat] ) = SimpleDF { getDF :: DataFrame Float ds}
type ProperDims ds =
  ( Eq (DataFrame Float ds)
  , Num (DataFrame Float ds)
  , Ord (DataFrame Float ds)
  , Fractional (DataFrame Float ds)
  , Floating (DataFrame Float ds)
  , Dimensions ds
  , SubSpace Float '[] ds ds
  )

instance ( SubSpace Float '[] ds ds
         , Num (DataFrame Float ds)
         ) => Arbitrary (SimpleDF (ds :: [Nat])) where
  arbitrary = SimpleDF <$> elementWise (dim @ds) f 0
    where
      f :: Scalar Float -> Gen (Scalar Float)
      f _ = scalar <$> choose (-10000,100000)
  shrink sdf = SimpleDF <$> elementWise (dim @ds) f (getDF sdf)
    where
      f :: Scalar Float -> [Scalar Float]
      f = fmap scalar . shrink . unScalar

data SomeSimpleDF
  = forall (ds :: [Nat]) . ProperDims ds
    => SomeSimpleDF ( SimpleDF ds )


instance Arbitrary SomeSimpleDF where
  arbitrary = do
    dimN <- choose (0, 6) :: Gen Int
    dims <- mapM (\_ -> choose (2, 9) :: Gen Int) [1..dimN]
    let eGen = withRuntimeDim dims (\(ds :: Dim ds) -> SomeSimpleDF <$> (arbitrary :: Gen (SimpleDF ds)) )
    case eGen of
      Left s -> error $ "Cannot generate arbitrary SomeSimpleDF: " ++ s
      Right v -> v
  shrink (SomeSimpleDF x) = SomeSimpleDF <$> shrink x



x :: Vec2f
x = vec2 (-1) 2

y :: Vec2f
y = vec2 2 1

eps :: Vec2f
eps = 0.0001

prop_Comparisons :: Bool
prop_Comparisons
  = and
    [ x < vec2 2 3
    , x <= vec2 1 2
    , x <= x, x <= x
    , x >= x, y >= y
    , x == x, y == y
    , x < x + 1, x > x - 1
    , abs x >= x, abs (-x) >= (-x)
    ]
  && all not
    [ x > x, x < x
    , y > y, y < y
    , y > x, y < x, y >= x, y <= x, y == x
    ]

prop_Numeric :: Bool
prop_Numeric = and
    [ x + x == 2 * x
    , x + y == y + x
    , x / 2 + x / 2 == x
    , x * y == y * x
    , x * 0 + vec2 1 5 == vec2 3 5 * 3 - vec2 8 10
    , abs (sin x * sin x + cos x * cos x - 1) <= 0.0001
    ]


prop_Floating :: Bool
prop_Floating = and
    [ abs (sin x * sin x + cos x * cos x - 1) <= eps
    , abs (exp x * exp y - exp (x + y)) <= eps
    , abs (exp (log $ abs x) - abs x) <= eps
    , abs (sin (asin (sin y)) - sin y) <= eps
    , abs (cos (acos (cos x)) - cos x) <= eps
    ]


return []
runTests :: IO Bool
runTests = $quickCheckAll
