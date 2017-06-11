-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.BasicTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Numeric.DataFrame.Arbitraries where

import           GHC.TypeLits
import           Test.QuickCheck
import           Data.Type.Equality
--import           Data.Proxy
import           Unsafe.Coerce

import           Numeric.Commons
import           Numeric.DataFrame
import           Numeric.Dimensions



maxDims :: Int
maxDims = 5

maxDimSize :: Int
maxDimSize = 7

-- | Fool typechecker by saying that a ~ b
unsafeEqProof :: forall (a :: k) (b :: k) . a :~: b
unsafeEqProof = unsafeCoerce Refl



-- | Generating random DataFrames
newtype SimpleDF (ds :: [Nat] ) = SDF { getDF :: DataFrame Float ds}
data SomeSimpleDF = forall (ds :: [Nat])
                  . (Dimensions ds, NumericFrame Float ds)
                 => SSDF !(SimpleDF ds)
data SomeSimpleDFNonScalar
    = forall (ds :: [Nat]) (a :: Nat) (as :: [Nat])
    . ( Dimensions ds
      , NumericFrame Float ds
      , ds ~ (a :+ as)
      )
   => SSDFN !(SimpleDF ds)
data SomeSimpleDFPair = forall (ds :: [Nat])
                      . ( Dimensions ds
                        , NumericFrame Float ds
                        )
                     => SSDFP !(SimpleDF ds) !(SimpleDF ds)

instance ( Dimensions ds
         , NumericFrame Float ds
         , PrimBytes (DataFrame Float ds)
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
    intDims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Int) [1..dimN]
    let eGen = case someDimVal intDims of
          Just (SomeDim (_ :: Dim ds)) -> case inferArrayInstance @Float @ds of
            ArrayInstanceEvidence -> case inferNumericFrame @Float @ds of
              NumericFrameEvidence -> Right $ SSDF <$> (arbitrary :: Gen (SimpleDF ds))
          Nothing -> Left "cannot construct Dim value."
    case eGen of
      Left s -> error $ "Cannot generate arbitrary SomeSimpleDF: " ++ s
      Right v -> v
  shrink (SSDF x) = SSDF <$> shrink x


instance Arbitrary SomeSimpleDFNonScalar where
  arbitrary = do
    dimN <- choose (1, maxDims) :: Gen Int
    intDims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Int) [1..dimN]
    let eGen = case someDimVal intDims of
          Just (SomeDim (_ :: Dim ds)) -> case inferArrayInstance @Float @ds of
            ArrayInstanceEvidence -> case inferNumericFrame @Float @ds of
              NumericFrameEvidence -> case ( unsafeEqProof :: ds :~: (Head ds :+ Tail ds)
                                           , unsafeEqProof :: ds :~: (Init ds +: Last ds)
                                           ) of
                (Refl, Refl) -> Right $ SSDFN <$> (arbitrary :: Gen (SimpleDF ds))
          Nothing -> Left "cannot construct Dim value."
    case eGen of
      Left s -> error $ "Cannot generate arbitrary SomeSimpleDF: " ++ s
      Right v -> v
  shrink (SSDFN x) = SSDFN <$> shrink x


instance Arbitrary SomeSimpleDFPair where
  arbitrary = do
    dimN <- choose (0, maxDims) :: Gen Int
    intDims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Int) [1..dimN]
    let eGen = case someDimVal intDims of
          Just (SomeDim (_ :: Dim ds)) -> case inferArrayInstance @Float @ds of
            ArrayInstanceEvidence -> case inferNumericFrame @Float @ds of
              NumericFrameEvidence -> Right $ SSDFP
                          <$> (arbitrary :: Gen (SimpleDF ds))
                          <*> (arbitrary :: Gen (SimpleDF ds))
          Nothing -> Left "cannot construct Dim value."
    case eGen of
      Left s -> error $ "Cannot generate arbitrary SomeSimpleDF: " ++ s
      Right v -> v
  shrink (SSDFP x y) = SSDFP <$> shrink x <*> shrink y

instance Show (DataFrame Float ds) => Show (SimpleDF ds) where
  show (SDF sdf) = show sdf
instance Show SomeSimpleDF where
  show (SSDF sdf) = show sdf
instance Show SomeSimpleDFNonScalar where
  show (SSDFN sdf) = show sdf
instance Show SomeSimpleDFPair where
  show (SSDFP x y) = "Pair:\n" ++ show (x,y)
