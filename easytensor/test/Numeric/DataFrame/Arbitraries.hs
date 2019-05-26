{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Provide instance of Arbitrary for all DataFrame types.
--   Also, this module is an example of fancy type inference and DataFrame
--   traversals with monadic actions.
module Numeric.DataFrame.Arbitraries where

import Test.QuickCheck

import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Quaternion
import Numeric.Semigroup  hiding (All)


maxDims :: Word
maxDims = 5

maxDimSize :: Word
maxDimSize = 7

fromScalarChanceFactor :: Int
fromScalarChanceFactor = 5


instance (Quaternion t, Arbitrary t, Num t) => Arbitrary (Quater t) where
  arbitrary = sequence
    [ Quater <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Quater <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Quater <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Quater <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Quater <$> arbitrary <*> pure 0 <*> pure 0 <*> pure 0
    , Quater <$> pure 0 <*> arbitrary <*> pure 0 <*> pure 0
    , Quater <$> pure 0 <*> pure 0 <*> arbitrary <*> pure 0
    , Quater <$> pure 0 <*> pure 0 <*> pure 0 <*> arbitrary
    , Quater <$> arbitrary <*> arbitrary <*> pure 0 <*> pure 0
    , Quater <$> arbitrary <*> pure 0 <*> arbitrary <*> pure 0
    , Quater <$> arbitrary <*> pure 0 <*> pure 0 <*> arbitrary
    , Quater <$> pure 0 <*> arbitrary <*> arbitrary <*> pure 0
    , Quater <$> pure 0 <*> arbitrary <*> pure 0 <*> arbitrary
    , Quater <$> pure 0 <*> pure 0 <*> arbitrary <*> arbitrary
    , Quater <$> arbitrary <*> arbitrary <*> arbitrary <*> pure 0
    , Quater <$> arbitrary <*> arbitrary <*> pure 0 <*> arbitrary
    , Quater <$> arbitrary <*> pure 0 <*> arbitrary <*> arbitrary
    , Quater <$> pure 0 <*> arbitrary <*> arbitrary <*> arbitrary
    ] >>= elements

  shrink (Quater x y z t)
      -- shrink either real or the whole imaginary part
    = ($) <$> zipWith3 Quater (shrink x) (shrink y) (shrink z) <*> shrink t



instance (Arbitrary t, PrimBytes t, Num t, Ord t, Dimensions ds)
      => Arbitrary (DataFrame t (ds :: [Nat])) where
    arbitrary
        | -- First, we need to find out exact array implementation to use
          -- inside this DataFrame.
          -- We need to do that whenever exact value of ds is not known
          Dict <- inferKnownBackend @t @ds
          -- After that, GHC can infer all necessary fancy things like SubSpace
          -- to do complex operations on sub-dimensions of a DataFrame.
          --
          -- Note, we could put SubSpace into constraints of this instance as well.
          -- That would render the above lines unnecessary, but would make
          -- inference more difficult later.
        = do
        full <- (1 < ) <$> choose (1, fromScalarChanceFactor)
        if full -- I want to check fromScalar code path sometimes
        then arbitrary >>= elementWise @_ @ds @'[] f . ewgen . scalar
        else fromScalar . scalar <$> arbitrary
      where
        f :: Arbitrary a => Scalar a -> Gen (Scalar a)
        f _ = scalar <$> arbitrary
    shrink df
        | Dict <- inferKnownBackend @t @ds
        , mma <- ewfoldMap @t @ds @'[] @ds
            ((\x -> if x == 0 then Nothing else Just (minMax x)) . abs) df
        = case mma of
            Nothing
              -> [] -- all-zero is the most primitive DF possible
            Just (MinMax mi ma)
             -> [ ewmap (\x -> if abs x == ma then 0 else x) df | mi /= ma ]
             <> [ ewmap (\x -> if abs x == mi then 0 else x) df
                , ewmap (scalar . withAbs . unScalar) df
                ]
            where
              withAbs x
                | abs x <= 1 = 0
                | otherwise  = signum x * closest2 (abs x) 1
              closest2 x b = if x <= b * 2 then b else closest2 x (b*2)

instance ( All Arbitrary ts, All PrimBytes ts, All Num ts, All Ord ts
         , RepresentableList ts, Dimensions ds)
      => Arbitrary (DataFrame ts (ds :: [Nat])) where
    -- We create arbitrary MultiFrame by combining several SingleFrames.
    -- SingleFrames are "variables" or "columns" of a MultiFrame that are
    -- independent byte arrays bounded by a common dimensions type signature.
    arbitrary = -- Use RepresentableList to find out how many columns are there.
                case tList @_ @ts of
        -- Zero columns, empty MultiFrame
        U -> return Z
        -- Cons-like construction.
        -- Note, pattern matching TypeList brings RepresentableList evidence
        -- for Tail ts.
        _ :* (TypeList :: TypeList ts') -> do
          at   <- arbitrary
          ats' <- arbitrary @(DataFrame ts' ds)
          return (at :*: ats')
    shrink Z = []
    -- MultiFrame is a newtype wrapper on a TypedList.
    -- Thus, we can always recover RepresentableList ts by using function @types@
    shrink (at :*: ats@(MultiFrame ats'))
      | TypeList <- types ats'
      = (:*:) <$> shrink at <*> shrink ats


instance KnownDim a => Arbitrary (Dim (N a)) where
    arbitrary = return $ Dn (dim @_ @a)
    shrink _ = []

instance KnownDim m => Arbitrary (Dim (XN m)) where
    arbitrary = do
      dimN <- choose (dimVal' @m, maxDims)
      case constrain @m (someDimVal dimN) of
        Nothing -> error "impossible argument"
        Just d  -> return d
    shrink _ = []

instance Arbitrary SomeDims where
    arbitrary = do
      dimN <- choose (0, maxDims) :: Gen Word
      wdims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Word) [1..dimN]
      return $ someDimsVal wdims
    shrink (SomeDims U)         = []
    shrink (SomeDims (_ :* ds)) = [SomeDims ds]

instance Arbitrary (Dims '[]) where
    arbitrary = return U
    shrink _ = []

instance (KnownDim n, Arbitrary (Dims xs)) => Arbitrary (Dims (N n ': xs)) where
    arbitrary = (:*) <$> arbitrary <*> arbitrary
    shrink _ = []

instance (KnownDim m, Arbitrary (Dims xs)) => Arbitrary (Dims (XN m ': xs)) where
    arbitrary = (:*) <$> arbitrary <*> arbitrary
    shrink _ = []

instance (Arbitrary t, PrimBytes t, Num t, Ord t)
      => Arbitrary (SomeDataFrame t) where
    arbitrary = do
      -- Generate random dimension list
      --  and pattern-match against it with Dims pattern.
      --  This gives Dimensions ds evidence immediately.
      SomeDims (Dims :: Dims ds) <- arbitrary
      -- We also need to figure out an array implementation...
      case inferKnownBackend @t @ds of
        -- ... and generating a random DataFrame becomes a one-liner
        Dict -> SomeDataFrame <$> arbitrary @(DataFrame t ds)
    shrink (SomeDataFrame df) = SomeDataFrame <$> shrink df

-- All same as above, just change constraints a bit
instance ( All Arbitrary ts, All PrimBytes ts, All Num ts, All Ord ts
         , RepresentableList ts)
      => Arbitrary (SomeDataFrame ts) where
    arbitrary = do
      SomeDims (Dims :: Dims ds) <- arbitrary
      case inferKnownBackend @ts @ds of
        Dict -> SomeDataFrame <$> arbitrary @(DataFrame ts ds)
    shrink (SomeDataFrame df) = SomeDataFrame <$> shrink df

instance ( Arbitrary t, PrimBytes t, Num t, Ord t
         , Arbitrary (Dims xs), All KnownXNatType xs)
      => Arbitrary (DataFrame t (xs :: [XNat])) where
    arbitrary = do
      XDims (_ :: Dims ds) <- arbitrary @(Dims xs)
      case inferKnownBackend @t @ds of
        Dict -> XFrame <$> arbitrary @(DataFrame t ds)
    shrink (XFrame df) = XFrame <$> shrink df

instance ( All Arbitrary ts, All PrimBytes ts, All Num ts, All Ord ts
         , RepresentableList ts
         , Arbitrary (Dims xs), All KnownXNatType xs)
      => Arbitrary (DataFrame ts (xs :: [XNat])) where
    arbitrary = do
      XDims (_ :: Dims ds) <- arbitrary @(Dims xs)
      case inferKnownBackend @ts @ds of
        Dict -> XFrame <$> arbitrary @(DataFrame ts ds)
    shrink (XFrame df) = XFrame <$> shrink df


data AnyMatrix
data NonSingular

data SomeSquareMatrix prop t
  = forall (n :: Nat)
  . (KnownDim n, KnownBackend t '[n], KnownBackend t '[n, n])
  => SSM (DataFrame t '[n,n])

instance Show t => Show (SomeSquareMatrix prop t) where
  show (SSM df) = show df

instance (Arbitrary t, PrimBytes t, Num t, Ord t)
      => Arbitrary (SomeSquareMatrix AnyMatrix t) where
    arbitrary = do
      Dx (D :: Dim n) <- arbitrary @(Dim (XN 2))
      case inferKnownBackend @t @'[n] of
        Dict -> SSM <$> arbitrary @(DataFrame t '[n,n])
    shrink (SSM df)= SSM <$> shrink df

instance (Arbitrary t, PrimBytes t, Num t, Ord t)
      => Arbitrary (SomeSquareMatrix NonSingular t) where
    arbitrary = do
      SSM (someMat :: DataFrame t '[n, n]) <- arbitrary @(SomeSquareMatrix AnyMatrix t)
      -- https://en.wikipedia.org/wiki/Diagonally_dominant_matrix
      return . SSM $
        iwmap @t @'[n] @'[n] @'[n,n]
              @t @'[n] @'[n,n]
          ( \i v ->
            let s = ewfoldl (\a -> (a +) . abs) 1 v
            in update i s v
          ) someMat
    shrink _ = []
