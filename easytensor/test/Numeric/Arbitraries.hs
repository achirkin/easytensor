{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Provide instance of Arbitrary for all DataFrame types.
--   Also, this module is an example of fancy type inference and DataFrame
--   traversals with monadic actions.
module Numeric.Arbitraries where

import Test.QuickCheck

import           Control.Monad.Fail
import           Data.Kind            (Type)
import           Data.List            (inits, tails)
import           Data.Semigroup       hiding (All)
import           Numeric.DataFrame
import           Numeric.Dimensions
import           Numeric.Quaternion
import qualified Numeric.Tuple.Lazy   as LT
import qualified Numeric.Tuple.Strict as ST

-- | Maximum number of elements in SomeDims lists
maxDims :: Word
maxDims = 5

-- | Some tests are rather slow when we have too many elements
maxTotalDim :: Word
maxTotalDim = 1000

-- | Maximum value of @Dim (XN _)@
maxDimSize :: Word
maxDimSize = 50

-- | Odds of using `fromScalar` constructor instead of element-by-element.
--   Interpret as "one to fromScalarChanceFactor"
fromScalarChanceFactor :: Int
fromScalarChanceFactor = 5

-- | Remove dims from a dim list until its totalDim is less than maxTotalDim
removeDims :: SomeDims -> SomeDims
removeDims = removeDimsAbove maxTotalDim

-- | Remove dims from a dim list until its totalDim is less than a given value
removeDimsAbove :: Word -> SomeDims -> SomeDims
removeDimsAbove _ (SomeDims U) = SomeDims U
removeDimsAbove z (SomeDims nns@(_ :* ns))
  | totalDim nns > z = removeDimsAbove z (SomeDims ns)
  | otherwise        = SomeDims nns

-- | Reduce individual XN-dims untils its totalDim is less than maxTotalDim
reduceDims :: (All KnownXNatType xns, BoundedDims xns)
           => Dims (xns :: [XNat]) -> Dims (xns :: [XNat])
reduceDims = reduceDims' 1

reduceDims' :: (All KnownXNatType xns, BoundedDims xns)
            => Word -> Dims (xns :: [XNat]) -> Dims (xns :: [XNat])
reduceDims' _ U = U
reduceDims' l nns@(n :* ns)
  | totalDim nns * l <= maxTotalDim = nns
  | otherwise = case n of
      Dn d -> n :* reduceDims' (l * dimVal d) ns
      (Dx d :: Dim xn) -> case compareDim (dimBound @xn) D2 of
        SLT -> Dx D2 :* reduceDims' (l * 2) ns
        SEQ -> Dx D2 :* reduceDims' (l * 2) ns
        SGT -> n :* reduceDims' (l * dimVal d) ns

-- | Most of the time, we assume the error is proportional to the maginutude of
--   the biggest element.
maxElem :: (SubSpace t ds '[] ds, Ord t, Num t)
        => DataFrame t (ds :: [Nat]) -> Scalar t
maxElem = ewfoldl (\a -> max a . abs) 0

rotateList :: [a] -> [[a]]
rotateList xs = init (zipWith (++) (tails xs) (inits xs))

class (RealFloatExtras t, Show t) => Approx t x | x -> t where
    -- | Check if two values are approximately equal
    approxEq :: t -- ^ Extra multiplier constant
             -> x -> x -> Property

-- | Check if two values are approximately equal.
(=~=) :: Approx t x => x -> x -> Property
(=~=) = approxEq 1
infix 4 =~=

instance Approx Double Double where
    approxEq c a b = counterexample
      (unlines
        [ "  Double approxEq failed:"
        , "    values:    "   ++ show (a, b)
        , "    error:     "   ++ show err
        , "    tolerance: "   ++ show tol
        ]
      ) $ err <= tol
      where
        err = abs (a - b)
        mel = max a b
        tol = M_EPS*mel*c

instance Approx Float Float where
    approxEq c a b = counterexample
      (unlines
        [ "  Double approxEq failed:"
        , "    values:    "   ++ show (a, b)
        , "    error:     "   ++ show err
        , "    tolerance: "   ++ show tol
        ]
      ) $ err <= tol
      where
        err = abs (a - b)
        mel = max a b
        tol = M_EPS*mel*c

instance MonadFail Gen where fail = error

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

instance (RealFloatExtras t, Show t, Quaternion t) => Approx t (Quater t) where
    approxEq c a b = counterexample
        (unlines
          [ "  Quaternion approxEq failed:"
          , "    max elem:  "   ++ show mel
          , "    error:     "   ++ show err
          , "    tolerance: "   ++ show tol
          ]
        ) $ err <= tol
      where
        v1 = toVec4 a
        v2 = toVec4 b
        err = unScalar $ maxElem (v1 - v2)
        mel = unScalar $ 1 `max` maxElem v1 `max` maxElem v2
        tol = M_EPS*mel*c

instance (Arbitrary t, PrimBytes t, Num t, Ord t, Dimensions ds)
      => Arbitrary (DataFrame t (ds :: [Nat])) where
    arbitrary = do
        full <- (1 < ) <$> choose (1, fromScalarChanceFactor)
        zeroChance <- choose (0, 8)
        if full -- I want to check fromScalar code path sometimes
        then arbitrary >>= elementWise @_ @ds @'[] (f zeroChance) . ewgen . scalar
        else fromScalar . scalar <$> arbitrary
      where
        f :: (Arbitrary a, Num a) => Double -> Scalar a -> Gen (Scalar a)
        f zeroChance _ = do
          dice <- (zeroChance >=) <$> choose (0, 10)
          if dice
          then return 0
          else scalar <$> arbitrary
    shrink df
        | Just (Max ma) <- ewfoldMap @t @ds @'[] @ds
            ((\x -> if x == 0 then Nothing else Just (Max x)) . abs) df
           = [ ewmap (\x -> if abs x == ma then 0 else x) df
             , ewmap (scalar . withAbs . unScalar) df
             ]
        | otherwise = []
            where
              withAbs :: t -> t
              withAbs x
                | abs x <= 1 = 0
                | otherwise  = signum x * closest2 (abs x) 1
              closest2 :: t -> t -> t
              closest2 x b = if x <= b * 2 then b else closest2 x (b*2)

instance (RealFloatExtras t, Show t, Dimensions ds)
       => Approx t (DataFrame t (ds :: [Nat])) where
    approxEq = approxEqDF

approxEqDF ::
       forall t (ds :: [Nat])
     . ( Dimensions ds
       , Show t, RealFloatExtras t
       , KnownBackend t ds)
    => t -> DataFrame t ds -> DataFrame t (ds :: [Nat]) -> Property
approxEqDF c a b
  | U <- dims @ds = counterexample
      (unlines
        [ "  Scalar approxEq failed:"
        , "    values:    "   ++ show (unScalar a, unScalar b)
        , "    error:     "   ++ show err
        , "    tolerance: "   ++ show tol
        ]
      ) $ err <= tol
  | otherwise = counterexample
      (unlines
        [ "  DataFrame approxEq failed:"
        , "    max elem:  "   ++ show mel
        , "    error:     "   ++ show err
        , "    tolerance: "   ++ show tol
        ]
      ) $ err <= tol
  where
    err = unScalar $ maxElem (a - b)
    mel = unScalar $ maxElem a `max` maxElem b
    tol = M_EPS*mel*c

instance ( All Arbitrary ts, All PrimBytes ts, All Num ts, All Ord ts
         , RepresentableList ts, Dimensions ds)
      => Arbitrary (DataFrame ts (ds :: [Nat])) where
    -- We create arbitrary MultiFrame by combining several SingleFrames.
    -- SingleFrames are "variables" or "columns" of a MultiFrame that are
    -- independent byte arrays bounded by a common dimensions type signature.
    arbitrary = -- Use RepresentableList to find out how many columns are there.
                case tList @ts of
        -- Zero columns, empty MultiFrame
        U -> return Z
        -- Cons-like construction.
        -- Note, pattern matching TypeList brings RepresentableList evidence
        -- for Tail ts.
        _ :* (TypeList :: TypeList ts') -> do
          at   <- arbitrary
          ats' <- arbitrary @(DataFrame ts' ds)
          return (at :*: ats')
    -- MultiFrame is a newtype wrapper on a TypedList.
    -- Thus, we can always recover RepresentableList ts by using function @types@
    shrink (at :*: ats@(MultiFrame ats'))
      | TypeList <- types ats'
      = (:*:) <$> shrink at <*> shrink ats
    shrink _ = []


instance KnownDim a => Arbitrary (Dim (N a)) where
    arbitrary = return $ Dn (dim @a)
    shrink _ = []

instance KnownDim m => Arbitrary (Dim (XN m)) where
    arbitrary = do
      dimN <- choose (dimVal' @m, maxDims)
      case constrainDim @(XN m) (someDimVal dimN) of
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
      SomeDims (Dims :: Dims ds) <- removeDims <$> arbitrary
      SomeDataFrame <$> arbitrary @(DataFrame t ds)
    shrink (SomeDataFrame df) = SomeDataFrame <$> shrink df

-- All same as above, just change constraints a bit
instance ( All Arbitrary ts, All PrimBytes ts, All Num ts, All Ord ts
         , RepresentableList ts)
      => Arbitrary (SomeDataFrame ts) where
    arbitrary = do
      SomeDims ds <- removeDims <$> arbitrary
      case ds of
        (Dims :: Dims ds) -> case inferKnownBackend @ts @ds of
          Dict -> SomeDataFrame <$> arbitrary @(DataFrame ts ds)
    shrink (SomeDataFrame df) = SomeDataFrame <$> shrink df


instance ( Arbitrary t, PrimBytes t, Num t, Ord t
         , Arbitrary (Dims xs), All KnownXNatType xs, BoundedDims xs)
      => Arbitrary (DataFrame t (xs :: [XNat])) where
    arbitrary = do
      XDims (_ :: Dims ds) <- reduceDims <$> arbitrary @(Dims xs)
      XFrame <$> arbitrary @(DataFrame t ds)
    shrink (XFrame df) = XFrame <$> shrink df

instance ( All Arbitrary ts, All PrimBytes ts, All Num ts, All Ord ts
         , RepresentableList ts
         , Arbitrary (Dims xs), All KnownXNatType xs, BoundedDims xs)
      => Arbitrary (DataFrame ts (xs :: [XNat])) where
    arbitrary = do
      ds <- reduceDims <$> arbitrary @(Dims xs)
      case ds of
        XDims (_ :: Dims ds) -> case inferKnownBackend @ts @ds of
          Dict -> XFrame <$> arbitrary @(DataFrame ts ds)
    shrink (XFrame df) = XFrame <$> shrink df


instance KnownDim n => Arbitrary (Idx (n :: Nat)) where
    arbitrary = elements [0..]

instance KnownDim n => Arbitrary (Idx (N n)) where
    arbitrary = elements [0..]

instance Dimensions ns => Arbitrary (Idxs (ns :: [Nat])) where
    arbitrary = go (dims @ns)
      where
        go :: forall (bs :: [Nat]) . Dims bs -> Gen (Idxs bs)
        go U         = pure U
        go (D :* bs) = (:*) <$> arbitrary <*> go bs

instance Dimensions ns => Arbitrary (Idxs (ns :: [XNat])) where
    arbitrary = withKnownXDims @ns $ go (dims @ns)
      where
        go :: forall (bs :: [XNat])
            . bs ~ Map 'N (DimsBound bs)
           => Dims bs -> Gen (Idxs bs)
        go U            = pure U
        go (Dn D :* bs) = (:*) <$> arbitrary <*> go bs


instance (RepresentableList xs, All Arbitrary xs) => Arbitrary (ST.Tuple xs) where
    arbitrary = go (tList @xs)
      where
        go :: forall (bs :: [Type])
            . All Arbitrary bs
           => TypeList bs -> Gen (ST.Tuple bs)
        go U         = pure U
        go (_ :* bs) = (ST.:$) <$> arbitrary <*> go bs

instance (RepresentableList xs, All Arbitrary xs) => Arbitrary (LT.Tuple xs) where
    arbitrary = go (tList @xs)
      where
        go :: forall (bs :: [Type])
            . All Arbitrary bs
           => TypeList bs -> Gen (LT.Tuple bs)
        go U         = pure U
        go (_ :* bs) = (LT.:$) <$> arbitrary <*> go bs

data AnyMatrix
data NonSingular

data SomeSquareMatrix prop t
  = forall (n :: Nat)
  . (KnownDim n, KnownBackend t '[n], KnownBackend t '[n, n])
  => SSM (DataFrame t '[n,n])

instance (Show t, PrimBytes t) => Show (SomeSquareMatrix prop t) where
  show (SSM df) = show df

instance (Arbitrary t, PrimBytes t, Num t, Ord t)
      => Arbitrary (SomeSquareMatrix AnyMatrix t) where
    arbitrary = do
      Dx (D :: Dim n) <- arbitrary @(Dim (XN 2))
      SSM <$> arbitrary @(DataFrame t '[n,n])
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
