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
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
-- | Provide instance of Arbitrary for all DataFrame types.
--   Also, this module is an example of fancy type inference and DataFrame
--   traversals with monadic actions.
module Numeric.Arbitraries where

import Test.QuickCheck

import           Control.Monad.Fail
import           Data.Int
import           Data.Kind            (Type)
import           Data.List            (inits, tails)
import           Data.Semigroup       hiding (All)
import           Data.Word
import           Language.Haskell.TH  (ExpQ, Q, TypeQ)
import qualified Language.Haskell.TH  as TH
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
maxTotalDim = 400

-- | Maximum value of @Dim (XN _)@
maxDimSize :: Word
maxDimSize = 30

-- | Odds of using `fromScalar` constructor instead of element-by-element.
--   Interpret as "one to fromScalarChanceFactor"
fromScalarChanceFactor :: Int
fromScalarChanceFactor = 5

concatDimsQ :: TypeQ -> TypeQ -> TypeQ
concatDimsQ as bs = [t| Concat $(as) $(bs) |]

aFewPropVariants :: ExpQ -> ExpQ
aFewPropVariants = TH.listE . replicate 100

someElemTypeFQ :: Q TypeQ
someElemTypeFQ = TH.runIO . generate $ elements elemTypesF

someElemTypeIQ :: Q TypeQ
someElemTypeIQ = TH.runIO . generate $ elements elemTypesI

someElemTypeQ :: Q TypeQ
someElemTypeQ = TH.runIO . generate . elements $ elemTypesI ++ elemTypesF

elemTypesF :: [TypeQ]
elemTypesF = [ [t|Float|], [t|Double|] ]

elemTypesI :: [TypeQ]
elemTypesI =
  [ [t|Int|],  [t|Int8|],  [t|Int16|],  [t|Int32|],  [t|Int64|]
  , [t|Word|], [t|Word8|], [t|Word16|], [t|Word32|], [t|Int64|]
  ]


-- | Generate type (n :: Nat) at compile time
someDimNQ :: Q TypeQ
someDimNQ = TH.litT . TH.numTyLit . toInteger
         <$> TH.runIO (generate (chooseDim 1))

-- | Generate type (n :: XNat) at compile time
someDimXQ :: Q TypeQ
someDimXQ = fmap pure $ do
  d <- TH.litT . TH.numTyLit . toInteger
     <$> TH.runIO (generate (chooseDim 1))
  TH.runIO (generate arbitrary) >>=
    \isXN -> if isXN then [t|XN $(d)|] else [t|N $(d)|]

-- | Generate type (N n) at compile time
someDimXfixedQ :: Q TypeQ
someDimXfixedQ = fmap pure $ do
  d <- TH.litT . TH.numTyLit . toInteger
     <$> TH.runIO (generate (chooseDim 1))
  [t| N $(d) |]

-- | Generate type (ns :: [k]) at compile time
someDimsQ :: Q TypeQ
someDimsQ = do
  isXNat <- TH.runIO (generate arbitrary)
  if isXNat then someDimsXQ
            else someDimsNQ

-- | Generate type (ns :: [Nat]) at compile time
someDimsNQ :: Q TypeQ
someDimsNQ = fmap pure $ TH.runIO (removeDims <$> generate arbitrary)
         >>= \(SomeDims ds) -> wordsToDimsNQ (listDims ds)

-- | Generate type (Map 'N (ns :: [Nat])) at compile time
someDimsXQfixed :: Q TypeQ
someDimsXQfixed = fmap pure $ TH.runIO (removeDims <$> generate arbitrary)
         >>= \(SomeDims ds) -> wordsToDimsXQfixed (listDims ds)

-- | Generate type (Map 'N (ns :: [Nat])) at compile time
someDimsXQ :: Q TypeQ
someDimsXQ = fmap pure $ TH.runIO (removeDims <$> generate arbitrary)
         >>= \(SomeDims ds) -> wordsToDimsXQ(listDims ds)

-- | Generate type (ns :: [k]) at compile time
someLenAsBsQ :: Int -> Q (TypeQ, TypeQ)
someLenAsBsQ len = do
  isXNat <- TH.runIO (generate arbitrary)
  if isXNat then someLenAsBsXQ len
            else someLenAsBsNQ len

-- | Generate type (ns :: [k]) at compile time
someAsBsQ :: Q (TypeQ, TypeQ)
someAsBsQ = do
  isXNat <- TH.runIO (generate arbitrary)
  if isXNat then someAsBsXQ
            else someAsBsNQ

-- | Generate type (ns :: [k]) at compile time
someAsBsQfixed :: Q (TypeQ, TypeQ)
someAsBsQfixed = do
  isXNat <- TH.runIO (generate arbitrary)
  if isXNat then someAsBsXQfixed
            else someAsBsNQ

-- | Generate type (as :: [Nat], bs :: [Nat]) at compile time,
--   given a fixed length of the first dims (as).
someLenAsBsNQ :: Int -> Q (TypeQ, TypeQ)
someLenAsBsNQ len = do
    (asw, bsw) <- TH.runIO (generate (genConcatDims len))
    as <- wordsToDimsNQ asw
    bs <- wordsToDimsNQ bsw
    return (pure as, pure bs)

-- | Generate type (as :: [XNat], Map 'N bs :: [XNat]) at compile time,
--   given a fixed length of the first dims (as).
someLenAsBsXQ :: Int -> Q (TypeQ, TypeQ)
someLenAsBsXQ len = do
    (asw, bsw) <- TH.runIO (generate (genConcatDims len))
    as <- wordsToDimsXQ asw
    bs <- wordsToDimsXQfixed bsw
    return (pure as, pure bs)

-- | Generate type (Map 'N as :: [XNat], Map 'N bs :: [XNat]) at compile time,
--   given a fixed length of the first dims (as).
someLenAsBsXQfixed :: Int -> Q (TypeQ, TypeQ)
someLenAsBsXQfixed len = do
    (asw, bsw) <- TH.runIO (generate (genConcatDims len))
    as <- wordsToDimsXQfixed asw
    bs <- wordsToDimsXQfixed bsw
    return (pure as, pure bs)

-- | Generate type (as :: [Nat], bs :: [Nat]) at compile time.
someAsBsNQ :: Q (TypeQ, TypeQ)
someAsBsNQ = TH.runIO (generate (choose (0, fromIntegral maxDims - 1)))
  >>= someLenAsBsNQ

-- | Generate type (as :: [XNat], Map 'N bs :: [XNat]) at compile time.
someAsBsXQ :: Q (TypeQ, TypeQ)
someAsBsXQ = TH.runIO (generate (choose (0, fromIntegral maxDims - 1)))
  >>= someLenAsBsXQ

-- | Generate type (Map 'N as :: [XNat], Map 'N bs :: [XNat]) at compile time.
someAsBsXQfixed:: Q (TypeQ, TypeQ)
someAsBsXQfixed = TH.runIO (generate (choose (0, fromIntegral maxDims - 1)))
  >>= someLenAsBsXQfixed


wordsToDimsNQ :: [Word] -> TypeQ
wordsToDimsNQ = (\ns -> [t| ($(ns) :: [Nat]) |]) . foldr f TH.promotedNilT
  where
    f :: Word -> TypeQ -> TypeQ
    f w l = let d = TH.litT . TH.numTyLit $ toInteger w
            in  TH.promotedConsT `TH.appT` d `TH.appT` l

wordsToDimsXQfixed :: [Word] -> TypeQ
wordsToDimsXQfixed = (\ns -> [t| ($(ns) :: [XNat]) |]) . foldr f TH.promotedNilT
  where
    f :: Word -> TypeQ -> TypeQ
    f w l = let d = TH.litT . TH.numTyLit $ toInteger w
            in  TH.promotedConsT `TH.appT` [t| N $(d) |] `TH.appT` l

wordsToDimsXQ :: [Word] -> TypeQ
wordsToDimsXQ = (\ns -> [t| ($(ns) :: [XNat]) |]) . foldr f TH.promotedNilT
  where
    f :: Word -> TypeQ -> TypeQ
    f w l = let d = TH.litT . TH.numTyLit $ toInteger w
                xd = TH.runIO (generate arbitrary) >>=
                        \isXN -> if isXN then [t|XN $(d)|] else [t|N $(d)|]
            in  TH.promotedConsT `TH.appT` xd `TH.appT` l

-- | Generate two lists of Dims as Words, such that the combined list
--   is under the maxTotalDim limit, but the first list has exactly the given
--   number of dims.
genConcatDims :: Int -> Gen ([Word], [Word])
genConcatDims len
    = arbitrary >>= (\(SomeDims xs) -> checkLen (listDims xs)) . removeDims
  where
    checkLen :: [Word] -> Gen ([Word], [Word])
    checkLen xs =
      let td = product xs
          (as, bs) = splitAt len xs
          l  = length as
      in if l == len then pure (as, bs)
                     else (,) <$> addDims (len - l) (maxTotalDim `quot` td) as
                              <*> pure bs
    addDims :: Int -> Word -> [Word] -> Gen [Word]
    addDims 0 _ xs = pure xs
    addDims n lim xs = do
      x <- choose (1, max 1 (min lim maxDimSize))
      addDims (n-1) (lim `quot` x) (x : xs)


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
maxElem = ewfoldr' (max . abs) 0

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
        mel = abs a `max` abs b `max` 1
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
        mel = abs a `max` abs b `max` 1
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
       => Approx (DataFrame t ('[] :: [Nat])) (DataFrame t (ds :: [Nat])) where
    approxEq (S t) = approxEqDF t

instance (RealFloatExtras t, Show t)
       => Approx (DataFrame t ('[] :: [XNat])) (DataFrame t (ds :: [XNat])) where
    approxEq (XFrame (S t)) (XFrame a) (XFrame b) = case sameDims da db of
        Just Dict -> approxEqDF t a b
        Nothing -> counterexample
          (unlines
            [ "  DataFrame approxEq failed [XNat] due to different dims:"
            , "    dims a:  "   ++ show da
            , "    dims b:  "   ++ show db
            ]
          ) False
      where
        da = dims `inSpaceOf` a
        db = dims `inSpaceOf` b

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
    mel = unScalar $ maxElem a `max` maxElem b `max` 1
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

-- | Generate a random word not smaller than the given number, and not bigger
--   than maxDimSize. Also tend to generate smaller values more often to have
--   a chance to pack more dims within maxTotalDim.
chooseDim :: Word -> Gen Word
chooseDim lowLim = f <$> choose (0, maxDimSize)
  where
    l = realToFrac lowLim :: Double
    f :: Word -> Word
    f w = let x = realToFrac w :: Double
              n = realToFrac maxDimSize :: Double
          in round $ l + max 0 (n - l) * (x / n) ** 3

instance KnownDim a => Arbitrary (Dim (N a)) where
    arbitrary = return $ Dn (dim @a)
    shrink _ = []

instance KnownDim m => Arbitrary (Dim (XN m)) where
    arbitrary = do
      dimN <- chooseDim $ dimVal' @m
      case constrainDim @(XN m) (someDimVal dimN) of
        Nothing -> error "impossible argument"
        Just d  -> return d
    shrink _ = []

instance Arbitrary SomeDims where
    arbitrary = do
      dimN <- choose (0, maxDims) :: Gen Word
      wdims <- mapM (\_ -> chooseDim 1) [1..dimN]
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

instance KnownDim n => Arbitrary (Idx (XN n)) where
    arbitrary = elements [0..]

instance Dimensions ns => Arbitrary (Idxs (ns :: [Nat])) where
    arbitrary = go (dims @ns)
      where
        go :: forall (bs :: [Nat]) . Dims bs -> Gen (Idxs bs)
        go U         = pure U
        go (D :* bs) = (:*) <$> arbitrary <*> go bs

instance (BoundedDims ns, KnownXNatTypes ns) => Arbitrary (Idxs (ns :: [XNat])) where
    arbitrary = go (minimalDims @ns)
      where
        go :: forall (bs :: [XNat])
            . (BoundedDims bs, KnownXNatTypes bs) => Dims bs -> Gen (Idxs bs)
        go U            = pure U
        go (Dn D :* bs) = (:*) <$> arbitrary <*> go bs
        go (Dx D :* bs) = (:*) <$> arbitrary <*> go bs


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
