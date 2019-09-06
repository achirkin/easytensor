{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Matrix.SVDTest (runTests) where


import Control.Monad
import Data.Kind
import Data.List                     (inits, tails)
import Data.Maybe                    (isJust)
import Numeric.DataFrame
import Numeric.DataFrame.Arbitraries ()
import Numeric.Dimensions
import Numeric.Matrix.SVD
import Test.QuickCheck

eps :: Fractional t => Scalar t
eps = 0.00001

-- check invariants of SVD
validateSVD :: forall (t :: Type) (n :: Nat) (m :: Nat)
             . ( KnownDim n, KnownDim m, KnownDim (Min n m)
               , PrimBytes t, Floating t, Ord t, Show t
               , MatrixDeterminant t n
               )
            => Matrix t n m -> SVD t n m -> Property
validateSVD x s@SVD {..}
  | Dict <- inferKnownBackend @_ @t @'[Min n m]
  , Dict <- inferKnownBackend @_ @t @'[m] =
    counterexample
      (unlines
        [ "failed m ~==~ u %* asDiag s %* transpose v:"
        , "m:  " ++ show x
        , "m': " ++ show x'
        , "svd:" ++ show s
        ]
      ) (approxEq x x')
    .&&.
    counterexample
      (unlines
        [ "SVD left-singular basis is not quite orthogonal:"
        , "svd:" ++ show s
        , "m:"   ++ show x
        ]
      ) (approxEq eye $ svdU %* transpose svdU)
    .&&.
    counterexample
      (unlines
        [ "Failed an extra invariant det svdU == 1:"
        , "svd:" ++ show s
        , "m:"   ++ show x
        ]
      ) (approxEq 1 $ det svdU)
    .&&.
    counterexample
      (unlines
        [ "SVD right-singular basis is not quite orthogonal:"
        , "svd:" ++ show s
        , "m:"   ++ show x
        ]
      ) (approxEq eye $ svdV %* transpose svdV)
    .&&.
      counterexample
        (unlines
          [ "Bad singular values (should be decreasing, non-negative):"
          , "svd:" ++ show s
          , "m:"   ++ show x
          ]
        )
        (isJust $ ewfoldr @_ @'[Min n m] @'[] checkIncreasing (Just 0) svdS)
  where
    checkIncreasing :: Scalar t -> Maybe (Scalar t) -> Maybe (Scalar t)
    checkIncreasing _ Nothing = Nothing
    checkIncreasing a (Just b)
      | a >= b    = Just a
      | otherwise = Nothing
    x'  = svdU %* asDiag svdS %* transpose svdV


-- | Most of the time, the error is proportional to the maginutude of the biggest element
maxElem :: (SubSpace t ds '[] ds, Ord t, Num t)
        => DataFrame t (ds :: [Nat]) -> Scalar t
maxElem = ewfoldl (\a -> max a . abs) 0

rotateList :: [a] -> [[a]]
rotateList xs = init (zipWith (++) (tails xs) (inits xs))

approxEq ::
  forall t (ds :: [Nat]) .
  (
    Dimensions ds,
    Fractional t, Ord t, Show t,
    Num (DataFrame t ds),
    PrimBytes (DataFrame t ds),
    PrimArray t (DataFrame t ds)
  ) => DataFrame t ds -> DataFrame t ds -> Property
approxEq a b = counterexample
    (unlines
      [ "  approxEq failed:"
      , "    max rows: "   ++ show m
      , "    max diff: "   ++ show dif
      ]
    ) $ maxElem (a - b) <= eps * m
  where
    m = maxElem a `max` maxElem b
    dif = maxElem (a - b)
infix 4 `approxEq`


prop_svd2x :: Property
prop_svd2x = once . conjoin $ map prop_svd2 $ xs ++ map negate xs
  where
    mkM :: [Double] -> Mat22d
    mkM = fromFlatList dims 0
    xs :: [Mat22d]
    xs =
      [ 0, 1, 2, eye]
      ++ map mkM (rotateList [4,0,0,0])
      ++ map mkM (rotateList [4,1,0,0])
      ++ map mkM (rotateList [3,0,-2,0])
      ++ map mkM (rotateList [3,1,-2,0])
      ++ map mkM (rotateList [1,1,3,3])
      ++ map ((eye + ). mkM) (rotateList [4,0,0,0])
      ++ map ((eye - ). mkM) (rotateList [4,0,0,0])

prop_svd2 :: Matrix Double 2 2 -> Property
prop_svd2 m = validateSVD m (svd2 m)

prop_svd3x :: Property
prop_svd3x = once . conjoin $ map prop_svd3 $ xs ++ map negate xs
  where
    mkM :: [Double] -> Mat33d
    mkM = fromFlatList dims 0
    xs :: [Mat33d]
    xs =
      [ 0, 1, 2, eye]
      ++ map mkM (rotateList [4,0,0,0,0,0,0,0,0])
      ++ map ((eye + ). mkM) (rotateList [4,0,0,0,0,0,0,0,0])
      ++ map ((eye - ). mkM) (rotateList [4,0,0,0,0,0,0,0,0])
      ++ map mkM (rotateList [4,1,0,0,0,0,0,0,0])
      ++ map mkM (rotateList [4,2,1,0,0,0,0,0,0])
      ++ map mkM (rotateList [4,2,1,0,0,7,0,0,0])
      ++ map mkM (rotateList [-5,0,1,0,0,7,1,0,10])

prop_svd3 :: Matrix Double 3 3 -> Property
prop_svd3 m = validateSVD m (svd3 m)


prop_svdSimple :: Property
prop_svdSimple = once . conjoin $ prop_svd <$> dfs

dfs :: [DataFrame Double '[XN 1, XN 1]]
dfs = xs
  where
    mkM :: Dims ([n,m]) -> [Double] -> DataFrame Double '[XN 1, XN 1]
    mkM ds
      | Just (XDims ds') <- constrainDims ds :: Maybe (Dims '[XN 1, XN 1])
        = XFrame . fromFlatList ds' 0
    mkM _ = error "prop_qrSimple: bad dims"
    variants :: Num a => [a] -> [[a]]
    variants as = rotateList as ++ rotateList (map negate as)
    xs :: [DataFrame Double '[XN 1, XN 1]]
    xs = join
      [ [mkM $ D2 :* D2 :* U, mkM $ D5 :* D2 :* U, mkM $ D3 :* D7 :* U]
            <*> [repeat 0, repeat 1, repeat 2]
      , mkM (D2 :* D2 :* U) <$> variants [3,2, 4,1]
      , mkM (D3 :* D3 :* U) <$> variants [0,0,1, 3,2,0, 4,1,0]
      , mkM (D4 :* D2 :* U) <$> variants [3,2, 4,1, 0,0, 0,2]
      , mkM (D2 :* D3 :* U) <$> variants [3,2,0, 4,1,2]
      , mkM (D2 :* D2 :* U) <$> variants [2, 0, 0, 0]
      , mkM (D2 :* D2 :* U) <$> variants [4, 1, 0, 0]
      , mkM (D2 :* D2 :* U) <$> variants [3, 1, -2, 0]
      , [mkM $ D2 :* D3 :* U, mkM $ D3 :* D2 :* U]
            <*> join
                [ variants [2, 0, 0, 0, 0, 0]
                , variants [4, 1, 0, 0, 0, 0]
                , variants [3, 1, -2, 0, 0, 0]
                , variants [3, 0, -2, 9, 0, 0]
                ]
      ]

prop_svd :: DataFrame Double '[XN 1, XN 1] -> Property
prop_svd (XFrame x)
  | n@D :* m@D :* U <- dims `inSpaceOf` x
  , D <- minDim n m = validateSVD x (svd x)
prop_svd _ = error "prop_svd: impossible pattern"


return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
