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
import Data.Maybe          (isJust)
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Matrix.SVD
import Test.QuickCheck

-- check invariants of SVD
validateSVD :: forall (t :: Type) (n :: Nat) (m :: Nat)
             . ( KnownDim n, KnownDim m, KnownDim (Min n m)
               , RealFloatExtras t, Show t
               , MatrixDeterminant t n
               )
            => t -> Matrix t n m -> SVD t n m -> Property
validateSVD extraTolerance x s@SVD {..}
  | nm <- fromIntegral $ dimVal' @n * dimVal' @m =
    counterexample
      (unlines
        [ "failed m =~= u %* asDiag s %* transpose v:"
        , "m:  " ++ show x
        , "m': " ++ show x'
        , "svd:" ++ show s
        ]
      ) (approxEq (nm * nm * extraTolerance) x x')
    .&&.
    counterexample
      (unlines
        [ "SVD left-singular basis is not quite orthogonal:"
        , "svd:" ++ show s
        , "m:"   ++ show x
        ]
      ) (eye =~= svdU %* transpose svdU)
    .&&.
    counterexample
      (unlines
        [ "Failed an extra invariant det svdU == 1:"
        , "svd:" ++ show s
        , "m:"   ++ show x
        ]
      ) (1 =~= det svdU)
    .&&.
    counterexample
      (unlines
        [ "SVD right-singular basis is not quite orthogonal:"
        , "svd:" ++ show s
        , "m:"   ++ show x
        ]
      ) (eye =~= svdV %* transpose svdV)
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

prop_svd2x :: Property
prop_svd2x = once . conjoin $ map prop_svd2 $ xs ++ map negate xs
  where
    mkM :: [Double] -> Mat22d
    mkM = fromFlatList dims 0
    xs :: [Mat22d]
    xs =
      [ 0, 1, 2, eye]
      ++ map mkM (rotateList [4,0,0,0])
      ++ map mkM (rotateList [4,3,0,0])
      ++ map mkM (rotateList [4,-3,0,0])
      ++ map mkM (rotateList [3,0,-2,0])
      ++ map mkM (rotateList [3,1,-2,0])
      ++ map mkM (rotateList [1,1,3,3])
      ++ map ((eye + ). mkM) (rotateList [4,0,0,0])
      ++ map ((eye - ). mkM) (rotateList [4,0,0,0])

prop_svd2 :: Matrix Double 2 2 -> Property
prop_svd2 m = validateSVD 1 m (svd2 m)

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
      ++ map mkM (rotateList [0,4.237014829207297,-2.0275007228244943
                             ,0,-0.9998659636721602,0
                             ,0,0,0.34652350292261475])
      ++ map mkM (rotateList [0,0,15.33245854346668
                             ,0,-18.99787842921179,0
                             ,0,29.953254288817046,0.78897034887446])
      ++ map mkM (rotateList [-306.5504258807584,0,0
                             ,-58.78957738886663,0,95.51923744049921
                             ,0,0,-292.92428690917154])
      ++ map mkM (rotateList [0,116.15251855804613,-94.95820664249142
                             ,0,-69.551119690005,0
                             ,0,0,46.77872074296825])



prop_svd3 :: Matrix Double 3 3 -> Property
prop_svd3 m = validateSVD extraTolerance m (svd3 m)
  where
    -- svd3 is fast, but not particularly precise,
    -- though, 6-8 digits of precision are still there.
    extraTolerance = 10e3 :: Double


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
  , D <- minDim n m = validateSVD 1 x (svd x)
prop_svd _ = error "prop_svd: impossible pattern"


return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
