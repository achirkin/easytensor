{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Matrix.BidiagonalTest (runTests) where


import Control.Monad             (join)
import Data.Kind
import Data.Monoid               (All (..))
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Matrix.Bidiagonal
import Test.QuickCheck

validateBidiagonal :: forall (t :: Type) (n :: Nat) (m :: Nat)
             . ( KnownDim n, KnownDim m
               , RealFloatExtras t, Show t
               , MatrixDeterminant t n
               , MatrixDeterminant t m
               )
            => Matrix t n m -> BiDiag t n m -> Property
validateBidiagonal a BiDiag {..} =
    counterexample
      (unlines
        [ "failed a ~==~ u %* b %* transpose v:"
        , "a:  " ++ show a
        , "a': " ++ show a'
        , "u:  "   ++ show u
        , "b:  "   ++ show b
        , "v:  "   ++ show v
        ]
      ) (a =~= a')
    .&&.
    counterexample
      (unlines
        [ "u is not quite orthogonal:"
        , "a: "   ++ show a
        , "u: "   ++ show u
        , "b: "   ++ show b
        , "v: "   ++ show v
        ]
      ) (eye =~= u %* transpose u)
    .&&.
    counterexample
      (unlines
        [ "v is not quite orthogonal:"
        , "a: "  ++ show a
        , "u: "  ++ show u
        , "b: "  ++ show b
        , "v: "  ++ show v
        ]
      ) (eye =~= v %* transpose v)
    .&&.
    counterexample
      (unlines
        [ "b is not upper-bidiagonal"
        , "a: " ++ show a
        , "u: " ++ show u
        , "b: " ++ show b
        , "v: " ++ show v
        ]
      ) (getAll $ iwfoldMap @t @'[n,m]
            (\(Idx i :* Idx j :* U) x -> All (j == i || j == i + 1 || x == 0))
            b
        )
    .&&.
    counterexample
      (unlines
        [ "Incorrect determinant carried out for U"
        , "a: " ++ show a
        , "u: " ++ show u
        , "bdUDet: " ++ show bdUDet
        , "det u:  " ++ show (det u)
        ]
      ) (bdUDet =~= det u)
    .&&.
    counterexample
      (unlines
        [ "Incorrect determinant carried out for V"
        , "a: " ++ show a
        , "v: " ++ show v
        , "bdVDet: " ++ show bdVDet
        , "det v:  " ++ show (det v)
        ]
      ) (bdVDet =~= det v)
  where
    a'  = u %* b %* transpose v
    b = biDiag dims bdAlpha bdBeta
    u = bdU
    v = bdV

prop_bidiagonalSimple :: Property
prop_bidiagonalSimple = once . conjoin $ map prop_bidiagonal $ xs
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

prop_bidiagonal :: DataFrame Double '[XN 1, XN 1] -> Property
prop_bidiagonal (XFrame x)
  | n@D :* m@D :* U <- dims `inSpaceOf` x
  , D <- minDim n m
    = validateBidiagonal x (bidiagonalHouseholder x)
prop_bidiagonal _ = property False




return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
