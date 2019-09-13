{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Matrix.QRTest (runTests) where


import Control.Monad       (join)
import Data.Kind
import Data.Monoid         (All (..))
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Matrix.QR
import Test.QuickCheck


validateQR :: forall (t :: Type) (n :: Nat) (m :: Nat)
             . ( KnownDim n, KnownDim m, KnownDim (Min n m)
               , RealFloatExtras t, Show t
               )
            => Matrix t n m -> QR t n m -> Property
validateQR x q@QR {..}
  | Dict <- inferKnownBackend @_ @t @'[Min n m]
  , Dict <- inferKnownBackend @_ @t @'[m] =
    counterexample
      (unlines
        [ "failed m =~= qrQ %* qrR:"
        , "m:  " ++ show x
        , "m': " ++ show x'
        , "qr:" ++ show q
        ]
      ) (x =~= x')
    .&&.
    counterexample
      (unlines
        [ "qrQ is not quite orthogonal:"
        , "qr:" ++ show q
        , "m:"  ++ show x
        ]
      ) (eye =~= qrQ %* transpose qrQ)
    .&&.
    counterexample
      (unlines
        [ "qrR is not upper-triangular"
        , "qr:" ++ show q
        , "m:"   ++ show x
        ]
      ) (getAll $ iwfoldMap @t @'[n,m]
            (\(Idx i :* Idx j :* U) a -> All (j >= i || a == 0))
            qrR
        )
  where
    x'  = qrQ %* qrR

validateLQ :: forall (t :: Type) (n :: Nat) (m :: Nat)
             . ( KnownDim n, KnownDim m, KnownDim (Min n m)
               , RealFloatExtras t, Show t
               )
            => Matrix t n m -> LQ t n m -> Property
validateLQ x q@LQ {..}
  | Dict <- inferKnownBackend @_ @t @'[Min n m]
  , Dict <- inferKnownBackend @_ @t @'[n] =
    counterexample
      (unlines
        [ "failed m =~= lqL %* lqQ:"
        , "m:  " ++ show x
        , "m': " ++ show x'
        , "lq:"  ++ show q
        ]
      ) (x =~= x')
    .&&.
    counterexample
      (unlines
        [ "lqQ is not quite orthogonal:"
        , "lq:" ++ show q
        , "m:"  ++ show x
        ]
      ) (eye =~= lqQ %* transpose lqQ)
    .&&.
    counterexample
      (unlines
        [ "lqL is not lower-triangular"
        , "lq:" ++ show q
        , "m:"  ++ show x
        ]
      ) (getAll $ iwfoldMap @t @'[n,m]
            (\(Idx i :* Idx j :* U) a -> All (j <= i || a == 0))
            lqL
        )
  where
    x'  = lqL %* lqQ

manualMats :: [DataFrame Double '[XN 1, XN 1]]
manualMats = join
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
  where
    mkM :: Dims ([n,m]) -> [Double] -> DataFrame Double '[XN 1, XN 1]
    mkM ds
      | Just (XDims ds') <- constrainDims ds :: Maybe (Dims '[XN 1, XN 1])
        = XFrame . fromFlatList ds' 0
    mkM _ = error "prop_qrSimple: bad dims"
    variants :: Num a => [a] -> [[a]]
    variants as = rotateList as ++ rotateList (map negate as)

prop_lqSimple :: Property
prop_lqSimple = once . conjoin $ map prop_lq manualMats

prop_lq :: DataFrame Double '[XN 1, XN 1] -> Property
prop_lq (XFrame x)
  | n@D :* m@D :* U <- dims `inSpaceOf` x
  , D <- minDim n m
    = validateLQ x (lq x)
prop_lq _ = property False

prop_qrSimple :: Property
prop_qrSimple = once . conjoin $ map prop_qr manualMats

prop_qr :: DataFrame Double '[XN 1, XN 1] -> Property
prop_qr (XFrame x)
  | n@D :* m@D :* U <- dims `inSpaceOf` x
  , D <- minDim n m
    = validateQR x (qr x)
prop_qr _ = property False


return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }