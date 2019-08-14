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


import Data.Kind
import Numeric.DataFrame
import Numeric.DataFrame.Arbitraries ()
import Numeric.Dimensions
import Numeric.Matrix.SVD
import Test.QuickCheck

eps :: Fractional t => Scalar t
eps = 0.001


validateSVD :: forall (t :: Type) (n :: Nat) (m :: Nat)
             . ( KnownDim n, KnownDim m, KnownDim (Min n m)
               , PrimBytes t, Floating t, Ord t, Show t
               )
            => Matrix t n m -> SVD t n m -> Property
validateSVD x s@SVD {..} =
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
        ]
      ) (approxEq eye $ svdU %* transpose svdU)
    .&&.
    counterexample
      (unlines
        [ "SVD right-singular basis is not quite orthogonal:"
        , "svd:" ++ show s
        ]
      ) (approxEq eye $ svdV %* transpose svdV)

  where
    x'  = svdU %* asDiag svdS %* transpose svdV


-- | Most of the time, the error is proportional to the maginutude of the biggest element
maxElem :: (SubSpace t ds '[] ds, Ord t, Num t)
        => DataFrame t (ds :: [Nat]) -> Scalar t
maxElem = ewfoldl (\a -> max a . abs) 0


approxEq ::
  forall t (ds :: [Nat]) .
  (
    Dimensions ds,
    Fractional t, Ord t,
    Num (DataFrame t ds),
    PrimBytes (DataFrame t ds),
    PrimArray t (DataFrame t ds)
  ) => DataFrame t ds -> DataFrame t ds -> Bool
approxEq a b = maxElem (a - b) <= eps * m
  where
    m = maxElem a `max` maxElem b
infix 4 `approxEq`

prop_svd1 :: Matrix Double 1 1 -> Property
prop_svd1 m = validateSVD m (svd1 m)

prop_svd21 :: Property
prop_svd21 = prop_svd2 $ DF2
  (DF2 1 0)
  (DF2 0 2)

prop_svd22 :: Property
prop_svd22 = prop_svd2 $ DF2
  (DF2 4 1)
  (DF2 0 0)

prop_svd23 :: Property
prop_svd23 = prop_svd2 $ DF2
  (DF2 0 3)
  (DF2 0 0)

prop_svd24 :: Property
prop_svd24 = prop_svd2 $ DF2
  (DF2 0 0)
  (DF2 0 (-2))

prop_svd25 :: Property
prop_svd25 = prop_svd2 $ DF2
  (DF2 0 7)
  (DF2 3 0)

prop_svd2x :: Property
prop_svd2x = conjoin $ map prop_svd2 $ xs ++ map negate xs
  where
    xs :: [Mat22d]
    xs =
      [ 0, 1, 2, eye
      , DF2 (DF2 4 0)
            (DF2 0 0)
      , DF2 (DF2 0 4)
            (DF2 0 0)
      , DF2 (DF2 0 0)
            (DF2 0 4)
      , DF2 (DF2 0 0)
            (DF2 4 0)
      , DF2 (DF2 1 0)
            (DF2 4 0)
      , DF2 (DF2 4 1)
            (DF2 0 0)
      , DF2 (DF2 0 4)
            (DF2 0 1)
      , DF2 (DF2 0 0)
            (DF2 1 4)
      , DF2 (DF2 1 0)
            (DF2 0 4)
      , DF2 (DF2 4 0)
            (DF2 0 1)
      , DF2 (DF2 0 4)
            (DF2 1 0)
      , DF2 (DF2 0 1)
            (DF2 4 0)
      , DF2 (DF2 2 1)
            (DF2 4 0)
      , DF2 (DF2 4 2)
            (DF2 0 1)
      , DF2 (DF2 0 4)
            (DF2 1 2)
      , DF2 (DF2 1 0)
            (DF2 2 4)
      , DF2 (DF2 3 (-2))
            (DF2 2   3)
      , DF2 (DF2 3   2 )
            (DF2 2 (-3))
      ]

prop_svd2 :: Matrix Double 2 2 -> Property
prop_svd2 m = counterexample
  (unlines
   [ "m ="
   , pprM m
   , "u ="
   , pprM (svdU s)
   , "s = " ++ show (svdS s)
   , "v ="
   , pprM (svdV s)
   , "u %* s ="
   , pprM (svdU s %* asDiag (svdS s))
   , "u %* s %* v ="
   , pprM (svdU s %* asDiag (svdS s) %* transpose (svdV s) )
   , "ut %* m ="
   , pprM (transpose (svdU s) %* m)
   ]
  ) (validateSVD m (svd2 m))
  .&&.
    counterexample ("Bad singular values: " ++
      show (svdS s)) (case svdS s of DF2 a b -> a >= b && b >= 0 )
  where
    s = svd2 m
    pprV :: Vec2d -> String
    pprV (DF2 (S a) (S b)) = show a ++ "\t" ++ show b
    pprM :: Mat22d -> String
    pprM (DF2 a b) = unlines [pprV a, pprV b]


prop_svd3 :: Matrix Double 3 3 -> Property
prop_svd3 m = validateSVD m (svd3 m)


return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
