{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.MatrixTest (runTests) where


import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries ()
import           Numeric.Dimensions
import           Test.QuickCheck

eps :: Scd
eps = 0.0000001

prop_detTranspose :: Matrix '[Double, Double] (XN 2) (XN 2) -> Bool
prop_detTranspose (XFrame (x :*: y :*: Z))
  | -- infer KnownDim for both dimensions of matrix x (and y)
    KnownDims <- dims `inSpaceOf` x
  = let m = diag (ewfoldl max 0 $ abs x) + x %* transpose y
        a = det m
        b = det $ transpose m
    in abs (a - b) / (abs a + abs b + 1) <= eps

prop_inverse :: Matrix '[Double, Double] (XN 2) (XN 2) -> Bool
prop_inverse (XFrame (x :*: y :*: Z))
  | -- infer KnownDim for both dimensions of matrix x (and y)
    (KnownDims :: Dims ns) <- dims `inSpaceOf` x
    -- cumbersose inverse instance requires PrimBytes (Vector t n)
  , E <- inferASing' @Double @'[Head ns]
  , E <- inferPrim' @Double @'[Head ns]
  = let m = diag base + x %* transpose y
        mi = inverse m
        err a b = ewfoldl max 0 (abs (b - a)) / base
        base = ewfoldl max 0.5 (abs x) + ewfoldl max 0.5 (abs y)
    in   err eye (m %* mi) <= eps
      && err eye (mi %* m) <= eps

prop_LU :: Matrix '[Double, Double] (XN 2) (XN 2) -> Bool
prop_LU (XFrame (x :*: y :*: Z))
  | -- infer KnownDim for both dimensions of matrix x (and y)
    (KnownDims :: Dims ns) <- dims `inSpaceOf` x
    -- cumbersose inverse instance requires PrimBytes (Vector t n)
  , E <- inferASing' @Double @'[Head ns]
  , E <- inferPrim' @Double @'[Head ns]
  = let m = diag base + x %* transpose y
        f = lu m
        err a b = ewfoldl max 0 (abs (b - a)) / base
        base = ewfoldl max 0.5 (abs x) + ewfoldl max 0.5 (abs y)
    in err (luPerm f %* m) (luLower f %* luUpper f) <= eps


return []
runTests :: IO Bool
runTests = $quickCheckAll
