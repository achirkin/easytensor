{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.MatrixFloatTest (runTests) where


import Data.Fixed
import Numeric.DataFrame
import Numeric.DataFrame.Arbitraries
import Numeric.Dimensions
import Numeric.Matrix.LU
import Test.QuickCheck

type TestElem = Float
type TestDF = DataFrame TestElem

eps :: Scalar TestElem
eps = 1e-5

dropW :: (SubSpace t '[3] '[] '[3], SubSpace t '[4] '[] '[4])
      => Vector t 4 -> Vector t 3
dropW (Vec4 x y z _) = Vec3 x y z

-- | Most of the time, the error is proportional to the maginutude of the biggest element
maxElem :: (SubSpace TestElem ds '[] ds)
        => TestDF (ds :: [Nat]) -> Scalar TestElem
maxElem = ewfoldl (\a -> max a . abs) 0

-- | \( {|| A ||}_{\Infty} = \max_{0 \leq i < n} \sum_{j = 0}^{m} | a_{ij} | \)
--     is useful for bounding numeric errors of matrix algorithms.
matNormPInf :: forall t (n :: Nat) (m :: Nat)
             . (KnownDim n, KnownDim m, PrimBytes t, Ord t, Num t)
            => Matrix t n m -> Scalar t
matNormPInf
  | Dict <- inferKnownBackend @_ @t @'[m]
  = ewfoldl @t @'[n] @'[m]
      (\a -> max a . ewfoldl @t @'[m] @'[] (\e -> (e+) . abs) 0) 0

approxEq ::
  forall (ds :: [Nat]) .
  (
    Dimensions ds,
    Num (TestDF ds),
    PrimBytes (TestDF ds),
    PrimArray TestElem (TestDF ds)
  ) =>
  TestDF ds -> TestDF ds -> Property
approxEq a b = counterexample
    (unlines
      [ "  approxEq failed:"
      , "    error:     "   ++ show tol
      , "    tolerance: "   ++ show dif
      ]
    ) $ maxElem (a - b) <= tol
  where
    tol = eps * (maxElem a `max` maxElem b)
    dif = maxElem (a - b)
infix 4 `approxEq`

prop_detTranspose :: SomeSquareMatrix AnyMatrix TestElem -> Property
prop_detTranspose (SSM m) = counterexample
    (unlines
      [ "Failed det/transpose:"
      , "m:  " ++ show m
      , "mT: " ++ show (transpose m)
      , show a ++ " /= " ++ show b
        ++ " (tolerance: " ++ show mag ++ ")."
      ]
    ) $ abs (a - b) <= mag
  where
    a = det m
    b = det $ transpose m
    nw = (case dims `inSpaceOf` m of dn :* _ -> dimVal dn) :: Word
    n4 = fromIntegral nw ^ (4 :: Int)
    mag = eps * n4 * matNormPInf m ^ nw

prop_inverse :: SomeSquareMatrix NonSingular TestElem -> Property
prop_inverse (SSM m) = check (m %* mi) .&&. check (mi %* m)
  where
    mi = inverse m
    nw = (case dims `inSpaceOf` m of dn :* _ -> dimVal dn) :: Word
    n4 = fromIntegral nw ^ (4 :: Int)
    mag = eps * n4 * matNormPInf m
    check a = counterexample
      ( unlines
        [ "Failed inverse:"
        , "  m:    " ++ show m
        , "  mi:   " ++ show mi
        , "  eye': " ++ show a
        , "    error:     " ++ show err
        , "    tolerance: " ++ show mag
        ]
      ) $ err <= mag
      where
        err = maxElem (a - eye)


prop_LU :: SomeSquareMatrix NonSingular TestElem -> Property
prop_LU (SSM m) = counterexample
      (unlines
        [ "failed LU:"
        , "  m:                  " ++ show m
        , "  luPerm %* m:        " ++ show a
        , "  luLower %* luUpper: " ++ show b
        , ""
        , "    error:     " ++ show err
        , "    tolerance: " ++ show mag
        ]
      ) (err <= mag)
  where
    f = lu m
    nw = (case dims `inSpaceOf` m of dn :* _ -> dimVal dn) :: Word
    n4 = fromIntegral nw ^ (4 :: Int)
    mag = eps * n4 * matNormPInf m -- p.131
    a = luPerm f %* m
    b = luLower f %* luUpper f
    err = matNormPInf (b - a)

prop_translate3vs4 :: Vector TestElem 4 -> Bool
prop_translate3vs4 v = translate4 v == translate3 (dropW v)

prop_translate4 :: Vector TestElem 4 -> Vector TestElem 3 -> Bool
prop_translate4 a b = toHomPoint b %* translate4 a == toHomPoint (dropW a + b)

prop_translate3 :: Vector TestElem 3 -> Vector TestElem 3 -> Bool
prop_translate3 a b = toHomPoint b %* translate3 a == toHomPoint (a + b)

prop_rotateX :: Vector TestElem 4 -> Property
prop_rotateX v@(Vec4 x y z w) =
  conjoin [
    v %* rotateX (-2 * pi)   `approxEq` v,
    v %* rotateX (-1.5 * pi) `approxEq` vec4 x (-z) y w,
    v %* rotateX (-pi)       `approxEq` vec4 x (-y) (-z) w,
    v %* rotateX (-0.5 * pi) `approxEq` vec4 x z (-y) w,
    v %* rotateX 0           `approxEq` v,
    v %* rotateX (0.5 * pi)  `approxEq` vec4 x (-z) y w,
    v %* rotateX pi          `approxEq` vec4 x (-y) (-z) w,
    v %* rotateX (1.5 * pi)  `approxEq` vec4 x z (-y) w,
    v %* rotateX (2 * pi)    `approxEq` v
  ]

prop_rotateY :: Vector TestElem 4 -> Property
prop_rotateY v@(Vec4 x y z w) =
  conjoin [
    v %* rotateY (-2 * pi)   `approxEq` v,
    v %* rotateY (-1.5 * pi) `approxEq` vec4 z y (-x) w,
    v %* rotateY (-pi)       `approxEq` vec4 (-x) y (-z) w,
    v %* rotateY (-0.5 * pi) `approxEq` vec4 (-z) y x w,
    v %* rotateY 0           `approxEq` v,
    v %* rotateY (0.5 * pi)  `approxEq` vec4 z y (-x) w,
    v %* rotateY pi          `approxEq` vec4 (-x) y (-z) w,
    v %* rotateY (1.5 * pi)  `approxEq` vec4 (-z) y x w,
    v %* rotateY (2 * pi)    `approxEq` v
  ]

prop_rotateZ :: Vector TestElem 4 -> Property
prop_rotateZ v@(Vec4 x y z w) =
  conjoin [
    v %* rotateZ (-2 * pi)   `approxEq` v,
    v %* rotateZ (-1.5 * pi) `approxEq` vec4 (-y) x z w,
    v %* rotateZ (-pi)       `approxEq` vec4 (-x) (-y) z w,
    v %* rotateZ (-0.5 * pi) `approxEq` vec4 y (-x) z w,
    v %* rotateZ 0           `approxEq` v,
    v %* rotateZ (0.5 * pi)  `approxEq` vec4 (-y) x z w,
    v %* rotateZ pi          `approxEq` vec4 (-x) (-y) z w,
    v %* rotateZ (1.5 * pi)  `approxEq` vec4 y (-x) z w,
    v %* rotateZ (2 * pi)    `approxEq` v
  ]

prop_rotate :: TestElem -> Property
prop_rotate a =
  conjoin [
    rotate (vec3 1 0 0) a `approxEq` rotateX a,
    rotate (vec3 0 1 0) a `approxEq` rotateY a,
    rotate (vec3 0 0 1) a `approxEq` rotateZ a
  ]

prop_rotateEuler :: TestElem -> TestElem -> TestElem -> Property
prop_rotateEuler pitch yaw roll =
  rotateEuler pitch yaw roll `approxEq` rotateZ roll %* rotateY yaw %* rotateX pitch

prop_lookAt :: Vector TestElem 3 -> Vector TestElem 3 -> Vector TestElem 3 -> Property
prop_lookAt up cam foc =
  (apart cam foc && apart up cam) ==>
  conjoin [
    (normalized . fromHom $ toHomPoint foc %* m) `approxEq` vec3 0 0 (-1),
    fromHom (toHomPoint cam %* m) `approxEq` 0,
    fromHom (toHomVector xb %* m) `approxEq` vec3 1 0 0,
    fromHom (toHomVector yb %* m) `approxEq` vec3 0 1 0,
    fromHom (toHomVector zb %* m) `approxEq` vec3 0 0 1
  ]
  where
    apart :: Vector TestElem 3 -> Vector TestElem 3 -> Bool
    apart a b = maxElem (a - b) > 0.01 * (maxElem a `max` maxElem b)
    m = lookAt up cam foc
    zb = normalized $ cam - foc
    xb = normalized $ up `cross` zb
    yb = zb `cross` xb

prop_perspective :: TestElem -> TestElem -> TestElem -> TestElem -> Property
prop_perspective a b c d =
  conjoin [
    projectTo 0 0 n       `approxEq` vec3 0 0 (-1),
    projectTo 0 0 f       `approxEq` vec3 0 0 1,
    projectTo 1 1 n       `approxEq` vec3 1 1 (-1),
    projectTo 1 (-1) n    `approxEq` vec3 1 (-1) (-1),
    projectTo (-1) 1 n    `approxEq` vec3 (-1) 1 (-1),
    projectTo (-1) (-1) n `approxEq` vec3 (-1) (-1) (-1),
    projectTo 1 1 f       `approxEq` vec3 1 1 1,
    projectTo 1 (-1) f    `approxEq` vec3 1 (-1) 1,
    projectTo (-1) 1 f    `approxEq` vec3 (-1) 1 1,
    projectTo (-1) (-1) f `approxEq` vec3 (-1) (-1) 1
  ]
  where
    n = 1.0 + mod' a 9.0 -- Near plane in range [1, 10)
    f = n + 1.0 + mod' b 99.0 -- Far plane in range [n + 1, n + 100)
    fovy = (0.1 * pi) + mod' c (0.8 * pi) -- Y-axis field of view in range [0.1*pi, 0.9*pi)
    aspect = 0.25 + mod' d 4.0 -- Aspect ration in range [1/4, 4/1]
    hpd = tan (fovy * 0.5) -- height/distance
    wpd = aspect * hpd -- width/distance
    m = perspective n f fovy aspect
    projectTo x' y' z = fromHom $ vec4 (x' * wpd * z) (y' * hpd * z) (-z) 1 %* m

prop_orthogonal :: TestElem -> TestElem -> TestElem -> TestElem -> Property
prop_orthogonal a b c d =
  conjoin [
    projectTo 0 0 n       `approxEq` vec3 0 0 (-1),
    projectTo 0 0 f       `approxEq` vec3 0 0 1,
    projectTo 1 1 n       `approxEq` vec3 1 1 (-1),
    projectTo 1 (-1) n    `approxEq` vec3 1 (-1) (-1),
    projectTo (-1) 1 n    `approxEq` vec3 (-1) 1 (-1),
    projectTo (-1) (-1) n `approxEq` vec3 (-1) (-1) (-1),
    projectTo 1 1 f       `approxEq` vec3 1 1 1,
    projectTo 1 (-1) f    `approxEq` vec3 1 (-1) 1,
    projectTo (-1) 1 f    `approxEq` vec3 (-1) 1 1,
    projectTo (-1) (-1) f `approxEq` vec3 (-1) (-1) 1
  ]
  where
    n = 1.0 + mod' a 9.0 -- Near plane in range [1, 10)
    f = n + 1.0 + mod' b 99.0 -- Far plane in range [n + 1, n + 100)
    w = 1.0 + mod' c 9999.0 -- Width in range [1, 10000)
    h = 1.0 + mod' d 9999.0 -- Height in range [1, 10000)
    m = orthogonal n f w h
    projectTo x' y' z = fromHom $ vec4 (x' * w * 0.5) (y' * h * 0.5) (-z) 1 %* m

prop_toHomPoint :: Vector TestElem 3 -> Bool
prop_toHomPoint v@(Vec3 x y z) = toHomPoint v == vec4 x y z 1

prop_toHomVector :: Vector TestElem 3 -> Bool
prop_toHomVector v@(Vec3 x y z) = toHomVector v == vec4 x y z 0

prop_fromHom :: Vector TestElem 4 -> Property
prop_fromHom v@(Vec4 x y z w) =
  case w of
    0 -> fromHom v === vec3 x y z
    _ -> fromHom v `approxEq` vec3 (x/w) (y/w) (z/w)

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
