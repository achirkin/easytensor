{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.MatrixDoubleTest (runTests) where


import           Data.Fixed
import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries ()
import           Numeric.Dimensions
import           Test.QuickCheck

eps :: Scd
eps = 0.0000001

dropW :: (SubSpace t '[3] '[] '[3], SubSpace t '[4] '[] '[4])
      => Vector t 4 -> Vector t 3
dropW (Vec4 x y z _) = Vec3 x y z

approxEq ::
  forall (ds :: [Nat]).
  (
    Dimensions ds,
    Num (DataFrame Double ds),
    PrimBytes (DataFrame Double ds),
    PrimArray Double (DataFrame Double ds)
  ) =>
  DataFrame Double ds -> DataFrame Double ds -> Bool
approxEq a b = (eps >=) . ewfoldl @_ @_ @'[] max 0 . abs $ a - b
infix 4 `approxEq`

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
  , Dict <- inferKnownBackend @Double @'[Head ns]
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
  , Dict <- inferKnownBackend @Double @'[Head ns]
  = let m = diag base + x %* transpose y
        f = lu m
        err a b = ewfoldl max 0 (abs (b - a)) / base
        base = ewfoldl max 0.5 (abs x) + ewfoldl max 0.5 (abs y)
    in err (luPerm f %* m) (luLower f %* luUpper f) <= eps

prop_translate3vs4 :: Vector Double 4 -> Bool
prop_translate3vs4 v = translate4 v == translate3 (dropW v)

prop_translate4 :: Vector Double 4 -> Vector Double 3 -> Bool
prop_translate4 a b = toHomPoint b %* translate4 a == toHomPoint (dropW a + b)

prop_translate3 :: Vector Double 3 -> Vector Double 3 -> Bool
prop_translate3 a b = toHomPoint b %* translate3 a == toHomPoint (a + b)

prop_rotateX :: Vector Double 4 -> Bool
prop_rotateX v@(Vec4 x y z w) =
  and [
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

prop_rotateY :: Vector Double 4 -> Bool
prop_rotateY v@(Vec4 x y z w) =
  and [
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

prop_rotateZ :: Vector Double 4 -> Bool
prop_rotateZ v@(Vec4 x y z w) =
  and [
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

prop_rotate :: Double -> Bool
prop_rotate a =
  and [
    rotate (vec3 1 0 0) a `approxEq` rotateX a,
    rotate (vec3 0 1 0) a `approxEq` rotateY a,
    rotate (vec3 0 0 1) a `approxEq` rotateZ a
  ]

prop_rotateEuler :: Double -> Double -> Double -> Bool
prop_rotateEuler pitch yaw roll =
  rotateEuler pitch yaw roll `approxEq` rotateZ roll %* rotateY yaw %* rotateX pitch

prop_lookAt :: Vector Double 3 -> Vector Double 3 -> Vector Double 3 -> Bool
prop_lookAt up cam foc =
  and [
    (normalized . fromHom $ toHomPoint foc %* m) `approxEq` vec3 0 0 (-1),
    fromHom (toHomPoint cam %* m) `approxEq` 0,
    fromHom (toHomVector xb %* m) `approxEq` vec3 1 0 0,
    fromHom (toHomVector yb %* m) `approxEq` vec3 0 1 0,
    fromHom (toHomVector zb %* m) `approxEq` vec3 0 0 1
  ]
  where
    m = lookAt up cam foc
    zb = normalized $ cam - foc
    xb = normalized $ up `cross` zb
    yb = zb `cross` xb

prop_perspective :: Double -> Double -> Double -> Double -> Bool
prop_perspective a b c d =
  and [
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

prop_orthogonal :: Double -> Double -> Double -> Double -> Bool
prop_orthogonal a b c d =
  and [
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

prop_toHomPoint :: Vector Double 3 -> Bool
prop_toHomPoint v@(Vec3 x y z) = toHomPoint v == vec4 x y z 1

prop_toHomVector :: Vector Double 3 -> Bool
prop_toHomVector v@(Vec3 x y z) = toHomVector v == vec4 x y z 0

prop_fromHom :: Vector Double 4 -> Bool
prop_fromHom v@(Vec4 x y z w) =
  case w of
    0 -> fromHom v == vec3 x y z
    _ -> fromHom v `approxEq` vec3 (x/w) (y/w) (z/w)

return []
runTests :: IO Bool
runTests = $quickCheckAll
