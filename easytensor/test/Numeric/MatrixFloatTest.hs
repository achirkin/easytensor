{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.MatrixFloatTest (runTests) where


import           Data.Fixed
import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries ()
import           Numeric.Dimensions
import           Test.QuickCheck

eps :: Scf
eps = 0.01

dropW :: (SubSpace t '[] '[3] '[3], SubSpace t '[] '[4] '[4]) => Vector t 4 -> Vector t 3
dropW v | (x,y,z,_) <- unpackV4 v = vec3 x y z

approxEqF3 :: Vector Float 3 -> Vector Float 3 -> Bool
approxEqF3 a b = (eps >=) . ewfoldl @_ @'[] max 0 . abs $ a - b
infix 4 `approxEqF3`

approxEqF4 :: Vector Float 4 -> Vector Float 4 -> Bool
approxEqF4 a b = (eps >=) . ewfoldl @_ @'[] max 0 . abs $ a - b
infix 4 `approxEqF4`

approxEqF44 :: Matrix Float 4 4 -> Matrix Float 4 4 -> Bool
approxEqF44 a b = (eps >=) . ewfoldl @_ @'[] max 0 . abs $ a - b
infix 4 `approxEqF44`

prop_detTranspose :: Matrix '[Float, Float] (XN 2) (XN 2) -> Bool
prop_detTranspose (XFrame (x :*: y :*: Z))
  | -- infer KnownDim for both dimensions of matrix x (and y)
    KnownDims <- dims `inSpaceOf` x
  = let m = diag (ewfoldl max 0 $ abs x) + x %* transpose y
        a = det m
        b = det $ transpose m
    in abs (a - b) / (abs a + abs b + 1) <= eps

prop_inverse :: Matrix '[Float, Float] (XN 2) (XN 2) -> Bool
prop_inverse (XFrame (x :*: y :*: Z))
  | -- infer KnownDim for both dimensions of matrix x (and y)
    (KnownDims :: Dims ns) <- dims `inSpaceOf` x
    -- cumbersose inverse instance requires PrimBytes (Vector t n)
  , E <- inferASing' @Float @'[Head ns]
  , E <- inferPrim' @Float @'[Head ns]
  = let m = diag base + x %* transpose y
        mi = inverse m
        err a b = ewfoldl max 0 (abs (b - a)) / base
        base = ewfoldl max 0.5 (abs x) + ewfoldl max 0.5 (abs y)
    in   err eye (m %* mi) <= eps
      && err eye (mi %* m) <= eps

prop_LU :: Matrix '[Float, Float] (XN 2) (XN 2) -> Bool
prop_LU (XFrame (x :*: y :*: Z))
  | -- infer KnownDim for both dimensions of matrix x (and y)
    (KnownDims :: Dims ns) <- dims `inSpaceOf` x
    -- cumbersose inverse instance requires PrimBytes (Vector t n)
  , E <- inferASing' @Float @'[Head ns]
  , E <- inferPrim' @Float @'[Head ns]
  = let m = diag base + x %* transpose y
        f = lu m
        err a b = ewfoldl max 0 (abs (b - a)) / base
        base = ewfoldl max 0.5 (abs x) + ewfoldl max 0.5 (abs y)
    in err (luPerm f %* m) (luLower f %* luUpper f) <= eps

prop_translate3vs4 :: Vector Float 4 -> Bool
prop_translate3vs4 v = translate4 v == translate3 (dropW v)
  
prop_translate4 :: Vector Float 4 -> Vector Float 3 -> Bool
prop_translate4 a b = translate4 a %* toHomPoint b == toHomPoint (dropW a + b)

prop_translate3 :: Vector Float 3 -> Vector Float 3 -> Bool
prop_translate3 a b = translate3 a %* toHomPoint b == toHomPoint (a + b)

prop_rotateX :: Vector Float 4 -> Bool
prop_rotateX v | (x,y,z,w) <- unpackV4 v =
  and [
    rotateX (-2 * pi)   %* v `approxEqF4` v,
    rotateX (-1.5 * pi) %* v `approxEqF4` vec4 x (-z) y w,
    rotateX (-pi)       %* v `approxEqF4` vec4 x (-y) (-z) w,
    rotateX (-0.5 * pi) %* v `approxEqF4` vec4 x z (-y) w,
    rotateX 0           %* v `approxEqF4` v,
    rotateX (0.5 * pi)  %* v `approxEqF4` vec4 x (-z) y w,
    rotateX pi          %* v `approxEqF4` vec4 x (-y) (-z) w,
    rotateX (1.5 * pi)  %* v `approxEqF4` vec4 x z (-y) w,
    rotateX (2 * pi)    %* v `approxEqF4` v
  ]

prop_rotateY :: Vector Float 4 -> Bool
prop_rotateY v | (x,y,z,w) <- unpackV4 v =
  and [
    rotateY (-2 * pi)   %* v `approxEqF4` v,
    rotateY (-1.5 * pi) %* v `approxEqF4` vec4 z y (-x) w,
    rotateY (-pi)       %* v `approxEqF4` vec4 (-x) y (-z) w,
    rotateY (-0.5 * pi) %* v `approxEqF4` vec4 (-z) y x w,
    rotateY 0           %* v `approxEqF4` v,
    rotateY (0.5 * pi)  %* v `approxEqF4` vec4 z y (-x) w,
    rotateY pi          %* v `approxEqF4` vec4 (-x) y (-z) w,
    rotateY (1.5 * pi)  %* v `approxEqF4` vec4 (-z) y x w,
    rotateY (2 * pi)    %* v `approxEqF4` v
  ]

prop_rotateZ :: Vector Float 4 -> Bool
prop_rotateZ v | (x,y,z,w) <- unpackV4 v =
  and [
    rotateZ (-2 * pi)   %* v `approxEqF4` v,
    rotateZ (-1.5 * pi) %* v `approxEqF4` vec4 (-y) x z w,
    rotateZ (-pi)       %* v `approxEqF4` vec4 (-x) (-y) z w,
    rotateZ (-0.5 * pi) %* v `approxEqF4` vec4 y (-x) z w,
    rotateZ 0           %* v `approxEqF4` v,
    rotateZ (0.5 * pi)  %* v `approxEqF4` vec4 (-y) x z w,
    rotateZ pi          %* v `approxEqF4` vec4 (-x) (-y) z w,
    rotateZ (1.5 * pi)  %* v `approxEqF4` vec4 y (-x) z w,
    rotateZ (2 * pi)    %* v `approxEqF4` v
  ]

prop_rotate :: Float -> Bool
prop_rotate a =
  and [
    rotate (vec3 1 0 0) a `approxEqF44` rotateX a,
    rotate (vec3 0 1 0) a `approxEqF44` rotateY a,
    rotate (vec3 0 0 1) a `approxEqF44` rotateZ a
  ]

prop_rotateEuler :: Float -> Float -> Float -> Bool
prop_rotateEuler x y z = rotateEuler x y z `approxEqF44` rotateX x %* rotateY y %* rotateZ z

prop_lookAt :: Vector Float 3 -> Vector Float 3 -> Vector Float 3 -> Bool
prop_lookAt up cam foc =
  and [
    (normalized . fromHom $ m %* toHomPoint foc) `approxEqF3` vec3 0 0 (-1),
    fromHom (m %* toHomPoint cam) `approxEqF3` 0,
    fromHom (m %* toHomVector xb) `approxEqF3` vec3 1 0 0,
    fromHom (m %* toHomVector yb) `approxEqF3` vec3 0 1 0,
    fromHom (m %* toHomVector zb) `approxEqF3` vec3 0 0 1
  ]
  where
    m = lookAt up cam foc
    zb = normalized $ cam - foc
    xb = normalized $ up `cross` zb
    yb = zb `cross` xb

prop_perspective :: Float -> Float -> Float -> Float -> Bool
prop_perspective a b c d =
  and [
    projectTo 0 0 n       `approxEqF3` vec3 0 0 (-1),
    projectTo 0 0 f       `approxEqF3` vec3 0 0 1,
    projectTo 1 1 n       `approxEqF3` vec3 1 1 (-1),
    projectTo 1 (-1) n    `approxEqF3` vec3 1 (-1) (-1),
    projectTo (-1) 1 n    `approxEqF3` vec3 (-1) 1 (-1),
    projectTo (-1) (-1) n `approxEqF3` vec3 (-1) (-1) (-1),
    projectTo 1 1 f       `approxEqF3` vec3 1 1 1,
    projectTo 1 (-1) f    `approxEqF3` vec3 1 (-1) 1,
    projectTo (-1) 1 f    `approxEqF3` vec3 (-1) 1 1,
    projectTo (-1) (-1) f `approxEqF3` vec3 (-1) (-1) 1
  ]
  where
    n = 1.0 + mod' a 9.0 -- Near plane in range [1, 10)
    f = n + 1.0 + mod' b 99.0 -- Far plane in range [n + 1, n + 100)
    fovy = (0.1 * pi) + mod' c (0.8 * pi) -- Y-axis field of view in range [0.1*pi, 0.9*pi)
    aspect = 0.25 + mod' d 4.0 -- Aspect ration in range [1/4, 4/1]
    hpd = tan (fovy * 0.5) -- height/distance
    wpd = aspect * hpd -- width/distance
    m = perspective n f fovy aspect
    projectTo x' y' z = fromHom $ m %* vec4 (x' * wpd * z) (y' * hpd * z) (-z) 1

prop_orthogonal :: Float -> Float -> Float -> Float -> Bool
prop_orthogonal a b c d =
  and [
    projectTo 0 0 n       `approxEqF3` vec3 0 0 (-1),
    projectTo 0 0 f       `approxEqF3` vec3 0 0 1,
    projectTo 1 1 n       `approxEqF3` vec3 1 1 (-1),
    projectTo 1 (-1) n    `approxEqF3` vec3 1 (-1) (-1),
    projectTo (-1) 1 n    `approxEqF3` vec3 (-1) 1 (-1),
    projectTo (-1) (-1) n `approxEqF3` vec3 (-1) (-1) (-1),
    projectTo 1 1 f       `approxEqF3` vec3 1 1 1,
    projectTo 1 (-1) f    `approxEqF3` vec3 1 (-1) 1,
    projectTo (-1) 1 f    `approxEqF3` vec3 (-1) 1 1,
    projectTo (-1) (-1) f `approxEqF3` vec3 (-1) (-1) 1
  ]
  where
    n = 1.0 + mod' a 9.0 -- Near plane in range [1, 10)
    f = n + 1.0 + mod' b 99.0 -- Far plane in range [n + 1, n + 100)
    w = 1.0 + mod' c 9999.0 -- Width in range [1, 10000)
    h = 1.0 + mod' d 9999.0 -- Height in range [1, 10000)
    m = orthogonal n f w h
    projectTo x' y' z = fromHom $ m %* vec4 (x' * w * 0.5) (y' * h * 0.5) (-z) 1

prop_toHomPoint :: Vector Float 3 -> Bool
prop_toHomPoint v | (x,y,z) <- unpackV3 v = toHomPoint v == vec4 x y z 1

prop_toHomVector :: Vector Float 3 -> Bool
prop_toHomVector v | (x,y,z) <- unpackV3 v = toHomVector v == vec4 x y z 0

prop_fromHom :: Vector Float 4 -> Bool
prop_fromHom v | (x,y,z,w) <- unpackV4 v =
  case w of
    0 -> fromHom v == vec3 x y z
    _ -> fromHom v `approxEqF3` vec3 (x/w) (y/w) (z/w)

return []
runTests :: IO Bool
runTests = $quickCheckAll
