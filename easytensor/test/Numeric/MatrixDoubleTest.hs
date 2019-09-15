{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.MatrixDoubleTest (runTests) where


import Data.Fixed
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Test.QuickCheck

type TestElem = Double

dropW :: (SubSpace t '[3] '[] '[3], SubSpace t '[4] '[] '[4])
      => Vector t 4 -> Vector t 3
dropW (Vec4 x y z _) = Vec3 x y z

-- | \( {|| A ||}_{\Infty} = \max_{0 \leq i < n} \sum_{j = 0}^{m} | a_{ij} | \)
--     is useful for bounding numeric errors of matrix algorithms.
matNormPInf :: forall t (n :: Nat) (m :: Nat)
             . (KnownDim n, KnownDim m, PrimBytes t, Ord t, Num t)
            => Matrix t n m -> Scalar t
matNormPInf = ewfoldl @t @'[n] @'[m]
      (\a -> max a . ewfoldl @t @'[m] @'[] (\e -> (e+) . abs) 0) 0

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
    mag = M_EPS * n4 * matNormPInf m ^ nw

prop_inverse :: SomeSquareMatrix NonSingular TestElem -> Property
prop_inverse (SSM m) = check (m %* mi) .&&. check (mi %* m)
  where
    mi = inverse m
    nw = (case dims `inSpaceOf` m of dn :* _ -> dimVal dn) :: Word
    n4 = fromIntegral nw ^ (4 :: Int)
    mag = M_EPS * n4 * matNormPInf m
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

prop_translate3vs4 :: Vector TestElem 4 -> Bool
prop_translate3vs4 v = translate4 v == translate3 (dropW v)

prop_translate4 :: Vector TestElem 4 -> Vector TestElem 3 -> Bool
prop_translate4 a b = toHomPoint b %* translate4 a == toHomPoint (dropW a + b)

prop_translate3 :: Vector TestElem 3 -> Vector TestElem 3 -> Bool
prop_translate3 a b = toHomPoint b %* translate3 a == toHomPoint (a + b)

prop_rotateX :: Vector TestElem 4 -> Property
prop_rotateX v@(Vec4 x y z w) =
  conjoin [
    v %* rotateX (-2 * pi)   =~= v,
    v %* rotateX (-1.5 * pi) =~= vec4 x (-z) y w,
    v %* rotateX (-pi)       =~= vec4 x (-y) (-z) w,
    v %* rotateX (-0.5 * pi) =~= vec4 x z (-y) w,
    v %* rotateX 0           =~= v,
    v %* rotateX (0.5 * pi)  =~= vec4 x (-z) y w,
    v %* rotateX pi          =~= vec4 x (-y) (-z) w,
    v %* rotateX (1.5 * pi)  =~= vec4 x z (-y) w,
    v %* rotateX (2 * pi)    =~= v
  ]

prop_rotateY :: Vector TestElem 4 -> Property
prop_rotateY v@(Vec4 x y z w) =
  conjoin [
    v %* rotateY (-2 * pi)   =~= v,
    v %* rotateY (-1.5 * pi) =~= vec4 z y (-x) w,
    v %* rotateY (-pi)       =~= vec4 (-x) y (-z) w,
    v %* rotateY (-0.5 * pi) =~= vec4 (-z) y x w,
    v %* rotateY 0           =~= v,
    v %* rotateY (0.5 * pi)  =~= vec4 z y (-x) w,
    v %* rotateY pi          =~= vec4 (-x) y (-z) w,
    v %* rotateY (1.5 * pi)  =~= vec4 (-z) y x w,
    v %* rotateY (2 * pi)    =~= v
  ]

prop_rotateZ :: Vector TestElem 4 -> Property
prop_rotateZ v@(Vec4 x y z w) =
  conjoin [
    v %* rotateZ (-2 * pi)   =~= v,
    v %* rotateZ (-1.5 * pi) =~= vec4 (-y) x z w,
    v %* rotateZ (-pi)       =~= vec4 (-x) (-y) z w,
    v %* rotateZ (-0.5 * pi) =~= vec4 y (-x) z w,
    v %* rotateZ 0           =~= v,
    v %* rotateZ (0.5 * pi)  =~= vec4 (-y) x z w,
    v %* rotateZ pi          =~= vec4 (-x) (-y) z w,
    v %* rotateZ (1.5 * pi)  =~= vec4 y (-x) z w,
    v %* rotateZ (2 * pi)    =~= v
  ]

prop_rotate :: TestElem -> Property
prop_rotate a =
  conjoin [
    rotate (vec3 1 0 0) a =~= rotateX a,
    rotate (vec3 0 1 0) a =~= rotateY a,
    rotate (vec3 0 0 1) a =~= rotateZ a
  ]

prop_rotateEuler :: TestElem -> TestElem -> TestElem -> Property
prop_rotateEuler pitch yaw roll =
  rotateEuler pitch yaw roll =~= rotateZ roll %* rotateY yaw %* rotateX pitch

prop_lookAt :: Vector TestElem 3 -> Vector TestElem 3 -> Vector TestElem 3 -> Property
prop_lookAt up cam foc =
  (apart cam foc && apart up cam) ==>
  conjoin [
    (normalized . fromHom $ toHomPoint foc %* m) =~= vec3 0 0 (-1),
    fromHom (toHomPoint cam %* m) =~= 0,
    fromHom (toHomVector xb %* m) =~= vec3 1 0 0,
    fromHom (toHomVector yb %* m) =~= vec3 0 1 0,
    fromHom (toHomVector zb %* m) =~= vec3 0 0 1
  ]
  where
    apart :: Vector TestElem 3 -> Vector TestElem 3 -> Bool
    apart a b = normL2 (cross a b) > 0.1 * abs (dot a b)
    m = lookAt up cam foc
    zb = normalized $ cam - foc
    xb = normalized $ up `cross` zb
    yb = zb `cross` xb

prop_perspective :: TestElem -> TestElem -> TestElem -> TestElem -> Property
prop_perspective a b c d =
  conjoin [
    projectTo 0 0 n       =~= vec3 0 0 (-1),
    projectTo 0 0 f       =~= vec3 0 0 1,
    projectTo 1 1 n       =~= vec3 1 1 (-1),
    projectTo 1 (-1) n    =~= vec3 1 (-1) (-1),
    projectTo (-1) 1 n    =~= vec3 (-1) 1 (-1),
    projectTo (-1) (-1) n =~= vec3 (-1) (-1) (-1),
    projectTo 1 1 f       =~= vec3 1 1 1,
    projectTo 1 (-1) f    =~= vec3 1 (-1) 1,
    projectTo (-1) 1 f    =~= vec3 (-1) 1 1,
    projectTo (-1) (-1) f =~= vec3 (-1) (-1) 1
  ]
  where
    n = 1.0 + mod' a 9.0 -- Near plane in range [1, 10)
    f = 2*n + mod' b 99.0 -- Far plane in range [2*n, 2*n + 100)
    fovy = (0.1 * pi) + mod' c (0.8 * pi) -- Y-axis field of view in range [0.1*pi, 0.9*pi)
    aspect = 0.25 + mod' d 4.0 -- Aspect ration in range [1/4, 4/1]
    hpd = tan (fovy * 0.5) -- height/distance
    wpd = aspect * hpd -- width/distance
    m = perspective n f fovy aspect
    projectTo x' y' z = fromHom $ vec4 (x' * wpd * z) (y' * hpd * z) (-z) 1 %* m

prop_orthogonal :: TestElem -> TestElem -> TestElem -> TestElem -> Property
prop_orthogonal a b c d =
  conjoin [
    projectTo 0 0 n       =~= vec3 0 0 (-1),
    projectTo 0 0 f       =~= vec3 0 0 1,
    projectTo 1 1 n       =~= vec3 1 1 (-1),
    projectTo 1 (-1) n    =~= vec3 1 (-1) (-1),
    projectTo (-1) 1 n    =~= vec3 (-1) 1 (-1),
    projectTo (-1) (-1) n =~= vec3 (-1) (-1) (-1),
    projectTo 1 1 f       =~= vec3 1 1 1,
    projectTo 1 (-1) f    =~= vec3 1 (-1) 1,
    projectTo (-1) 1 f    =~= vec3 (-1) 1 1,
    projectTo (-1) (-1) f =~= vec3 (-1) (-1) 1
  ]
  where
    n = 1.0 + mod' a 9.0 -- Near plane in range [1, 10)
    f = 2*n + mod' b 99.0 -- Far plane in range [2*n, 2*n + 100)
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
    _ -> fromHom v =~= vec3 (x/w) (y/w) (z/w)

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
