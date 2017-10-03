{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.QuaternionTest (runTests) where

import Test.QuickCheck
import Numeric.Quaternion
import Numeric.Vector


instance (Quaternion t, Arbitrary t) => Arbitrary (Quater t) where
  arbitrary = packQ <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink q | (x,y,z,t) <- unpackQ q = packQ <$> shrink x <*> shrink y  <*> shrink z <*> shrink t

instance Arbitrary Vec3d where
  arbitrary = vec3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink v | (x,y,z) <- unpackV3 v = vec3 <$> shrink x <*> shrink y  <*> shrink z


(=~=) :: (Quaternion a, Num a, Ord a, Num (Quater a), Fractional a) => Quater a -> Quater a -> Bool
(=~=) a b = taker (abs (a - b)) <= eps
    where
      s = max 1e-6 $ max (taker (abs a)) (taker (abs b))
      eps = s * 1e-6
infix 4 =~=


prop_Eq :: QDouble -> Bool
prop_Eq q = and
    [ (\(x,y,z,t) -> packQ x y z t) (unpackQ q)     == q
    , packQ (takei q) (takej q) (takek q) (taker q) == q
    , fromVecNum (imVec q) (taker q)                == q
    , im q + re q                                   == q
    ]


prop_DoubleConjugate :: QDouble -> Bool
prop_DoubleConjugate q = conjugate (conjugate q) == q

prop_Square :: QDouble -> Bool
prop_Square q = q * conjugate q  =~= realToFrac (square q)


prop_RotScale :: QDouble -> Vec3d -> Bool
prop_RotScale q v = fromVecNum (rotScale q v) 0 =~= q * fromVecNum v 0 * conjugate q

prop_GetRotScale :: Vec3d -> Vec3d -> Bool
prop_GetRotScale a b = fromVecNum (rotScale q a) 0 =~= fromVecNum b 0
  where
    q = getRotScale a b


prop_InverseRotScale :: QDouble -> Vec3d -> Bool
prop_InverseRotScale q v | q /= 0    = fromVecNum (rotScale (1/q) (rotScale q v)) 0 =~= fromVecNum v 0
                         | otherwise = True

prop_FromToMatrix33 :: QDouble -> Bool
prop_FromToMatrix33 q | q == 0 = True
                      | otherwise = fromMatrix33 (toMatrix33 (signum q)) =~= signum q * signum (re q)

prop_FromToMatrix44 :: QDouble -> Bool
prop_FromToMatrix44 q | q == 0 = True
                      | otherwise = fromMatrix44 (toMatrix44 (signum q)) =~= signum q * signum (re q)

prop_RotationArg :: QDouble -> Bool
prop_RotationArg q | q == 0 = axisRotation (imVec q) (qArg q) =~= 1
                   | otherwise = axisRotation (imVec q) (qArg q) =~= signum q


prop_UnitQ :: QDouble -> Bool
prop_UnitQ q | q == 0 = True
             | otherwise = realToFrac (square (q / q)) =~= (1 :: QDouble)


prop_ExpLog :: QDouble -> Bool
prop_ExpLog q | q == 0 = log (exp q) == q
              | otherwise = exp (log q) =~= q

prop_SinAsin :: QDouble -> Bool
prop_SinAsin q = sin (asin q') =~= q'
    where
      q' = signum q

prop_CosAcos :: QDouble -> Bool
prop_CosAcos q = cos (acos q') =~= q'
    where
      q' = signum q

prop_TanAtan :: QDouble -> Bool
prop_TanAtan q = tan (atan q') =~= q'
    where -- protect agains big numbers and rounding errors
      q' = if square q >= 1e6 then signum q else q


prop_SinhAsinh :: QDouble -> Bool
prop_SinhAsinh q = sinh (asinh q') =~= q'
    where -- protect agains big numbers and rounding errors
      q' = if square q >= 1e6 then signum q else q

prop_CoshAcosh :: QDouble -> Bool
prop_CoshAcosh q = cosh (acosh q') =~= q'
    where -- protect agains big numbers and rounding errors
      q' = if square q >= 1e6 then signum q else q

prop_TanhAtanh :: QDouble -> Bool
prop_TanhAtanh q = tanh (atanh q') =~= q'
    where -- protect agains big numbers and rounding errors
      q' = if square q >= 1e6 then signum q else q

prop_SinCos :: QDouble -> Bool
prop_SinCos q = sin q' * sin q' + cos q' * cos q' =~= 1
    where
      q' = signum q

prop_SinhCosh :: QDouble -> Bool
prop_SinhCosh q = cosh q' * cosh q' - sinh q' * sinh q' =~= 1
    where
      q' = signum q

return []
runTests :: IO Bool
runTests = $quickCheckAll
