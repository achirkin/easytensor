{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Numeric.QuaterFloatTest (runTests) where

import Numeric.DataFrame.Arbitraries ()
import Numeric.Quaternion
import Numeric.Scalar
import Numeric.Vector
import Test.QuickCheck

type T = Float

eps :: T
eps = 1e-3

-- | Some non-linear function are very unstable;
--   it would be a downting task to determine the uncertainty precisely.
--   Instead, I just make sure the numbers are not so big to find at least
--   the most obvious bugs.
smaller :: Quater T -> Quater T
smaller q@(Quater x y z w)
  = if square q >= 1 then Quater (f x) (f y) (f z) (f w) else q
  where
    f :: T -> T
    f a | abs a > 2 = signum a * log (abs a)
        | otherwise = a

(=~=) :: Quater T -> Quater T -> Property
(=~=) = approxEq eps
infix 4 =~=

-- Even lower precision - for naturally fragile operations
approxEq :: T -> Quater T -> Quater T -> Property
approxEq e a b = property $ err <= c * scalar e
    where
      v1 = toVec4 a
      v2 = toVec4 b
      c = 1 `max` normL1 v1 `max` normL1 v2
      err = normL1 $ v1 - v2

prop_Eq :: Quater T -> Bool
prop_Eq q = and
    [ q == q
    , Quater (takei q) (takej q) (takek q) (taker q) == q
    , fromVecNum (imVec q) (taker q)                 == q
    , im q + re q                                    == q
    , fromVec4 (toVec4 q)                            == q
    ]

prop_DoubleConjugate :: Quater T -> Property
prop_DoubleConjugate q = property $ conjugate (conjugate q) == q

prop_Square :: Quater T -> Property
prop_Square q = q * conjugate q  =~= realToFrac (square q)

prop_RotScale :: Quater T -> Vector T 3 -> Property
prop_RotScale q v = fromVecNum (rotScale q v) 0 =~= q * fromVecNum v 0 * conjugate q

prop_GetRotScale :: Vector T 3 -> Vector T 3 -> Property
prop_GetRotScale a b
  = a /= 0 ==> approxEq eps' (fromVecNum (rotScale q a) 0) (fromVecNum b 0)
  where
    q = getRotScale a b
    -- when a and b are almost opposite, precision of getRotScale suffers a lot
    -- compensate it by increasing eps:
    eps' = if cross a b == 0
           then eps
           else eps / (max eps . min 1 . unScalar $
                         1 + normalized a `dot` normalized b)

prop_InverseRotScale :: Quater T -> Vector T 3 -> Property
prop_InverseRotScale q v
  = square q > eps ==> fromVecNum (rotScale (1/q) (rotScale q v)) 0 =~= fromVecNum v 0

prop_NegateToMatrix33 :: Quater T -> Bool
prop_NegateToMatrix33 q = toMatrix33 q == toMatrix33 (negate q)

prop_NegateToMatrix44 :: Quater T -> Bool
prop_NegateToMatrix44 q = toMatrix44 q == toMatrix44 (negate q)

prop_FromToMatrix33 :: Quater T -> Property
prop_FromToMatrix33 q
  = q /= 0 ==> fromMatrix33 (toMatrix33 q) =~= q
          .||. fromMatrix33 (toMatrix33 q) =~= negate q

prop_FromToMatrix44 :: Quater T -> Property
prop_FromToMatrix44 q
  = q /= 0 ==> fromMatrix44 (toMatrix44 q) =~= q
          .||. fromMatrix44 (toMatrix44 q) =~= negate q


prop_RotationArg :: Quater T -> Property
prop_RotationArg q | q == 0 = axisRotation (imVec q) (qArg q) =~= 1
                   | otherwise = axisRotation (imVec q) (qArg q) =~= signum q

prop_UnitQ :: Quater T -> Property
prop_UnitQ q
  = square q > eps ==> realToFrac (square (q / q)) =~= 1


prop_ExpLog :: Quater T -> Property
prop_ExpLog q | square q < eps = log (exp q) =~= q
              | otherwise = exp (log q) =~= q

prop_SinAsin :: Quater T -> Property
prop_SinAsin q = sin (asin q') =~= q'
    where
      q' = signum q

prop_CosAcos :: Quater T -> Property
prop_CosAcos q = cos (acos q') =~= q'
    where
      q' = signum q

prop_TanAtan :: Quater T -> Property
prop_TanAtan q = tan (atan q') =~= q'
    where
      q' = sqrt (smaller q) -- because this inverse fun is fragile

prop_SinhAsinh :: Quater T -> Property
prop_SinhAsinh q = sinh (asinh q') =~= q'
    where
      q' = sqrt (smaller q) -- because this inverse fun is fragile

prop_CoshAcosh :: Quater T -> Property
prop_CoshAcosh q = cosh (acosh q) =~= q

prop_TanhAtanh :: Quater T -> Property
prop_TanhAtanh q = tanh (atanh q') =~= q'
    where
      q' = sqrt (smaller q) -- because this inverse fun is fragile

prop_SqrtSqr :: Quater T -> Property
prop_SqrtSqr q = sqrt q * sqrt q =~= q

prop_SinCos :: Quater T -> Property
prop_SinCos q = sin q' * sin q' + cos q' * cos q' =~= 1
    where
      q' = sqrt (smaller q) -- because it involves exponents

prop_SinhCosh :: Quater T -> Property
prop_SinhCosh q = cosh q' * cosh q' - sinh q' * sinh q' =~= 1
    where
      q' = sqrt (smaller q) -- because it involves exponents

prop_ReadShow :: Quater T -> Bool
prop_ReadShow q = q == read (show q)

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
