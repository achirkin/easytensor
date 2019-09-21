{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Numeric.QuaterDoubleTest (runTests) where

import Numeric.Arbitraries
import Numeric.Basics
import Numeric.Quaternion
import Numeric.Vector
import Test.QuickCheck

type T = Double

-- Extra factor for compensating too strict Epsilon.
extraTolerance :: T
extraTolerance = 1

-- | Some non-linear function are very unstable;
--   it would be a downting task to determine the uncertainty precisely.
--   Instead, I just make sure the tolerance is small enough to find at least
--   the most obvious bugs.
--   This function increases the tolerance by the span of magnitudes in q.
qSpan :: Quater T -> T
qSpan (Quater a b c d) = asSpan . foldl mm (1,1) $ map (\x -> x*x) [a, b, c, d]
  where
    mm :: (T,T) -> T -> (T,T)
    mm (mi, ma) x
      | x > M_EPS = (min mi x, max ma x)
      | otherwise = (mi, ma)
    asSpan :: (T,T) -> T
    asSpan (mi, ma) = extraTolerance * ma / mi


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
    = normL2 a * ab > M_EPS * normL2 b
      ==> approxEq (recip ab) b (rotScale q a)
  where
    q = getRotScale a b
    -- when a and b are almost opposite, precision of getRotScale suffers a lot
    -- compensate it by increasing tolerance:
    ab = min 1 $ 1 + normalized a `dot` normalized b


prop_InverseRotScale :: Quater T -> Vector T 3 -> Property
prop_InverseRotScale q v
  = min (recip s) s > M_EPS ==> v =~= rotScale (1/q) (rotScale q v)
  where
    s = square q

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
  = square q > M_EPS ==> square (q / q) =~= 1


prop_ExpLog :: Quater T -> Property
prop_ExpLog q | square q < M_EPS = log (exp q) =~= q
              | otherwise = exp (log q) =~= q

prop_SinAsin :: Quater T -> Property
prop_SinAsin q = approxEq (qSpan q `max` qSpan q') q $ sin q'
  where
    q' = asin q

prop_CosAcos :: Quater T -> Property
prop_CosAcos q = approxEq (qSpan q `max` qSpan q') q $ cos q'
  where
    q' = acos q

prop_TanAtan :: Quater T -> Property
prop_TanAtan q = approxEq (qSpan q `max` qSpan q') q $ tan q'
  where
    q' = atan q

prop_SinhAsinh :: Quater T -> Property
prop_SinhAsinh q = approxEq (qSpan q `max` qSpan q') q $ sinh q'
  where
    q' = asinh q

prop_CoshAcosh :: Quater T -> Property
prop_CoshAcosh q = approxEq (qSpan q `max` qSpan q') q $ cosh q'
  where
    q' = acosh q

prop_TanhAtanh :: Quater T -> Property
prop_TanhAtanh q = approxEq (qSpan q `max` qSpan q') q $ tanh q'
  where
    q' = atanh q

prop_SqrtSqr :: Quater T -> Property
prop_SqrtSqr q = approxEq (qSpan q) q $ sqrt q * sqrt q

prop_SinCos :: Quater T -> Property
prop_SinCos q' = approxEq (qSpan s `max` qSpan c) 1 $ c * c + s * s
  where
    q = signum q' -- avoid exploding exponents
    s = sin q
    c = cos q

prop_SinhCosh :: Quater T -> Property
prop_SinhCosh q' = approxEq (qSpan s `max` qSpan c) 1 $ c * c - s * s
  where
    q = signum q' -- avoid exploding exponents
    s = sinh q
    c = cosh q

prop_ReadShow :: Quater T -> Bool
prop_ReadShow q = q == read (show q)

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
