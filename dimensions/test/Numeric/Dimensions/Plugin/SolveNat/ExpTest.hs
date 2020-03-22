{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
module Numeric.Dimensions.Plugin.SolveNat.ExpTest where

import           Data.Ratio
import           Data.Functor.Identity
import           Outputable              hiding ( (<>) )
import           Data.String                    ( IsString(..) )
import           Numeric.Natural
import           Test.QuickCheck
import qualified System.IO.Unsafe              as Sys
import qualified Control.Exception             as Sys
import qualified System.Timeout                as Sys
import           Data.Maybe

import           Numeric.Dimensions.Plugin.SolveNat.Exp

{-# ANN module ("HLint: ignore Evaluate" :: String) #-}


type TestExp = Exp Type Var

newtype Var = Var Natural
  deriving (Eq, Ord)

instance Show Var where
  show (Var x) = c : show x
    where
      z = fromEnum 'z'
      a = fromEnum 'a'
      c = toEnum $ (fromIntegral x - a) `mod` (z - a + 1) + a

instance Outputable Var where
  ppr = text . show

newtype Type = Type String
  deriving (Eq, Ord, IsString)

instance Show Type where
  show (Type x) = x

instance Outputable Type where
  ppr (Type x) = text x

instance Arbitrary Var where
  arbitrary = Var . fromInteger . abs <$> arbitrary
  shrink (Var x) = Var . fromInteger . abs <$> shrink (toInteger x)

instance Arbitrary Type where
  arbitrary = fmap Type $ choose  ('A', 'Z') >>= go
    where
      go c = fmap (c:) $ do
        oneMore <- arbitrary
        if oneMore
        then choose ('a', 'z') >>= go
        else return []

instance Arbitrary (Exp Type Var) where
  arbitrary = do
    funList <- choose (1, 4) >>= flip vectorOf arbitrary
    varList <- choose (1, 20) >>= flip vectorOf arbitrary
    let div' True a b = Div a (1 :+ b :* b)
        div' False a b = Div a b
        go n small = frequency
          [ (10 * n, N . (if small then (`mod` 10) else id) . fromInteger . abs <$> arbitrary)
          , (1, F <$> elements funList)
          , (8, V <$> elements varList)
          , (if small then 5 else 6, (:+) <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 5 else 4, (:-) <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 1 else 5, (:*) <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 0 else 2, (:^) <$> go (n + 1) True <*> go (n + 1) True)
          , (if small then 5 else 2, div' small <$> go (n + 1) small <*> go (n + 1) False)
          , (if small then 5 else 2, Mod <$> go (n + 1) False <*> go (n + 1) small)
          , (if small then 1 else 3, Max <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 6 else 3, Min <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 5 else 2, Log2 <$>  go (n + 1) small)
          ]
    go 1 False
  shrink (N n) = [N $ div n 2]
  shrink (V (Var n)) = [N n]
  shrink (F _) = []
  shrink (a :+ b) = [a, b]
  shrink (a :* b) = [a, b]
  shrink (a :- b) = [a, b]
  shrink (a :^ b) = [a, b]
  shrink (Div a b) = [a, b]
  shrink (Mod a b) = [a, b]
  shrink (Max a b) = [a, b]
  shrink (Min a b) = [a, b]
  shrink (Log2 a)  = [a]


-- | Try to evaluate something to WHNF, return Nothing otherwise.
inTime :: a -> Maybe a
inTime a = Sys.unsafePerformIO $ Sys.timeout 100000 $ Sys.evaluate a


failSlow :: Testable p => a -> p -> Property
failSlow a = (i'mStuck .&&.)
  where
    i'mStuck = counterexample "was not evaluated in time" (isJust $ inTime a)



evalExp :: TestExp -> Either TestExp Integer
evalExp =
  evaluate . runIdentity . substituteVar (\(Var x) -> pure (N x :: TestExp))

prop_evaluatable :: TestExp -> Property
prop_evaluatable e = failSlow (evalExp e) True


prop_evaluatable1 :: Property
prop_evaluatable1 = once $ prop_evaluatable $ (F "Dv" :^ 230) :^ 700

prop_evaluatable2 :: Property
prop_evaluatable2 = once $ prop_evaluatable $ 9 :^ (23 :^ (-16))


prop_log2RRange :: Rational -> Property
prop_log2RRange x = (x > 0) === isJust (log2R x)

prop_log2RValue :: Rational -> Property
prop_log2RValue x | Just y <- log2R x = if x >= 1
  then 2 ^^ y <= x .&&. 2 ^^ (y + 1) > x
  else 2 ^^ y >= x .&&. 2 ^^ (y - 1) < x
prop_log2RValue _ = property True

prop_log2RSymmetry :: Rational -> Property
prop_log2RSymmetry x = x /= 0 ==> fmap negate (log2R x) === log2R (recip x)

prop_divRInt :: Integer -> Integer -> Property
prop_divRInt x y =
  y /= 0 ==> Just (div x y) === divR (fromInteger x) (fromInteger y)

prop_modRInt :: Integer -> Integer -> Property
prop_modRInt x y = y /= 0 ==> Just (fromInteger $ mod x y) === modR
  (fromInteger x)
  (fromInteger y)

prop_divModR :: Rational -> Rational -> Property
prop_divModR _ 0 = property True
prop_divModR x y | Just d <- fromInteger <$> divR x y, Just m <- modR x y =
  x === m + y * d
prop_divModR _ _ = counterexample "Unexpected Nothing" False

prop_divRScale :: Rational -> Rational -> Rational -> Property
prop_divRScale x y c = c /= 0 ==> divR x y === divR (x * c) (y * c)

prop_modRScale :: Rational -> Rational -> Rational -> Property
prop_modRScale x y c =
  c /= 0 ==> fmap (* c) (modR x y) === modR (x * c) (y * c)

prop_powRInt :: Rational -> Integer -> Property
prop_powRInt x y =
  (x /= 0 || y > 0) ==> powR x (fromInteger y) === Just (x ^^ y)

prop_powRValidRoot :: Rational -> Integer -> Property
prop_powRValidRoot x' y' =
  (x /= 0 && y /= 0 && (x > 0 || odd y)) ==> Just x == powR p r
  where
    y = signum y' * mod y' 50
    x = signum x' * (mod (numerator x') 1000 % (1 + mod (denominator x') 1000))
    p = x ^^ y
    r = recip $ fromInteger y

return []
runTests :: IO Bool
runTests =
  $(forAllProperties) $ quickCheckWithResult stdArgs { maxSuccess = 1000 }

