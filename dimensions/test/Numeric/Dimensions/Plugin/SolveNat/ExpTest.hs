{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
module Numeric.Dimensions.Plugin.SolveNat.ExpTest where


import           Data.Functor.Identity
import           Outputable              hiding ( (<>) )
import           Data.String                    ( IsString(..) )
import           Numeric.Natural
import           Test.QuickCheck                ( Property
                                                , Arbitrary(..)
                                                , (.&&.)
                                                )
import qualified Test.QuickCheck               as QC
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
  arbitrary = fmap Type $ QC.choose  ('A', 'Z') >>= go
    where
      go c = fmap (c:) $ do
        oneMore <- arbitrary
        if oneMore
        then QC.choose ('a', 'z') >>= go
        else return []

instance Arbitrary (Exp Type Var) where
  arbitrary = do
    funList <- QC.choose (1, 4) >>= flip QC.vectorOf arbitrary
    varList <- QC.choose (1, 20) >>= flip QC.vectorOf arbitrary
    let div' True a b = Div a (1 :+ b :* b)
        div' False a b = Div a b
        go n small = QC.frequency
          [ (10 * n, N . (if small then (`mod` 10) else id) . fromInteger . abs <$> arbitrary)
          , (1, F <$> QC.elements funList)
          , (8, V <$> QC.elements varList)
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
inTime a = Sys.unsafePerformIO $ Sys.timeout 1000000 $ Sys.evaluate a


failSlow :: QC.Testable p => a -> p -> Property
failSlow a = (i'mStuck .&&.)
  where
    i'mStuck =
      QC.counterexample "was not evaluated in time" (isJust $ inTime a)



evalExp :: TestExp -> Either TestExp Integer
evalExp =
  evaluate . runIdentity . substituteVar (\(Var x) -> pure (N x :: TestExp))

prop_evaluatable :: TestExp -> Property
prop_evaluatable e = failSlow (evalExp e) True


prop_evaluatable1 :: Property
prop_evaluatable1 = QC.once $ prop_evaluatable $ (F "Dv" :^ 230) :^ 700

prop_evaluatable2 :: Property
prop_evaluatable2 = QC.once $ prop_evaluatable $ 9 :^ (23 :^ (-16))


return []
runTests :: IO Bool
-- runTests = $quickCheckAll
runTests = $(QC.forAllProperties)
  $ QC.quickCheckWithResult QC.stdArgs { QC.maxSuccess = 1000 }

