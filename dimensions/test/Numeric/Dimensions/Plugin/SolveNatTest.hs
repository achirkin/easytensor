{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
module Numeric.Dimensions.Plugin.SolveNatTest where


import           Data.Functor.Identity
import           Outputable              hiding ( (<>) )
import           Data.String                    ( IsString(..) )
import           Numeric.Natural
import           Test.QuickCheck                ( Property
                                                , Arbitrary(..)
                                                , (.||.)
                                                )
import qualified Test.QuickCheck               as QC

import           Numeric.Dimensions.Plugin.SolveNat.Exp
import           Numeric.Dimensions.Plugin.SolveNat.NormalForm
import           Numeric.Dimensions.Plugin.SolveNat

{-# ANN module ("HLint: ignore Evaluate" :: String) #-}


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
    let go n small = QC.frequency
          [ (10 * n, N . (if small then (`mod` 10) else id) . fromInteger . abs <$> arbitrary)
          , (1, F <$> QC.elements funList)
          , (8, V <$> QC.elements varList)
          , (if small then 5 else 6, (:+) <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 5 else 4, (:-) <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 1 else 5, (:*) <$> go (n + 1) small <*> go (n + 1) small)
          , (if small then 0 else 2, (:^) <$> go (n + 1) True <*> go (n + 1) True)
          , (if small then 5 else 2, Div <$> go (n + 1) small <*> go (n + 1) False)
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

keepSolutions :: Exp Type Var -> Property
keepSolutions expOrig = i'mStuck .||. QC.counterexample
  (showSDocUnsafe $ vcat
    [ text "  Original expr:" <+> ppr expOrig
    , text "Normalized expr:" <+> ppr expNorm
    , text "    Normal form:" <+> text (take 100 $ show normal)
    ]
  )
  (case (resOrig, resNorm) of
    (Left _, Left _) -> QC.classify True "both not evaluated" True
    (Right a, Right b)
      | a == b -> QC.property True
      | otherwise -> QC.counterexample
        ("  Results are not equal! " ++ show a ++ " /= " ++ show b)
        False
    (Left  _, Right _) -> QC.classify True "simplifed to evaluatable" True
    (Right a, Left b ) -> QC.counterexample
      (  "  Non-evaluatable after normalization: "
      ++ show b
      ++ "\n  Evaluated original: "
      ++ show a
      )
      False
  )
  where
    normal  = simplify $ normalize expOrig
    expNorm = fromNormal normal
    substVals =
      runIdentity . substituteVar (\(Var x) -> pure (N x :: Exp Type Var))
    resOrig = evaluate $ substVals expOrig
    resNorm = evaluate $ substVals expNorm
    i'mStuck =
      QC.classify True "was not evaluated in time"
        $   QC.ioProperty
        $   not
        .   QC.isSuccess
        <$> QC.quickCheckWithResult
              QC.stdArgs { QC.chatty = False }
              (QC.within 1000000 (resOrig `seq` resNorm `seq` True))

ks :: Exp Type Var -> Property
ks = QC.once . keepSolutions


-- prop_keepSolutions :: Exp Type Var -> Property
-- prop_keepSolutions = keepSolutions

prop_keepSolutions_1 :: Property
prop_keepSolutions_1 = ks $ N 0 :^ N 0

prop_keepSolutions_2 :: Property
prop_keepSolutions_2 = ks $ N 3 :^ (N 1 :- Min (N 4) (N 0))

prop_keepSolutions_3 :: Property
prop_keepSolutions_3 = ks $ Min (N 3) (N 1) :* (N 0 :- N 16)

prop_keepSolutions_4 :: Property
prop_keepSolutions_4 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ N 2

prop_keepSolutions_5 :: Property
prop_keepSolutions_5 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ N 3

prop_keepSolutions_6 :: Property
prop_keepSolutions_6 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ N 0

prop_keepSolutions_7 :: Property
prop_keepSolutions_7 =
  ks $ Max (V (Var 12) - V (Var 13)) (V (Var 15) :- V (Var 17)) :^ V (Var 2)

prop_keepSolutions_8 :: Property
prop_keepSolutions_8 =
  ks $ Max (V (Var 12) - V (Var 13)) (V (Var 15) :- V (Var 17)) :^ V (Var 3)

prop_keepSolutions_9 :: Property
prop_keepSolutions_9 =
  ks $ Max (V (Var 12) - V (Var 13)) (V (Var 15) :- V (Var 17)) :^ V (Var 0)

prop_keepSolutions_10 :: Property
prop_keepSolutions_10 = ks $ Min (V (Var 1)) (N 2) :^ (N 0 :- N 2)

prop_keepSolutions_11 :: Property
prop_keepSolutions_11 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ (N 0 :- N 2)

return []
runTests :: IO Bool
-- runTests = $quickCheckAll
runTests = $(QC.forAllProperties)
  $ QC.quickCheckWithResult QC.stdArgs { QC.maxSuccess = 10000 }

