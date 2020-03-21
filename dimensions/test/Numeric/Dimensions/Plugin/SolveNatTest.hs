{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Numeric.Dimensions.Plugin.SolveNatTest where


import           Outputable              hiding ( (<>) )
import           Test.QuickCheck                ( Property
                                                , Arbitrary(..)
                                                , (.||.)
                                                )
import qualified Test.QuickCheck               as QC
import           Data.Maybe
import           Data.Foldable                  ( toList )

import           Numeric.Dimensions.Plugin.SolveNat
import           Numeric.Dimensions.Plugin.SolveNat.Exp
import           Numeric.Dimensions.Plugin.SolveNat.NormalForm
import           Numeric.Dimensions.Plugin.SolveNat.ExpTest

{-# ANN module ("HLint: ignore Evaluate" :: String) #-}
{-# ANN module ("HLint: ignore Redundant negate" :: String) #-}


prop_valid :: NormalE Type Var -> Property
prop_valid x = case validate x of
  Ok           -> QC.property True
  Invalid errs -> QC.counterexample
    (showSDocUnsafe $ hang ("Expr:" <+> ppr (fromNormal x)) 2 $ vcat
      (toList errs)
    )
    False

ignoreSlow :: QC.Testable p => a -> p -> Property
ignoreSlow a = (i'mStuck .||.)
  where
    i'mStuck =
      QC.classify True "was not evaluated in time" (isNothing $ inTime a)

evalN :: NormalE Type Var -> Either TestExp Integer
evalN = evalExp . fromNormal

instance Arbitrary (NormalE Type Var) where
  arbitrary = do
    expOrig <- arbitrary
    let x  = normalize expOrig
        expNorm = fromNormal x
        resOrig = evalExp expOrig
        resNorm = evalExp expNorm
    evald <- pure . inTime $ resOrig `seq` resNorm `seq` (resOrig, resNorm)
    case evald of
      Just (Right a, Right b) | a == b -> return x
      _ -> arbitrary


prop_numPlusZero :: NormalE Type Var -> Property
prop_numPlusZero x = ignoreSlow (rx0 `seq` rx1 `seq` r0x `seq` r)
  $ and [r == rx0, r == rx1, r == r0x]
  where
    r   = evalN x
    rx0 = evalN (x + 0)
    rx1 = evalN (x - 0)
    r0x = evalN (0 + x)

prop_numPlusSym :: NormalE Type Var -> NormalE Type Var -> Property
prop_numPlusSym x y = ignoreSlow (xy `seq` yx) $ xy == yx
  where
    xy = evalN (x + y)
    yx = evalN (y + x)

prop_numTimesOne :: NormalE Type Var -> Property
prop_numTimesOne x = ignoreSlow (r1x `seq` rx1 `seq` r) $ r == r1x && r == rx1
  where
    r   = evalN x
    r1x = evalN (1 * x)
    rx1 = evalN (x * 1)

prop_numTimesSym :: NormalE Type Var -> NormalE Type Var -> Property
prop_numTimesSym x y = ignoreSlow (xy `seq` yx) $ xy == yx
  where
    xy = evalN (x * y)
    yx = evalN (y * x)

prop_negateTwice :: NormalE Type Var -> Property
prop_negateTwice x = ignoreSlow (rm `seq` rp) $ rp == rm
  where
    rp = evalN x
    rm = evalN $ negate $ negate x


prop_negateOnce :: NormalE Type Var -> Property
prop_negateOnce x = ignoreSlow (rm `seq` rp) (rp == fmap negate rm)
  where
    rp = evalN x
    rm = evalN $ negate x

prop_abs :: NormalE Type Var -> Property
prop_abs x =
  QC.counterexample (showSDocUnsafe . ppr $ fromNormal x)
    $ ignoreSlow (a `seq` m `seq` p)
    $ and [a == p || a == m, a >= m, a >= p, a >= 0]
  where
    Right a = evalN $ abs x
    Right p = evalN x
    Right m = evalN $ negate x

prop_max :: NormalE Type Var -> NormalE Type Var -> Property
prop_max x y = ignoreSlow (rx `seq` ry `seq` rm)
  $ and [rm >= rx, rm >= ry, rx == rm || ry == rm]
  where
    Right rx = evalN x
    Right ry = evalN y
    Right rm = evalN $ neMax x y

prop_min :: NormalE Type Var -> NormalE Type Var -> Property
prop_min x y = ignoreSlow (rx `seq` ry `seq` rm)
  $ and [rm <= rx, rm <= ry, rx == rm || ry == rm]
  where
    Right rx = evalN x
    Right ry = evalN y
    Right rm = evalN $ neMin x y

prop_sumPN :: NormalE Type Var -> Property
prop_sumPN x =
  QC.counterexample (showSDocUnsafe . ppr $ fromNormal x)
    $ ignoreSlow (rx `seq` ry) (rx QC.=== ry)
  where
    rx = evalN x
    ry = evalN $ neMin x 0 + neMax x 0


keepSolutions :: TestExp -> Property
keepSolutions expOrig = ignoreSlow (resOrig `seq` resNorm) $ QC.counterexample
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
    normal  = normalize expOrig
    expNorm = fromNormal normal
    resOrig = evalExp expOrig
    resNorm = evalExp expNorm

ks :: TestExp -> Property
ks = QC.once . keepSolutions


prop_keepSolutions :: TestExp -> Property
prop_keepSolutions = keepSolutions

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

prop_keepSolutions_12 :: Property
prop_keepSolutions_12 = ks $ Max (Min (N 0) (N 1)) (N 3) :^ N 2

prop_keepSolutions_13 :: Property
prop_keepSolutions_13 = ks $ Min (N 1) (N 3) :^ (N 0 :- N 2)

prop_keepSolutions_14 :: Property
prop_keepSolutions_14 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ N 2

prop_keepSolutions_15 :: Property
prop_keepSolutions_15 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ N 3

prop_keepSolutions_16 :: Property
prop_keepSolutions_16 = ks $ Max (N 0 - N 1) (N 0 :- N 3) :^ (N 0 :- N 3)

prop_keepSolutions_17 :: Property
prop_keepSolutions_17 =
  ks $ Max (V (Var 1) - V (Var 2)) (V (Var 1) + V (Var 2)) :^ (N 0 :- N 3)

prop_keepSolutions_18 :: Property
prop_keepSolutions_18 =
  ks $ Min (V (Var 1) - V (Var 2)) (V (Var 1) + V (Var 2)) :^ (N 0 :- N 3)


prop_keepSolutions_19 :: Property
prop_keepSolutions_19 =
  ks $ Max (V (Var 1) - V (Var 2)) (V (Var 2) - V (Var 1)) :^ N 3

prop_keepSolutions_20 :: Property
prop_keepSolutions_20 =
  ks $ Max (V (Var 1) - V (Var 2)) (V (Var 2) - V (Var 1)) :^ (N 0 :- N 3)

prop_keepSolutions_21 :: Property
prop_keepSolutions_21 = ks $ (N 5 - (V (Var 2))) * (N 7 - (V (Var 3)))


prop_keepSolutions_22 :: Property
prop_keepSolutions_22 =
  ks $ Div (N 44 + Min (V (Var 20)) (V (Var 19) * 10)) (4 - 9)

prop_keepSolutions_23 :: Property
prop_keepSolutions_23 = ks $ Div (N 0 :- N 7) (Min (N 5) (N 3))

prop_keepSolutions_24 :: Property
prop_keepSolutions_24 = ks $ Mod (-1) 12

-- *** Failed! Falsified, Falsified (after 4662 tests):                  
-- Min (Div (N 0) (N 59)) (Log2 (N 0)) :^ Div (Log2 (N 6 :+ N 5)) (Max (V g51 :* V y17) (N 14))

-- Div (N 28) (Log2 (Min (N 20) (N 12 :* N 13)) :- Max (V d22 :^ Mod (Log2 (N 4) :^ N 6) (N 6)) (N 7))


return []
runTests :: IO Bool
-- runTests = $quickCheckAll
runTests = $(QC.forAllProperties)
  $ QC.quickCheckWithResult QC.stdArgs { QC.maxSuccess = 10000 }

