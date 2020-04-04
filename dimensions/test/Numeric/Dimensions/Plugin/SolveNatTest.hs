{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
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


data TestNE = TestNE TestExp (NormalE Type Var)

pattern T :: NormalE Type Var -> TestNE
pattern T x <- TestNE _ x

instance Show TestNE where
  show (TestNE e _) = show e

instance Arbitrary TestNE where
  arbitrary = do
    expOrig <- arbitrary
    let x  = normalize expOrig
        expNorm = fromNormal x
        resOrig = evalExp expOrig
        resNorm = evalExp expNorm
    evald <- pure . inTime $ resOrig `seq` resNorm `seq` (resOrig, resNorm)
    case evald of
      Just (Right a, Right b) | a == b -> return $ TestNE expOrig x
      _ -> arbitrary

ignoreSlow :: QC.Testable p => a -> p -> Property
ignoreSlow a = (i'mStuck .||.)
  where
    i'mStuck =
      QC.classify True "was not evaluated in time" (isNothing $ inTime a)

evalN :: NormalE Type Var -> Either TestExp Integer
evalN = evalExp . fromNormal

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

-- prop_numPlusZero :: TestNE -> Property
-- prop_numPlusZero (T x) = ignoreSlow (rx0 `seq` rx1 `seq` r0x `seq` r)
--   $ and [r == rx0, r == rx1, r == r0x]
--   where
--     r   = evalN x
--     rx0 = evalN (x + 0)
--     rx1 = evalN (x - 0)
--     r0x = evalN (0 + x)

-- prop_numPlusSym :: TestNE -> TestNE -> Property
-- prop_numPlusSym (T x) (T y) = ignoreSlow (xy `seq` yx) $ xy == yx
--   where
--     xy = evalN (x + y)
--     yx = evalN (y + x)

-- prop_numTimesOne :: TestNE -> Property
-- prop_numTimesOne (T x) =
--   ignoreSlow (r1x `seq` rx1 `seq` r) $ r == r1x && r == rx1
--   where
--     r   = evalN x
--     r1x = evalN (1 * x)
--     rx1 = evalN (x * 1)

-- prop_numTimesSym :: TestNE -> TestNE -> Property
-- prop_numTimesSym (T x) (T y) = ignoreSlow (xy `seq` yx) $ xy == yx
--   where
--     xy = evalN (x * y)
--     yx = evalN (y * x)

-- prop_negateTwice :: TestNE -> Property
-- prop_negateTwice (T x) = ignoreSlow (rm `seq` rp) $ rp == rm
--   where
--     rp = evalN x
--     rm = evalN $ negate $ negate x

-- prop_negateOnce :: TestNE -> Property
-- prop_negateOnce (T x) = ignoreSlow (rm `seq` rp) (rp == fmap negate rm)
--   where
--     rp = evalN x
--     rm = evalN $ negate x

-- prop_abs :: TestNE -> Property
-- prop_abs (T x) =
--   QC.counterexample (showSDocUnsafe . ppr $ fromNormal x)
--     $ ignoreSlow (a `seq` m `seq` p)
--     $ and [a == p || a == m, a >= m, a >= p, a >= 0]
--   where
--     Right a = evalN $ abs x
--     Right p = evalN x
--     Right m = evalN $ negate x

-- prop_max :: TestNE -> TestNE -> Property
-- prop_max (T x) (T y) = ignoreSlow (rx `seq` ry `seq` rm)
--   $ and [rm >= rx, rm >= ry, rx == rm || ry == rm]
--   where
--     Right rx = evalN x
--     Right ry = evalN y
--     Right rm = evalN $ neMax x y

-- prop_min :: TestNE -> TestNE -> Property
-- prop_min (T x) (T y) = ignoreSlow (rx `seq` ry `seq` rm)
--   $ and [rm <= rx, rm <= ry, rx == rm || ry == rm]
--   where
--     Right rx = evalN x
--     Right ry = evalN y
--     Right rm = evalN $ neMin x y

-- prop_sumPN :: TestNE -> Property
-- prop_sumPN (T x) =
--   QC.counterexample (showSDocUnsafe . ppr $ fromNormal x)
--     $ ignoreSlow (rx `seq` ry) (rx QC.=== ry)
--   where
--     rx = evalN x
--     ry = evalN $ neMin x 0 + neMax x 0

-- prop_keepSolutions :: TestExp -> Property
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

prop_keepSolutions_25 :: Property
prop_keepSolutions_25 = ks $ 2 :^ Min (1 :+ V (Var 3)) (2 :+ V (Var 3))

prop_keepSolutions_26 :: Property
prop_keepSolutions_26 = ks $ 2 :^ ((Min 4 (Max 5 0) :+ V (Var 2)) :- N 3)

prop_keepSolutions_27 :: Property
prop_keepSolutions_27 = ks $ Min (Div (N 0) (N 59)) (Log2 (N 0)) :^ Div (Log2 (N 6 :+ N 5)) (Max (V (Var 51) :* V (Var 17)) (N 14))

prop_keepSolutions_28 :: Property
prop_keepSolutions_28 = ks $ Div 3 (Min (-3) 0)

prop_keepSolutions_29 :: Property
prop_keepSolutions_29 = ks $ V (Var 0) :^ (Div (N 4) (N 1 :+ V (Var 2) :* V (Var 2)) :+ Div (Mod (N 2 :+ (N 0 :+ N 3)) (N 3) :* Min (N 2) (N 3)) 5)

prop_valid :: TestNE -> Property
prop_valid (TestNE _ x) = case validate x of
  Ok           -> QC.property True
  Invalid errs -> QC.counterexample
    (showSDocUnsafe $ hang ("Expr:" <+> ppr (fromNormal x)) 2 $ vcat
      (toList errs)
    )
    False

pv :: TestExp -> Property
pv e = prop_valid $ TestNE e (normalize e)

prop_valid_1 :: Property
prop_valid_1 = pv $ Mod (Div (N 36) (V (Var 2)) :* Min (Div (N 73) (N 57)) (N 84) :- 19) (V (Var 56) :- 21) :- Mod (N 84) (V (Var 29)) :^ N 6

prop_valid_2 :: Property
prop_valid_2 = pv $ Mod 2 (V (Var 27)) :- Log2 25

prop_valid_3 :: Property
prop_valid_3 = pv $ (Min 3 (V (Var 2)) :+ 4) :^ Log2 (V (Var 1))

prop_valid_4 :: Property
prop_valid_4 = pv $ N 2 :^ V (Var 4) :- N 3

prop_valid_5 :: Property
prop_valid_5 = pv $ N 0 :^ V (Var 2)

prop_valid_6 :: Property
prop_valid_6 = pv $ (2 :+ V (Var 1)) :^ 5

prop_valid_7 :: Property
prop_valid_7 = pv $ Min (V (Var 3)) 5 :^ (Min (Mod 18 (V (Var 3))) 3 :+ 4 :+ V (Var 3))

prop_valid_8 :: Property
prop_valid_8 = pv $ Mod (1 :+ Min (V (Var 3)) 0) 2

prop_valid_9 :: Property
prop_valid_9 = pv $ Min (Min 4 9) 1 :^ Log2 (Min (V (Var 2)) 7)

prop_valid_10 :: Property
prop_valid_10 = pv $ Mod 1 (Min (N 8) (N 88 :* (N 24 :+ V (Var 34))))

prop_valid_11 :: Property
prop_valid_11 = pv $ Max (V (Var 0)) (Mod (V (Var 5)) 1 :* Max (3 :- 4 :^ V (Var 4) :* 4) 5)

prop_valid_12 :: Property
prop_valid_12 = pv $ Min (Max 6 (Mod (Min 1 (V (Var 4) :+ 4)) (Min 3 2))) (V (Var 1) :+ 1)

prop_valid_13 :: Property
prop_valid_13 = pv $ Mod 28 4 :^ (V (Var 36) :* 8 :- 1)

prop_valid_14 :: Property
prop_valid_14 = pv $ Div ((V (Var 3) :+ 1) :^ Min (-1) 1) 2


return []
runTests :: IO Bool
-- runTests = $quickCheckAll
runTests = $(QC.forAllProperties)
  $ QC.quickCheckWithResult QC.stdArgs { QC.maxSuccess = 1000 }

