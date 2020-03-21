{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-monomorphism-restriction #-}
module Numeric.Dimensions.Plugin.SolveNat.ExpStateTest where


import           Data.Ratio
import           Test.QuickCheck                ( Property
                                                , Arbitrary(..)
                                                )
import qualified Test.QuickCheck               as QC

import           Numeric.Dimensions.Plugin.SolveNat.Exp
import           Numeric.Dimensions.Plugin.SolveNat.ExpState

{-# ANN module ("HLint: ignore Evaluate" :: String) #-}


unknownState :: ExpState
unknownState = ExpState False False False False False False False False False

newtype R = R Rational
newtype SmallR = SmallR Rational

instance Show R where show (R x) = show x
instance Show SmallR where show (SmallR x) = show x

instance Arbitrary R where
  arbitrary = R <$> QC.oneof
      [ pure 0, pure 1, pure (negate 1), pure 2, pure (negate 2), pure 3, pure (1 / 2), pure (negate $ 1 / 2)
      , arbitrary, arbitrary, arbitrary, arbitrary, arbitrary
      , fromInteger <$> arbitrary, fromInteger <$> arbitrary, fromInteger <$> arbitrary]
  shrink (R x) = R <$> shrink x

instance Arbitrary SmallR where
  arbitrary = do
    R x <- arbitrary
    let n = numerator x
        d = denominator x
    return . SmallR $ (mod n 50 * signum n) % max 1 (mod d 50)
  shrink (SmallR x) = SmallR <$> shrink x


prop_esMinus :: R -> R -> Property
prop_esMinus (R a) (R b) = actual `esCmp` ideal
  { _isZero    = _isZero ideal && _isZero actual
  , _isNonZero = _isNonZero ideal && _isNonZero actual
  , _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isNonNeg  = _isNonNeg ideal && _isNonNeg actual
  , _isNonPos  = _isNonPos ideal && _isNonPos actual
  , _isEven    = _isEven ideal && _isEven actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isWhole   = _isWhole ideal && _isWhole actual
  }
  where
    actual = fromRational a - fromRational b
    ideal  = fromRational $ a - b

prop_esPlus :: R -> R -> Property
prop_esPlus (R a) (R b) = actual `esCmp` ideal
  { _isZero    = _isZero ideal && _isZero actual
  , _isNonZero = _isNonZero ideal && _isNonZero actual
  , _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isNonNeg  = _isNonNeg ideal && _isNonNeg actual
  , _isNonPos  = _isNonPos ideal && _isNonPos actual
  , _isEven    = _isEven ideal && _isEven actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isWhole   = _isWhole ideal && _isWhole actual
  }
  where
    actual = fromRational a + fromRational b
    ideal  = fromRational $ a + b

prop_esTimes :: R -> R -> Property
prop_esTimes (R a) (R b) = actual `esCmp` ideal
  { _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isEven    = _isEven ideal && _isEven actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isWhole   = _isWhole ideal && _isWhole actual
  }
  where
    actual = fromRational a * fromRational b
    ideal  = fromRational $ a * b

prop_esNegate :: R -> Property
prop_esNegate (R a) = actual `esCmp` ideal
  where
    actual = negate $ fromRational a
    ideal  = fromRational $ negate a

prop_esAbs :: R -> Property
prop_esAbs (R a) = actual `esCmp` ideal
  where
    actual = abs $ fromRational a
    ideal  = fromRational $ abs a

prop_esSignum :: R -> Property
prop_esSignum (R a) = actual `esCmp` ideal
  where
    actual = signum $ fromRational a
    ideal  = fromRational $ signum a

prop_esRecip :: R -> Property
prop_esRecip (R a) = actual `esCmp` ideal
  { _isEven  = _isEven ideal && _isEven actual
  , _isOdd   = _isOdd ideal && _isOdd actual
  , _isWhole = _isWhole ideal && _isWhole actual
  }
  where
    actual = recip $ fromRational a
    ideal  = if a == 0 then unknownState else fromRational $ recip a

prop_esMax :: R -> R -> Property
prop_esMax (R a) (R b) = actual `esCmp` ideal
  { _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isEven    = _isEven ideal && _isEven actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isWhole   = _isWhole ideal && _isWhole actual
  }
  where
    actual = esMax (fromRational a) (fromRational b)
    ideal  = fromRational (max a b)

prop_esMin :: R -> R -> Property
prop_esMin (R a) (R b) = actual `esCmp` ideal
  { _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isEven    = _isEven ideal && _isEven actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isWhole   = _isWhole ideal && _isWhole actual
  }
  where
    actual = esMin (fromRational a) (fromRational b)
    ideal  = fromRational (min a b)

prop_esPow :: R -> SmallR -> Property
prop_esPow (R a) (SmallR b) = actual `esCmp` ideal
  { _isSignOne  = (_isSignOne ideal || _isSignOne ideal') && _isComplete actual
  , _isNonZero  = _isNonZero ideal || _isNonZero ideal'
  , _isNonNeg   = _isNonNeg ideal || _isNonNeg ideal'
  , _isNonPos   = (_isNonPos ideal || _isNonPos ideal') && _isComplete actual
  , _isOdd      = (_isOdd ideal || _isOdd ideal') && _isOdd actual
  , _isEven     = (_isEven ideal || _isEven ideal') && _isEven actual
  , _isWhole    = _isWhole ideal && _isWhole actual
  , _isComplete = (_isComplete ideal || _isComplete ideal')
                    && _isComplete actual
  }
  where
    actua' = esPow (fromRational a) (fromRational b)
    actual = actua'
      { _isNonNeg = _isNonNeg actua' && (_isComplete actua' || _isNonNeg ideal')
      , _isNonPos = _isNonPos actua' && (_isComplete actua' || _isNonPos ideal')
      }
    ideal  = maybe unknownState fromRational (powR a b)
    ideal' = maybe unknownState fromRational (powR a $ numerator b % 1)

prop_esDiv :: R -> R -> Property
prop_esDiv (R a) (R b) = actual `esCmp` ideal
  { _isZero    = _isZero ideal && _isZero actual
  , _isNonZero = _isNonZero ideal && _isNonZero actual
  , _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isNonNeg  = _isNonNeg ideal && _isNonNeg actual
  , _isNonPos  = _isNonPos ideal && _isNonPos actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isEven    = _isEven ideal && _isEven actual
  }
  where
    actual = esDiv (fromRational a) (fromRational b)
    ideal  = maybe unknownState fromInteger (divR a b)

prop_esMod :: R -> R -> Property
prop_esMod (R a) (R b) = actual `esCmp` ideal
  { _isZero    = _isZero ideal && _isZero actual
  , _isNonZero = _isNonZero ideal && _isNonZero actual
  , _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isNonNeg  = _isNonNeg ideal && _isNonNeg actual
  , _isNonPos  = _isNonPos ideal && _isNonPos actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isEven    = _isEven ideal && _isEven actual
  , _isWhole   = _isWhole ideal && _isWhole actual
  }
  where
    actual = esMod (fromRational a) (fromRational b)
    ideal  = maybe unknownState fromRational (modR a b)


prop_esLog2 :: R -> Property
prop_esLog2 (R a) = actual `esCmp` ideal
  { _isZero    = _isZero ideal && _isZero actual
  , _isNonZero = _isNonZero ideal && _isNonZero actual
  , _isSignOne = _isSignOne ideal && _isSignOne actual
  , _isNonNeg  = _isNonNeg ideal && _isNonNeg actual
  , _isNonPos  = _isNonPos ideal && _isNonPos actual
  , _isOdd     = _isOdd ideal && _isOdd actual
  , _isEven    = _isEven ideal && _isEven actual
  }
  where
    actual = esLog2 (fromRational a)
    ideal  = maybe unknownState fromInteger (log2R a)


esCmp :: ExpState -> ExpState -> Property
esCmp a b = QC.counterexample "ExpStates are not equal:" $ QC.conjoin
  [ QC.counterexample
    ("  isZero: " ++ show (_isZero a) ++ " " ++ show (_isZero b))
    (_isZero a == _isZero b)
  , QC.counterexample
    ("  isNonZero: " ++ show (_isNonZero a) ++ " " ++ show (_isNonZero b))
    (_isNonZero a == _isNonZero b)
  , QC.counterexample
    ("  isSignOne: " ++ show (_isSignOne a) ++ " " ++ show (_isSignOne b))
    (_isSignOne a == _isSignOne b)
  , QC.counterexample
    ("  isNonNeg: " ++ show (_isNonNeg a) ++ " " ++ show (_isNonNeg b))
    (_isNonNeg a == _isNonNeg b)
  , QC.counterexample
    ("  isNonPos: " ++ show (_isNonPos a) ++ " " ++ show (_isNonPos b))
    (_isNonPos a == _isNonPos b)
  , QC.counterexample
    ("  isEven: " ++ show (_isEven a) ++ " " ++ show (_isEven b))
    (_isEven a == _isEven b)
  , QC.counterexample
    ("  isOdd: " ++ show (_isOdd a) ++ " " ++ show (_isOdd b))
    (_isOdd a == _isOdd b)
  , QC.counterexample
    ("  isWhole: " ++ show (_isWhole a) ++ " " ++ show (_isWhole b))
    (_isWhole a == _isWhole b)
  , QC.counterexample
    ("  isComplete: " ++ show (_isComplete a) ++ " " ++ show (_isComplete b))
    (_isComplete a == _isComplete b)
  ]


return []
runTests :: IO Bool
-- runTests = $quickCheckAll
runTests = $(QC.forAllProperties)
  $ QC.quickCheckWithResult QC.stdArgs { QC.maxSuccess = 10000 }

