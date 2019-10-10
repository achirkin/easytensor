{-# LANGUAGE OverloadedStrings #-}
module Numeric.Dimensions.Plugin.SolveNat.Exp
  (Exp (..), log2Nat) where

import Control.Exception (ArithException (..), throw)
import Data.Bits
import Numeric.Natural
import Outputable        hiding ((<>))


log2Nat :: Natural -> Natural
log2Nat 0 = throw Underflow
log2Nat 1 = 0
log2Nat n = succ . log2Nat $ shiftR n 1

-- | Type-level expression of kind @Nat@
data Exp v t
  = N Natural
    -- ^ Constant, non-negative number
  | F t
    -- ^ Irreducible type family of kind Nat
  | V v
    -- ^ Variable of kind Nat
  | Exp v t :+ Exp v t
  | Exp v t :- Exp v t
  | Exp v t :* Exp v t
  | Exp v t :^ Exp v t
  | Div (Exp v t) (Exp v t)
  | Mod (Exp v t) (Exp v t)
  | Max (Exp v t) (Exp v t)
  | Min (Exp v t) (Exp v t)
  | Log2 (Exp v t)
  deriving (Eq, Ord, Show)

infixl 6 :+
infixl 7 :*
infixr 8 :^
infixl 6 :-
infixl 7 `Div`
infixl 7 `Mod`


instance Num (Exp v t) where
  (+) = (:+)
  (-) = (:-)
  (*) = (:*)
  negate = (N 0 :-)
  abs (N n)      = (N n)
  abs (N 0 :- e) = e
  abs (Log2 e)   = (Log2 e)
  abs e          = e * signum e
  signum (N n) = N (signum n)
  signum _     = error "Tried to take signum of a complex expression"
  fromInteger = N . fromInteger


instance (Outputable v, Outputable t) => Outputable (Exp v t) where
  pprPrec _ (N n) = pprPrec 10 (toInteger n)
  pprPrec p (F n) = pprPrec p n
  pprPrec p (V v) = pprPrec p v

  pprPrec p (a :+ b) = cparen (p > 6) $ pprPrec 6 a <+> "+" <+> pprPrec 6 b
  pprPrec p (a :- b) = cparen (p > 6) $ pprPrec 6 a <+> "-" <+> pprPrec 6.1 b
  pprPrec p (a :* b) = cparen (p > 7) $ pprPrec 7 a <+> "*" <+> pprPrec 7 b
  pprPrec p (a :^ b) = cparen (p > 8) $ pprPrec 8.1 a <+> "^" <+> pprPrec 8 b
  pprPrec p (Div a b)  = cparen (p > 10) $ "Div"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Mod a b)  = cparen (p > 10) $ "Mod"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Log2 v)   = cparen (p > 10) $ "Log2" <+> pprPrec 11 v
  pprPrec p (Max a b)  = cparen (p > 10) $ "Max"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Min a b)  = cparen (p > 10) $ "Min"  <+> pprPrec 11 a <+> pprPrec 11 b
