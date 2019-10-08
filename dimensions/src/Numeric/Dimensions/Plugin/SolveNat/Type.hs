{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
module Numeric.Dimensions.Plugin.SolveNat.Type where

import Control.Applicative
import Control.Arrow            (second)
import Data.Bits
import Data.Constraint
import Data.Constraint.Deriving
import Data.Type.Bool           (type (||))
import Numeric.Natural
import Outputable               hiding ((<>))
import Prelude                  hiding ((^))
import Unsafe.Coerce

type Flag = Bool
type Yes  = 'True
type No  = 'False

data ExpOp
  = OpConst | OpFun | OpVar | OpPlus | OpMinus | OpTimes | OpPow
  | OpDiv | OpMod | OpMax | OpMin | OpLog2
  deriving (Eq, Ord)

type OpConst = 'OpConst
type OpFun   = 'OpFun
type OpVar   = 'OpVar
type OpPlus  = 'OpPlus
type OpMinus = 'OpMinus
type OpTimes = 'OpTimes
type OpPow   = 'OpPow
type OpDiv   = 'OpDiv
type OpMod   = 'OpMod
type OpMax   = 'OpMax
type OpMin   = 'OpMin
type OpLog2  = 'OpLog2

data ExpState
  = ExpState Flag Flag Flag Flag Flag Flag Flag Flag Flag Flag Flag Flag
type DefaultState = 'ExpState Yes Yes Yes Yes Yes Yes Yes Yes Yes Yes Yes Yes

type family Get (o :: ExpOp) (s :: ExpState) :: Flag where
  Get OpConst ('ExpState a _ _ _ _ _ _ _ _ _ _ _) = a
  Get OpFun   ('ExpState _ a _ _ _ _ _ _ _ _ _ _) = a
  Get OpVar   ('ExpState _ _ a _ _ _ _ _ _ _ _ _) = a
  Get OpPlus  ('ExpState _ _ _ a _ _ _ _ _ _ _ _) = a
  Get OpMinus ('ExpState _ _ _ _ a _ _ _ _ _ _ _) = a
  Get OpTimes ('ExpState _ _ _ _ _ a _ _ _ _ _ _) = a
  Get OpPow   ('ExpState _ _ _ _ _ _ a _ _ _ _ _) = a
  Get OpDiv   ('ExpState _ _ _ _ _ _ _ a _ _ _ _) = a
  Get OpMod   ('ExpState _ _ _ _ _ _ _ _ a _ _ _) = a
  Get OpMax   ('ExpState _ _ _ _ _ _ _ _ _ a _ _) = a
  Get OpMin   ('ExpState _ _ _ _ _ _ _ _ _ _ a _) = a
  Get OpLog2  ('ExpState _ _ _ _ _ _ _ _ _ _ _ a) = a

type family Set (f :: flag) (o :: expOp) (s :: ExpState) :: ExpState where
  Set f OpConst s = 'ExpState f (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpFun   s = 'ExpState (Get OpConst s) f (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpVar   s = 'ExpState (Get OpConst s) (Get OpFun s) f (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpPlus  s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) f
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpMinus s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    f (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpTimes s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) f (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpPow   s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) f (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpDiv   s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) f
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpMod   s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    f (Get OpMax s) (Get OpMin s) (Get OpLog2 s)
  Set f OpMax   s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) f (Get OpMin s) (Get OpLog2 s)
  Set f OpMin   s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) f (Get OpLog2 s)
  Set f OpLog2  s = 'ExpState (Get OpConst s) (Get OpFun s) (Get OpVar s) (Get OpPlus s)
                    (Get OpMinus s) (Get OpTimes s) (Get OpPow s) (Get OpDiv s)
                    (Get OpMod s) (Get OpMax s) (Get OpMin s) f

  Set ('[] :: [Flag]) ( _  :: [ExpOp]) s = s
  Set ( _  :: [Flag]) ('[] :: [ExpOp]) s = s
  Set (f ': fs :: [Flag]) (o ': os :: [ExpOp]) s = Set f o (Set fs os s)

type family Allow (ops :: expOp) (s :: ExpState) :: Constraint where
  Allow (o       :: ExpOp)   s = s ~ Set Yes o s
  Allow ('[]     :: [ExpOp]) s = ()
  Allow (x ': xs :: [ExpOp]) s = (Allow x s, Allow xs s)

type family Deny (ops :: expOp) (s :: ExpState) :: Constraint where
  Deny (o       :: ExpOp)   s = s ~ Set No o s
  Deny ('[]     :: [ExpOp]) s = ()
  Deny (x ': xs :: [ExpOp]) s = (Deny x s, Deny xs s)

type Combine a b = 'ExpState
    (Get OpConst a || Get OpConst b) (Get OpFun a || Get OpFun b)
    (Get OpVar a || Get OpVar b)     (Get OpPlus a || Get OpPlus b)
    (Get OpMinus a || Get OpMinus b) (Get OpTimes a || Get OpTimes b)
    (Get OpPow a || Get OpPow b) (Get OpDiv a || Get OpDiv b)
    (Get OpMod a || Get OpMod b) (Get OpMax a || Get OpMax b)
    (Get OpMin a || Get OpMin b) (Get OpLog2 a || Get OpLog2 b)


type AnyExp = Exp DefaultState

-- | Type-level expression of kind @Nat@
data Exp (s :: ExpState) v t where
  N :: Allow OpConst s  => Natural -> Exp s v t
    -- ^ Constant, non-negative number
  F :: Allow OpFun s    => t -> Exp s v t
    -- ^ Irreducible type family of kind Nat
  V :: Allow OpVar s    => v -> Exp s v t
    -- ^ Variable of kind Nat
  (:+) :: Allow OpPlus s  => Exp s v t -> Exp s v t -> Exp s v t
  (:-) :: Allow '[ OpMinus
                 , OpPlus -- double negation
                 ] s
       => Exp s v t -> Exp s v t -> Exp s v t
  (:*) :: Allow OpTimes s => Exp s v t -> Exp s v t -> Exp s v t
  (:^) :: Allow OpPow s   => Exp s v t -> Exp s v t -> Exp s v t
  Div  :: Allow OpDiv s   => Exp s v t -> Exp s v t -> Exp s v t
  Mod  :: ( Allow OpMod b
          , a ~ Set '[Yes, Yes] '[OpMin, OpMax] b -- non-distributive
          )
       => Exp a v t -> Exp a v t -> Exp b v t
  Max  :: Allow OpMax s   => Exp s v t -> Exp s v t -> Exp s v t
  Min  :: Allow OpMin s   => Exp s v t -> Exp s v t -> Exp s v t
  Log2 :: Allow OpLog2 s  => Exp s v t -> Exp s v t

infixl 6 :+
infixl 7 :*
infixr 8 :^
infixl 6 :-
infixl 7 `Div`
infixl 7 `Mod`

instance (Ord v, Ord t) => Eq (Exp s v t) where
  (==) a b = compare a b == EQ


instance (Ord v, Ord t) => Ord (Exp s v t) where

  -- Max and Min are the outermost constructors in the normal form
  compare (Min a1 a2) (Min b1 b2) = compare a1 b1 <> compare a2 b2
  compare (Max a1 a2) (Max b1 b2) = compare a1 b1 <> compare a2 b2
  compare (Min _ _) _ = GT
  compare _ (Min _ _) = LT
  compare (Max _ _) _ = GT
  compare _ (Max _ _) = LT

  -- comparing within the same types follows the standard Ord
  compare (N a) (N b) = compare a b
  compare (N _) (F _) = compare OpConst OpFun
  compare (N _) (V _) = compare OpConst OpVar
  compare (F _) (N _) = compare OpFun OpConst
  compare (F a) (F b) = compare a b
  compare (F _) (V _) = compare OpFun OpVar
  compare (V _) (N _) = compare OpVar OpConst
  compare (V _) (F _) = compare OpVar OpFun
  compare (V a) (V b) = compare a b
  compare  l     r    = compare (flattenE l) (flattenE r)
    where
      flattenE :: Exp s v t -> [(Exp s v t, [ExpOp])]
      flattenE a@(N _)   = [(a, [OpConst])]
      flattenE a@(F _)   = [(a, [OpFun])]
      flattenE a@(V _)   = [(a, [OpVar])]
      flattenE (x :+ y)  = second (OpPlus:) <$> flattenE x ++ flattenE y
      flattenE (x :- y)  = second (OpMinus:) <$> flattenE x ++ flattenE y
      flattenE (x :* y)  = second (OpTimes:) <$> flattenE x ++ flattenE y
      flattenE (x :^ y)  = second (OpPow:) <$> flattenE x ++ flattenE y
      flattenE (Div x y) = second (OpDiv:) <$> flattenE x ++ flattenE y
      flattenE (Mod x y) = second (OpMod:) <$> flattenE (unsafeCoerce x)
                                            ++ flattenE (unsafeCoerce y)
      flattenE (Max x y) = second (OpMax:) <$> flattenE x ++ flattenE y
      flattenE (Min x y) = second (OpMin:) <$> flattenE x ++ flattenE y
      flattenE (Log2 a)  = second (OpLog2:) <$> flattenE a



-- | @Combine a b@ is always more permissive than its parts, so you can convert
--   it for free.
--   I could honestly repack the whole structure, but we know it would be
--   just a waste of my time and the runtime cycles.
relax :: b ~ Combine a b => Exp a v t -> Exp b v t
relax = unsafeCoerce


(^) :: Allow OpPow s
    => Exp s v t -> Exp s v t -> Exp s v t
(^) = (:^)
infixr 8 ^


instance ( Allow '[OpConst, OpPlus, OpMinus, OpTimes] s)
      => Num (Exp s v t) where
  (+) = (:+)
  (-) = (:-)
  (*) = (:*)
  negate = undefined
  abs = undefined
  signum _ = N 1
  fromInteger = N . fromInteger



instance (Outputable v, Outputable t)
      => Outputable (Exp s v t) where
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


{-# ANN defineOutputable ClassDict #-}
defineOutputable :: (a -> SDoc) -> (Rational -> a -> SDoc)
                 -> Dict (Outputable a)
defineOutputable = defineOutputable

instance (Show v, Show t) => Show (Exp s v t) where
  showsPrec p e s
      | Dict <- tOutputable
      , Dict <- vOutputable
        = showSDocUnsafe (pprPrec (toRational p) e) ++ s
    where
      tOutputable :: Dict (Outputable t)
      tOutputable = defineOutputable
        (text . (show :: t -> String))
        (\k x -> text (showsPrec (round k) x ""))
      vOutputable :: Dict (Outputable v)
      vOutputable = defineOutputable
        (text . (show :: v -> String))
        (\k x -> text (showsPrec (round k) x ""))

data EqConstraint s v t where
  CLT :: Exp s v t -> Exp s v t -> EqConstraint s v t
    -- ^ @CmpNat a b ~ 'LT@
  CEQ :: Exp s v t -> Exp s v t -> EqConstraint s v t
    -- ^ @CmpNat a b ~ 'EQ@ or @a ~ b@
  CGT :: Exp s v t -> Exp s v t -> EqConstraint s v t
    -- ^ @CmpNat a b ~ 'GT@
  CLE :: Exp s v t -> Exp s v t -> EqConstraint s v t
    -- ^ @a <=? b ~ 'True@

deriving instance (Ord v, Ord t) => Eq (EqConstraint s v t)
deriving instance (Ord v, Ord t) => Ord (EqConstraint s v t)

instance (Outputable v, Outputable t)
      => Outputable (EqConstraint s v t) where
  ppr (CLT a b) = ppr a <+> "<" <+> ppr b
  ppr (CEQ a b) = ppr a <+> "~" <+> ppr b
  ppr (CGT a b) = ppr a <+> ">" <+> ppr b
  ppr (CLE a b) = ppr a <+> "<=" <+> ppr b

data SolveResult v t ct
  = Contradiction
    { solveRef  :: ct }
  | Success
    { solveRef  :: ct
    , solveDeps :: [EqConstraint DefaultState v t]
    }

instance (Outputable v, Outputable t, Outputable ct)
      => Outputable (SolveResult v t ct) where
  pprPrec p (Contradiction ct) = cparen (p > 10) $ "Contradiction" <+> pprPrec 10 ct
  pprPrec _ (Success ct ctx) = "Success" <+> braces
    ( pprWithCommas id ["solveRef =" <+> ppr ct, "solveDeps =" <+> ppr ctx])


log2Nat :: Natural -> Either SDoc Natural
log2Nat 0 = Left "Log2 0 is undefined"
log2Nat 1 = Right 0
log2Nat n = succ <$> log2Nat (shiftR n 1)


data PatPM s v t
  = PatPM Bool
          (Exp s v t -> Exp s v t -> Exp s v t)
          (Exp s v t) (Exp s v t)

-- | A handy pattern matching both plus and minus
pattern PM :: Bool
              -- ^ True if plus, False if minus
           -> (Exp s v t -> Exp s v t -> Exp s v t)
              -- ^ the operation itself, no constraint needed
           -> Exp s v t -> Exp s v t -> Exp s v t
pattern PM p k a b <- (patPM -> Just (PatPM p k a b))
  where
    PM _ k a b = k a b

patPM :: Exp s v t -> Maybe (PatPM s v t)
patPM (a :+ b) = Just (PatPM True (:+) a b)
patPM (a :- b) = Just (PatPM False (:-) a b)
patPM _        = Nothing
