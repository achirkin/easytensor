{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-
Relation properties:

Reflexive: x ∘ x
    EQ, LE
Symmetric: x ∘ y ⇔ y ∘ x
    EQ, LE
Transitive: x ∘ y ⋀ y ∘ z ⇒ x ∘ z
    EQ, LE, LT, GT

Binary ops properties:

Commutative: x ∘ y == y ∘ x
  +, *, Max, Min
Associative: (x ∘ y) ∘ z == x ∘ (y ∘ z) == x ∘ y ∘ z
  +, *, Max, Min
Distributive: (f, ∘): f (x ∘ y) == f x ∘ f y
    (c*,+), (c*,-),
     ([ c*, c+, c^, c`Max`, c`Min`, Log2,
      , *c, +c, -c, ^c, `Max`c, `Min`c, `Div`c], [Max, Min]),

Other:
  a == b * Div a b + Mod a b
  a ^ (b * c) == (a ^ b) ^ c == a ^ b ^ c
  a ^ (b + c) == a^b * a^c
  x ^ 0 == 1
  0 ^ x == 0
  1 * x == x
  x * 1 == x
  x + 0 == x
  x - 0 == x
  0 + x == x
  a * a ^ b = a ^ (b + 1)
  a ^ b * a = a ^ (b + 1)
  a * a == a ^ 2
  a ^ x * b ^ x == (a * b) ^ x
  Log2 (2^x) == x
  c `Div` Max a b == Min (Div c a) (Div c b)
  c - Max a b == Min (c - a) (c - b)
  c `Div` Min a b == Max (Div c a) (Div c b)
  c - Min a b == Max (c - a) (c - b)

Show stoppers:
  Mod a 0
  Div a 0
  Log2 0

 -}
module Numeric.Dimensions.Plugin.SolveNat where

-- import           Data.Functor.Identity
import           Outputable              hiding ( (<>) )

import           Numeric.Dimensions.Plugin.SolveNat.Exp
import           Numeric.Dimensions.Plugin.SolveNat.NormalForm

data EqConstraint t v
  = CLT (Exp t v) (Exp t v)
    -- ^ @CmpNat a b ~ 'LT@
  | CEQ (Exp t v) (Exp t v)
    -- ^ @CmpNat a b ~ 'EQ@ or @a ~ b@
  | CGT (Exp t v) (Exp t v)
    -- ^ @CmpNat a b ~ 'GT@
  | CLE (Exp t v) (Exp t v)
    -- ^ @a <=? b ~ 'True@
  deriving (Eq, Ord, Show)

instance (Outputable v, Outputable t)
      => Outputable (EqConstraint t v) where
  ppr (CLT a b) = ppr a <+> "<" <+> ppr b
  ppr (CEQ a b) = ppr a <+> "~" <+> ppr b
  ppr (CGT a b) = ppr a <+> ">" <+> ppr b
  ppr (CLE a b) = ppr a <+> "<=" <+> ppr b

data SolveResult t v ct
  = Contradiction
    { solveRef  :: ct }
  | Success
    { solveRef  :: ct
    , solveDeps :: [EqConstraint t v]
    }
  deriving (Eq, Ord, Show)

instance (Outputable v, Outputable t, Outputable ct)
      => Outputable (SolveResult t v ct) where
  pprPrec p (Contradiction ct) = cparen (p > 10) $ "Contradiction" <+> pprPrec 10 ct
  pprPrec _ (Success ct ctx) = "Success" <+> braces
    ( pprWithCommas id ["solveRef =" <+> ppr ct, "solveDeps =" <+> ppr ctx])


-- | Derive all constraints that come out of the expression itself.
--   Do not simplify constraints yet.
implCts :: Exp t v -> [EqConstraint t v]
implCts (N _     ) = mempty
implCts (F _     ) = mempty
implCts (V _     ) = mempty
implCts (a   :+ b) = implCts a <> implCts b
implCts (a   :- b) = [CLE b a] <> implCts a <> implCts b
implCts (a   :* b) = implCts a <> implCts b
implCts (a   :^ b) = implCts a <> implCts b
implCts (Div a  b) = [CLE 0 b] <> implCts a <> implCts b
implCts (Mod a  b) = [CLE 0 b] <> implCts a <> implCts b
implCts (Max a  b) = implCts a <> implCts b
implCts (Min a  b) = implCts a <> implCts b
implCts (Log2 a  ) = [CGT a 0] <> implCts a


normalize :: (Ord v, Ord t) => Exp t v -> NormalE t v
normalize (N n     ) = fromIntegral n
normalize (F t     ) = toNormalE (UF t)
normalize (V v     ) = toNormalE (UV v)
normalize (a   :+ b) = normalize a + normalize b
normalize (a   :- b) = normalize a - normalize b
normalize (a   :* b) = normalize a * normalize b
normalize (a   :^ b) = nePow (normalize a) (normalize b)
normalize (Div a  b) = neDiv (normalize a) (normalize b)
normalize (Mod a  b) = neMod (normalize a) (normalize b)
normalize (Max a  b) = neMax (normalize a) (normalize b)
normalize (Min a  b) = neMin (normalize a) (normalize b)
normalize (Log2 a  ) = neLog2 (normalize a)
