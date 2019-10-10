{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}

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
module Numeric.Dimensions.Plugin.SolveNat  where

import Data.Foldable         (toList)
import Data.Functor.Identity
import Data.String           (IsString)
import Outputable            hiding ((<>))

import Numeric.Dimensions.Plugin.AtLeast
import Numeric.Dimensions.Plugin.SolveNat.Exp
import Numeric.Dimensions.Plugin.SolveNat.NormalForm



data EqConstraint v t
  = CLT (Exp v t) (Exp v t)
    -- ^ @CmpNat a b ~ 'LT@
  | CEQ (Exp v t) (Exp v t)
    -- ^ @CmpNat a b ~ 'EQ@ or @a ~ b@
  | CGT (Exp v t) (Exp v t)
    -- ^ @CmpNat a b ~ 'GT@
  | CLE (Exp v t) (Exp v t)
    -- ^ @a <=? b ~ 'True@
  deriving (Eq, Ord, Show)

instance (Outputable v, Outputable t)
      => Outputable (EqConstraint v t) where
  ppr (CLT a b) = ppr a <+> "<" <+> ppr b
  ppr (CEQ a b) = ppr a <+> "~" <+> ppr b
  ppr (CGT a b) = ppr a <+> ">" <+> ppr b
  ppr (CLE a b) = ppr a <+> "<=" <+> ppr b

data SolveResult v t ct
  = Contradiction
    { solveRef  :: ct }
  | Success
    { solveRef  :: ct
    , solveDeps :: [EqConstraint v t]
    }
  deriving (Eq, Ord, Show)

instance (Outputable v, Outputable t, Outputable ct)
      => Outputable (SolveResult v t ct) where
  pprPrec p (Contradiction ct) = cparen (p > 10) $ "Contradiction" <+> pprPrec 10 ct
  pprPrec _ (Success ct ctx) = "Success" <+> braces
    ( pprWithCommas id ["solveRef =" <+> ppr ct, "solveDeps =" <+> ppr ctx])


-- | Derive all constraints that come out of the expression itself.
--   Do not simplify constraints yet.
implCts :: Exp v t -> [EqConstraint v t]
implCts (N _)     = mempty
implCts (F _)     = mempty
implCts (V _)     = mempty
implCts (a :+ b)  = implCts a <> implCts b
implCts (a :- b)  = [CLE b a] <> implCts a <> implCts b
implCts (a :* b)  = implCts a <> implCts b
implCts (a :^ b)  = implCts a <> implCts b
implCts (Div a b) = [CLE 0 b] <> implCts a <> implCts b
implCts (Mod a b) = [CLE 0 b] <> implCts a <> implCts b
implCts (Max a b) = implCts a <> implCts b
implCts (Min a b) = implCts a <> implCts b
implCts (Log2 a)  = [CGT a 0] <> implCts a


normalize :: (Ord v, Ord t) => Exp v t -> NormalE v t

normalize (N n)
  = minMax $ fromIntegral n
normalize (F t)
  = unit (UF t)
normalize (V v)
  = unit (UV v)

normalize (a :+ b)
  = map2Sums (+) (normalize a) (normalize b)

normalize (a :- b)
  = map2Sums (-) (normalize a) (inverseMM $ normalize b)

normalize (a :* b)
  = map2Sums (*) (normalize a) (normalize b)

normalize (a :^ b)
  = map2Sums powSums (normalize a) (normalize b)

normalize (Div a b)
  = map2Sums normalizeDiv (normalize a) (inverseMM $ normalize b)

normalize (Mod a b)
  = normalizeMod (normalize a) (normalize b)

normalize (Max a b)
  = map2Maxs (<>) (normalize a) (normalize b)

normalize (Min a b)
  = map2Mins (<>) (normalize a) (normalize b)

normalize (Log2 a)
  = NormalE $ MinsE
            $ fmap (MaxsE . fmap normalizeLog2 . getMaxsE)
            $ getMinsE $ getNormalE $ normalize a


powSums :: (Ord v, Ord t)
        => SumsE None v t -> SumsE None v t -> SumsE None v t
powSums (SumsE (L [])) _ = 0
powSums _ (SumsE (L [])) = 1
powSums (SumsE (L [a])) b
  | isOne b   = SumsE (L [a])
  | otherwise = powProd a b
powSums (SumsE (L (a1:a2:as))) (SumsE (L (b:bs)))
  = singlePowAsSums $ PowS (SumsE $ a1 :| a2 :| L as)
                           (SumsE $ b  :| L bs)

powProd :: (Ord v, Ord t)
        => Signed (ProdE v t) -> SumsE None v t -> SumsE None v t
powProd a b = sig $ go $ getProdE $ getAbs a
  where
    sig x
      | isPositive a = x
      | otherwise = x - 2 * x * unitAsSums (UMod (minMax b) (minMax 2))
    powPow (PowU u p) = singlePowAsSums $ PowU u (p * b)
    powPow (PowS s p) = case (SumsE $ L $ toList $ getSumsE p) * b of
      SumsE (L []) -> 0
      p'@(SumsE (L (x:xs)))
        | isOne p'      -> SumsE $ L $ toList $ getSumsE s
        | otherwise     -> singlePowAsSums
                         $ PowS s (SumsE $ x :| L xs)
    go (x  :| L [])      = powPow x
    go (x1 :| L (x2:xs)) = powPow x1 * go (x2 :| L xs)


map2Mins :: (Ord v, Ord t)
         => (MinsE vl tl -> MinsE vr tr -> MinsE v t)
         -> NormalE vl tl -> NormalE vr tr -> NormalE v t
map2Mins k a = NormalE . k (getNormalE a) . getNormalE

map2Maxs :: (Ord v, Ord t)
         => (MaxsE vl tl -> MaxsE vr tr -> MaxsE v t)
         -> NormalE vl tl -> NormalE vr tr -> NormalE v t
map2Maxs k = map2Mins $ \a -> runIdentity . lift2Mins (\x -> pure . k x) a

map2Sums :: (Ord v, Ord t)
         => (SumsE None vl tl -> SumsE None vr tr -> SumsE None v t)
         -> NormalE vl tl -> NormalE vr tr -> NormalE v t
map2Sums k = map2Maxs $ \a -> runIdentity . lift2Maxs (\x -> pure . k x) a

lift2Maxs :: (Ord v, Ord t, Applicative m)
          => (SumsE None vl tl -> SumsE None vr tr -> m (SumsE None v t))
          -> MaxsE vl tl -> MaxsE vr tr
          -> m (MaxsE v t)
lift2Maxs f (MaxsE a)
  = fmap (MaxsE . flattenDesc) . traverse (\b -> traverse (`f` b) a) . getMaxsE

lift2Mins :: (Ord v, Ord t, Applicative m)
          => (MaxsE vl tl -> MaxsE vr tr -> m (MaxsE v t))
          -> MinsE vl tl -> MinsE vr tr
          -> m (MinsE v t)
lift2Mins f (MinsE a)
  = fmap (MinsE . flattenDesc) . traverse (\b -> traverse (`f` b) a) . getMinsE

-- | Swap Mins and Maxs and then renormalize according to the distributivity law.
--   Use this for 2nd argument of @Div@ or @(:-)@.
inverseMM :: (Ord v, Ord t) => NormalE v t -> NormalE v t
inverseMM (NormalE x) = NormalE (inverseMM' x)

inverseMM' :: (Ord v, Ord t) => MinsE v t -> MinsE v t
inverseMM' (MinsE (MaxsE xs :| L []))
    = MinsE $ (MaxsE . pure) <$> xs
inverseMM' (MinsE (maxs1 :| L (maxs2 : maxS)))
    = MinsE $ (<>) <$> a <*> b
  where
    MinsE a = inverseMM' $ MinsE $ pure maxs1
    MinsE b = inverseMM' $ MinsE $ maxs2 :| L maxS


normalizeDiv :: (Ord v, Ord t)
             => SumsE None v t -> SumsE None v t -> SumsE None v t
normalizeDiv a b
  | (ca, SumsE (L [])) <- unconstSumsE a
  , (cb, SumsE (L [])) <- unconstSumsE b
  , cb /= 0   = fromInteger $ div ca cb
  | isZero a  = zero
  | isOne  b  = a
  | otherwise = unitAsSums $ UDiv a b

normalizeMod :: (Ord v, Ord t)
             => NormalE v t -> NormalE v t -> NormalE v t
normalizeMod (NormalE (MinsE (MaxsE (a :| L []) :| L [])))
             (NormalE (MinsE (MaxsE (b :| L []) :| L [])))
  | (ca, SumsE (L [])) <- unconstSumsE a
  , (cb, SumsE (L [])) <- unconstSumsE b
  , cb /= 0      = minMax $ fromInteger $ mod ca cb
normalizeMod a b
  | isZero a  = minMax zero
  | isOne  b  = minMax zero
  | otherwise = unit $ UMod a b

normalizeLog2 :: (Ord v, Ord t) => SumsE None v t -> SumsE None v t
normalizeLog2 p
  | (c, SumsE (L [])) <- unconstSumsE p
  , c > 0 = fromIntegral $ log2Nat (fromInteger c)
  | otherwise = unitAsSums $ ULog2 p



newtype Var = Var String
  deriving (Eq, Ord, IsString)

instance Show Var where
  show (Var x) = x

instance Outputable Var where
  ppr (Var x) = text x

newtype XType = XType String
  deriving (Eq, Ord, IsString)

instance Show XType where
  show (XType x) = x

instance Outputable XType where
  ppr (XType x) = text x

runSolveNat :: IO ()
runSolveNat = do
    putStrLn "Hello!"
    mapM_ (\e -> pprTraceM "implCts:   " $
                 ppr e $$ vcat (ppr <$> implCts e)) $ exprs
    mapM_ (\e ->
            let en = normalize e :: NormalE Var XType
            in  pprTraceM "normalize: "
                 $  ppr e
                 -- $$ pprNormalize (Var . show <$> en)
                 $$ ppr en
                 $$ ("validate: " <+> ppr (validate en))
          ) $ exprs
  where
    x = V "x" :: Exp Var XType
    y = V "y" :: Exp Var XType
    z1 = V "z1" :: Exp Var XType
    z2 = V "z2" :: Exp Var XType
    z3 = V "z3" :: Exp Var XType
    exprs :: [Exp Var XType]
    exprs =
      [ Log2 (Max 4 (Min (Div y x) (Mod (Max z1 3) 7)))
      , x + y + Min (Log2 z1) (Max 5 y)
      , x + y - Min (Log2 z1) (Max 5 y)
      , x * y + z1
      , x * ((y + z2) :^ z3) :^ z2 :^ 2
      , (z2 + z3 + (x * y - 7 * x)) :^ (z1 + 2)
      , F "Foo x z1 k" * x + 2 - y
      , 4 * 2 + 2 - 3:^2 + 19
      , 4 * 1 + 2 - 3:^2 + 19
      , 4 * 1 + 2 - (3:^2 + 1) + 19
      , 4 * 1 + (2 - 3:^2) + 19
      , 19 + (4 * 1 + 2 - 3:^2)
      , 4 * 1 + 2 - Log2 (3:^2) + 19
      , x + 3 + 3 + 5 - 3 + y * z1 + Max y (100 - x - 8 + z1 * 2)
          + Min (2 + Log2 y) (3 + Log2 (2 * x + Mod (5 + Min x 3) 7))
          + Div (9 - Max 6 3 + 2 * 2 :^ 3) (Log2 18 :^ 3)
      , x + 3 + 3 + 5 - 3 + y * z1 + Max y (100 - x - 8 + z1 * 2)
          + Min (2 + Log2 y) (3 - Log2 (2 * x + Mod (5 + Min x 3) 7))
          + Div (9 - Max 6 3 + 2 * 2 :^ 3) (Log2 18 :^ 3)
      , (x + y + 4) * (x - z1) + Max (x * (y + 2)) (y * (z1 + x)) - (x - z1) :^ 3
        + y :^ (z1 + 2)
      ]
