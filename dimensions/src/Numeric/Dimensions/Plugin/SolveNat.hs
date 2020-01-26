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

import           Numeric.Dimensions.Plugin.AtLeast
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
normalize (N n   ) = fromIntegral n
normalize (F t   ) = toNormalE (UF t)
normalize (V v   ) = toNormalE (UV v)
normalize (a :+ b) = normalize a + normalize b
normalize (a :- b) = normalize a - normalize b
normalize (a :* b) = normalize a * normalize b
normalize (a :^ b) = nePow (normalize a) (normalize b)
normalize (Div a b) =
  foldr1 (map2Mins (<>))
    . fmap (foldr1 (map2Maxs (<>)) . fmap normDiv . getMaxsE)
    . getMinsE
    $ getNormalE
    $ normalize a
  -- we can map over the first argument of the Div but not second, because
  --  it would mess up mins, maxs and zeroes.
  where normDiv = flip normalizeDiv (getNormalE $ normalize b)
normalize (Mod a b) = normalizeMod (normalize a) (normalize b)
normalize (Max a b) = neMax (normalize a) (normalize b)
normalize (Min a b) = neMin (normalize a) (normalize b)
normalize (Log2 a) =
  NormalE $ minsE $ fmap normalizeLog2 $ getMinsE $ getNormalE $ normalize a


lift2Maxs
  :: (Ord v, Ord t, Applicative m)
  => (SumsE None vl tl -> SumsE None vr tr -> m (SumsE None t v))
  -> MaxsE vl tl
  -> MaxsE vr tr
  -> m (MaxsE t v)
lift2Maxs f (MaxsE _ a) =
  fmap (maxsE . flattenDesc) . traverse (\b -> traverse (`f` b) a) . getMaxsE

lift2Mins
  :: (Ord v, Ord t, Applicative m)
  => (MaxsE vl tl -> MaxsE vr tr -> m (MaxsE t v))
  -> MinsE vl tl
  -> MinsE vr tr
  -> m (MinsE t v)
lift2Mins f (MinsE _ a) =
  fmap (minsE . flattenDesc) . traverse (\b -> traverse (`f` b) a) . getMinsE

normalizeDiv :: (Ord v, Ord t) => SumsE None t v -> MinsE t v -> NormalE t v
normalizeDiv a b@(MinsE _ bs)
  | isZero a  = 0
  | isOne b   = toNormalE a
  |
    -- Note, I convert the minimum of a list of maximimums (MinsE bs) into
    -- the maximum of a list of sums, because bs is the second argument of Div,
    -- which means swapping MinsE-MaxsE
    otherwise = foldr1 (map2Maxs (<>)) $ normalizeDiv' a <$> bs

normalizeDiv' :: (Ord v, Ord t) => SumsE None t v -> MaxsE t v -> NormalE t v
normalizeDiv' a b
  | (ca, SumsE _ (L [])) <- unconstSumsE a
  , (cb, True) <-
    foldr
        (\x (cb, nothin) -> case unconstSumsE x of
          (cb', SumsE _ (L [])) -> (max cb cb', nothin)
          _                     -> (0, False)
        )
        (0, True)
      $ getMaxsE b
  , cb /= 0
  = fromInteger $ div ca cb
  | otherwise
  = toNormalE $ UDiv a (mapSupersedeMax id b)

normalizeMod :: (Ord v, Ord t) => NormalE t v -> NormalE t v -> NormalE t v
normalizeMod (NormalE (MinsE _ (MaxsE _ (a :| L []) :| L []))) (NormalE (MinsE _ (MaxsE _ (b :| L []) :| L [])))
  | (ca, sa) <- unconstSumsE a
  , (cb, sb) <- unconstSumsE b
  , cb /= 0 && isZero sa && isZero sb
  = fromInteger $ mod ca cb
normalizeMod a b | isZero a  = 0
                 | isOne b   = 0
                 | otherwise = toNormalE $ UMod (simplify a) (simplify b)

normalizeLog2 :: (Ord v, Ord t) => MaxsE t v -> MaxsE t v
normalizeLog2 p
  | (c, True) <-
    foldr
        (\x (cb, nothin) -> case unconstSumsE x of
          (cb', SumsE _ (L [])) -> (max cb cb', nothin)
          _                     -> (0, False)
        )
        (0, True)
      $ getMaxsE p
  , c > 0
  = maxsE . pure . fromIntegral $ log2Nat (fromInteger c)
  | otherwise
  = maxsE . pure . unitAsSums $ ULog2 (mapSupersedeMax id p)


-- | Another series of simplifications at different levels of a normal expr.
simplify :: (Ord t, Ord v) => NormalE t v -> NormalE t v
simplify =
  NormalE
    . (mapSupersedeMin $ mapSupersedeMax $ regroupSumsE . mapSimplifySum
        (mapSimplifyProd id)
      )
    . getNormalE

--  NB: mapped function must keep monotonicity of maxs
mapSupersedeMin
  :: (Ord t, Ord v) => (MaxsE t v -> MaxsE t v) -> MinsE t v -> MinsE t v
mapSupersedeMin k mins = minsE (head y :| L (tail y))
  where
    x :| L xs = k <$> getMinsE mins
    y         = f x xs
    allGE :: (Ord t, Ord v) => MaxsE t v -> MaxsE t v -> Bool
    allGE (MaxsE _ a) (MaxsE _ b) = all isNonNeg $ (-) <$> a <*> b
    f :: (Ord t, Ord v) => MaxsE t v -> [MaxsE t v] -> [MaxsE t v]
    f a bs = case filter (not . flip allGE a) bs of
      [] -> [a]
      bs'@(c : cs) | any (allGE a) bs' -> f c cs
                   | otherwise         -> a : f c cs

--  NB: mapped function must keep monotonicity of sums
mapSupersedeMax
  :: (Ord t, Ord v)
  => (SumsE None t v -> SumsE None t v)
  -> MaxsE t v
  -> MaxsE t v
mapSupersedeMax k maxs = maxsE (head y :| L (tail y))
  where
    x :| L xs = k <$> getMaxsE maxs
    y         = f x xs
    sumGE :: (Ord t, Ord v) => SumsE None t v -> SumsE None t v -> Bool
    sumGE a b = isNonNeg (a - b)
    f
      :: (Ord t, Ord v)
      => SumsE None t v
      -> [SumsE None t v]
      -> [SumsE None t v]
    f a bs = case filter (not . sumGE a) bs of
      [] -> [a]
      bs'@(c : cs) | any (flip sumGE a) bs' -> f c cs
                   | otherwise              -> a : f c cs

--  NB: mapped function must keep monotonicity of prods
mapSimplifySum :: (ProdE t v -> ProdE t v) -> SumsE n t v -> SumsE None t v
mapSimplifySum k =
  sumsE . L . filter (not . isZero . getAbs) . map (fmap k) . toList . getSumsE

--  NB: mapped function must keep monotonicity of pows
mapSimplifyProd
  :: (Ord t, Ord v) => (PowE t v -> PowE t v) -> ProdE t v -> ProdE t v
mapSimplifyProd k =
  prodE . f . filter (not . isOne) . map k . toList . getProdE
  where
    f :: (Ord t, Ord v) => [PowE t v] -> AtLeast One (PowE t v)
    f []       = powU UOne 0 :| mempty
    f (x : xs) = x :| L xs
