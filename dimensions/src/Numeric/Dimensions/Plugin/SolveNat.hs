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

import Data.Functor.Identity
import Data.String           (IsString)
import Numeric.Natural
import Outputable            hiding ((<>))

import Numeric.Dimensions.Plugin.AtLeast
import Numeric.Dimensions.Plugin.SolveNat.Exp
import Numeric.Dimensions.Plugin.SolveNat.NormalForm



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


normalize :: (Ord v, Ord t) => Exp t v -> NormalE t v

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
  -- we can map over the first argument of the Dic but not second, because
  --  it would mess up mins, maxs and zeroes.
  =   foldr1 (map2Mins (<>))
    . fmap ( foldr1 (map2Maxs (<>))
           . fmap normDiv . getMaxsE)
    . getMinsE $ getNormalE $ normalize a
  where
    normDiv = flip normalizeDiv (getNormalE $ normalize b)

normalize (Mod a b)
  = normalizeMod (normalize a) (normalize b)

normalize (Max a b)
  = map2Maxs (<>) (normalize a) (normalize b)

normalize (Min a b)
  = map2Mins (<>) (normalize a) (normalize b)

normalize (Log2 a)
  = NormalE $ MinsE $ fmap normalizeLog2 $ getMinsE $ getNormalE $ normalize a


map2Mins :: (Ord v, Ord t)
         => (MinsE vl tl -> MinsE vr tr -> MinsE t v)
         -> NormalE vl tl -> NormalE vr tr -> NormalE t v
map2Mins k a = NormalE . k (getNormalE a) . getNormalE

map2Maxs :: (Ord v, Ord t)
         => (MaxsE vl tl -> MaxsE vr tr -> MaxsE t v)
         -> NormalE vl tl -> NormalE vr tr -> NormalE t v
map2Maxs k = map2Mins $ \a -> runIdentity . lift2Mins (\x -> pure . k x) a

map2Sums :: (Ord v, Ord t)
         => (SumsE None vl tl -> SumsE None vr tr -> SumsE None t v)
         -> NormalE vl tl -> NormalE vr tr -> NormalE t v
map2Sums k = map2Maxs $ \a -> runIdentity . lift2Maxs (\x -> pure . k x) a

lift2Maxs :: (Ord v, Ord t, Applicative m)
          => (SumsE None vl tl -> SumsE None vr tr -> m (SumsE None t v))
          -> MaxsE vl tl -> MaxsE vr tr
          -> m (MaxsE t v)
lift2Maxs f (MaxsE a)
  = fmap (MaxsE . flattenDesc) . traverse (\b -> traverse (`f` b) a) . getMaxsE

lift2Mins :: (Ord v, Ord t, Applicative m)
          => (MaxsE vl tl -> MaxsE vr tr -> m (MaxsE t v))
          -> MinsE vl tl -> MinsE vr tr
          -> m (MinsE t v)
lift2Mins f (MinsE a)
  = fmap (MinsE . flattenDesc) . traverse (\b -> traverse (`f` b) a) . getMinsE

-- | Swap Mins and Maxs and then renormalize according to the distributivity law.
--   Use this for 2nd argument of @Div@ or @(:-)@.
inverseMM :: (Ord v, Ord t) => NormalE t v -> NormalE t v
inverseMM (NormalE x) = NormalE (inverseMM' x)

inverseMM' :: (Ord v, Ord t) => MinsE t v -> MinsE t v
inverseMM' (MinsE (MaxsE xs :| L []))
    = MinsE $ (MaxsE . pure) <$> xs
inverseMM' (MinsE (maxs1 :| L (maxs2 : maxS)))
    = MinsE $ (<>) <$> a <*> b
  where
    MinsE a = inverseMM' $ MinsE $ pure maxs1
    MinsE b = inverseMM' $ MinsE $ maxs2 :| L maxS


normalizeDiv :: (Ord v, Ord t)
             => SumsE None t v -> MinsE t v -> NormalE t v
normalizeDiv a b@(MinsE bs)
  | isZero a  = minMax 0
  | isOne  b  = minMax a
    -- Note, I convert the minimum of a list of maximimums (MinsE bs) into
    -- the maximum of a list of sums, because bs is the second argument of Div,
    -- which means swapping MinsE-MaxsE
  | otherwise = foldr1 (map2Maxs (<>)) $ normalizeDiv' a <$> bs

normalizeDiv' :: (Ord v, Ord t)
              => SumsE None t v -> MaxsE t v -> NormalE t v
normalizeDiv' a b
  | (ca, SumsE (L [])) <- unconstSumsE a
  , (cb, True) <- foldr
      ( \x (cb, nothin) -> case unconstSumsE x of
                (cb', SumsE (L [])) -> (max cb cb', nothin)
                _                   -> (0, False)
      ) (0, True) $ getMaxsE b
  , cb /= 0   = minMax . fromInteger $ div ca cb
  | otherwise = unit $ UDiv a (mapSupersedeMax id b)

normalizeMod :: (Ord v, Ord t)
             => NormalE t v -> NormalE t v -> NormalE t v
normalizeMod (NormalE (MinsE (MaxsE (a :| L []) :| L [])))
             (NormalE (MinsE (MaxsE (b :| L []) :| L [])))
  | (ca, sa) <- unconstSumsE a
  , (cb, sb) <- unconstSumsE b
  , cb /= 0 && isZero sa && isZero sb = minMax $ fromInteger $ mod ca cb
normalizeMod a b
  | isZero a  = minMax 0
  | isOne  b  = minMax 0
  | otherwise = unit $ UMod (simplify a) (simplify b)

normalizeLog2 :: (Ord v, Ord t) => MaxsE t v -> MaxsE t v
normalizeLog2 p
  | (c, True) <- foldr
      ( \x (cb, nothin) -> case unconstSumsE x of
                (cb', SumsE (L [])) -> (max cb cb', nothin)
                _                   -> (0, False)
      ) (0, True) $ getMaxsE p
  , c > 0 = MaxsE . pure . fromIntegral $ log2Nat (fromInteger c)
  | otherwise = MaxsE . pure . unitAsSums $ ULog2 (mapSupersedeMax id p)


-- | Another series of simplifications at different levels of a normal expr.
simplify :: (Ord t, Ord v)
         => NormalE t v -> NormalE t v
simplify
  = NormalE
  . ( mapSupersedeMin $ mapSupersedeMax $ mapSimplifySum $ mapSimplifyProd id )
  . getNormalE

--  NB: mapped function must keep monotonicity of maxs
mapSupersedeMin :: (Ord t, Ord v)
                => (MaxsE t v -> MaxsE t v) -> MinsE t v -> MinsE t v
mapSupersedeMin k mins = MinsE (head y :| L (tail y))
  where
    x :| L xs = k <$> getMinsE mins
    y = f x xs
    allGE :: (Ord t, Ord v) => MaxsE t v -> MaxsE t v -> Bool
    allGE (MaxsE a) (MaxsE b) = all isNonNeg $ (-) <$> a <*> b
    f :: (Ord t, Ord v) => MaxsE t v -> [MaxsE t v] -> [MaxsE t v]
    f a bs = case filter (not . flip allGE a) bs of
      []  -> [a]
      bs'@(c:cs)
        | any (allGE a) bs'
          -> f c cs
        | otherwise
          -> a : f c cs

--  NB: mapped function must keep monotonicity of sums
mapSupersedeMax :: (Ord t, Ord v)
                => (SumsE None t v -> SumsE None t v) -> MaxsE t v -> MaxsE t v
mapSupersedeMax k maxs = MaxsE (head y :| L (tail y))
  where
    x :| L xs = k <$> getMaxsE maxs
    y = f x xs
    sumGE :: (Ord t, Ord v) => SumsE None t v -> SumsE None t v -> Bool
    sumGE a b = isNonNeg (a - b)
    f :: (Ord t, Ord v) => SumsE None t v -> [SumsE None t v] -> [SumsE None t v]
    f a bs = case filter (not . sumGE a) bs of
      []  -> [a]
      bs'@(c:cs)
        | any (flip sumGE a) bs'
          -> f c cs
        | otherwise
          -> a : f c cs

--  NB: mapped function must keep monotonicity of prods
mapSimplifySum :: (Ord t, Ord v)
               => (ProdE t v -> ProdE t v) -> SumsE n t v -> SumsE None t v
mapSimplifySum k
  = SumsE . L
  . filter (not . isZero . getAbs) . map (fmap k) . toList . getSumsE

--  NB: mapped function must keep monotonicity of pows
mapSimplifyProd :: (Ord t, Ord v)
                => (PowE t v -> PowE t v) -> ProdE t v -> ProdE t v
mapSimplifyProd k
  = ProdE . f . filter (not . isOne) . map k . toList . getProdE
  where
    f []     = PowU UOne 0 :| mempty
    f (x:xs) = x :| L xs



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
            let en = simplify $ normalize e :: NormalE XType Var
            in  pprTraceM "normalize: "
                 $  ppr e
                 -- $$ pprNormalize (Var . show <$> en)
                 $$ ppr en
                 $$ ("validate:" <+> ppr (validate en))
                 $$ test5 e
          ) $ exprs
  where
    x = V "x" :: Exp XType Var
    y = V "y" :: Exp XType Var
    z1 = V "z1" :: Exp XType Var
    z2 = V "z2" :: Exp XType Var
    z3 = V "z3" :: Exp XType Var
    f1 a = Log2 (Max (Div (Min a x) (Max (x - y) (Log2 a))) 2)
    f2 a = (x + y - 5) :^ (Log2 $ z1 + 4 * Div 194 (Max a y) - 1)
        +  (x + y - 5) :^ (Log2 $ z1 + 4 * Div 194 (Max a y) - 4)
        + Mod z3 (Min 4 z1) * 4
    exprs :: [Exp XType Var]
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
      , f1 x
      , f1 (x + z1)
      , f2 y
      , f2 (Div x (f1 y))
      , f2 (Log2 (Max (f1 z2) (x - 15))) + (-1) :^ f1 (15 + y - x)
      , Min (x + 2 * y) (x + 3 * y)
      , Max (x + 2 * y) (x + 3 * y)
      ]

test5 :: Exp XType Var -> SDoc
test5 e1 = case counts of
    (total, success1, success2, fail1success2, errs) ->
      vcat $
        [ "Total:   " <+> ppr total
        , "Success1:" <+> ppr success1
        , "Success2:" <+> ppr success2
        , "Relaxed: " <+> ppr fail1success2
        ] <> [hang "Errors:" 2 (vcat $ take 5 errs) | not (null errs)]
  where
    e2 = fromNormal $ simplify $ normalize e1
    n = 7
    ns = [0..n]
    evs = f <$> ns <*> ns <*> ns <*> ns <*> ns
    f x y z1 z2 z3 = ( (x, y, z1, z2, z3)
                     , evalWith x y z1 z2 z3 e1, evalWith x y z1 z2 z3 e2)
    counts = foldr g (0,0,0,0,[]) evs :: (Int, Int, Int, Int, [SDoc])
    g (args, Right r1, Right r2)
      (total, success1, success2, fail1success2, errs)
      | r1 == r2
        = (total + 1, success1 + 1, success2 + 1, fail1success2, errs)
      | otherwise
        = (total + 1, success1 + 1, success2 + 1, fail1success2
          , hsep [ "Results do not match! ", ppr r1 , "/=", ppr r2
                 , ". Arguments:", text (show args)] : errs)
    g (_, Left _, Right _)
      (total, success1, success2, fail1success2, errs)
        = (total + 1, success1, success2 + 1, fail1success2 + 1, errs)
    g (args, Right _, Left e)
      (total, success1, success2, fail1success2, errs)
        = (total + 1, success1, success2 + 1, fail1success2 + 1
          , hsep [ "Failed new!", ppr e
                 , ". Arguments:", text (show args)] : errs)
    g (_, Left _, Left _)
      (total, success1, success2, fail1success2, errs)
        = (total + 1, success1, success2, fail1success2, errs)


evalWith :: Natural -> Natural -> Natural -> Natural -> Natural
         -> Exp XType Var -> Either (Exp XType Var) Integer
evalWith x y z1 z2 z3 = evaluate . runIdentity . substituteVar s
  where
    s "x"  = pure $ N x
    s "y"  = pure $ N y
    s "z1" = pure $ N z1
    s "z2" = pure $ N z2
    s "z3" = pure $ N z3
    s v    = pure $ V v
