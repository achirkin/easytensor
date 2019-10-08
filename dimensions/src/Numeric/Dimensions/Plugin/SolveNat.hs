{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}


{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

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
module Numeric.Dimensions.Plugin.SolveNat (normalize, solve, runSolveNat) where


import Control.Applicative
import Control.Arrow       (second)
import Control.Monad
import Control.Monad.Fail
import Data.Functor
-- import Data.Functor.Identity
import           Data.String     (IsString)
import           Numeric.Natural
import           Outputable      hiding ((<>))
import           Prelude         hiding ((^))
import qualified Prelude


import Numeric.Dimensions.Plugin.SolveNat.Type

-- -- | Test if the expression does not contains any variables or unevaluated
-- --   unknown type families.
-- --   If so, return the expression with a restricted state.
-- testIfConst :: Exp s v t
--             -> Maybe (Exp (Set '[Yes,No,No] '[OpConst,OpFun,OpVar] s) v t)
-- testIfConst (N n)     = Just (N n)
-- testIfConst (F _)     = Nothing
-- testIfConst (V _)     = Nothing
-- testIfConst (a :+ b)  = (:+) <$> testIfConst a <*> testIfConst b
-- testIfConst (a :- b)  = (:-) <$> testIfConst a <*> testIfConst b
-- testIfConst (a :* b)  = (:*) <$> testIfConst a <*> testIfConst b
-- testIfConst (a :^ b)  = (:^) <$> testIfConst a <*> testIfConst b
-- testIfConst (Div a b) = Div <$> testIfConst a <*> testIfConst b
-- testIfConst (Mod a b) = Mod <$> testIfConst a <*> testIfConst b
-- testIfConst (Max a b) = Max <$> testIfConst a <*> testIfConst b
-- testIfConst (Min a b) = Min <$> testIfConst a <*> testIfConst b
-- testIfConst (Log2 a)  = Log2 <$> testIfConst a


data Normalize e
  = Insoluble SDoc
    -- ^ Error message
  | Normalized Rerun e
    -- ^ Whether need to rerun current step and the result.

data Rerun = IAmFine | RerunMe | RerunInner

instance Semigroup Rerun where
  IAmFine <> x = x
  x <> IAmFine = x
  RerunMe <> _ = RerunMe
  _ <> RerunMe = RerunMe
  RerunInner <> RerunInner = RerunInner

instance Monoid Rerun where
  mempty = IAmFine

instance Functor Normalize where
  fmap f (Normalized b e) = Normalized b (f e)
  fmap _ (Insoluble err)  = Insoluble err

instance Applicative Normalize where
  pure = Normalized mempty
  Insoluble err <*> _ = Insoluble err
  _ <*> Insoluble err = Insoluble err
  Normalized b1 f <*> Normalized b2 x = Normalized (b1 <> b2) (f x)

instance Monad Normalize where
  Insoluble err >>= _ = Insoluble err
  Normalized b1 x >>= k = case k x of
    Insoluble err   -> Insoluble err
    Normalized b2 y -> Normalized (b1 <> b2) y

instance MonadFail Normalize where
  fail = insol . text

insol :: SDoc -> Normalize e
insol = Insoluble

needRerun :: Normalize ()
needRerun = Normalized RerunMe ()

inner :: Normalize a -> Normalize a
inner (Normalized RerunMe a) = Normalized RerunInner a
inner x                      = x

rerun :: (a -> Normalize a) -> a -> Normalize a
rerun k x = case k x of
  Normalized RerunInner y -> rerun k y
  my                      -> my

pprNormalize :: Outputable e => Normalize e -> SDoc
pprNormalize (Insoluble err)  = "Error:   " <+> err
pprNormalize (Normalized _ e) = "Success: " <+> ppr e


-- | Derive all constraints that come out of the expression itself.
--   Do not simplify constraints yet.
implCts :: Allow '[OpConst, OpMin, OpMax] s
        => Exp s v t -> [EqConstraint s v t]
implCts (N _)     = mempty
implCts (F _)     = mempty
implCts (V _)     = mempty
implCts (a :+ b)  = implCts a <> implCts b
implCts (a :- b)  = [CLE b a] <> implCts a <> implCts b
implCts (a :* b)  = implCts a <> implCts b
implCts (a :^ b)  = implCts a <> implCts b
implCts (Div a b) = [CLE (N 0) b] <> implCts a <> implCts b
implCts (Mod a b) = [CLE (N 0) b] <> implCts a <> implCts b
implCts (Max a b) = implCts a <> implCts b
implCts (Min a b) = implCts a <> implCts b
implCts (Log2 a)  = [CGT a (N 0)] <> implCts a


-- -- | If an expression does not contain any variables or unknown type families,
-- --   we can compute it to a Nat.
-- computeE :: ( Outputable v, Outputable t
--             , Deny '[OpFun, OpVar] s)
--          => Exp s v t -> Either SDoc Natural
-- computeE (N n)     = Right n
-- computeE (a :+ b)  = (+) <$> computeE a <*> computeE b
-- computeE (a :- b)  = do
--   x <- computeE a
--   y <- computeE b
--   if x >= y then Right (x - y)
--             else Left  ("Negative Nat:" <+> text (show x) <+> "-"  <+> text (show y))
-- computeE (a :* b)  = (*) <$> computeE a <*> computeE b
-- computeE (a :^ b)  = (Prelude.^) <$> computeE a <*> computeE b
-- computeE (Div a b) = div <$> computeE a <*> computeE b
-- computeE (Mod a b) = mod <$> computeE a <*> computeE b
-- computeE (Max a b) = max <$> computeE a <*> computeE b
-- computeE (Min a b) = min <$> computeE a <*> computeE b
-- computeE (Log2 a)  = computeE a >>= log2Nat


-- -- | Expand the expression into the normal form. The order of terms, from
-- --   outside in:
-- --    Min, Max, Plus/Minus, Times, Product, Power, Div/Mod/Log.
-- --
-- --   All associative operations are flattened.
-- expand :: (Ord v, Ord t) => AnyExp v t -> AnyExp v t
-- expand = runIdentity . pullOutMinMax (sortSums (sortTimes (pure . f)) . relax)
--   where
--     f e@(N _)   = e
--     f e@(V _)   = e
--     f e@(F _)   = e
--     f (a :^ b)  = expand a :^ expand b
--     f (Div a b) = Div (expand a) (expand b)
--     f (Mod a b) = Mod (expand a) (expand b)
--     f (Log2 a)  = Log2 (expand a)
--     f e         = expand e


-- | I can always pull the @Min@ and @Max@ operation to the outermost level,
--   the inside will no longer contain any @Min@ or @Max@ constructors.
--
--   Note, I have to pull both Mins and Maxes at the same time, because some
--   operations flip the Min/Max, e.g. substraction, or division.
pullOutMinMax ::
       (Applicative m, Ord v, Ord t)
    => (Exp (Set '[No, No] '[OpMin, OpMax] a) v t -> m (Exp b v t))
    -> Exp a v t -> m (Exp (Set '[Yes, Yes] '[OpMin, OpMax] b) v t)
pullOutMinMax k = fmap sortMinMax . pullOutMinMax' k

pullOutMinMax' ::
       (Applicative m, Ord v, Ord t)
    => (Exp (Set '[No, No] '[OpMin, OpMax] a) v t -> m (Exp b v t))
    -> Exp a v t -> m (Exp (Set '[Yes, Yes] '[OpMin, OpMax] b) v t)
pullOutMinMax' k e = case pullOutMinMax1 e of
    NoMM e'  -> relax <$> k e'
    MM f a b -> mm f <$> pullOutMinMax' k a <*> pullOutMinMax' k b

mm :: Allow '[OpMin, OpMax] s => Bool -> Exp s v t -> Exp s v t -> Exp s v t
mm True  = Max
mm False = Min

mMax :: Exp s v t -> Exp s v t -> MM s v t
mMax = MM True

mMin :: Exp s v t -> Exp s v t -> MM s v t
mMin = MM False

data MM s v t
  = NoMM (Exp (Set '[No, No] '[OpMin, OpMax] s) v t)
  | MM Bool (Exp s v t) (Exp s v t)

pullOutMinMax1 :: Exp s v t -> MM s v t
pullOutMinMax1 (N a) = NoMM (N a)
pullOutMinMax1 (F a) = NoMM (F a)
pullOutMinMax1 (V a) = NoMM (V a)
pullOutMinMax1 (a :+ b)  = case (pullOutMinMax1 a, pullOutMinMax1 b) of
  (NoMM a', NoMM b') -> NoMM ( a' :+ b')
  (MM f a1 a2, _)    -> MM f (a1 :+ b) (a2 :+ b)
  (_, MM f b1 b2)    -> MM f (a :+ b1) (a :+ b2)
pullOutMinMax1 (a :- b)  = case (pullOutMinMax1 a, pullOutMinMax1 b) of
  (NoMM a', NoMM b') -> NoMM ( a' :- b')
  (MM f a1 a2, _)    -> MM f (a1 :- b) (a2 :- b)
  (_, MM f b1 b2)    -> MM (not f) (a :- b1) (a :- b2)
pullOutMinMax1 (a :* b)  = case (pullOutMinMax1 a, pullOutMinMax1 b) of
  (NoMM a', NoMM b') -> NoMM ( a' :* b')
  (MM f a1 a2, _)    -> MM f (a1 :* b) (a2 :* b)
  (_, MM f b1 b2)    -> MM f (a :* b1) (a :* b2)
pullOutMinMax1 (a :^ b)  = case (pullOutMinMax1 a, pullOutMinMax1 b) of
  (NoMM a', NoMM b') -> NoMM ( a' :^ b')
  (MM f a1 a2, _)    -> MM f (a1 :^ b) (a2 :^ b)
  (_, MM f b1 b2)    -> MM f (a :^ b1) (a :^ b2)
pullOutMinMax1 (Div a b) = case (pullOutMinMax1 a, pullOutMinMax1 b) of
  (NoMM a', NoMM b') -> NoMM (Div a' b')
  (MM f a1 a2, _)    -> MM f (Div a1 b) (Div a2 b)
  (_, MM f b1 b2)    -> MM (not f) (Div a b1) (Div a b2)
pullOutMinMax1 (Mod a b) = NoMM (Mod a b) -- can do nothing about it
pullOutMinMax1 (Max a b) = mMax a b
pullOutMinMax1 (Min a b) = mMin a b
pullOutMinMax1 (Log2 a)  = case pullOutMinMax1 a of
  NoMM a'    -> NoMM (Log2 a')
  MM f a' b' -> MM f (Log2 a') (Log2 b')

-- | Sort:
--     * Mins outside of Maxs (no mins inside maxes)
--     * Both flat (no Min on the right of Min, no Max on the right of Max)
--     * Sorted inside using Ord.
sortMinMax :: (Ord v, Ord t)
           => Exp s v t -> Exp s v t
sortMinMax (Min a b) = mergeMins (sortMinMax a) (sortMinMax b)
  where
    mergeMins (Min xs x) (Min ys y)
      = case compare x y of
          LT -> Min (Min (mergeMins xs ys) x) y
          EQ -> Min (mergeMins xs ys) x
          GT -> Min (Min (mergeMins xs ys) y) x
    mergeMins (Min xs x) y
      = case compare x y of
          LT -> Min (Min xs x) y
          EQ -> Min xs x
          GT -> Min (mergeMins xs y) x
    mergeMins x (Min ys y)
      = case compare x y of
          LT -> Min (mergeMins ys x) y
          EQ -> Min ys y
          GT -> Min (Min ys y) x
    mergeMins x y
      = case compare x y of
          LT -> Min x y
          EQ -> x
          GT -> Min y x
sortMinMax (Max a b) = pullMins (sortMinMax a) (sortMinMax b)
  where
    pullMins (Min x y) z
      | x == z || y == z = sortMinMax z
      | x == y           = sortMinMax (Max x z)
      | otherwise        = sortMinMax (Min (Max x z) (Max y z))
    pullMins x y@(Min _ _) = pullMins y x
    pullMins x y = mergeMaxs x y
    mergeMaxs (Max xs x) (Max ys y)
      = case compare x y of
          LT -> Max (Max (mergeMaxs xs ys) x) y
          EQ -> Max (mergeMaxs xs ys) x
          GT -> Max (Max (mergeMaxs xs ys) y) x
    mergeMaxs (Max xs x) y
      = case compare x y of
          LT -> Max (Max xs x) y
          EQ -> Max xs x
          GT -> Max (mergeMaxs xs y) x
    mergeMaxs x (Max ys y)
      = case compare x y of
          LT -> Max (mergeMaxs ys x) y
          EQ -> Max ys y
          GT -> Max (Max ys y) x
    mergeMaxs x y
      = case compare x y of
          LT -> Max x y
          EQ -> x
          GT -> Max y x
sortMinMax a         = a


-- | Sort OpPlus and OpMinus at the current level, ignoring other terms.
sortSums :: (Ord v, Ord t, Allow '[OpConst, OpPlus, OpTimes] s)
         => (Exp s v t -> Normalize (Exp s v t))
         -> Exp s v t -> Normalize (Exp s v t)
sortSums f = fmap collapseSums . rerun go
  where
    go (PM isPlus k a b) = mergeSums <$> go a <*> go b
      where
        op True  _  = k
        op False k'
          | isPlus    = k'
          | otherwise = (:+)
        mergeSums (PM isPlusX kx xs x) (PM isPlusY ky ys y)
          = if x <= y
            then op isPlusY ky (op isPlusX kx (mergeSums xs ys) x) y
            else op isPlusX kx (op isPlusY ky (mergeSums xs ys) y) x
        mergeSums ex@(PM _ kx xs x) y
          = if x <= y then k ex y else mergeSums xs y `kx` x
        mergeSums x ey@(PM _ ky ys y)
          = if y <= x && isPlus then k ey x else mergeSums ys x `ky` y
        mergeSums x y
          = if x <= y || not isPlus then k x y else k y x
    go a                 = inner $ f a


    collapseSums (PM p k (PM p' k' x (a :* N ca)) (b :* N cb))
      | a == b = if p == p'
                 then collapseSums (PM p' k' x (a :* N (ca + cb)))
                 else if ca >= cb
                      then collapseSums (PM p' k' x (a :* N (ca - cb)))
                      else collapseSums (PM p  k  x (a :* N (cb - ca)))
    collapseSums (PM p k (PM p' k' x (a :* N ca)) b)
      | a == b = if p == p'
                 then collapseSums (PM p' k' x (a :* N (ca + 1)))
                 else if ca >= 1
                      then collapseSums (PM p' k' x (a :* N (ca - 1)))
                      else collapseSums (PM p  k  x a)
    collapseSums (PM p k (PM p' k' x a) (b :* N cb))
      | a == b = if p == p'
                 then collapseSums (PM p' k' x (a :* N (1 + cb)))
                 else if cb >= 1
                      then collapseSums (PM p  k  x (a :* N (cb - 1)))
                      else collapseSums (PM p' k' x a)
    collapseSums (PM p _ (PM p' k' x a) b)
      | a == b = if p == p'
                 then collapseSums (PM p' k' x (a :* N 2))
                 else collapseSums x
    collapseSums (PM p k (N 0 :+ a) b) = PM p k a b -- stop last
    collapseSums (PM p k a@PM {} b) = PM p k (collapseSums a) b -- go deeper
    collapseSums (PM p k a b) = collapseSums (PM p k (N 0 :+ a) b) -- try last
    collapseSums a = a


sortTimes :: (Ord v, Ord t, Allow '[OpConst, OpPlus, OpPow] s)
          => (Exp s v t -> Normalize (Exp s v t))
          -> Exp s v t -> Normalize (Exp s v t)
sortTimes f = fmap collapseTimes . rerun go
  where
    -- exercise distributivity
    go (PM p k a b :* c) = do
      needRerun
      a' <- go (a :* c)
      b' <- go (b :* c)
      go (PM p k a' b')
    go (c :* PM p k a b) = do
      needRerun
      a' <- go (c :* a)
      b' <- go (c :* b)
      go (PM p k a' b')
    -- times+pow of a
    go (a :* b) = mergeTimes <$> go a <*> go b
    go a        = inner $ f a
    mergeTimes (xs :* x) (ys :* y)
      = if x >= y
        then mergeTimes xs ys :* x :* y
        else mergeTimes xs ys :* y :* x
    mergeTimes ex@(xs :* x) y
      = if x >= y then ex :* y else mergeTimes xs y :* x
    mergeTimes x ey@(ys :* y)
      = if x >= y then mergeTimes ys x :* y else ey :* x
    mergeTimes x y
      = if x >= y then x :* y else y :* x

    collapseTimes :: (Ord v, Ord t, Allow '[OpConst, OpPlus, OpPow] s)
                  => Exp s v t -> Exp s v t
    collapseTimes (as :* a :^ pa :* b :^ pb)
      | a == b = collapseTimes (as :* a :^ (pa :+ pb))
    collapseTimes (as :* a :^ pa :* b)
      | a == b = collapseTimes (as :* a :^ (N 1 :+ pa))
    collapseTimes (as :* a :* b :^ pb)
      | a == b = collapseTimes (as :* a :^ (N 1 :+ pb))
    collapseTimes (as :* a :* b)
      | a == b = collapseTimes (as :* a :^ N 2)
    collapseTimes (a :^ pa :* b :^ pb)
      | a == b = a :^ (pa :+ pb)
    collapseTimes (a :^ pa :* b)
      | a == b = a :^ (N 1 :+ pa)
    collapseTimes (a :* b :^ pb)
      | a == b = a :^ (N 1 :+ pb)
    collapseTimes (a :* b)
      | a == b    = a :^ N 2
      | otherwise = collapseTimes a :* b
    collapseTimes a = a

    -- collapseTimes = collapseList . mapPow
    -- collapseList :: Allow '[OpPlus, OpTimes, OpPow, OpConst] s
    --              => [(Exp s v t, Exp s v t)] -> Exp s v t
    -- collapseList [] = N 1
    -- collapseList [(a, ap)] = a :^ ap
    -- collapseList ((a, ap):(b, bp):xs)
    --   | a == b = collapseList (a :^ (ap :+ bp) : xs)
    --   | otherwise = collapseList ((b, bp):xs) :* (a :^ ap)
    -- mapPow :: Allow '[OpPlus, OpTimes, OpPow, OpConst] s
    --        => Exp s v t -> [(Exp s v t, Exp s v t)]
    -- mapPow (as :* (a :^ ap)) = (a, ap) : mapPow as
    -- mapPow (as :* a)         = (a, N 1) : mapPow as
    -- mapPow (a :^ ap)         = [(a, ap)]
    -- mapPow a                 = [(a, N 1)]




rearrangePows :: (Ord v, Ord t, Allow OpTimes s)
              => (Exp s v t -> Normalize (Exp s v t))
              -> Exp s v t -> Normalize (Exp s v t)
rearrangePows f = rerun go
  where
    go ((a :^ b) :^ c) = needRerun >> go (a :^ (b :* c))
    go ((a :* b) :^ c) = needRerun >> go (a :^ c :* b :^ c)
    go (a@PM {} :^ b) = do
      a' <- go a
      (n, b') <- getConstSum <$> go b
      when (n > 0) needRerun
      return $ app n (:* a') (a' :^ b')
    go (a :^ b) = (:^) <$> go a <*> go b
    go a               = inner $ f a
    getConstSum :: Exp s v t -> (Natural, Exp s v t)
    getConstSum (N n :+ a)   = (n, a)
    getConstSum (N n :- a)   = (n, N 0 :- a)
    getConstSum (N n)        = (n, N 0)
    getConstSum (PM p k a b) = second (\a' -> PM p k a' b) (getConstSum a)
    getConstSum a            = (0, a)
    app 0 _ a = a
    app n k a = k (app (n-1) k a)


-- | Simplify a chain of plus and minus patterns, assuming they are already
--   sorted.
--   This means that right part of a sum is never Plus or Minus.
simplifyPM :: (Outputable v, Outputable t, Allow OpPlus s, Ord v, Ord t)
           => Exp s v t -> Normalize (Exp s v t)
simplifyPM (PM isPlus k a b) = case (a, b) of
    -- constants
    (_  , N 0)        -> simplifyPM a
    (N 0, _  )        -> simplifyConst b
    (N x, N y)
      | isPlus        -> pure $ N (x + y)
      | x >= y        -> pure $ N (x - y)
      | otherwise     -> pure $ N x `k` N y

    (PM isPlusA kA a1 (N x2), N y)
      | isPlus == isPlusA
                      -> simplifyPM a1 <&> (:+ N (x2 + y))
      | y == x2       -> simplifyPM a1
      | y >  x2       -> simplifyPM a1 <&> (`k` N (y - x2))
      | otherwise     -> simplifyPM a1 <&> (`kA` N (x2 - y))

    (N xp :- N xn, N y)
      | xp + y >= xn  -> pure $ N (xp + y - xn)
    (N x, N yp :- N yn)
      | x + yp >= yn  -> pure $ N (x + yp - yn)
    (N xp :- N xn, N yp :- N yn)
                      -> pure $ N (xp + yp) :- N (xn + yn)

    _                 -> k <$> simplifyPM a <*> simplifyConst b
simplifyPM a = simplifyConst a

simplifyTimes :: (Outputable v, Outputable t, Ord v, Ord t)
              => Exp s v t -> Normalize (Exp s v t)
simplifyTimes (a :* b) = case (a, b) of
    (N 0, _  ) -> pure $ N 0
    (_  , N 0) -> pure $ N 0
    (N 1, _  ) -> simplifyConst b
    (_  , N 1) -> simplifyTimes a
    (N x, N y) -> pure $ N (x * y)
    _          -> (:*) <$> simplifyTimes a <*> simplifyConst b
simplifyTimes a = simplifyConst a

-- Group and simplify constant parts of an expression.
-- Match obvious identities, such as 0+x = x, or 0*x = 0 etc.
simplifyConst :: (Outputable v, Outputable t, Ord v, Ord t)
              => Exp s v t -> Normalize (Exp s v t)
simplifyConst (N a) = pure (N a)
simplifyConst (F a) = pure (F a)
simplifyConst (V a) = pure (V a)
simplifyConst (a :+ b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  simplifyPM (a' :+ b')
simplifyConst (a :- b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  simplifyPM (a' :- b')
simplifyConst (a :* b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  simplifyTimes (a' :* b')
simplifyConst (a :^ b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  pure $ case (a', b') of
    (N 0, _  ) -> N 0
    (N 1, _  ) -> N 1
    (_  , N 0) -> N 1
    (_  , N 1) -> a'
    (N x, N y) -> N (x Prelude.^ y)
    _          -> a' :^ b'
simplifyConst e@(Div a b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  case (a', b') of
    (_  , N 0) -> insol $ "Division by zero:" <+> ppr (Div a' b') <+>
                        parens ("original:" <+> ppr e)
    (_  , N 1) -> pure a'
    (N 0, _  ) -> pure (N 0)
    (N x, N y) -> pure (N (div x y))
    _          -> pure (Div a' b')
simplifyConst e@(Mod a b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  case (a', b') of
    (_  , N 0) -> insol $ "Division by zero:" <+> ppr (Mod a' b') <+>
                        parens ("original:" <+> ppr e)
    (_  , N 1) -> pure (N 0)
    (N 0, _  ) -> pure (N 0)
    (N x, N y) -> pure (N (mod x y))
    _          -> pure (Mod a' b')
simplifyConst (Max a b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  pure $ case (a', b') of
    (N 0, _  ) -> b'
    (_  , N 0) -> a'
    (N x, N y) -> N (max x y)
    _          -> Max a' b'
simplifyConst (Min a b) = do
  a' <- simplifyConst a
  b' <- simplifyConst b
  pure $ case (a', b') of
    (N 0, _  ) -> N 0
    (_  , N 0) -> N 0
    (N x, N y) -> N (min x y)
    _          -> Min a' b'
simplifyConst e@(Log2 a) = do
  a' <- simplifyConst a
  case a' of
    N x -> case log2Nat x of
      Left err -> insol $ err <+> parens ("original:" <+> ppr e)
      Right n  -> pure (N n)
    _   -> pure (Log2 a')



normalize :: (Outputable v, Outputable t, Ord v, Ord t)
          => AnyExp v t -> Normalize (AnyExp v t)
normalize = sc $ pullOutMinMax (sortSums (sortTimes (rearrangePows f)) . relax)
  where
    sc k x = simplifyConst x >>= k >>= simplifyConst
    f e@(N _)   = pure e
    f e@(V _)   = pure e
    f e@(F _)   = pure e
    f (Div a b) = Div <$> normalize a <*> normalize b
    f (Mod a b) = Mod <$> normalize a <*> normalize b
    f (Log2 a)  = Log2 <$> normalize a
    f e         = normalize e


-- | Try to simplify a list of constraints;
--   Returns a list of solved constraints.
--   The returned list contains only those constraints, for wich some advances
--   has been done.
--   If the list is empty, the solver could do nothing.
solve :: [EqConstraint s v t]
         -- ^ Given constraints
      -> [(ct, EqConstraint s v t)]
         -- ^ Wanted constraints
      -> [SolveResult v t ct]
solve = undefined








newtype Var = Var String
  deriving (Eq, Ord, IsString)

instance Outputable Var where
  ppr (Var x) = text x

newtype XType = XType String
  deriving (Eq, Ord, IsString)

instance Outputable XType where
  ppr (XType x) = text x

runSolveNat :: IO ()
runSolveNat = do
    putStrLn "Hello!"
    mapM_ (\e -> pprTraceM "implCts:   " $
                 ppr e $$ vcat (map ppr (implCts e))) $ exprs
    mapM_ (\e -> pprTraceM "normalize: " $
                 ppr e $$ pprNormalize (normalize e)) $ exprs
  where
    x = V "x"
    y = V "y"
    z1 = V "z1"
    z2 = V "z2"
    z3 = V "z3"
    exprs :: [AnyExp Var XType]
    exprs =
      [ x + y - Min (Log2 z1) (Max 5 y)
      , x * y + z1
      , x * ((y + z2) ^ z3) ^ z2 ^ 2
      , (z2 + z3 + (x * y - 7 * x)) ^ (z1 + 2)
      , F "Foo x z1 k" * x + 2 - y
      , 4 * 2 + 2 - 3^2 + 19
      , 4 * 1 + 2 - 3^2 + 19
      , 4 * 1 + 2 - (3^2 + 1) + 19
      , 4 * 1 + (2 - 3^2) + 19
      , 19 + (4 * 1 + 2 - 3^2)
      , 4 * 1 + 2 - Log2 (3^2) + 19
      , x + 3 + 3 + 5 - 3 + y * z1 + Max y (100 - x - 8 + z1 * 2)
          + Min (2 + Log2 y) (3 + Log2 (2 * x + Mod (5 + Min x 3) 7))
          + Div (9 - Max 6 3 + 2 * 2 ^ 3) (Log2 18 ^ 3)
      , x + 3 + 3 + 5 - 3 + y * z1 + Max y (100 - x - 8 + z1 * 2)
          + Min (2 + Log2 y) (3 - Log2 (2 * x + Mod (5 + Min x 3) 7))
          + Div (9 - Max 6 3 + 2 * 2 ^ 3) (Log2 18 ^ 3)
      , (x + y + 4) * (x - z1) + Max (x * (y + 2)) (y * (z1 + x)) - (x - z1) ^ 3
        + y ^ (z1 + 2)
      ]
