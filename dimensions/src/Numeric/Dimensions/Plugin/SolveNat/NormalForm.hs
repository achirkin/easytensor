{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
module Numeric.Dimensions.Plugin.SolveNat.NormalForm
  ( NormalForm(..)
  , Validate(..)
    -- * Parts of the normal form
  , Prime()
  , NormalE(..)
  , MinsE(..)
  , MaxsE(..)
  , SumsE(..)
  , ProdE(..)
  , PowE(..)
  , UnitE(..)
    -- * Extra convenience functions
  , isZero
  , isNonZero
  , isSignOne
  , isOne
  , isNonNeg
  , isNonPos
  , isComplete
  , isEven
  , isOdd
  , neMin, neMax, nePow, neMod, neDiv, neLog2
  , minsE, maxsE, sumsE, prodE, powU, powS
  , map2Maxs, map2Mins, map2Sums
  -- , asNatural
  , factorize
  , factorizeR
  , unitAsSums
  , unconstSumsE
  -- , powSums
  , regroupSumsE
  -- , ifNonNeg
  -- , ifEven
  -- , normal'max
  -- , normal'min
  -- , normal'pow
  )
where

import           Data.Void
import           Control.Monad                  (join)
import           Data.List                      ( group )
import           Data.Ratio
import           Numeric.Dimensions.Plugin.AtLeast
import           Numeric.Dimensions.Plugin.SolveNat.Exp
import           Numeric.Dimensions.Plugin.SolveNat.ExpState
import           Numeric.Natural
import           Outputable              hiding ( (<>) )
import           Unsafe.Coerce                  (unsafeCoerce)

{- |
The normal form of an expression
  is an (almost) unique, rigid, layered representation.

Each layer of the normal form is represented by a dedicated type. Most of the
structure is represented by the types theirselves; but some aspects are encoded
by invariants that can be checked at runtime.

This makes comparison of the expressions in the normal form very simple except in
one particular case. Consider the following:
\[
   {( x + y )}^{z - 2} = x {( x + y )}^{z - 3} + y {( x + y )}^{z - 3}
\]
Here, \(x, y, z \) are unknown variables. As long as we don't know the value of
\(z\), we can expand the expression indefinitely (imposing stricter constraints
on \(z\), i.e. \(z > n\)). Thus, I decided to postpone this expandsion for later
stages.
 -}
class NormalForm e where
  -- | Every part of the normal form can be converted back to an expression.
  fromNormal :: e t v -> Exp t v
  -- | Every part of the normal form can be transformed to a full NormalE expr.
  toNormalE :: e t v -> NormalE t v
  -- | Extra information about the part of an expression in the normal form.
  expState :: e t v -> ExpState
  -- | Update the expression expState.
  mapExpState :: (ExpState -> ExpState) -> e t v -> e t v
  -- | Try to compute the expression to a rational number.
  asRational :: e t v -> Maybe Rational
  -- | Check the invariants that must hold by construction.
  --   Do not check arithmetic errors, such as division by zero, etc.
  validate   :: (Ord t, Ord v, Outputable t, Outputable v) => e t v -> Validate

-- | Check if the value is guaranteed to be zero
isZero :: NormalForm e => e t v -> Bool
isZero = _isZero . expState

-- | Check if the value is guaranteed to be non-zero
isNonZero :: NormalForm e => e t v -> Bool
isNonZero = _isNonZero . expState

-- | Check if the value is guaranteed to be one or minus one
isSignOne :: NormalForm e => e t v -> Bool
isSignOne = _isSignOne . expState

-- | Check if the value is guaranteed to be one
isOne :: NormalForm e => e t v -> Bool
isOne x = _isSignOne s && _isNonNeg s
  where s = expState x

-- | Check if the value is guaranteed to be zero or positive
isNonNeg :: NormalForm e => e t v -> Bool
isNonNeg = _isNonNeg . expState

-- | Check if the value is guaranteed to be zero or negative
isNonPos :: NormalForm e => e t v -> Bool
isNonPos = _isNonPos . expState

-- | Check if the value is guaranteed to be even
isEven :: NormalForm e => e t v -> Bool
isEven = _isEven . expState

-- | Check if the value is guaranteed to be odd
isOdd :: NormalForm e => e t v -> Bool
isOdd = _isOdd . expState

-- | Check if the value is guaranteed to be integral
isWhole :: NormalForm e => e t v -> Bool
isWhole = _isWhole . expState

-- | Check if the expression is guaranteed to be valid for all inputs
isComplete :: NormalForm e => e t v -> Bool
isComplete = _isComplete . expState

-- | Report broken invariants
data Validate = Invalid (AtLeast One SDoc) | Ok

-- | A plain wrapper that adds nothing to the expression.
newtype NormalE t v = NormalE { getNormalE :: MinsE t v }
  deriving (Eq, Ord, Show, Foldable)

-- | A minimum among a list of expressions is the outermost level.
data MinsE t v = MinsE { getMinsEState :: ExpState, getMinsE :: AtLeast One (MaxsE t v) }
  deriving (Show, Foldable)


-- | A maximum among a list of expressions is the second level.
data MaxsE t v = MaxsE { getMaxsEState :: ExpState, getMaxsE :: AtLeast One  (SumsE None t v) }
  deriving (Show, Foldable)

-- | An expression (empty of mins and maxs) is the sum of products.
--   It can be zero (empty list of products), or const (a single const product);
--   otherwise the list of products is sorted (descending).
--   None of the components have the same variable part.
data SumsE n t v = SumsE { getSumsEState :: ExpState, getSumsE :: AtLeast n (ProdE t v) }
  deriving (Show, Foldable)

-- | A product is a non-empty list of power expressions (@PowE@) sorted in
--   the descending order; none of the bases are equal.
--   Note, @ProdE@ is a positive value by construction (cannot be zero or negative).
data ProdE t v = ProdE { getProdEState :: ExpState, getProdE :: AtLeast One (PowE t v) }
  deriving (Show, Foldable)

-- | A power expression is either a unit or a non-trivial @SumE@ (base) in
--   the power of some @SumE@.
data PowE t v
  = PowU ExpState (UnitE t v) (SumsE None t v)
    -- ^ If the base is a UnitE, there is an extra invariant:
    --   the power is equal to zero iff the base is equal to @UOne@.
  | PowS ExpState (SumsE Two t v) (SumsE One t v)
    -- ^ If the base is a SumE, there is an extra invariant:
    --   the power must have variable or irreducible funs or be negative const
    --     (single non-negative const is not allowed and immediately expanded).
  deriving (Show, Foldable)

-- | Prime values and irreducible functions.
data UnitE t v
  = UMinusOne
    -- ^ Number @-1@, which is not considered prime here
  | UOne
    -- ^ Number @1@, which is not considered prime here
  | UN Prime
    -- ^ Prime numbers. Note, no zeroes possible!
  | UDiv (SumsE None t v) (NormalE t v)
    -- ^ Numerator is free of MaxsE and MinsE, but I cannot float out
    --   MaxsE from the denominator due to possible zeroes.
  | UMod (NormalE t v) (NormalE t v)
    -- ^ Mod is a tricky thing, I can do almost nothing about it.
  | ULog2 (MaxsE t v)
    -- ^ Cannot float out MaxsE due to possible zeroes.
  | UF t
    -- ^ An irreducible type family
  | UV v
    -- ^ A type variable
  deriving (Eq, Ord, Show, Foldable)

-- | 2,3,5,7,11...
--
--   Note, this does not include number 1.
newtype Prime = Prime { asNatural :: Natural }
  deriving (Eq, Ord, Show)

factorizeR :: Rational -> AtLeast None (PowE t v)
factorizeR = unsafeCoerce factorizeRV

factorizeRV :: Rational -> AtLeast None (PowE Void Void)
factorizeRV r
    | n == 0 = mempty
    | d == 1 = nf
    | otherwise = mergeDesc nf df
  where
    n = numerator r
    d = denominator r
    nf = factorizeV n
    df = f <$> factorizeV d
    f (PowU _ a b) = powU a (negateSum b)
    f (PowS _ a b) = powS a (negateSum b)

-- | Zero factorizes into an empty list of powers,
--   the rest of the Naturals factorize into a non-empty list of powers.
--
--   It does not include 1 in the list as long as there are any other numbers!
--
--   The resulting list of powers is decreasing in the magnitude of the base.
factorize :: Integer -> AtLeast None (PowE t v)
factorize = unsafeCoerce factorizeV

factorizeV :: Integer -> AtLeast None (PowE Void Void)
factorizeV (-1) = L [unitAsPow UMinusOne]
factorizeV i
  | i < 0     = factorize' (fromInteger $ negate i) <> factorizeV (-1)
  | otherwise = factorize' $ fromInteger i

factorize' :: Natural -> AtLeast None (PowE Void Void)
factorize' 0 = mempty
factorize' 1 = L [unitAsPow UOne]
factorize' x = L . reverse . fmap asPow . group $ factor x 2
  where
    asPow :: [Natural] -> PowE Void Void
    asPow ns =
      let L ps = factorize' $ fromIntegral p
          p    = length ns
          n    = head ns
      in  PowU (fromIntegral n) (UN $ Prime n) $ case ps of
            [] -> zero
            (p' : ps') ->
              singleProdAsSums $ ProdE (fromIntegral p) $ p' :| L ps'
    factor :: Natural -> Natural -> [Natural]
    factor n k | k * k > n             = [n]
               | (n', 0) <- divMod n k = k : factor n' k
               | otherwise             = factor n (k + 1)


minsE :: AtLeast One (MaxsE t v) -> MinsE t v
minsE xxs@(x :| L xs) = MinsE (foldl (\s -> esMin s . expState) (expState x) xs) xxs

maxsE :: AtLeast One (SumsE None t v) -> MaxsE t v
maxsE xxs@(x :| L xs) = MaxsE (foldl (\s -> esMax s . expState) (expState x) xs) xxs

sumsE :: AtLeast n (ProdE t v) -> SumsE n t v
sumsE xs = SumsE (sum $ expState <$> xs) xs

prodE :: AtLeast One (PowE t v) -> ProdE t v
prodE xxs@(x :| L xs) = ProdE (foldl (\s -> (s *) . expState) (expState x) xs) xxs

powU :: UnitE t v -> SumsE None t v -> PowE t v
powU a b = PowU (esPow (expState a) (expState b)) a b

powS :: SumsE Two t v -> SumsE One t v -> PowE t v
powS a b = PowS (esPow (expState a) (expState b)) a b


-- | Get the constant scale factor of a product expression.
unscaleProdE :: forall t v . (Ord t, Ord v) => ProdE t v -> (Rational, ProdE t v)
unscaleProdE p = atleast1 <$> go 1 (toList $ getProdE p)
  where
    atleast1 :: [PowE t v] -> ProdE t v
    atleast1 (a : as) = prodE $ a :| L as
    atleast1 []       = oneP
    go :: Rational -> [PowE t v] -> (Rational, [PowE t v])
    go c [] = (c, [])
    -- constant values
    go c (x : xs)
      | Just c' <- asRational x = go (c * c') xs
    -- bad values
    go c (PowU ps u s : xs) | not (isComplete u) || not (isComplete s) = (PowU ps u s :) <$> go c xs
    go c (PowS ps u s : xs) | not (isComplete u) || not (isComplete s) = (PowS ps u s :) <$> go c xs
    -- good zeros
    go c (PowU _ _ s : xs) | isZero s = go c xs
    go c (PowS _ _ s : xs) | isZero s = go c xs
    go _ (PowU _ u s : _)  | isZero u && isNonZero s && isNonNeg s = (0, [])
    go _ (PowS _ u s : _)  | isZero u && isNonZero s && isNonNeg s = (0, [])

    go c (x : xs)
      | Just (u, k, s) <- asRationalPow x
      , Just uk <- powR u k
      , Just (c', u') <- case compare u 0 of
          GT -> Just (c * uk, u)
          EQ -> Nothing
          LT | isEven s -> Just (c * uk, abs u)
             | isOdd s && isWhole s -> Just (c * negate uk, abs u)
             | isSignOne s -> Just (c * negate uk, abs u)
             | otherwise -> Just (c * uk, u)
      , L (u1:us) <- factorizeR u'
      , SumsE _ (L [ProdE _ upows]) <- powProd (prodE $ u1 :| L us) s
        = case u' of
          1 -> go c' xs
          _ -> toList . mergeDesc (L $ toList upows) . L <$> go c' xs

    -- TODO: unscaleSumsE?
    go c (x : xs) = (x :) <$> go c xs

    asRationalPow (PowU _ u s)
      | Just u' <- asRational u
      , (k, s') <- unconstSumsE s
        = Just (u', k, s')
    asRationalPow (PowS _ u s)
      | Just u' <- asRational u
      , (k, s') <- unconstSumsE s
        = Just (u', k, s')
    asRationalPow _ = Nothing


-- | Get a rational-valued constant part of a sum expression.
--   The remaining sum has no (rational) constant.
unconstSumsE :: forall n t v . (Ord t, Ord v) => SumsE n t v -> (Rational, SumsE None t v)
unconstSumsE s = sumsE . L <$> go 0 (toList $ getSumsE s)
  where
    go :: Rational -> [ProdE t v] -> (Rational, [ProdE t v])

    go c [] = (c, [])

    -- constant values
    go c (x : xs)
      | Just c' <- asRational x = go (c + c') xs
    
    -- try to simplify other cases
    go c (p : ps)
        | isZero p' = go c ps
        | c' == 0   = go c ps
        | isSignOne p' && isNonNeg p' = go (c + c') ps
        | isSignOne p' && isNonPos p' = go (c - c') ps
        | otherwise = (p :) <$> go c ps
      where (c', p') = unscaleProdE p

singleProdAsSums :: Applicative (AtLeast n) => ProdE t v -> SumsE n t v
singleProdAsSums = sumsE . pure

singlePowAsSums :: Applicative (AtLeast n) => PowE t v -> SumsE n t v
singlePowAsSums = singleProdAsSums . prodE . pure

unitAsPow :: UnitE t v -> PowE t v
unitAsPow UOne = PowU 1 UOne zero
unitAsPow u    = PowU (expState u) u one

unitAsSums :: Applicative (AtLeast n) => UnitE t v -> SumsE n t v
unitAsSums = singlePowAsSums . unitAsPow

zero :: SumsE None t v
zero = SumsE 0 mempty

one :: Applicative (AtLeast n) => SumsE n t v
one = singleProdAsSums oneP

oneP :: ProdE t v
oneP = ProdE 1 $ PowU 1 UOne zero :| mempty

-- minusOneP :: ProdE t v
-- minusOneP = ProdE s $ PowU s UMinusOne one :| mempty
--   where
--     s = -1

noneSumsE :: SumsE n t v -> SumsE None t v
noneSumsE s = SumsE (expState s) . L . toList $ getSumsE s

-- | Same as signum, but only if we have enough evidence.
normalSign :: NormalForm a => a t v -> Maybe Integer
normalSign a | isZero a   = Just 0
             | isNonNeg a && isNonZero a = Just 1
             | isNonPos a && isNonZero a = Just (-1)
             | otherwise  = Nothing

-- | Group expresssions like
--      \(    {(a + b)}^{x + c_1} + {(a + b)}^{x + c_2}    \)
--   into
--      \(    {(a + b)}^{x + \min c_1 c_2} * ( {(a + b)}^{\max c_1 c_2 - \min c_1 c_2) + 1}   \),
--   where
--      \(a\) and \(x\) are arbitrary variable expressions
--      and \(c_1, c_2\) are constants.
--
--   This solves the problem of ambiguous sum-of-products representation:
--   there must be no two product terms that differ only in constant power value.
--     (note, RHS of the resulting product is immediately expanded into simple terms).
--
--   If this invariant holds, comparing normalized expressions becomes simple:
--   substract one from another and compare result to zero.
regroupSumsE :: (Ord t, Ord v) => SumsE n t v -> SumsE None t v
regroupSumsE = noneSumsE
-- regroupSumsE = (\(SumsE s v) -> SumsE s (sortDesc $ fmap (\(ProdE s' v') -> (ProdE s' (sortDesc v'))) v))
--              . go . toList . getSumsE
--   where
--     go :: (Ord t, Ord v) => [ProdE t v] -> SumsE None t v
--     go [] = zero
--     go xs = foldl (+) (sumsE $ L xs') sums
--       where
--         (xs', sums) =
--           partitionEithers $ map (toSingleProdOrSum . splitExtraSums bases) xs
--         bases = foldMap (foldMap addBase . getProdE) xs []
--     -- add only PowS sums;
--     -- terms are ordered descending,
--     --   by base first, then by power
--     addBase :: (Ord t, Ord v) => PowE t v -> PowsS t v -> PowsS t v
--     addBase (PowU _ _ _) xs              = xs
--     addBase (PowS _ a p) []              = [(a, p)]
--     addBase (PowS _ a p) (x@(b, q) : xs) = case compare a b of
--       GT -> (a, p) : x : xs
--       EQ -> case unconstSumsE (noneSumsE p - noneSumsE q) of
--         (c, s) | isZero s  -> (b, if c < 0 then p else q) : xs
--                | p > q     -> (a, p) : x : xs
--                | otherwise -> x : addBase (powS a p) xs
--       LT -> x : addBase (powS a p) xs
--     subBase
--       :: (Ord t, Ord v)
--       => PowsS t v
--       -> PowE t v
--       -> ([(SumsE Two t v, Natural)], PowE t v)
--     subBase bases (PowS _ a p)
--       | (c, _) : _ <-
--         filter (isZero . snd)
--         . map (\(_, q) -> unconstSumsE (noneSumsE p - noneSumsE q))
--         $ filter ((a ==) . fst) bases
--       , c > 0
--       = let ps' = getSumsE $ noneSumsE p - fromInteger c
--             p'  = case ps' of
--               L [] ->
--                 error "regroupSumsE/subBase panic: expected non-empty SumsE!"
--               L (x : xs) -> sumsE (x :| L xs)
--         in  ([(a, fromInteger c)], powS a p')
--     subBase _ x = ([], x)

--     -- extracts terms like (a + b) ^ Nat from a product expression,
--     -- such that all complex sum expressions inside match the bases list
--     splitExtraSums
--       :: (Ord t, Ord v)
--       => PowsS t v
--       -> ProdE t v
--       -> ([(SumsE Two t v, Natural)], ProdE t v)
--     splitExtraSums bases =
--       fmap prodE . traverse (subBase bases) . getProdE

--     toSingleProdOrSum
--       :: (Ord t, Ord v)
--       => ([(SumsE Two t v, Natural)], ProdE t v)
--       -> Either (ProdE t v) (SumsE None t v)
--     toSingleProdOrSum ([], p) = Left p
--     toSingleProdOrSum (xs, p) = Right
--       $ foldl (\s (x, n) -> s * noneSumsE x ^ n) (singleProdAsSums p) xs


-- type PowsS t v = [(SumsE Two t v, SumsE One t v)]


instance NormalForm NormalE where
  fromNormal    = fromNormal . getNormalE
  toNormalE     = id
  asRational    = asRational . getNormalE
  expState      = expState . getNormalE
  mapExpState f = NormalE . mapExpState f . getNormalE
  validate      = validate . getNormalE

instance NormalForm MinsE where
  fromNormal (MinsE _ (e :| L []))
    = fromNormal e
  fromNormal (MinsE s (e1 :| L (e2:es)))
    = Min (fromNormal e1) (fromNormal (MinsE s (e2 :| L es)))
  toNormalE = NormalE
  asRational (MinsE _ xs) = minimum <$> traverse asRational xs
  expState = getMinsEState
  mapExpState f (MinsE s (e :| L [])) = let fs = f s in MinsE (f s) (mapExpState (const fs) e :| L [])
  mapExpState f (MinsE s es) = MinsE fs (mapExpState g <$> es)
    where
      fs = f s
      g a = a
        { _isNonZero  = _isNonZero a  || (_isNonNeg fs && _isNonZero fs)
        , _isNonNeg   = _isNonNeg a   || _isNonNeg fs
        , _isComplete = _isComplete a || _isComplete fs
        }
  validate x = foldMap validate xl <> decreasing xl
    where
      xl = toList $ getMinsE x
      decreasing :: (Ord t, Ord v) => [MaxsE t v] -> Validate
      decreasing []  = Ok
      decreasing [_] = Ok
      decreasing (x1:xs@(x2:_))
        | x1 <= x2 = Invalid $ pure $ hsep
          ["Components of", ppr x, "are not descending:", ppr xl]
        | otherwise = decreasing xs

instance NormalForm MaxsE where
  fromNormal (MaxsE _ (e :| L []))
    = fromNormal e
  fromNormal (MaxsE s (e1 :| L (e2:es)))
    = Max (fromNormal e1) (fromNormal (MaxsE s (e2 :| L es)))
  toNormalE x = toNormalE $ MinsE (expState x) (pure x)
  asRational (MaxsE _ xs) = maximum <$> traverse asRational xs
  expState = getMaxsEState
  mapExpState f (MaxsE s (e :| L [])) = let fs = f s in MaxsE (f s) (mapExpState (const fs) e :| L [])
  mapExpState f (MaxsE s es) = MaxsE fs (mapExpState g <$> es)
    where
      fs = f s
      g a = a
        { _isNonZero  = _isNonZero a  || (_isNonPos fs && _isNonZero fs)
        , _isNonPos   = _isNonPos a   || _isNonPos fs
        , _isComplete = _isComplete a || _isComplete fs
        }
  validate x = foldMap validate xl <> decreasing xl
    where
      xl = toList $ getMaxsE x
      decreasing :: (Ord t, Ord v) => [SumsE None t v] -> Validate
      decreasing []  = Ok
      decreasing [_] = Ok
      decreasing (x1:xs@(x2:_))
        | x1 <= x2 = Invalid $ pure $ hsep
          ["Components of", ppr x, "are not descending:", ppr xl]
        | otherwise = decreasing xs

instance NormalForm (SumsE n) where
  fromNormal x = case toList (getSumsE x) of
      [] -> N 0
      (p : ps) -> foldl f (fromNormal p) ps
    where
      f :: Exp t v -> ProdE t v -> Exp t v
      f e p = case fromNormal p of
        N 0 :- e' -> e :- e'
        e' -> e :+ e'
  toNormalE x = toNormalE $ MaxsE (expState x) (pure $ noneSumsE x)
  asRational (SumsE _ xs) = sum <$> traverse asRational xs
  expState = getSumsEState
  mapExpState f (SumsE s es)
      | length es == 1 = SumsE fs (mapExpState (const fs) <$> es)
      | otherwise      = SumsE fs (mapExpState g <$> es)
    where
      fs = f s
      g a = a { _isComplete = _isComplete a || _isComplete fs }
  validate x = foldMap validate xl <> decreasing xl <> maxOneConst
    where
      xl = toList $ getSumsE x
      ps = snd . unscaleProdE <$> xl
      decreasing :: (Ord t, Ord v) => [ProdE t v] -> Validate
      decreasing []  = Ok
      decreasing [_] = Ok
      decreasing (x1:xs@(x2:_))
        | x1 <= x2 = Invalid $ pure $ hang
          (hsep ["Components of", ppr x, "are not descending:"]) 2 (ppr xl)
          $$
          ppr ps
        | otherwise = decreasing xs
      maxOneConst
        | length (filter (oneP ==) ps) > 1 = Invalid $ pure $ hsep
          [ppr x, " has more than one const:", ppr xl]
        | otherwise = Ok

instance NormalForm ProdE where
  fromNormal = go 1 . fmap fromNormal . getProdE
    where
      go :: Natural -> AtLeast One (Exp t v) -> Exp t v
      go n (N m :| L [])      = N (n * m)
      go 1 (  e :| L [])      = e
      go n (  e :| L [])      = N n :* e
      go n (N m :| L (e:es))  = go (n * m) (e :| L es)
      go n ( e1 :| L (e2:es)) = go n (e2 :| L es) :* e1
  toNormalE = toNormalE . sumsE . L . (:[])
  asRational (ProdE _ xs) = product <$> traverse asRational xs
  expState = getProdEState
  mapExpState f (ProdE s (e :| L [])) = let fs = f s in ProdE (f s) (mapExpState (const fs) e :| L [])
  mapExpState f (ProdE s es) = ProdE fs (mapExpState g <$> es)
    where
      fs = f s
      g a = a
        { _isNonZero  = _isNonZero a  || _isNonZero fs
        , _isOdd      = _isOdd a      || _isOdd fs
        , _isComplete = _isComplete a || _isComplete fs
        }
  validate x@(ProdE _ x') = foldMap validate x' <> decreasing (toList x')
    where
      errMsg = Invalid $ pure $ hsep
        ["Components of", ppr x, "are not descending:", ppr (toList x')]
      decreasing :: (Ord t, Ord v) => [PowE t v] -> Validate
      decreasing []  = Ok
      decreasing [_] = Ok
      decreasing (PowU _ u1 _ : PowU _ u2 _ : _)
        | u1 <= u2 = errMsg
      decreasing (PowS _ s1 _ : PowS _ s2 _ : _)
        | s1 <= s2 = errMsg
      decreasing (x1:x2:_)
        | x1 <= x2 = errMsg
      decreasing (_:xs) = decreasing xs

instance NormalForm PowE where
  fromNormal x = cleanPow $ case x of
      PowU _ a b -> fromNormal a :^ fromNormal b
      PowS _ a b -> fromNormal a :^ fromNormal b
    where
      cleanPow :: Exp t v -> Exp t v
      cleanPow (_ :^ N 0)   = N 1
      cleanPow (a :^ N 1)   = cleanPow a
      cleanPow (N a :^ N p) = N (a ^ p)
      cleanPow a            = a
  toNormalE x = toNormalE $ ProdE (expState x) (pure x)
  asRational (PowU _ a b) = join $ powR <$> asRational a <*> asRational b
  asRational (PowS _ a b) = join $ powR <$> asRational a <*> asRational b
  expState (PowU s _ _) = s
  expState (PowS s _ _) = s
  mapExpState f (PowU s a b) = PowU (f s) a b
  mapExpState f (PowS s a b) = PowS (f s) a b
  validate (PowU _ a b) = validate a <> validate b <> checkConst
    where
      bIsZero = isZero b
      aIsOne  = isOne a
      checkConst
        | bIsZero && not aIsOne = Invalid $ pure $ hsep
          ["If power is zero, base must be one, but got", ppr a, "^", ppr b]
        | not bIsZero && aIsOne = Invalid $ pure $ hsep
          ["If base is one, power must be zero, but got", ppr a, "^", ppr b]
        | otherwise = Ok
  validate x@(PowS _ a b) = validate a <> validate b <> isNotConst
    where
      isNotConst
        | (p, SumsE _ (L [])) <- unconstSumsE b
        , p >= 1 = Invalid $ pure $ hsep
          ["Power of SumE must never be a bare constant with positive integer part:", ppr x]
        | otherwise = Ok

instance NormalForm UnitE where
  fromNormal  UMinusOne = -1
  fromNormal  UOne      = 1
  fromNormal (UN p)     = N (asNatural p)
  fromNormal (UDiv a b) = fromNormal a `Div` fromNormal b
  fromNormal (UMod a b) = fromNormal a `Mod` fromNormal b
  fromNormal (ULog2 a)  = Log2 (fromNormal a)
  fromNormal (UF t)     = F t
  fromNormal (UV v)     = V v
  toNormalE = toNormalE . unitAsPow
  asRational  UMinusOne = Just (-1)
  asRational  UOne      = Just 1
  asRational (UN p)     = Just $ fromIntegral (asNatural p)
  asRational (UDiv a b) = fmap fromInteger . join $ divR <$> asRational a <*> asRational b
  asRational (UMod a b) = join $ modR <$> asRational a <*> asRational b
  asRational (ULog2 a)  = fromInteger <$> (asRational a >>= log2R)
  asRational (UF _)     = Nothing
  asRational (UV _)     = Nothing
  expState UMinusOne = -1
  expState UOne = 1
  expState (UN p) = fromIntegral $ asNatural p
  expState (UDiv a b) = esDiv (expState a) (expState b)
  expState (UMod a b) = esMod (expState a) (expState b)
  expState (ULog2 a) = esLog2 $ expState a
  expState (UF _) = ExpState
    { _isZero     = False
    , _isNonZero  = False
    , _isSignOne  = False
    , _isNonNeg   = False
    , _isNonPos   = False
    , _isEven     = False
    , _isOdd      = False
    , _isWhole    = True
    , _isComplete = False
    }
  expState (UV _) = ExpState
    { _isZero     = False
    , _isNonZero  = False
    , _isSignOne  = False
    , _isNonNeg   = True
    , _isNonPos   = False
    , _isEven     = False
    , _isOdd      = False
    , _isWhole    = True
    , _isComplete = True
    }
  mapExpState _ = id
  validate UMinusOne = Ok
  validate UOne = Ok
  validate up@(UN (Prime p))
    | factorizeR (fromIntegral p) == L [unitAsPow up] = Ok
    | otherwise = Invalid $ pure $ ppr (toInteger p) <+> "is not a prime number."
  validate (UDiv a b) = validate a <> validate b
  validate (UMod a b) = validate a <> validate b
  validate (ULog2 a)  = validate a
  validate (UF _) = Ok
  validate (UV _) = Ok


instance (Eq t, Eq v) => Eq (MinsE t v) where
  a == b = getMinsE a == getMinsE b

instance (Eq t, Eq v) => Eq (MaxsE t v) where
  a == b = getMaxsE a == getMaxsE b

instance (Eq t, Eq v) => Eq (SumsE n t v) where
  a == b = getSumsE a == getSumsE b

instance (Eq t, Eq v) => Eq (ProdE t v) where
  a == b = getProdE a == getProdE b

instance (Eq t, Eq v) => Eq (PowE t v) where
  (PowU _ a1 a2) == (PowU _ b1 b2) = a1 == b1 && a2 == b2
  (PowS _ a1 a2) == (PowS _ b1 b2) = a1 == b1 && a2 == b2
  _ == _ = False

instance (Ord t, Ord v) => Ord (MinsE t v) where
  compare a b = compare (getMinsE a) (getMinsE b)

instance (Ord t, Ord v) => Ord (MaxsE t v) where
  compare a b = compare (getMaxsE a) (getMaxsE b)

instance (Ord t, Ord v) => Ord (SumsE n t v) where
  compare a b = compare (getSumsE a) (getSumsE b)

instance (Ord t, Ord v) => Ord (ProdE t v) where
  compare a b = compare (getProdE a) (getProdE b)

instance (Ord t, Ord v) => Ord (PowE t v) where
  compare (PowU _ a1 a2) (PowU _ b1 b2) = compare a1 b1 <> compare a2 b2
  compare (PowS _ a1 a2) (PowS _ b1 b2) = compare a1 b1 <> compare a2 b2
  compare PowU{} PowS{} = LT
  compare PowS{} PowU{} = GT

instance Outputable Validate where
  ppr Ok             = "Ok"
  ppr (Invalid errs) = hang "Invalid:" 2 $ vcat $ map (bullet <+>) $ toList errs
instance (Outputable v, Outputable t) => Outputable (NormalE t v) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (MinsE t v) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (MaxsE t v) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (SumsE n t v) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (ProdE t v) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (PowE t v) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (UnitE t v) where
  pprPrec p = pprPrec p . fromNormal


-- combine error messages
instance Semigroup Validate where
  Ok <> v = v
  v <> Ok = v
  Invalid a <> Invalid b = Invalid $ a <> b

-- take minimum
instance (Ord t, Ord v) => Semigroup (MinsE t v) where
  x <> y = MinsE (esMin (expState x) (expState y)) (sortDesc $ mergeDesc (getMinsE x) (getMinsE y))

-- take maximum
instance (Ord t, Ord v) => Semigroup (MaxsE t v) where
  x <> y = MaxsE (esMax (expState x) (expState y)) (sortDesc $ mergeDesc (getMaxsE x) (getMaxsE y))

-- add up
instance (Ord t, Ord v) => Semigroup (SumsE None t v) where
  x <> y = mapExpState (\s -> s <> (expState x + expState y))
         $ sumsE $ sortDesc $ L $ go (toList $ getSumsE x) (toList $ getSumsE y)
    where
      go :: [ProdE t v] -> [ProdE t v] -> [ProdE t v]
      go [] xs = xs
      go xs [] = xs
      go (a:as) (b:bs) = case compare pa pb of
          GT -> a : go as (b:bs)
          EQ -> case factorizeR c of
            L [] -> go as bs
            L (p:ps) -> (pa <> prodE (p :| L ps)) : go as bs
          LT -> b : go (a:as) bs
        where
          (ca, pa) = unscaleProdE a
          (cb, pb) = unscaleProdE b
          c = ca + cb

-- multiply
instance (Ord t, Ord v) => Semigroup (ProdE t v) where
  x <> y = atleast1 $ go (toList $ getProdE x) (toList $ getProdE y)
    where
      atleast1 :: [PowE t v] -> ProdE t v
      atleast1 (a:as) = ProdE (expState x * expState y) $ sortDesc $ a :| L as
      atleast1 []     = oneP
      go :: [PowE t v] -> [PowE t v] -> [PowE t v]
      go [] xs = filter (not . isOne) xs
      go xs [] = filter (not . isOne) xs
      go (a@(PowU _ ua pa) : as) (b@(PowU _ ub pb) : bs) = case compare ua ub of
        GT -> a : go as (b:bs)
        EQ -> case pa <> pb of
          SumsE _ (L []) -> go as bs
          p            -> powU ua p : go as bs
        LT -> b : go (a:as) bs
      go (a@(PowS _ sa (SumsE past (pa1 :| pas))) : as)
         (b@(PowS _ sb (SumsE pbst (pb1 :| pbs))) : bs) = case compare sa sb of
        GT -> a : go as (b:bs)
        EQ -> case SumsE past (cons pa1 pas) <> SumsE pbst (cons pb1 pbs) of
          SumsE _   (L [])     -> go as bs
          SumsE pst (L (p:ps)) -> powS sa (SumsE pst (p :| L ps)) : go as bs
        LT -> b : go (a:as) bs
      go (a:as) (b:bs)
        | a >= b    = a : go as (b:bs)
        | otherwise = b : go (a:as) bs

-- no errors
instance Monoid Validate where
  mempty = Ok

-- zero
instance (Ord t, Ord v) => Monoid (SumsE None t v) where
  mempty = zero

-- one
instance (Ord t, Ord v) => Monoid (ProdE t v) where
  mempty = oneP


instance (Ord t, Ord v) => Num (SumsE None t v) where
  (+) = (<>)
  a * b = mapExpState (\s -> s <> (expState a * expState b))
        $ foldr (\x -> (+) (sumsE (sortDesc $ L $ (x <>) <$> ys))) zero (getSumsE a)
    where
      ys = toList $ getSumsE b
  negate = negateSum
  abs x | isNonNeg x = x
        | otherwise  = x * signum x
  signum x = case normalSign x of
    Nothing -> error "Tried to take signum of a complex expression"
    Just n  -> fromInteger n
  fromInteger n = case toList $ factorize n of
    [] -> zero
    (x : xs) -> SumsE (fromInteger n) $ L [ProdE (fromInteger n) (x :| L xs)]



instance (Ord t, Ord v) => Num (NormalE t v) where
  a + b = mapExpState (\s -> s <> (expState a + expState b)) $ map2Sums (+) a b
  a * b = mapExpState (\s -> s <> (expState a * expState b))
        $ map2Sums (*) a' b' - cab - prodByNonNegSumUnchecked ca b - prodByNonNegSumUnchecked cb a
    where
      ca | isSingleSum b = 0
         | otherwise     = capConstant a
      cb | isSingleSum a = 0
         | otherwise     = capConstant b
      cab = toNormalE $ ca*cb
      a' = a + toNormalE ca
      b' = b + toNormalE cb
  negate x = mapExpState (\s -> s <> negate (expState x)) $ f x
    where
      f = foldl1 neMax . fmap g . getMinsE . getNormalE
      g = foldl1 neMin . fmap (toNormalE . negate) . getMaxsE
  abs a
    | isNonNeg a = a
    | isZero a = a
    | isNonPos a = negate a
    | otherwise = mapExpState (\s -> s <> abs (expState a)) $ neMax a (negate a)
  signum a
    | isZero a = 0
    | isSignOne a = a
    | isNonNeg a && isNonZero a = 1
    | isNonPos a && isNonZero a = -1
    | otherwise = mapExpState (\s -> s <> signum (expState a)) $ neMin x y
    where
      x = neMax 1 (negate a)
      y = neMax (-1) a
  fromInteger = toNormalE . (fromInteger :: Integer -> SumsE None t v)

instance (Ord t, Ord v) => Fractional (SumsE None t v) where
  recip s
    | Just r <- asRational s
    , r /= 0 = fromRational (recip r)
  recip (SumsE _ (L [])) = error "Division by zero"
  recip (SumsE _ (L [ProdE _ xs])) = singleProdAsSums $ prodE (f <$> xs)
    where
      f (PowU _ a b) = powU a (negateSum b)
      f (PowS _ a b) = powS a (negateSum b)
  recip (SumsE s (L (x1:x2:xs))) = singlePowAsSums $ powS (SumsE s (x1 :| x2 :| L xs)) (unitAsSums UMinusOne)
  fromRational r = case toList $ factorizeR r of
    [] -> zero
    (x : xs) -> SumsE (fromRational r) $ L [ProdE (fromRational r) (x :| L xs)]

negateSum :: forall n t v . (Ord t, Ord v) => SumsE n t v -> SumsE n t v
negateSum s = SumsE (negate $ expState s) $ neg <$> getSumsE s
    where
      neg :: ProdE t v -> ProdE t v
      neg (ProdE st x) = ProdE (negate st) (negPows x)
      negPows :: AtLeast One (PowE t v) -> AtLeast One (PowE t v)
      negPows (PowU st UMinusOne p :| L ps)
        | isZero p || isEven p = powU UMinusOne one :| L ps
        | isSignOne p  || isOdd p = case ps of
            [] -> powU UOne zero :| L []
            x:xs -> x :| L xs
        | otherwise = PowU (negate st) UMinusOne (p + 1) :| L ps
      negPows (PowU _ UOne _ :| L []) = pure $ powU UMinusOne one
      negPows (PowU _ UOne _ :| L (x:xs)) = negPows (x :| L xs)
      negPows (p :| L []) = p :| L [powU UMinusOne one]
      negPows (p :| L (x : xs)) = p :| L (toList $ negPows $ x :| L xs)

-- | Check if there are no minimums or maximums
isSingleSum :: NormalE t v -> Bool
isSingleSum (NormalE (MinsE _ (MaxsE _ (_ :| L []) :| L []))) = True
isSingleSum _ = False

-- | Multiply the expression by a non-negative expression that does not contain MinsE or MaxsE
prodByNonNegSumUnchecked :: (Ord t, Ord v) => SumsE None t v -> NormalE t v -> NormalE t v
prodByNonNegSumUnchecked s
  | isZero s  = const 0
  | otherwise = mapMonoSums (toNormalE . (s*))


map2Sums
  :: (Ord t, Ord v)
  => (SumsE None t v -> SumsE None t v -> SumsE None t v)
  -> NormalE t v
  -> NormalE t v
  -> NormalE t v
map2Sums f = map2Maxs g
  where g a b = (\(MaxsE s v) -> MaxsE s (sortDesc v))
              $ foldl1 (<>) $ (\x -> maxsE . pure . f x) <$> getMaxsE a <*> getMaxsE b
  -- where g a b = maxsE . flattenDesc $ (\x -> f x <$> getMaxsE b) <$> getMaxsE a

map2Maxs
  :: (Ord t, Ord v)
  => (MaxsE t v -> MaxsE t v -> MaxsE t v)
  -> NormalE t v
  -> NormalE t v
  -> NormalE t v
map2Maxs f = map2Mins g
  where g a b = (\(MinsE s v) -> MinsE s (sortDesc v))
              $ foldl1 (<>) $ (\x -> minsE . pure . f x) <$> getMinsE a <*> getMinsE b
  -- where g a b = minsE . flattenDesc $ (\x -> f x <$> getMinsE b) <$> getMinsE a

map2Mins
  :: (MinsE t v -> MinsE t v -> MinsE t v)
  -> NormalE t v
  -> NormalE t v
  -> NormalE t v
map2Mins f a b = NormalE $ f (getNormalE a) (getNormalE b)


neMax :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
neMax a b = mapExpState (\s -> s <> (expState a `esMax` expState b)) $ map2Maxs (<>) a b

neMin :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
neMin a b = mapExpState (\s -> s <> (expState a `esMin` expState b)) $ map2Mins (<>) a b

neMod :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
neMod a b = case (asRational a, asRational b) of
  (Nothing, Nothing) -> toNormalE $ UMod a b
  (Just a', Nothing) -> toNormalE $ UMod (toNormalE $ fromR a') b
  (Nothing, Just b') -> toNormalE $ UMod a (toNormalE $ fromR b')
  (Just a', Just b') -> case modR a' b' of
    Just r -> toNormalE $ fromR r
    Nothing -> toNormalE $ UMod (toNormalE $ fromR a') (toNormalE $ fromR b')
  where
    fromR :: (Ord t, Ord v) => Rational -> SumsE None t v
    fromR = fromRational

neDiv :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
neDiv a b
  = mapSumsOrderly (toNormalE . (`uDiv` b)) a


uDiv :: (Ord t, Ord v) => SumsE None t v -> NormalE t v -> SumsE None t v
uDiv a b = case (asRational a, asRational b) of
  (Nothing, Nothing) -> unitAsSums $ UDiv a b
  (Just a', Nothing) -> unitAsSums $ UDiv (fromRational a') b
  (Nothing, Just b') -> unitAsSums $ UDiv a (toNormalE $ fromR b')
  (Just a', Just b') -> case divR a' b' of
    Just r -> fromInteger r
    Nothing -> unitAsSums $ UDiv (fromRational a') (toNormalE $ fromR b')
  where
    fromR :: (Ord t, Ord v) => Rational -> SumsE None t v
    fromR = fromRational

neLog2 :: (Ord v, Ord t) => NormalE t v -> NormalE t v
neLog2 = NormalE . foldl1 (<>) . fmap (minsE . pure . maxsLog2) . getMinsE . getNormalE

maxsLog2 :: (Ord v, Ord t) => MaxsE t v -> MaxsE t v
maxsLog2 p
  | Just r <- asRational p = case log2R r of
      Nothing -> maxsE . pure . unitAsSums . ULog2 . maxsE . pure $ fromRational r
      Just i -> maxsE . pure $ fromInteger i
  | otherwise = maxsE . pure . unitAsSums $ ULog2 p


nePow :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
nePow a b
  | isZero b                 = 1
  | isZero a && isNonZero b  = 0
  | isSingleSum a && isSingleSum b = map2Sums powSums a b
nePow a (NormalE (MinsE _ (MaxsE _ (b :| L []) :| L []))) = powNeSums a b
nePow a b = mapSumsOrderly (powNeSums a) b
-- nePow a b = mapMonoSums g b - mapMonoSums f b
--   where
--     f x = 3 * toNormalE x * powNeSums (abs a `neMax` 1) x
--     g x = f x + powNeSums a x


powNeSums :: (Ord v, Ord t) => NormalE t v -> SumsE None t v -> NormalE t v
powNeSums (NormalE (MinsE _ (MaxsE _ (a :| L []) :| L []))) b = toNormalE (powSums a b)
powNeSums a b
  | isZero b = 1
  | isZero a && isNonZero b = 0
  | otherwise = mapSumsOrderly (toNormalE . (`powSums` b)) a
  -- | isNonNeg b = let ca = capConstant a + 1 -- so that (x + ca >= 1 and x + ca > x)
  --                    f x = 3 * powSums (x + ca) (b + 1)
  --                    g x = f x + powSums x b
  --                in  mapMonoSums (toNormalE . g) a - mapMonoSums (toNormalE . f) a
  -- | isNonPos b = let f x = x
  --                    g x = f x + powSums x b
  --                 in mapMonoSums (toNormalE . g) a - mapMonoSums (toNormalE . f) a
  -- | otherwise  = let cb = capConstant $ toNormalE b
  --                    setNonNeg = mapExpState $ \s -> s { _isNonNeg = True, _isNonPos = _isZero s}
  --                    setNonPos = mapExpState $ \s -> s { _isNonPos = True, _isNonNeg = _isZero s}
  --                in powNeSums a (setNonNeg $ b + cb) * powNeSums a (setNonPos $ negate cb)

-- | Take @SumE@ expression into a power of @SumE@ expression
powSums :: (Ord v, Ord t) => SumsE None t v -> SumsE None t v -> SumsE None t v
powSums a b | SumsE _ (L []) <- b     = 1
            | isZero b                = 1
            | isOne b                 = a
            | isOne a                 = 1
            | isNonNeg b && isNonZero b && isZero a = 0
powSums (SumsE _ (L [a])) b = powProd a b
powSums a'@(SumsE sa' (L (a1 : a2 : as))) b'@(SumsE sb' (L (b : bs)))
  |
    -- expand sum if the power is a non-negative constant
    (c, SumsE _ (L [])) <- unconstSumsE b'
  , c >= 1 = let cf = floor c :: Integer
                 cr = c - fromInteger cf
                 ac' = a' ^ cf
             in case factorizeR cr of
                  L [] -> ac'
                  L (f:fs) -> ac' * powSums a' (singleProdAsSums $ prodE (f :| L fs))
  | otherwise = singlePowAsSums
  $ powS (SumsE sa' $ sortDesc $ a1 :| a2 :| L as) (SumsE sb' $ sortDesc $ b :| L bs)
-- in this weird case I don't know if the power is zero or not,
-- I have to use a Mod trick to workaround that
powSums (SumsE _ (L [])) b = r { getSumsEState = ExpState
      { _isZero     = isNonZero b
      , _isNonZero  = isZero b
      , _isSignOne  = isZero b
      , _isNonNeg   = True
      , _isNonPos   = isNonZero b
      , _isEven     = isNonZero b
      , _isOdd      = isZero b
      , _isWhole    = isWhole b
      , _isComplete = isComplete b
      }}
  where
    r = 1 - unitAsSums (UMod 1 (toNormalE $ MaxsE s $ sortDesc $ 1 + b :| L [1 - b]))
    s = ExpState
      { _isZero     = False
      , _isNonZero  = True
      , _isSignOne  = isZero b
      , _isNonNeg   = True
      , _isNonPos   = False
      , _isEven     = isOdd b
      , _isOdd      = isEven b
      , _isWhole    = isWhole b
      , _isComplete = isComplete b
      }

-- | Take signed @ProdE@ expression into a power of @SumE@ expression
powProd
  :: (Ord v, Ord t) => ProdE t v -> SumsE None t v -> SumsE None t v
powProd a b = go $ getProdE a
  where
    powPow (PowU _ u p)
      | isZero pb = 1
      | isZero u && isNonNeg pb && isNonZero pb && isComplete pb = 0
      | isSignOne u && isNonNeg u = 1
      | isSignOne u && isNonPos u && isEven pb = 1
      | isSignOne u && isNonPos u && isOdd pb = -1
      | Just ur <- asRational u
      , Just pr <- asRational pb
      , Just r <- powR ur pr = fromRational r
      | otherwise = singlePowAsSums $ PowU (esPow (expState u) (expState pb)) u pb
      where
        pb = p * b
    powPow (PowS _ s p)
      | isZero pb = 1
      | isZero s' && isNonNeg pb && isNonZero pb  && isComplete pb = 0
      | isSignOne s' && isNonNeg s' = 1
      | isSignOne s' && isNonPos s' && isEven pb = 1
      | isSignOne s' && isNonPos s' && isOdd pb = -1
      | Just ur <- asRational s
      , Just pr <- asRational pb
      , Just r <- powR ur pr = fromRational r
      | otherwise = case pb of
                      SumsE _ (L []) -> 0
                      SumsE es' (L (x : xs)) -> singlePowAsSums $ powS s (SumsE es' $ x :| L xs)
      where
        s' = noneSumsE s
        pb = sumsE (sortDesc $ L $ toList $ getSumsE p) * b
    go (x  :| L []       ) = powPow x
    go (x1 :| L (x2 : xs)) = powPow x1 * go (x2 :| L xs)

mapSumsOrderly :: (Ord v, Ord t) => (SumsE None t v -> NormalE t v) -> NormalE t v -> NormalE t v
mapSumsOrderly f (NormalE (MinsE _ (MaxsE _ (x :| L []) :| L []))) = f x
mapSumsOrderly f a = mapMonoSums (\x -> toNormalE (lim * x) + f x) a
                   - mapMonoSums (toNormalE . (lim *)) a
  where
    lim = 3 * neBound (mapMonoSums f a) * sumsE (pure $ neDelta a)

-- showNE :: NormalForm e => e t v -> String
-- showNE e = showSDocUnsafe $ ppr (unsafeCoerce (fromNormal $ toNormalE e) :: Exp F X)

-- data X
-- data F
-- instance Show X where
--   show _ = "x"
-- instance Show F where
--   show _ = "f"
-- instance Outputable X where
--   ppr = text . show
-- instance Outputable F where
--   ppr = text . show

mapMonoSums :: (Ord v, Ord t) => (SumsE None t v -> NormalE t v) -> NormalE t v -> NormalE t v
mapMonoSums f = foldl1 neMin . fmap (foldl1 neMax . fmap f . getMaxsE) . getMinsE . getNormalE

-- | Positive fractional value, not smaller than 1.
--   @(1/neDelta)@ is a lower bound on difference between components of an expression (and zero).
neDelta :: (Ord t, Ord v) => NormalE t v -> ProdE t v
neDelta  = foldl (\a' -> foldl (\a -> prodLcm a . deltaSumsE) a' . getMaxsE) oneP . getMinsE . getNormalE
  where
    deltaSumsE :: (Ord t, Ord v) => SumsE None t v -> ProdE t v
    deltaSumsE = foldl (\a -> prodLcm a . deltaProdE) oneP . getSumsE
    deltaProdE :: (Ord t, Ord v) => ProdE t v -> ProdE t v
    deltaProdE = foldl1 (<>) . fmap deltaPowE . getProdE
    deltaPowE :: (Ord t, Ord v) => PowE t v -> ProdE t v
    deltaPowE (PowU _ UOne _) = oneP
    deltaPowE (PowU _ UMinusOne _) = oneP
    deltaPowE (PowU _ x y)
      | isZero y = oneP
      | isNonNeg y = oneP
      | isNonPos y = prodE . pure $ powU x (negate y)
      | otherwise  = prodE . pure $ powU x (neBound $ toNormalE y)
    deltaPowE (PowS _ x y)
      | isZero y = oneP
      | isNonNeg y = oneP
      | isNonPos y = case neBound $ toNormalE $ negate (noneSumsE y) of
                      SumsE _ (L []) -> oneP
                      SumsE _ (L (p:ps)) -> prodE . pure $ powS x (sumsE (p :| L ps))
      | otherwise  = case neBound $ toNormalE $ noneSumsE y of
                      SumsE _ (L []) -> oneP
                      SumsE _ (L (p:ps)) -> prodE . pure $ powS x (sumsE (p :| L ps))
    


-- | MinMax-free upper bound on an expression (not smaller than 1).
--   Guaranteed to be not smaller than any of the components of the original expression.
neBound :: (Ord t, Ord v) => NormalE t v -> SumsE None t v
neBound = foldl (\a' -> foldl (\a -> cap2sums a . boundSumsE) a' . getMaxsE) 1 . getMinsE . getNormalE
  where
    boundSumsE :: (Ord t, Ord v) => SumsE None t v -> SumsE None t v
    boundSumsE a = case compare 0 <$> normalSign a of
      Just GT -> negate a
      Just EQ -> 0
      Just LT -> a
      Nothing -> sum $ boundProdE <$> getSumsE a
    boundProdE :: (Ord t, Ord v) => ProdE t v -> SumsE None t v
    boundProdE a = case compare 0 <$> normalSign a of
      Just GT -> negate $ singleProdAsSums a
      Just EQ -> 0
      Just LT -> singleProdAsSums a
      Nothing -> singleProdAsSums $ foldMap boundPowE $ getProdE a
    boundPowE :: (Ord t, Ord v) => PowE t v -> ProdE t v
    boundPowE a
      | isNonNeg a = a'
      | isNonPos a = a' <> ProdE (-1) (PowU (-1) UMinusOne 1 :| L [])
      | otherwise  = a' <> a'
      where
        a' = ProdE (expState a) (pure a)

-- | Get an expression, as small as possible,
--   such that if I add it to the original expression,
--   all its sums components are guaranteed to be not smaller than zero.
capConstant :: (Ord t, Ord v) => NormalE t v -> SumsE None t v
capConstant = foldl (\a' -> foldl (\a -> cap2sums a . capSumsE) a' . getMaxsE) 0 . getMinsE . getNormalE
  where
    capSumsE :: (Ord t, Ord v) => SumsE None t v -> SumsE None t v
    capSumsE a = case compare 0 <$> normalSign a of
      Just GT -> negate a
      Just EQ -> 0
      Just LT -> 0
      Nothing -> sum $ capProdE <$> getSumsE a
    capProdE :: (Ord t, Ord v) => ProdE t v -> SumsE None t v
    capProdE a = case compare 0 <$> normalSign a of
      Just GT -> negate $ singleProdAsSums a
      Just EQ -> 0
      Just LT -> 0
      Nothing -> singleProdAsSums $ foldMap capPowE $ getProdE a
    capPowE :: (Ord t, Ord v) => PowE t v -> ProdE t v
    capPowE a
      | isNonNeg a = a'
      | isNonPos a = a' <> ProdE (-1) (PowU (-1) UMinusOne 1 :| L [])
      | otherwise  = a' <> a'
      where
        a' = ProdE (expState a) (pure a)


prodLcm :: (Ord t, Ord v) => ProdE t v -> ProdE t v -> ProdE t v
prodLcm x y = atleast1 $ go (toList $ getProdE x) (toList $ getProdE y)
  where
    atleast1 :: (Ord t, Ord v) => [PowE t v] -> ProdE t v
    atleast1 (a:as) = prodE $ sortDesc $ a :| L as
    atleast1 []     = oneP
    go :: (Ord t, Ord v) => [PowE t v] -> [PowE t v] -> [PowE t v]
    go [] xs = filter (not . isOne) xs
    go xs [] = filter (not . isOne) xs
    go (a@(PowU _ ua pa) : as) (b@(PowU _ ub pb) : bs) = case compare ua ub of
      GT -> a : go as (b:bs)
      EQ -> powU ua (cap2sums pa pb) : go as bs
      LT -> b : go (a:as) bs
    go (a@(PowS _ sa (SumsE past (pa1 :| pas))) : as)
      (b@(PowS _ sb (SumsE pbst (pb1 :| pbs))) : bs) = case compare sa sb of
      GT -> a : go as (b:bs)
      EQ -> case cap2sums (SumsE past (cons pa1 pas)) (SumsE pbst (cons pb1 pbs)) of
        SumsE _   (L [])     -> go as bs
        SumsE pst (L (p:ps)) -> powS sa (SumsE pst (p :| L ps)) : go as bs
      LT -> b : go (a:as) bs
    go (a:as) (b:bs)
      | a >= b    = a : go as (b:bs)
      | otherwise = b : go (a:as) bs

cap2sums :: (Ord t, Ord v) => SumsE None t v -> SumsE None t v -> SumsE None t v
cap2sums x y = case compare 0 <$> normalSign (regroupSumsE $ x - y) of
  Just GT -> y
  Just EQ -> x
  Just LT -> x
  Nothing -> sumsE $ L $ go (toList $ getSumsE x) (toList $ getSumsE y)
  where
    go :: (Ord t, Ord v) => [ProdE t v] -> [ProdE t v] -> [ProdE t v]
    go [] xs = xs
    go xs [] = xs
    go (a:as) (b:bs) = case compare pa pb of
      GT -> a : go as (b:bs)
      EQ -> case factorizeR c of
        L [] -> go as bs
        L (f:fs) -> (pa <> prodE (f :| L fs)) : go as bs
      LT -> b : go (a:as) bs
      where
        (ca, pa) = unscaleProdE a
        (cb, pb) = unscaleProdE b
        c = max ca cb