{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
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
  , Signed(..)
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
  , neMin, neMax, nePow
  , minsE, maxsE, sumsE, prodE, powU, powS
  , map2Maxs, map2Mins, map2Sums, inverseMM
  -- , asNatural
  , factorize
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

import           Data.Either                    ( partitionEithers )
import           Data.List                      ( group )
import           Numeric.Dimensions.Plugin.AtLeast
import           Numeric.Dimensions.Plugin.SolveNat.Exp
import Numeric.Dimensions.Plugin.SolveNat.ExpState
import           Numeric.Natural
import           Outputable              hiding ( (<>) )

-- import Debug.Trace

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
data SumsE n t v = SumsE { getSumsEState :: ExpState, getSumsE :: AtLeast n (Signed (ProdE t v)) }
  deriving (Show, Foldable)

-- | A signed value with a weird Ord instance:
--   we first compare by magnitude, and then check the signs.
data Signed a = Pos { getAbs :: a } | Neg { getAbs :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

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

-- | Primitive values and irreducible functions.
data UnitE t v
  = UOne
    -- ^ Number @1@, which is not considered prime here
  | UN Prime
    -- ^ Prime numbers. Note, no zeroes possible!
  | UDiv (SumsE None t v) (MaxsE t v)
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

-- | Zero factorizes into an empty list of powers,
--   the rest of the Naturals factorize into a non-empty list of powers.
--
--   It does not include 1 in the list as long as there are any other numbers!
--
--   The resulting list of powers is decreasing in the magnitude of the base.
factorize :: Natural -> AtLeast None (PowE t v)
factorize 0 = mempty
factorize 1 = L [unitAsPow UOne]
factorize x = L . reverse . fmap asPow . group $ factor x 2
  where
    asPow :: [Natural] -> PowE t v
    asPow ns =
      let L ps = factorize $ fromIntegral p
          p    = length ns
          n    = head ns
      in  PowU (fromIntegral n) (UN $ Prime n) $ case ps of
            [] -> zero
            (p' : ps') ->
              singleProdAsSums $ Pos $ ProdE (fromIntegral p) $ p' :| L ps'
    factor :: Natural -> Natural -> [Natural]
    factor n k | k * k > n             = [n]
               | (n', 0) <- divMod n k = k : factor n' k
               | otherwise             = factor n (k + 1)

eStateSigned :: NormalForm a => Signed (a t v) -> ExpState
eStateSigned a = s
  { _isNonNeg = if isPositive a then _isNonNeg s else _isNonPos s
  , _isNonPos = if isPositive a then _isNonPos s else _isNonNeg s
  }
  where s = expState $ getAbs a

isPositive :: Signed a -> Bool
isPositive Pos{} = True
isPositive Neg{} = False


minsE :: AtLeast One (MaxsE t v) -> MinsE t v
minsE xxs@(x :| L xs) = MinsE (foldl (\s -> esMin s . expState) (expState x) xs) xxs

maxsE :: AtLeast One (SumsE None t v) -> MaxsE t v
maxsE xxs@(x :| L xs) = MaxsE (foldl (\s -> esMax s . expState) (expState x) xs) xxs

sumsE :: AtLeast n (Signed (ProdE t v)) -> SumsE n t v
sumsE xs = SumsE (sum $ fmap eStateSigned xs) xs

prodE :: AtLeast One (PowE t v) -> ProdE t v
prodE xxs@(x :| L xs) = ProdE (foldl (\s -> (s *) . expState) (expState x) xs) xxs

powU :: UnitE t v -> SumsE None t v -> PowE t v
powU a b = PowU (esPow (expState a) (expState b)) a b

powS :: SumsE Two t v -> SumsE One t v -> PowE t v
powS a b = PowS (esPow (expState a) (expState b)) a b

-- | Split product into two parts: constant and irreducible prod.
--   Removes all constant components from the result prod if they are evaluatable
--   (i.e. an integer in negative power is not evaluatable to an integer)
unscaleProdE :: Signed (ProdE t v) -> (Integer, ProdE t v)
unscaleProdE p = atleast1
  <$> go (if isPositive p then 1 else -1) (toList $ getProdE $ getAbs p)
  where
    atleast1 :: [PowE t v] -> ProdE t v
    atleast1 (a : as) = prodE $ a :| L as
    atleast1 []       = oneP
    go :: Integer -> [PowE t v] -> (Integer, [PowE t v])
    go c [] = (c, [])
    go c (PowU _ u@(UN (Prime n)) s : xs) = case unshiftPosSumsE s of
      (k, SumsE _ (L [])) -> go (c * toInteger n ^ k) xs
      (k, s'            ) -> (PowU ps u s' :) <$> go (c * toInteger n ^ k) xs
                              where
                                ps = esPow (fromIntegral n) (expState s')
    go c (x : xs) = (x :) <$> go c xs

-- | Take the positive constant part of SumsE.
--   The constant component of the output SumsE is guaranteed to be non-positive.
unshiftPosSumsE :: SumsE n t v -> (Natural, SumsE None t v)
unshiftPosSumsE s = toPos $ go 0 (toList $ getSumsE s)
  where
    toPos (n, xs) = if n >= 0
      then (fromInteger n, sumsE $ L xs)
      else (0, SumsE (getSumsEState s) $ L $ toList $ getSumsE s)
    go :: Integer -> [Signed (ProdE t v)] -> (Integer, [Signed (ProdE t v)])
    go c [] = (c, [])
    go c (p : ps) | isOne p'  = go (c + c') ps
                  | otherwise = (p :) <$> go c ps
      where (c', p') = unscaleProdE p

-- | Take the constant part of SumsE;
--   The returned SumsE is guaranteed to not have a constant component
--    (i.e. it is either empty or has unknown funs or vars inside).
unconstSumsE :: SumsE n t v -> (Integer, SumsE None t v)
unconstSumsE s = sumsE . L <$> go 0 (toList $ getSumsE s)
  where
    go :: Integer -> [Signed (ProdE t v)] -> (Integer, [Signed (ProdE t v)])
    go c [] = (c, [])
    go c (p : ps) | isOne p'  = go (c + c') ps
                  | otherwise = (p :) <$> go c ps
      where (c', p') = unscaleProdE p

singleProdAsSums :: Applicative (AtLeast n) => Signed (ProdE t v) -> SumsE n t v
singleProdAsSums p = SumsE (eStateSigned p) $ pure p

singlePowAsSums :: Applicative (AtLeast n) => PowE t v -> SumsE n t v
singlePowAsSums p = singleProdAsSums $ Pos (ProdE (expState p) $ pure p)

unitAsPow :: UnitE t v -> PowE t v
unitAsPow UOne = PowU 1 UOne zero
unitAsPow u    = PowU (expState u) u one

unitAsSums :: Applicative (AtLeast n) => UnitE t v -> SumsE n t v
unitAsSums = singlePowAsSums . unitAsPow

zero :: SumsE None t v
zero = SumsE 0 mempty

one :: Applicative (AtLeast n) => SumsE n t v
one = singleProdAsSums $ Pos oneP

oneP :: ProdE t v
oneP = ProdE 1 $ PowU 1 UOne zero :| mempty

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
regroupSumsE = go . toList . getSumsE
  where
    go :: (Ord t, Ord v) => [Signed (ProdE t v)] -> SumsE None t v
    go [] = zero
    go xs = foldl (+) (sumsE $ L xs') sums
      where
        (xs', sums) =
          partitionEithers $ map (toSingleProdOrSum . splitExtraSums bases) xs
        bases = foldMap (foldMap addBase . getProdE . getAbs) xs []
    -- add only PowS sums;
    -- terms are ordered descending,
    --   by base first, then by power
    addBase :: (Ord t, Ord v) => PowE t v -> PowsS t v -> PowsS t v
    addBase (PowU _ _ _) xs              = xs
    addBase (PowS _ a p) []              = [(a, p)]
    addBase (PowS _ a p) (x@(b, q) : xs) = case compare a b of
      GT -> (a, p) : x : xs
      EQ -> case unconstSumsE (noneSumsE p - noneSumsE q) of
        (c, s) | isZero s  -> (b, if c < 0 then p else q) : xs
               | p > q     -> (a, p) : x : xs
               | otherwise -> x : addBase (powS a p) xs
      LT -> x : addBase (powS a p) xs
    subBase
      :: (Ord t, Ord v)
      => PowsS t v
      -> PowE t v
      -> ([(SumsE Two t v, Natural)], PowE t v)
    subBase bases (PowS _ a p)
      | (c, _) : _ <-
        filter (isZero . snd)
        . map (\(_, q) -> unconstSumsE (noneSumsE p - noneSumsE q))
        $ filter ((a ==) . fst) bases
      , c > 0
      = let ps' = getSumsE $ noneSumsE p - fromInteger c
            p'  = case ps' of
              L [] ->
                error "regroupSumsE/subBase panic: expected non-empty SumsE!"
              L (x : xs) -> sumsE (x :| L xs)
        in  ([(a, fromInteger c)], powS a p')
    subBase _ x = ([], x)

    -- extracts terms like (a + b) ^ Nat from a product expression,
    -- such that all complex sum expressions inside match the bases list
    splitExtraSums
      :: (Ord t, Ord v)
      => PowsS t v
      -> Signed (ProdE t v)
      -> ([(SumsE Two t v, Natural)], Signed (ProdE t v))
    splitExtraSums bases =
      traverse $ fmap prodE . traverse (subBase bases) . getProdE

    toSingleProdOrSum
      :: (Ord t, Ord v)
      => ([(SumsE Two t v, Natural)], Signed (ProdE t v))
      -> Either (Signed (ProdE t v)) (SumsE None t v)
    toSingleProdOrSum ([], p) = Left p
    toSingleProdOrSum (xs, p) = Right
      $ foldl (\s (x, n) -> s * noneSumsE x ^ n) (singleProdAsSums p) xs


type PowsS t v = [(SumsE Two t v, SumsE One t v)]


instance NormalForm NormalE where
  fromNormal = fromNormal . getNormalE
  toNormalE  = id
  expState    = expState . getNormalE
  validate   = validate . getNormalE

instance NormalForm MinsE where
  fromNormal (MinsE _ (e :| L []))
    = fromNormal e
  fromNormal (MinsE s (e1 :| L (e2:es)))
    = Min (fromNormal e1) (fromNormal (MinsE s (e2 :| L es)))
  toNormalE = NormalE
  expState = getMinsEState
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
  expState = getMaxsEState
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
  fromNormal x = case x' of
      []            -> e0
      (Pos p : ps)
        | c == 0    -> foldl f (fromNormal p) ps
        | otherwise -> foldl f e0 x'
      (Neg _ : _)   -> foldl f e0 x'
    where
      e0 = if c >= 0 then N (fromInteger c) else 0 :- N (fromInteger $ negate c)
      (c, SumsE _ (L x')) = unconstSumsE x
      f :: Exp t v -> Signed (ProdE t v) -> Exp t v
      f e (Pos p) = e :+ fromNormal p
      f e (Neg p) = e :- fromNormal p
  toNormalE x = toNormalE $ MaxsE (expState x) (pure $ noneSumsE x)
  expState = getSumsEState
  validate x = foldMap (validate . getAbs) xl <> decreasing ps <> maxOneConst
    where
      xl = toList $ getSumsE x
      ps = snd . unscaleProdE <$> xl
      decreasing :: (Ord t, Ord v) => [ProdE t v] -> Validate
      decreasing []  = Ok
      decreasing [_] = Ok
      decreasing (x1:xs@(x2:_))
        | x1 <= x2 = Invalid $ pure $ hsep
          ["Components of", ppr x, "are not descending:", ppr $ map getAbs xl]
        | otherwise = decreasing xs
      maxOneConst
        | length (filter (oneP ==) ps) > 1 = Invalid $ pure $ hsep
          [ppr x, " has more than one const:", ppr $ map getAbs xl]
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
  toNormalE = toNormalE . sumsE . L . (:[]) . Pos
  expState = getProdEState
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
  expState (PowU s _ _) = s
  expState (PowS s _ _) = s
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
        | (_, SumsE _ (L [])) <- unshiftPosSumsE b = Invalid $ pure $ hsep
          ["Power of SumE must never be a bare positive constant:", ppr x]
        | otherwise = Ok

instance NormalForm UnitE where
  fromNormal  UOne      = N 1
  fromNormal (UN p)     = N (asNatural p)
  fromNormal (UDiv a b) = fromNormal a `Div` fromNormal b
  fromNormal (UMod a b) = fromNormal a `Mod` fromNormal b
  fromNormal (ULog2 a)  = Log2 (fromNormal a)
  fromNormal (UF t)     = F t
  fromNormal (UV v)     = V v
  toNormalE = toNormalE . unitAsPow
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
    , _isComplete = True
    }

  validate UOne = Ok
  validate up@(UN (Prime p))
    | factorize p == L [unitAsPow up] = Ok
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

instance Ord a => Ord (Signed a) where
  compare a b = compare (getAbs a) (getAbs b)
             <> compare (isPositive a) (isPositive b)

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
  x <> y = MinsE (esMin (expState x) (expState y)) (mergeDesc (getMinsE x) (getMinsE y))

-- take maximum
instance (Ord t, Ord v) => Semigroup (MaxsE t v) where
  x <> y = MaxsE (esMax (expState x) (expState y)) (mergeDesc (getMaxsE x) (getMaxsE y))

-- add up
instance (Ord t, Ord v) => Semigroup (SumsE None t v) where
  x <> y = SumsE (expState x + expState y) $ L $ go (toList $ getSumsE x) (toList $ getSumsE y)
    where
      go :: [Signed (ProdE t v)] -> [Signed (ProdE t v)] -> [Signed (ProdE t v)]
      go [] xs = xs
      go xs [] = xs
      go (a:as) (b:bs) = case compare pa pb of
          GT -> a : go as (b:bs)
          EQ -> case factorize (fromInteger $ abs c) of
            L [] -> go as bs
            L (f:fs)
              | p <- pa <> prodE (f :| L fs)
                -> (if c >= 0 then Pos p else Neg p) : go as bs
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
      atleast1 (a:as) = ProdE (expState x * expState y) $ a :| L as
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
  a - b = a + negate b
  a * b = foldr (\x -> (+) (sumsE (L $ times x <$> ys))) zero (getSumsE a)
    where
      ys = toList $ getSumsE b
      times :: Signed (ProdE t v) -> Signed (ProdE t v) -> Signed (ProdE t v)
      times x y = sig $ getAbs x <> getAbs y
        where
          sig = if isPositive x == isPositive y then Pos else Neg
  negate s = SumsE (negate $ expState s) $ neg <$> getSumsE s
    where
      neg :: Signed a -> Signed a
      neg (Pos x) = Neg x
      neg (Neg x) = Pos x
  abs (SumsE s (L []))      = SumsE (abs s) (L [])
  abs (SumsE s (L [Neg x])) = SumsE (abs s) (L [Pos x])
  abs (SumsE s (L [Pos x])) = SumsE (abs s) (L [Pos x])
  abs x                   = x * signum x
  signum x = case normalSign x of
    Nothing -> error "Tried to take signum of a complex expression"
    Just n  -> fromInteger n
  fromInteger n = case toList . factorize . fromInteger $ abs n of
    [] -> zero
    (x : xs) ->
      SumsE (fromInteger n)
        $  L
        $  (: [])
        $  sig
        $  ProdE (fromInteger $ abs n)
        $  x
        :| L xs
    where sig = if n >= 0 then Pos else Neg


instance (Ord t, Ord v) => Num (NormalE t v) where
  (+) = map2Sums (+)
  a * b = case (good a, good b) of
    (Just ga, Just gb) -> map2Sums (*) (mInverseMM gb a) (mInverseMM ga b)
    (Just True, Nothing) ->
      map2Sums (*) a bPos + map2Sums (*) (inverseMM a) bNeg
    (Just False, Nothing) ->
      map2Sums (*) a bNeg + map2Sums (*) (inverseMM a) bPos
    (Nothing, Just True) ->
      map2Sums (*) aPos b + map2Sums (*) aNeg (inverseMM b)
    (Nothing, Just False) ->
      map2Sums (*) aNeg b + map2Sums (*) aPos (inverseMM b)
    (Nothing, Nothing) -> sum
      [ map2Sums (*) aPos bPos
      , map2Sums (*) aNeg (inverseMM bPos)
      , map2Sums (*) (inverseMM aPos) bNeg
      , map2Sums (*) (inverseMM aNeg) (inverseMM bNeg)
      ]
    where
      aPos = neMax a 0
      aNeg = neMin a 0
      bPos = neMax b 0
      bNeg = neMin b 0
      mInverseMM :: Bool -> NormalE t v -> NormalE t v
      mInverseMM True  = id
      mInverseMM False = inverseMM
      good :: NormalE t v -> Maybe Bool
      good x | isNonNeg x = Just True
             | isNonPos x = Just False
             | otherwise  = Nothing
  negate = inverseMM . neg
    where
      neg :: NormalE t v -> NormalE t v
      neg = NormalE . minsE . fmap (maxsE . fmap negate . getMaxsE) . getMinsE . getNormalE
  abs a
    | isNonNeg a = a
    | isZero a = a
    | isNonPos a = negate a
    | otherwise = neMax a (negate a)
  signum a
    | isZero a = 0
    | isSignOne a = a
    | isNonNeg a && isNonZero a = 1
    | isNonPos a && isNonZero a = -1
    | otherwise = neMin x y
    where
      x = neMax 1 (negate a)
      y = neMax (-1) a
  fromInteger = toNormalE . (fromInteger :: Integer -> SumsE None t v)

inverseMM :: (Ord t, Ord v) => NormalE t v -> NormalE t v
inverseMM = NormalE . go . getNormalE
  where
    go :: (Ord t, Ord v) => MinsE t v -> MinsE t v
    go (MinsE _ (MaxsE _ xs :| L [])) = minsE ((maxsE . pure) <$> xs)
    go (MinsE _ (maxs1 :| L (maxs2 : maxS))) = minsE ((<>) <$> a <*> b)
      where
        MinsE _ a = go $ minsE (pure maxs1)
        MinsE _ b = go $ minsE (maxs2 :| L maxS)

map2Sums
  :: (Ord t, Ord v)
  => (SumsE None t v -> SumsE None t v -> SumsE None t v)
  -> NormalE t v
  -> NormalE t v
  -> NormalE t v
map2Sums f = map2Maxs g
  where g a b = maxsE . flattenDesc $ (\x -> f x <$> getMaxsE b) <$> getMaxsE a

map2Maxs
  :: (Ord t, Ord v)
  => (MaxsE t v -> MaxsE t v -> MaxsE t v)
  -> NormalE t v
  -> NormalE t v
  -> NormalE t v
map2Maxs f = map2Mins g
  where g a b = minsE . flattenDesc $ (\x -> f x <$> getMinsE b) <$> getMinsE a

map2Mins
  :: (MinsE t v -> MinsE t v -> MinsE t v)
  -> NormalE t v
  -> NormalE t v
  -> NormalE t v
map2Mins f a b = NormalE $ f (getNormalE a) (getNormalE b)


neMax :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
neMax = map2Maxs (<>)

neMin :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
neMin = map2Mins (<>)

nePow :: (Ord t, Ord v) => NormalE t v -> NormalE t v -> NormalE t v
nePow a b
  | isNonNeg a && isNonNeg b = map2Sums powSums a b
  | isNonNeg a && isNonPos b = map2Sums powSums (inverseMM a) b
  | isNonPos a && isNonNeg b && isEven b = map2Sums powSums (negate a) (inverseMM b)
  | isNonPos a && isNonNeg b && isOdd  b = map2Sums powSums a b
  | isNonPos a && isNonPos b && isEven b = map2Sums (powSums . negate) a b
  | isNonPos a && isNonPos b && isOdd  b = map2Sums powSums (inverseMM a) b
  | isNonNeg a = error "Uknown state of b"
  | isNonPos a = error "Uknown state of b"
  | otherwise = 
    let NormalE ap' = neMax 0 a
        ap = NormalE $ ap' { getMinsEState = (expState ap') { _isNonNeg = True, _isNonPos = isZero ap' } }
        NormalE am' = neMin 0 a
        am = NormalE $ am' { getMinsEState = (expState am') { _isNonPos = True, _isNonNeg = isZero am' } }
        sb | isZero b = 1
           | isNonZero b = 0
           | otherwise = 1 - neMin 1 (abs b)
    in nePow ap b + nePow am b - sb 

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
    (c, SumsE _ (L [])) <- unshiftPosSumsE b' = a' ^ c
  | otherwise = singlePowAsSums
  $ powS (SumsE sa' $ a1 :| a2 :| L as) (SumsE sb' $ b :| L bs)
-- in this weird case I don't know if the power is zero or not,
-- I have to use a Mod trick to workaround that
powSums (SumsE _ (L [])) b =
  1 - unitAsSums (UMod 1 (toNormalE $ MaxsE s $ 1 + b :| L [1 - b]))
  where
    s = ExpState
      { _isZero     = False
      , _isNonZero  = True
      , _isSignOne  = isZero b
      , _isNonNeg   = True
      , _isNonPos   = False
      , _isEven     = isOdd b
      , _isOdd      = isEven b
      , _isComplete = isComplete b
      }

-- | Take signed @ProdE@ expression into a power of @SumE@ expression
powProd
  :: (Ord v, Ord t) => Signed (ProdE t v) -> SumsE None t v -> SumsE None t v
powProd a b = sig $ go $ getProdE $ getAbs a
  where
    sig x | isPositive a = x
          | otherwise    = x - 2 * x * unitAsSums (UMod (toNormalE b) 2)
    powPow (PowU _ u p) = let pb = p * b in singlePowAsSums $ PowU (esPow (expState u) (expState pb)) u pb
    powPow (PowS es s p) = case (SumsE es $ L $ toList $ getSumsE p) * b of
      SumsE _ (L []) -> 0
      p'@(SumsE es' (L (x : xs)))
        | isOne p'  -> SumsE es' $ L $ toList $ getSumsE s
        | otherwise -> singlePowAsSums $ powS s (SumsE es' $ x :| L xs)
    go (x  :| L []       ) = powPow x
    go (x1 :| L (x2 : xs)) = powPow x1 * go (x2 :| L xs)