{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Numeric.Dimensions.Plugin.SolveNat.NormalForm
  ( NormalForm (..)
  , Validate (..)
    -- * Parts of the normal form
  , NormalE (..), MinsE (..), MaxsE (..)
  , SumsE (..), ProdE (..), PowE (..), UnitE (..)
  , Prime ()
    -- * Extra convenience functions
  , asNatural, factorize
  , minMax, unit, unitAsSums, unconstSumsE, powSums
  ) where

import Data.List                              (group)
import Numeric.Dimensions.Plugin.AtLeast
import Numeric.Dimensions.Plugin.SolveNat.Exp
import Numeric.Natural
import Outputable                             hiding ((<>))

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
  -- | A helper to check if the value is guaranteed to be zero
  isZero     :: e t v -> Bool
  -- | A helper to check if the value is guaranteed to be one
  isOne      :: e t v -> Bool
  -- | Check the invariants that must hold by construction.
  --   Do not check arithmetic errors, such as division by zero, etc.
  validate   :: (Ord t, Ord v, Outputable t, Outputable v) => e t v -> Validate

-- | Report broken invariants
data Validate = Invalid (AtLeast One SDoc) | Ok

-- | A plain wrapper that adds nothing to the expression.
newtype NormalE t v = NormalE { getNormalE :: MinsE t v }
  deriving (Eq, Ord, Show, Foldable)

-- | A minimum among a list of expressions is the outermost level.
newtype MinsE t v = MinsE { getMinsE :: AtLeast One (MaxsE t v) }
  deriving (Eq, Ord, Show, Foldable)

-- | A maximum among a list of expressions is the second level.
newtype MaxsE t v = MaxsE { getMaxsE :: AtLeast One  (SumsE None t v) }
  deriving (Eq, Ord, Show, Foldable)

-- | An expression (empty of mins and maxs) is the sum of products.
--   It can be zero (empty list of products), or const (a single const product);
--   otherwise the list of products is sorted (descending).
--   None of the components have the same variable part.
newtype SumsE n t v = SumsE { getSumsE :: AtLeast n (Signed (ProdE t v)) }
  deriving (Eq, Ord, Show, Foldable)

-- | A signed value with a weird Ord instance:
--   we first compare by magnitude, and then check the signs.
data Signed a = Pos { getAbs :: a } | Neg { getAbs :: a }
  deriving (Eq, Show, Functor, Foldable)

-- | A product is a non-empty list of power expressions (@PowE@) sorted in
--   the descending order; none of the bases are equal.
--   Note, @ProdE@ is a positive value by construction (cannot be zero or negative).
newtype ProdE t v = ProdE { getProdE :: AtLeast One (PowE t v) }
  deriving (Eq, Ord, Show, Foldable)

-- | A power expression is either a unit or a non-trivial @SumE@ (base) in
--   the power of some @SumE@.
data PowE t v
  = PowU (UnitE t v) (SumsE None t v)
  | PowS (SumsE Two t v) (SumsE One t v)
  deriving (Eq, Ord, Show, Foldable)

-- | Primitive values and irreducible functions.
data UnitE t v
  = UN Prime
    -- ^ Prime numbers. Note, no zeroes possible!
  | UDiv  (SumsE None t v) (MaxsE t v)
    -- ^ Numerator is free of MaxsE and MinsE, but I cannot float out
    --   MaxsE from the denominator due to possible zeroes.
  | UMod  (NormalE t v) (NormalE t v)
    -- ^ Mod is a tricky thing, I can do almost nothing about it.
  | ULog2 (MaxsE t v)
    -- ^ Cannot float out MaxsE due to possible zeroes.
  | UF t
    -- ^ An irreducible type family
  | UV v
    -- ^ A type variable
  deriving (Eq, Ord, Show, Foldable)

-- | 1,2,3,5,7,11...
--
--   Note, I conside 1 a Prime here for convenience.
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
factorize 1 = L [unitAsPow $ UN (Prime 1)]
factorize x = L . reverse . fmap asPow . group $ factor x 2
  where
    asPow :: [Natural] -> PowE t v
    asPow ns = let L ps = factorize $ fromIntegral (length ns)
                   u  = UN $ Prime $ head ns
               in PowU u $ case ps of
                 []       -> zero
                 (p':ps') -> singleProdAsSums $ Pos $ ProdE $ p' :| L ps'
    factor :: Natural -> Natural -> [Natural]
    factor n k
      | k*k > n               = [n]
      | (n', 0) <- divMod n k = k : factor n' k
      | otherwise             = factor n (k + 1)

isPositive :: Signed a -> Bool
isPositive Pos{} = True
isPositive Neg{} = False

-- | Split product into two parts: constant and irreducible prod.
--   Removes all constant components from the result prod.
unscaleProdE :: Signed (ProdE t v) -> (Integer, ProdE t v)
unscaleProdE p =
  atleast1 <$> go (if isPositive p then 1 else -1) (toList $ getProdE $ getAbs p)
  where
    atleast1 :: [PowE t v] -> ProdE t v
    atleast1 (a:as) = ProdE $ a :| L as
    atleast1 []     = oneP
    go :: Integer -> [PowE t v] -> (Integer, [PowE t v])
    go c []                  = (c, [])
    go c (PowU u@(UN (Prime n)) s : xs) = case unshiftPosSumsE s of
        (k, SumsE (L [])) ->    go (c * toInteger n ^ k) xs
        (k, s')           -> (PowU u s' :) <$> go (c * toInteger n ^ k) xs
    go c (x : xs) = (x:) <$> go c xs

-- | Take the positive constant part of SumsE;
--   The returned SumsE is guaranteed to not have a constant component or have
--   negative constant value.
unshiftPosSumsE :: SumsE n t v -> (Natural, SumsE None t v)
unshiftPosSumsE s = toPos $ go 0 (toList $ getSumsE s)
  where
    toPos (n, xs) = if n >= 0 then (fromInteger n, SumsE $ L xs)
                              else (0, SumsE $ L $ toList $ getSumsE s)
    go :: Integer -> [Signed (ProdE t v)] -> (Integer, [Signed (ProdE t v)])
    go c [] = (c, [])
    go c (p:ps)
        | isOne p'  = go (c + c') ps
        | otherwise = (p:) <$> go c ps
      where
        (c', p') = unscaleProdE p

-- | Take the constant part of SumsE;
--   The returned SumsE is guaranteed to not have a constant component
--    (i.e. it is either empty or has unknown funs or vars inside).
unconstSumsE :: SumsE n t v -> (Integer, SumsE None t v)
unconstSumsE s = SumsE . L <$> go 0 (toList $ getSumsE s)
  where
    go :: Integer -> [Signed (ProdE t v)] -> (Integer, [Signed (ProdE t v)])
    go c [] = (c, [])
    go c (p:ps)
        | isOne p'  = go (c + c') ps
        | otherwise = (p:) <$> go c ps
      where
        (c', p') = unscaleProdE p

-- | Take @SumE@ expression into a power of @SumE@ expression
powSums :: (Ord v, Ord t)
        => SumsE None t v -> SumsE None t v -> SumsE None t v
powSums (SumsE (L [])) _ = 0
powSums _ (SumsE (L [])) = 1
powSums (SumsE (L [a])) b
  | isOne b   = SumsE (L [a])
  | otherwise = powProd a b
powSums (SumsE (L (a1:a2:as))) (SumsE (L (b:bs)))
  = singlePowAsSums $ PowS (SumsE $ a1 :| a2 :| L as)
                           (SumsE $ b  :| L bs)

-- | Take signed @ProdE@ expression into a power of @SumE@ expression
powProd :: (Ord v, Ord t)
        => Signed (ProdE t v) -> SumsE None t v -> SumsE None t v
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

minMax :: SumsE None t v -> NormalE t v
minMax x@(SumsE (L _)) = NormalE $ MinsE $ pure $ MaxsE $ pure x

unit :: UnitE t v -> NormalE t v
unit = minMax . unitAsSums

singleProdAsSums :: Applicative (AtLeast n) => Signed (ProdE t v) -> SumsE n t v
singleProdAsSums p = SumsE $ pure p

singlePowAsSums :: Applicative (AtLeast n) => PowE t v -> SumsE n t v
singlePowAsSums p = singleProdAsSums $ Pos (ProdE $ pure p)

unitAsPow :: UnitE t v -> PowE t v
unitAsPow u@(UN (Prime 1)) = PowU u zero
unitAsPow u                = PowU u one

unitAsSums :: Applicative (AtLeast n) => UnitE t v -> SumsE n t v
unitAsSums = singlePowAsSums . unitAsPow

zero :: SumsE None t v
zero = SumsE mempty

one :: Applicative (AtLeast n) => SumsE n t v
one = singleProdAsSums $ Pos oneP

oneP :: ProdE t v
oneP = ProdE $ PowU (UN (Prime 1)) zero :| mempty

instance Ord a => Ord (Signed a) where
  compare a b = compare (getAbs a) (getAbs b)
             <> compare (isPositive a) (isPositive b)

instance NormalForm NormalE where
  fromNormal = fromNormal . getNormalE
  isZero = isZero . getNormalE
  isOne  = isOne . getNormalE
  validate = validate . getNormalE

instance NormalForm MinsE where
  fromNormal (MinsE (e :| L []))
    = fromNormal e
  fromNormal (MinsE (e1 :| L (e2:es)))
    = Min (fromNormal e1) (fromNormal (MinsE (e2 :| L es)))
  isZero = (\(atLeastOneZero, allZeroOrOne) -> atLeastOneZero && allZeroOrOne)
         . foldr (\x (a, b) -> let is0 = isZero x
                               in  (a || is0, b && (is0 || isOne x) )
                 ) (False, True) . getMinsE
  isOne  = all isOne . getMinsE
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
  fromNormal (MaxsE (e :| L []))
    = fromNormal e
  fromNormal (MaxsE (e1 :| L (e2:es)))
    = Max (fromNormal e1) (fromNormal (MaxsE (e2 :| L es)))
  isZero = all isZero . getMaxsE
  isOne  = (\(atLeastOneOne, allZeroOrOne) -> atLeastOneOne && allZeroOrOne)
         . foldr (\x (a, b) -> let is1 = isOne x
                               in  (a || is1, b && (is1 || isZero x) )
                 ) (False, True) . getMaxsE
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
      (c, SumsE (L x')) = unconstSumsE x
      f :: Exp t v -> Signed (ProdE t v) -> Exp t v
      f e (Pos p) = e :+ fromNormal p
      f e (Neg p) = e :- fromNormal p
  isZero = null . getSumsE
  isOne (SumsE (L [Pos p])) = isOne p
  isOne  _                  = False
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
  isZero = const False
  isOne (ProdE (p :| L [])) = isOne p
  isOne _                   = False
  validate x@(ProdE x') = foldMap validate x' <> decreasing (toList x')
    where
      errMsg = Invalid $ pure $ hsep
        ["Components of", ppr x, "are not descending:", ppr (toList x')]
      decreasing :: (Ord t, Ord v) => [PowE t v] -> Validate
      decreasing []  = Ok
      decreasing [_] = Ok
      decreasing (PowU u1 _ : PowU u2 _ : _)
        | u1 <= u2 = errMsg
      decreasing (PowS s1 _ : PowS s2 _ : _)
        | s1 <= s2 = errMsg
      decreasing (x1:x2:_)
        | x1 <= x2 = errMsg
      decreasing (_:xs) = decreasing xs

instance NormalForm PowE where
  fromNormal x = cleanPow $ case x of
      PowU a b -> fromNormal a :^ fromNormal b
      PowS a b -> fromNormal a :^ fromNormal b
    where
      cleanPow :: Exp t v -> Exp t v
      cleanPow (_ :^ N 0)   = N 1
      cleanPow (a :^ N 1)   = cleanPow a
      cleanPow (N a :^ N p) = N (a ^ p)
      cleanPow a            = a
  isZero = const False
  isOne (PowU u p)
    | isOne u || isZero p = True
  isOne _                 = False
  validate (PowU (UN (Prime 1)) b)
    | not (isZero b) = Invalid $ pure $ hsep
      ["Power of unit that is equal to one must always be zero, but got", ppr b]
  validate (PowU a b) = validate a <> validate b
  validate (PowS a b) = validate a <> validate b

instance NormalForm UnitE where
  fromNormal (UN p)     = N (asNatural p)
  fromNormal (UDiv a b) = fromNormal a `Div` fromNormal b
  fromNormal (UMod a b) = fromNormal a `Mod` fromNormal b
  fromNormal (ULog2 a)  = Log2 (fromNormal a)
  fromNormal (UF t)     = F t
  fromNormal (UV v)     = V v
  isZero = const False
  isOne (UN (Prime 1)) = True
  isOne _              = False
  validate up@(UN (Prime p))
    | factorize p == L [unitAsPow up] = Ok
    | otherwise = Invalid $ pure $ ppr (toInteger p) <+> "is not a prime number."
  validate (UDiv a b) = validate a <> validate b
  validate (UMod a b) = validate a <> validate b
  validate (ULog2 a)  = validate a
  validate (UF _) = Ok
  validate (UV _) = Ok


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
  x <> y = MinsE $ mergeDesc (getMinsE x) (getMinsE y)

-- take maximum
instance (Ord t, Ord v) => Semigroup (MaxsE t v) where
  x <> y = MaxsE $ mergeDesc (getMaxsE x) (getMaxsE y)

-- add up
instance (Ord t, Ord v) => Semigroup (SumsE None t v) where
  x <> y = SumsE $ L $ go (toList $ getSumsE x) (toList $ getSumsE y)
    where
      go :: (Ord t, Ord v)
         => [Signed (ProdE t v)] -> [Signed (ProdE t v)] -> [Signed (ProdE t v)]
      go [] xs = xs
      go xs [] = xs
      go (a:as) (b:bs) = case compare pa pb of
          GT -> a : go as (b:bs)
          EQ -> case factorize (fromInteger $ abs c) of
            L [] -> go as bs
            L (f:fs)
              | p <- pa <> ProdE (f :| L fs)
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
      atleast1 (a:as) = ProdE $ a :| L as
      atleast1 []     = oneP
      go :: (Ord t, Ord v)
         => [PowE t v] -> [PowE t v] -> [PowE t v]
      go [] xs = filter (not . isOne) xs
      go xs [] = filter (not . isOne) xs
      go (a@(PowU ua pa) : as) (b@(PowU ub pb) : bs) = case compare ua ub of
        GT -> a : go as (b:bs)
        EQ -> case pa <> pb of
          SumsE (L []) -> go as bs
          p            -> PowU ua p : go as bs
        LT -> b : go (a:as) bs
      go (a@(PowS sa (SumsE (pa1 :| pas))) : as)
         (b@(PowS sb (SumsE (pb1 :| pbs))) : bs) = case compare sa sb of
        GT -> a : go as (b:bs)
        EQ -> case SumsE (cons pa1 pas) <> SumsE (cons pb1 pbs) of
          SumsE (L [])     -> go as bs
          SumsE (L (p:ps)) -> PowS sa (SumsE (p :| L ps)) : go as bs
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
  a * b = go (toList $ getSumsE a)
    where
      ys = toList $ getSumsE b
      go []     = zero
      go (x:xs) = SumsE (L $ times x <$> ys) + go xs
      times :: (Ord t, Ord v)
            => Signed (ProdE t v) -> Signed (ProdE t v) -> Signed (ProdE t v)
      times x y = sig $ getAbs x <> getAbs y
        where
          sig = if isPositive x == isPositive y then Pos else Neg
  negate = SumsE . fmap neg . getSumsE
    where
      neg :: Signed a -> Signed a
      neg (Pos x) = Neg x
      neg (Neg x) = Pos x
  abs (SumsE (L []))      = (SumsE (L []))
  abs (SumsE (L [Neg x])) = (SumsE (L [Pos x]))
  abs (SumsE (L [Pos x])) = (SumsE (L [Pos x]))
  abs x                   = x * signum x
  signum (SumsE (L []))      = (SumsE (L []))
  signum (SumsE (L [Neg _])) = (SumsE (L [Neg oneP]))
  signum (SumsE (L [Pos _])) = (SumsE (L [Pos oneP]))
  signum _ = error "Tried to take signum of a complex expression"
  fromInteger n = case toList $ factorize (fromInteger $ abs n) of
    []     -> zero
    (x:xs) -> SumsE $ L $ (:[]) $ sig $ ProdE $ x :| L xs
    where
      sig = if n >= 0 then Pos else Neg
