{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Numeric.Dimensions.Plugin.SolveNat.NormalForm where

import Data.List                              (group)
import Numeric.Dimensions.Plugin.AtLeast
import Numeric.Dimensions.Plugin.SolveNat.Exp
import Numeric.Natural
import Outputable                             hiding ((<>))


class NormalForm e where
  fromNormal :: e v t -> Exp v t
  isZero     :: e v t -> Bool
  isOne      :: e v t -> Bool
  -- | Check the invariants that must hold by construction.
  --   Do not check arithmetic errors, such as division by zero, etc.
  validate   :: (Ord v, Ord t, Outputable v, Outputable t) => e v t -> Validate
  -- simplify   :: (Ord v, Ord t) => e v t -> e v t

data Validate = Invalid (AtLeast One SDoc) | Ok

instance Semigroup Validate where
  Ok <> v = v
  v <> Ok = v
  Invalid a <> Invalid b = Invalid $ a <> b

instance Monoid Validate where
  mempty = Ok

instance Outputable Validate where
  ppr Ok             = "Ok"
  ppr (Invalid errs) = hang "Invalid:" 2 $ vcat $ map (bullet <+>) $ toList errs



-- | 1,2,3,5,7,11...
--
--   Note, I conside 1 a Prime here for convenience.
newtype Prime = Prime { unPrime :: Natural }
  deriving (Eq, Ord, Show)

-- | Zero factorizes into an empty list of powers,
--   the rest of the Naturals factorize into a non-empty list of powers.
--
--   It does not include 1 in the list as long as there are any other numbers!
--
--   The resulting list of powers is decreasing in the magnitude of the base.
factorize :: Natural -> AtLeast None (PowE v t)
factorize 0 = mempty
factorize 1 = L [unitAsPow $ UN (Prime 1)]
factorize x = L . reverse . fmap asPow . group $ factor x 2
  where
    asPow :: [Natural] -> PowE v t
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


newtype NormalE v t = NormalE { getNormalE :: MinsE v t }
  deriving (Eq, Ord, Show)

newtype MinsE v t = MinsE { getMinsE :: AtLeast One (MaxsE v t) }
  deriving (Eq, Ord, Show)

instance (Ord v, Ord t) => Semigroup (MinsE v t) where
  x <> y = MinsE $ mergeDesc (getMinsE x) (getMinsE y)

newtype MaxsE v t = MaxsE { getMaxsE :: AtLeast One  (SumsE None v t) }
  deriving (Eq, Ord, Show)

instance (Ord v, Ord t) => Semigroup (MaxsE v t) where
  x <> y = MaxsE $ mergeDesc (getMaxsE x) (getMaxsE y)

-- | A signed value with a weird Ord instance:
--   we first compare by magnitude, and then check the signs.
data Signed a
  = Pos { getAbs :: a }
  | Neg { getAbs :: a }
  deriving (Eq, Show)

isPositive :: Signed a -> Bool
isPositive Pos{} = True
isPositive Neg{} = False

instance Ord a => Ord (Signed a) where
  compare a b = compare (getAbs a) (getAbs b)
             <> compare (isPositive a) (isPositive b)

instance Functor Signed where
  fmap f (Pos a) = Pos (f a)
  fmap f (Neg a) = Neg (f a)

newtype SumsE n v t = SumsE { getSumsE :: AtLeast n (Signed (ProdE v t)) }
  deriving (Eq, Ord, Show)

instance (Ord v, Ord t) => Semigroup (SumsE None v t) where
  x <> y = SumsE $ L $ go (toList $ getSumsE x) (toList $ getSumsE y)
    where
      go :: (Ord v, Ord t)
         => [Signed (ProdE v t)] -> [Signed (ProdE v t)] -> [Signed (ProdE v t)]
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

instance (Ord v, Ord t) => Monoid (SumsE None v t) where
  mempty = SumsE mempty

instance (Ord v, Ord t) => Num (SumsE None v t) where
  (+) = (<>)
  a - b = a + negate b
  a * b = go (toList $ getSumsE a)
    where
      ys = toList $ getSumsE b
      go []     = zero
      go (x:xs) = SumsE (L $ times x <$> ys) + go xs
      times :: (Ord v, Ord t)
            => Signed (ProdE v t) -> Signed (ProdE v t) -> Signed (ProdE v t)
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


newtype ProdE v t = ProdE { getProdE :: AtLeast One (PowE v t) }
  deriving (Eq, Ord, Show)

instance (Ord v, Ord t) => Semigroup (ProdE v t) where
  x <> y = atleast1 $ go (toList $ getProdE x) (toList $ getProdE y)
    where
      atleast1 :: [PowE v t] -> ProdE v t
      atleast1 (a:as) = ProdE $ a :| L as
      atleast1 []     = oneP
      go :: (Ord v, Ord t)
         => [PowE v t] -> [PowE v t] -> [PowE v t]
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

instance (Ord v, Ord t) => Monoid (ProdE v t) where
  mempty = oneP



-- | Split product into two parts: constant and irreducible prod.
--   Removes all constant components from the result prod.
unscaleProdE :: Signed (ProdE v t) -> (Integer, ProdE v t)
unscaleProdE p =
  atleast1 <$> go (if isPositive p then 1 else -1) (toList $ getProdE $ getAbs p)
  where
    atleast1 :: [PowE v t] -> ProdE v t
    atleast1 (a:as) = ProdE $ a :| L as
    atleast1 []     = oneP
    go :: Integer -> [PowE v t] -> (Integer, [PowE v t])
    go c []                  = (c, [])
    go c (PowU u@(UN (Prime n)) s : xs) = case unshiftPosSumsE s of
        (k, SumsE (L [])) ->    go (c * toInteger n ^ k) xs
        (k, s')           -> (PowU u s' :) <$> go (c * toInteger n ^ k) xs
    go c (x : xs) = (x:) <$> go c xs

-- | Take the positive constant part of SumsE;
--   The returned SumsE is guaranteed to not have a constant component or have
--   negative constant value.
unshiftPosSumsE :: SumsE n v t -> (Natural, SumsE None v t)
unshiftPosSumsE s = toPos $ go 0 (toList $ getSumsE s)
  where
    toPos (n, xs) = if n >= 0 then (fromInteger n, SumsE $ L xs)
                              else (0, SumsE $ L $ toList $ getSumsE s)
    go :: Integer -> [Signed (ProdE v t)] -> (Integer, [Signed (ProdE v t)])
    go c [] = (c, [])
    go c (p:ps)
        | isOne p'  = go (c + c') ps
        | otherwise = (p:) <$> go c ps
      where
        (c', p') = unscaleProdE p

-- | Take the constant part of SumsE;
--   The returned SumsE is guaranteed to not have a constant component.
unconstSumsE :: SumsE n v t -> (Integer, SumsE None v t)
unconstSumsE s = SumsE . L <$> go 0 (toList $ getSumsE s)
  where
    go :: Integer -> [Signed (ProdE v t)] -> (Integer, [Signed (ProdE v t)])
    go c [] = (c, [])
    go c (p:ps)
        | isOne p'  = go (c + c') ps
        | otherwise = (p:) <$> go c ps
      where
        (c', p') = unscaleProdE p

-- | Note, constants in the power part of PowS are ambigous:
--   one can always substract a positive value from a constant by expanding the
--   sum.
--   At the same time, I cannot get rid of negative constants, so I cannot just
--   expand the expression until it does not have a constant in the power part.
--   Thus, I allow constants at the expense of a more difficult comparison of
--   two expressions.
data PowE v t
  = PowU (UnitE v t) (SumsE None v t)
  | PowS (SumsE Two v t) (SumsE One v t)
  deriving (Eq, Ord, Show)

data UnitE v t
  = UN Prime
  | UDiv  (SumsE None v t) (SumsE None v t)
  | UMod  (NormalE v t) (NormalE v t)
  | ULog2 (SumsE None v t)
  | UF t
  | UV v
  deriving (Eq, Ord, Show)

minMax :: SumsE None v t -> NormalE v t
minMax x@(SumsE (L _)) = NormalE $ MinsE $ pure $ MaxsE $ pure x

unit :: UnitE v t -> NormalE v t
unit = minMax . unitAsSums

singleProdAsSums :: Applicative (AtLeast n) => Signed (ProdE v t) -> SumsE n v t
singleProdAsSums p = SumsE $ pure p

singlePowAsSums :: Applicative (AtLeast n) => PowE v t -> SumsE n v t
singlePowAsSums p = singleProdAsSums $ Pos (ProdE $ pure p)

unitAsPow :: UnitE v t -> PowE v t
unitAsPow u@(UN (Prime 1)) = PowU u zero
unitAsPow u                = PowU u one

unitAsSums :: Applicative (AtLeast n) => UnitE v t -> SumsE n v t
unitAsSums = singlePowAsSums . unitAsPow

zero :: SumsE None v t
zero = SumsE mempty

one :: Applicative (AtLeast n) => SumsE n v t
one = singleProdAsSums $ Pos oneP

oneP :: ProdE v t
oneP = ProdE $ PowU (UN (Prime 1)) zero :| mempty


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
  isZero = all isZero . getMinsE
  isOne  = all isOne . getMinsE
  validate x = foldMap validate xl <> decreasing xl
    where
      xl = toList $ getMinsE x
      decreasing :: (Ord v, Ord t) => [MaxsE v t] -> Validate
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
  isOne  = all isOne . getMaxsE
  validate x = foldMap validate xl <> decreasing xl
    where
      xl = toList $ getMaxsE x
      decreasing :: (Ord v, Ord t) => [SumsE None v t] -> Validate
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
      f :: Exp v t -> Signed (ProdE v t) -> Exp v t
      f e (Pos p) = e :+ fromNormal p
      f e (Neg p) = e :- fromNormal p
  isZero = null . getSumsE
  isOne (SumsE (L [Pos p])) = isOne p
  isOne  _                  = False
  validate x = foldMap (validate . getAbs) xl <> decreasing ps <> maxOneConst
    where
      xl = toList $ getSumsE x
      ps = snd . unscaleProdE <$> xl
      decreasing :: (Ord v, Ord t) => [ProdE v t] -> Validate
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
      go :: Natural -> AtLeast One (Exp v t) -> Exp v t
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
      decreasing :: (Ord v, Ord t) => [PowE v t] -> Validate
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
  fromNormal x = cleanPow1 $ case x of
      PowU a b -> fromNormal a :^ fromNormal b
      PowS a b -> fromNormal a :^ fromNormal b
    where
      cleanPow1 :: Exp v t -> Exp v t
      cleanPow1 (_ :^ N 0) = N 1
      cleanPow1 (a :^ N 1) = cleanPow1 a
      cleanPow1 a          = a
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
  fromNormal (UN p)     = N (unPrime p)
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

instance (Outputable v, Outputable t) => Outputable (NormalE v t) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (MinsE v t) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (MaxsE v t) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (SumsE n v t) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (ProdE v t) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (PowE v t) where
  pprPrec p = pprPrec p . fromNormal
instance (Outputable v, Outputable t) => Outputable (UnitE v t) where
  pprPrec p = pprPrec p . fromNormal
