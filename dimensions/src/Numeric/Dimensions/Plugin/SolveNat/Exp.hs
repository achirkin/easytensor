{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Numeric.Dimensions.Plugin.SolveNat.Exp
  ( -- * Rational extensions for Integer functions
    log2R
  , divR
  , modR
  , powR
    -- * Expression
  , Exp(..)
  , substituteVar
  , substituteFun
  , asConstant
  , evaluate
  , evaluateR
  , _R
  )
where

import           Control.Exception              ( ArithException(..)
                                                , throw
                                                )
import           Control.Monad                  ( (>=>) )
import           Data.Bifunctor
import           Data.Bits
import           Data.Functor.Identity
import           Data.Ratio
import           Data.Void
import           Numeric.Natural
import           Outputable              hiding ( (<>) )

-- | Rounded logarithm with base 2.
--   Rounding is towards zero.
--
--   \( \mathrm{log2R}(x) = - \mathrm{log2R} \left( \frac{1}{x} \right) \)
log2R :: Rational -> Maybe Integer
log2R x | x <= 0    = Nothing
        | n >= d    = Just $ f (div n d)
        | otherwise = Just $ negate $ f (div d n)
  where
    n = numerator x
    d = denominator x
    f :: Integer -> Integer
    f a | a == 1    = 0
        | a > 1     = succ . f $ shiftR a 1
        | otherwise = throw Underflow

-- | Division rounded towards negative infinity.
--
--   \( \mathrm{divR}(x,y) = \left \lfloor \frac{x}{y} \right \rfloor \)
divR :: Rational -> Rational -> Maybe Integer
divR x y
  | y == 0 = Nothing
  | otherwise = Just
  $ div (numerator x * denominator y) (numerator y * denominator x)

-- | Remainder of `divR`.
--
--   \( x = \mathrm{modR}(x,y) + y \mathrm{divR}(x,y) \)
modR :: Rational -> Rational -> Maybe Rational
modR x y | y == 0    = Nothing
         | otherwise = Just $ mod (numerator x * dy) (numerator y * dx) % dr
  where
    dx = denominator x
    dy = denominator y
    dr = dx * dy

-- | Calculate @a ^ b@ for rationals if that is possible.
powR :: Rational -> Rational -> Maybe Rational
powR a b | b == 0           = Just 1
         | a == 0 && b > 0  = Just 0
         | a == 0 && b <= 0 = Nothing
         | b < 0            = powR (1 / a) (negate b)
         | a == 1           = Just 1
         | a < 0 && even nb = powR (negate a) b
         | a < 0 && even db = Nothing
         | a < 0            = negate <$> powR (negate a) b
         | otherwise        = f <$> intRoot na db <*> intRoot da db
  where
    db = denominator b
    nb = numerator b
    da = denominator a
    na = numerator a
    f :: Integer -> Integer -> Rational
    f nx dx = nx ^ nb % dx ^ nb


-- | Type-level expression of kind @Nat@
data Exp t v
  = N Natural
    -- ^ Constant, non-negative number
  | F t
    -- ^ Irreducible type family of kind Nat
  | V v
    -- ^ Variable of kind Nat
  | Exp t v :+ Exp t v
  | Exp t v :- Exp t v
  | Exp t v :* Exp t v
  | Exp t v :^ Exp t v
  | Div (Exp t v) (Exp t v)
  | Mod (Exp t v) (Exp t v)
  | Max (Exp t v) (Exp t v)
  | Min (Exp t v) (Exp t v)
  | Log2 (Exp t v)
  deriving (Eq, Ord, Show, Foldable, Traversable)

infixl 6 :+
infixl 7 :*
infixr 8 :^
infixl 6 :-
infixl 7 `Div`
infixl 7 `Mod`


instance Num (Exp t v) where
  (+) = (:+)
  (-) = (:-)
  (*) = (:*)
  negate (N 0 :- e) = e
  negate e          = N 0 :- e
  abs (N n       ) = N n
  abs (N 0 :- N n) = N n
  abs (Log2 e    ) = Log2 e
  abs e            = Max e (N 0 :- e)
  signum e@(F _     ) = Min (Max 1 (0 :- e)) (Max (-1) e) -- assume type family can be negative int
  signum e@(V _     ) = Min 1 e -- only Nats allowed
  signum (  N n     ) = N (signum n)
  signum (  N 0 :- e) = negate (signum e)
  -- NB: this is not exactly signum for rational-valued expressions;
  --     it's linear on segment [-1, 1].
  signum e            = Min (Max 1 (0 :- e)) (Max (-1) e)
  fromInteger i | i >= 0    = N (fromInteger i)
                | otherwise = N 0 :- N (fromInteger $ negate i)


instance Fractional (Exp t v) where
  recip (a :^ b) = a :^ negate b
  recip e        = e :^ (-1)
  fromRational r
    | denominator r == 1
    = fromInteger (numerator r)
    | otherwise
    = fromInteger (numerator r) :* fromInteger (denominator r) :^ (-1)

instance (Outputable v, Outputable t) => Outputable (Exp t v) where
  pprPrec _ (N n   ) = pprPrec 10 (toInteger n)
  pprPrec p (F n   ) = pprPrec p n
  pprPrec p (V v   ) = pprPrec p v

  pprPrec p (a :+ b) = cparen (p > 6) $ pprPrec 6 a <+> "+" <+> pprPrec 6 b
  pprPrec p (a :- b) = cparen (p > 6) $ pprPrec 6 a <+> "-" <+> pprPrec 6.1 b
  pprPrec p (a :* b) = cparen (p > 7) $ pprPrec 7 a <+> "*" <+> pprPrec 7 b
  pprPrec p (a :^ b) = cparen (p > 8) $ pprPrec 8.1 a <+> "^" <+> pprPrec 8 b
  pprPrec p (Div a b) =
    cparen (p > 10) $ "Div" <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Mod a b) =
    cparen (p > 10) $ "Mod" <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Max a b) =
    cparen (p > 10) $ "Max" <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Min a b) =
    cparen (p > 10) $ "Min" <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Log2 v) = cparen (p > 10) $ "Log2" <+> pprPrec 11 v


instance Functor (Exp t) where
  fmap = second

instance Bifunctor Exp where
  first f = runIdentity . substituteFun (pure . F . f)
  second f = runIdentity . substituteVar (pure . V . f)

-- | Replace occurrence of a var with something else, possibly doing something
--   applicative.
substituteVar :: Applicative m => (a -> m (Exp t b)) -> Exp t a -> m (Exp t b)
substituteVar _ (N n     ) = pure $ N n
substituteVar _ (F t     ) = pure $ F t
substituteVar f (V v     ) = f v
substituteVar f (a   :+ b) = (:+) <$> substituteVar f a <*> substituteVar f b
substituteVar f (a   :- b) = (:-) <$> substituteVar f a <*> substituteVar f b
substituteVar f (a   :* b) = (:*) <$> substituteVar f a <*> substituteVar f b
substituteVar f (a   :^ b) = (:^) <$> substituteVar f a <*> substituteVar f b
substituteVar f (Div a  b) = Div <$> substituteVar f a <*> substituteVar f b
substituteVar f (Mod a  b) = Mod <$> substituteVar f a <*> substituteVar f b
substituteVar f (Max a  b) = Max <$> substituteVar f a <*> substituteVar f b
substituteVar f (Min a  b) = Min <$> substituteVar f a <*> substituteVar f b
substituteVar f (Log2 a  ) = Log2 <$> substituteVar f a

-- | Replace occurrence of a fun with something else, possibly doing something
--   applicative.
substituteFun :: Applicative m => (a -> m (Exp b v)) -> Exp a v -> m (Exp b v)
substituteFun _ (N n     ) = pure $ N n
substituteFun f (F t     ) = f t
substituteFun _ (V v     ) = pure $ V v
substituteFun f (a   :+ b) = (:+) <$> substituteFun f a <*> substituteFun f b
substituteFun f (a   :- b) = (:-) <$> substituteFun f a <*> substituteFun f b
substituteFun f (a   :* b) = (:*) <$> substituteFun f a <*> substituteFun f b
substituteFun f (a   :^ b) = (:^) <$> substituteFun f a <*> substituteFun f b
substituteFun f (Div a  b) = Div <$> substituteFun f a <*> substituteFun f b
substituteFun f (Mod a  b) = Mod <$> substituteFun f a <*> substituteFun f b
substituteFun f (Max a  b) = Max <$> substituteFun f a <*> substituteFun f b
substituteFun f (Min a  b) = Min <$> substituteFun f a <*> substituteFun f b
substituteFun f (Log2 a  ) = Log2 <$> substituteFun f a

-- | Check if there are no variables or functions inside this expression.
asConstant :: Exp a b -> Maybe (Exp Void Void)
asConstant = substituteFun (const Nothing) >=> substituteVar (const Nothing)

-- | Try to evaluate an expression fully or partially and return either a single
--   Integer or a simplified expression.
--
--   Note, the expression can have negative value.
evaluate :: Exp a b -> Either (Exp a b) Integer
evaluate e = evaluateR e
  >>= \r -> if denominator r == 1 then Right (numerator r) else Left (_R r)

-- | Try to evaluate an expression fully or partially and return either a single
--   Rational or a simplified expression.
evaluateR :: Exp a b -> Either (Exp a b) Rational
evaluateR e = case e' of
  [] -> Right r
  _  -> Left . withDen $ foldl f (fromR r) e'
  where
    (r, e')   = evaluate' e
    commonDen = foldl (\d (x, _) -> lcm d (denominator x)) (denominator r) e'
    withDen :: Exp t v -> Exp t v
    withDen x | commonDen == 1  = x
              | commonDen == -1 = 0 :- x
              | otherwise       = x :* (_N commonDen :^ _N (-1))
    fromR :: Rational -> Exp t v
    fromR x = _N . numerator $ x * (commonDen % 1)
    f :: Exp t v -> (Rational, Exp t v) -> Exp t v
    f (N 0) (1, x)          = x
    f (N 0) (i, x) | i >= 0 = fromR i :* x
    f y (1 , x)             = y :+ x
    f y (-1, x)             = y :- x
    f y (i, x) | i >= 0    = y :+ fromR i :* x
               | otherwise = y :- (fromR $ negate i) :* x

evaluate' :: Exp a b -> (Rational, [(Rational, Exp a b)])
evaluate' (N n   ) = (toRational n, [])
evaluate' (F t   ) = (0, [(1, F t)])
evaluate' (V v   ) = (0, [(1, V v)])

evaluate' (a :+ b) = (na + nb, xsa ++ xsb)
  where
    (na, xsa) = evaluate' a
    (nb, xsb) = evaluate' b
evaluate' (a :- b) = (na - nb, xsa ++ map (first negate) xsb)
  where
    (na, xsa) = evaluate' a
    (nb, xsb) = evaluate' b

evaluate' (a :* b) =
  (na * nb, (xsa >>= f nb) ++ (xsb >>= f na) ++ (g <$> xsa <*> xsb))
  where
    (na, xsa) = evaluate' a
    (nb, xsb) = evaluate' b
    f :: Rational -> (Rational, Exp a b) -> [(Rational, Exp a b)]
    f 0 x | safe (snd x) = []
    f 1 x                = [x]
    f c (n, e)           = [(c * n, e)]
    g :: (Rational, Exp a b) -> (Rational, Exp a b) -> (Rational, Exp a b)
    g (nx, ex) (ny, ey) = (nx * ny, ex * ey)

evaluate' (a :^ b) = case (evaluateR a, evaluateR b) of
  (Right x, Right y) | Just z <- powR x y -> (z, [])
                     | otherwise          -> (0, [(1, _R x :^ _R y)])
  (Left (x :^ b'), _) -> evaluate' $ x :^ (b' :* b)
  (Left x, Right y) | y == 0 && safe x -> (1, [])
                    | y == 1           -> (0, [(1, x)])
                    | otherwise        -> (0, [(1, x :^ _R y)])
  (Right x, Left y) | x == 1 && safe y -> (1, [])
                    | otherwise        -> (0, [(1, _R x :^ y)])
  (Left x, Left y) -> (0, [(1, x :^ y)])

evaluate' (Div a b) = case (evaluateR a, evaluateR b) of
  (Right x, Right y) | Just r <- divR x y -> (r % 1, [])
                     | y == 0 -> (0, [(signum x, Div (_R $ abs x) 0)])
                     | otherwise -> (0, [(1, Div (_R x) (_R y))])
  (Left x, Right y) | y >= 0    -> (0, [(1, Div x (_R y))])
                    | otherwise -> (0, [(1, Div (0 :- x) (_R $ negate y))])
  (Right x, Left y) -> (0, [(1, _R x `Div` y)])
  (Left x, Left y) -> (0, [(1, Div x y)])

evaluate' (Mod a b) = case (evaluateR a, evaluateR b) of
  (Right x, Right y) | Just r <- modR x y -> (r, [])
                     | y == 0 -> (0, [(signum x, Mod (_R $ abs x) 0)])
                     | otherwise -> (0, [(1, Mod (_R x) (_R y))])
  (Left x, Right y) | y >= 0     -> (0, [(1, Mod x (_R y))])
                    | otherwise -> (0, [(1, Mod (0 :- x) (_R $ negate y))])
  (Right x, Left y) -> (0, [(1, _R x `Mod` y)])
  (Left x, Left y) -> (0, [(1, Mod x y)])

evaluate' (Max a b) = case (evaluateR a, evaluateR b) of
  (Right x, Right y) -> (max x y, [])
  (Left  x, Right y) -> (0, [(1, Max x (_R y))])
  (Right x, Left y ) -> (0, [(1, Max (_R x) y)])
  (Left  x, Left y ) -> (0, [(1, Max x y)])

evaluate' (Min a b) = case (evaluateR a, evaluateR b) of
  (Right x, Right y) -> (min x y, [])
  (Left  x, Right y) -> (0, [(1, Min x (_R y))])
  (Right x, Left y ) -> (0, [(1, Min (_R x) y)])
  (Left  x, Left y ) -> (0, [(1, Min x y)])

evaluate' (Log2 a) = case evaluateR a of
  Right x | Just r <- log2R x -> (r % 1, [])
          | otherwise         -> (0, [(1, Log2 $ _R x)])
  Left e -> (0, [(1, Log2 e)])


_N :: Integer -> Exp t v
_N = fromInteger

_R :: Rational -> Exp t v
_R x | dx == 1   = _N nx
     | dx == -1  = _N (negate nx)
     | otherwise = _N nx :* _N dx :^ (-1)
  where
    dx = denominator x
    nx = numerator x



-- | Calculate @a ^ (1/b)@ for integers.
--   @b@ must be positive.
--   @a@ must be positive if @even b@.
intRoot :: Integer -> Integer -> Maybe Integer
intRoot a b
  | b <= 0
  = Just $ error $ show a ++ " ^ (1/(" ++ show b ++ ")) : non-positive power"
  | a == 0
  = Just $ error $ "0 ^ (1/" ++ show b ++ ") : infinity"
  | a < 0 && even b
  = Just
    $  error
    $  "("
    ++ show a
    ++ ") ^ (1/"
    ++ show b
    ++ ") : imaginary number"
  | a < 0
  = negate <$> intRoot (negate a) b
  | a == 1
  = Just 1
  | shiftR a bInt == 0 -- if a < 2 ^ b
  = Nothing
  | otherwise
  = check $ go a
  where
    bInt :: Int
    bInt = if b > toInteger (maxBound :: Int) then maxBound else fromInteger b
    check :: Integer -> Maybe Integer
    check x | x ^ b == a = Just x
            | otherwise  = Nothing
    b1 = b - 1
    go :: Integer -> Integer
    go x =
      let x' = (x * b1 + div a (x ^ b1)) `div` b
      in  case compare x x' of
            GT -> go x'
            EQ -> x'
            LT -> x


-- | A basic check if the expression cannot be undefined for any valid input.
safe :: Exp t v -> Bool
safe (N _         ) = True
safe (F _         ) = False
safe (V _         ) = True
safe (a   :+ b    ) = safe a && safe b
safe (a   :- b    ) = safe a && safe b
safe (a   :* b    ) = safe a && safe b
safe (a   :^ b    )
    | not (safe a) || not (safe b) = False
    | Right rb <- eb, rb == 0 = True
    | Right rb <- eb, rb > 0 && odd (denominator rb) = True
    | Right ra <- ea, ra > 0 = True
    | Right ra <- ea, Right rb <- eb, ra == 0 && rb >= 0 = True
    | Right ra <- ea, Right rb <- eb, ra < 0 && odd (denominator rb) = True
    | otherwise = False
  where
    ea = evaluateR a
    eb = evaluateR b
safe (Div a b)
    | not (safe a) || not (safe b) = False
    | Right rb <- evaluateR b = rb /= 0
    | otherwise = False
safe (Mod a b)
    | not (safe a) || not (safe b) = False
    | Right rb <- evaluateR b = rb /= 0
    | otherwise = False
safe (Max a  b    ) = safe a && safe b
safe (Min a  b    ) = safe a && safe b
safe (Log2 a)
    | not (safe a)  = False
    | Right ra <- evaluateR a = ra > 0
    | otherwise = False
