{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Numeric.Dimensions.Plugin.SolveNat.Exp
  ( Exp(..)
  , log2Nat
  , substituteVar
  , substituteFun
  , evaluate
  )
where

import           Control.Exception              ( ArithException(..)
                                                , throw
                                                )
import           Data.Bifunctor
import           Data.Bits
import           Data.Functor.Identity
import           Numeric.Natural
import           Outputable              hiding ( (<>) )

log2Nat :: Natural -> Natural
log2Nat 0 = throw Underflow
log2Nat 1 = 0
log2Nat n = succ . log2Nat $ shiftR n 1

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
  negate e = N 0 :- e
  abs (N n)        = N n
  abs (N 0 :- N n) = N n
  abs (Log2 e)   = Log2 e
  abs e          = Max e (N 0 :- e)
  signum (N n) = N (signum n)
  signum (N 0 :- N 0) = N 0
  signum (N 0 :- N _) = N 0 :- N 1
  signum e    = Min (Max (N 1) (N 0 :- e)) (Max (N 0 :- N 1) e)
  fromInteger i
    | i >= 0    = N (fromInteger i)
    | otherwise = N 0 :- N (fromInteger $ negate i)


instance (Outputable v, Outputable t) => Outputable (Exp t v) where
  pprPrec _ (N n) = pprPrec 10 (toInteger n)
  pprPrec p (F n) = pprPrec p n
  pprPrec p (V v) = pprPrec p v

  pprPrec p (a :+ b) = cparen (p > 6) $ pprPrec 6 a <+> "+" <+> pprPrec 6 b
  pprPrec p (a :- b) = cparen (p > 6) $ pprPrec 6 a <+> "-" <+> pprPrec 6.1 b
  pprPrec p (a :* b) = cparen (p > 7) $ pprPrec 7 a <+> "*" <+> pprPrec 7 b
  pprPrec p (a :^ b) = cparen (p > 8) $ pprPrec 8.1 a <+> "^" <+> pprPrec 8 b
  pprPrec p (Div a b)  = cparen (p > 10) $ "Div"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Mod a b)  = cparen (p > 10) $ "Mod"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Max a b)  = cparen (p > 10) $ "Max"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Min a b)  = cparen (p > 10) $ "Min"  <+> pprPrec 11 a <+> pprPrec 11 b
  pprPrec p (Log2 v)   = cparen (p > 10) $ "Log2" <+> pprPrec 11 v


instance Functor (Exp t) where
  fmap = second

instance Bifunctor Exp where
  first  f = runIdentity . substituteFun (pure . F . f)
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

-- | Try to evaluate an expression fully or partially and return either a single
--   Integer or a simplified expression.
--
--   Note, the expression can have negative value.
evaluate :: Exp a b -> Either (Exp a b) Integer
evaluate e = case e' of
  [] -> Right r
  _  -> Left $ foldl f (_N r) e'
  where
    (r, e') = evaluate' e
    f :: Exp t v -> (Integer, Exp t v) -> Exp t v
    f (N 0) (1, x)          = x
    f (N 0) (i, x) | i >= 0 = N (fromInteger i) :* x
    f y (1 , x)             = y :+ x
    f y (-1, x)             = y :- x
    f y (i, x) | i >= 0    = y :+ N (fromInteger i) :* x
               | otherwise = y :- N (fromInteger $ negate i) :* x


evaluate' :: Exp a b -> (Integer, [(Integer, Exp a b)])
evaluate' (N n   ) = (toInteger n, [])
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
    f :: Integer -> (Integer, Exp a b) -> [(Integer, Exp a b)]
    f 0 x | safe (snd x) = []
    f 1 x                = [x]
    f c (n, e)           = [(c * n, e)]
    g :: (Integer, Exp a b) -> (Integer, Exp a b) -> (Integer, Exp a b)
    g (nx, ex) (ny, ey) = (nx * ny, ex * ey)

evaluate' (a :^ b) = case (evaluate a, evaluate b) of
  (Right x, Right y) | y >= 0    -> (x ^ y, [])
                     | x == 1    -> (1, [])
                     | x == -1   -> (if even y then 1 else -1, [])
                     | otherwise -> (0, [(1, _N x :^ _N y)])
  (Left x, Right y) | y == 0 && safe x -> (1, [])
                    | y == 1           -> (0, [(1, x)])
                    | y >= 0           -> evaluate' (x ^ y)
                    | otherwise        -> (0, [(1, x :^ _N y)])
  (Right x, Left y) | x == 0 && safe y -> (0, [])
                    | x == 1 && safe y -> (1, [])
                    | otherwise        -> (0, [(1, _N x :^ y)])
  (Left x, Left y) -> (0, [(1, x :^ y)])

evaluate' (Div a b) = case (evaluate a, evaluate b) of
  (Right x, Right y)
    | y == 0    -> (0, [(signum x, Div (N $ fromInteger $ abs x) (N 0))])
    | otherwise -> (div x y, [])
  (Left x, Right y)
    | y == 1 && safe x  -> evaluate' x
    | y == -1 && safe x -> bimap negate (map $ first negate) $ evaluate' x
    | y >= 0            -> (0, [(1, Div x (N $ fromInteger y))])
    | otherwise -> (0, [(1, Div (N 0 :- x) (N $ fromInteger $ negate y))])
  (Right x, Left y) | x == 0 && safe y -> (0, [])
                    | otherwise        -> (0, [(1, _N x `Div` y)])
  (Left x, Left y) -> (0, [(1, Div x y)])

evaluate' (Mod a b) = case (evaluate a, evaluate b) of
  (Right x, Right y)
    | y == 0    -> (0, [(signum x, Mod (N $ fromInteger $ abs x) (N 0))])
    | otherwise -> (mod x y, [])
  (Left x, Right y)
    | y == 1 && safe x  -> (0, [])
    | y == -1 && safe x -> (0, [])
    | y >= 0            -> (0, [(1, Mod x (N $ fromInteger y))])
    | otherwise -> (0, [(1, Mod (N 0 :- x) (N $ fromInteger $ negate y))])
  (Right x, Left y) | x == 0 && safe y -> (0, [])
                    | otherwise        -> (0, [(1, _N x `Mod` y)])
  (Left x, Left y) -> (0, [(1, Mod x y)])

evaluate' (Max a b) = case (evaluate a, evaluate b) of
  (Right x, Right y) -> (max x y, [])
  (Left (N x1 :^ (N 0 :- N x2)), Right y) | x1 > 0 && x2 > 0 && y > 0 -> (y, [])
  (Left x, Right y)
    | y >= 0 -> (0, [(1, Max x (N $ fromInteger y))])
    | otherwise -> evaluate'
      (N 0 :- Min (N 0 :- x) (N $ fromInteger $ negate y))
  (Right _, Left y) -> evaluate' (Max y a)
  (Left  x, Left y) -> (0, [(1, Max x y)])

evaluate' (Min a b) = case (evaluate a, evaluate b) of
  (Right x, Right y) -> (min x y, [])
  (Left x, Right y)
    | y >= 0 -> (0, [(1, Min x (N $ fromInteger y))])
    | otherwise -> evaluate'
      (N 0 :- Max (N 0 :- x) (N $ fromInteger $ negate y))
  (Right _, Left y) -> evaluate' (Min y a)
  (Left  x, Left y) -> (0, [(1, Min x y)])

evaluate' (Log2 a) = case evaluate a of
  Right x | x > 0     -> (toInteger $ log2Nat $ fromInteger x, [])
          | otherwise -> (0, [(1, Log2 $ _N x)])
  Left e -> (0, [(1, Log2 e)])


_N :: Integer -> Exp t v
_N n | n >= 0    = N (fromInteger n)
     | otherwise = N 0 :- N (fromInteger $ negate n)


-- | A basic check if the expression cannot be undefined for any valid input.
safe :: Exp t v -> Bool
safe (N _         ) = True
safe (F _         ) = False
safe (V _         ) = True
safe (a   :+ b    ) = safe a && safe b
safe (a   :- b    ) = safe a && safe b
safe (a   :* b    ) = safe a && safe b
safe (a   :^ b    ) = safe a && safe b
safe (Div a  (N n)) = safe a && n > 0
safe (Div _  _    ) = False
safe (Mod a  (N n)) = safe a && n > 0
safe (Mod _  _    ) = False
safe (Max a  b    ) = safe a && safe b
safe (Min a  b    ) = safe a && safe b
safe (Log2 (N n)  ) = n > 0
safe (Log2 _      ) = False
