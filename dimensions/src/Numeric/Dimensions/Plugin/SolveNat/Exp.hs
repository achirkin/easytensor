{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Numeric.Dimensions.Plugin.SolveNat.Exp
  ( Exp (..), log2Nat
  , substituteVar, substituteFun, evaluate
  ) where

import Control.Exception     (ArithException (..), throw)
import Data.Bifunctor
import Data.Bits
import Data.Functor.Identity
import Numeric.Natural
import Outputable            hiding ((<>))

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
  negate = (N 0 :-)
  abs (N n)      = (N n)
  abs (N 0 :- e) = e
  abs (Log2 e)   = (Log2 e)
  abs e          = e * signum e
  signum (N n) = N (signum n)
  signum _     = error "Tried to take signum of a complex expression"
  fromInteger = N . fromInteger


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
substituteVar :: Applicative m
              => (a -> m (Exp t b)) -> Exp t a -> m (Exp t b)
substituteVar _ (N n)     = pure $ N n
substituteVar _ (F t)     = pure $ F t
substituteVar f (V v)     = f v
substituteVar f (a :+ b)  = (:+) <$> substituteVar f a <*> substituteVar f b
substituteVar f (a :- b)  = (:-) <$> substituteVar f a <*> substituteVar f b
substituteVar f (a :* b)  = (:*) <$> substituteVar f a <*> substituteVar f b
substituteVar f (a :^ b)  = (:^) <$> substituteVar f a <*> substituteVar f b
substituteVar f (Div a b) = Div  <$> substituteVar f a <*> substituteVar f b
substituteVar f (Mod a b) = Mod  <$> substituteVar f a <*> substituteVar f b
substituteVar f (Max a b) = Max  <$> substituteVar f a <*> substituteVar f b
substituteVar f (Min a b) = Min  <$> substituteVar f a <*> substituteVar f b
substituteVar f (Log2 a)  = Log2 <$> substituteVar f a

-- | Replace occurrence of a fun with something else, possibly doing something
--   applicative.
substituteFun :: Applicative m
              => (a -> m (Exp b v)) -> Exp a v -> m (Exp b v)
substituteFun _ (N n)     = pure $ N n
substituteFun f (F t)     = f t
substituteFun _ (V v)     = pure $ V v
substituteFun f (a :+ b)  = (:+) <$> substituteFun f a <*> substituteFun f b
substituteFun f (a :- b)  = (:-) <$> substituteFun f a <*> substituteFun f b
substituteFun f (a :* b)  = (:*) <$> substituteFun f a <*> substituteFun f b
substituteFun f (a :^ b)  = (:^) <$> substituteFun f a <*> substituteFun f b
substituteFun f (Div a b) = Div  <$> substituteFun f a <*> substituteFun f b
substituteFun f (Mod a b) = Mod  <$> substituteFun f a <*> substituteFun f b
substituteFun f (Max a b) = Max  <$> substituteFun f a <*> substituteFun f b
substituteFun f (Min a b) = Min  <$> substituteFun f a <*> substituteFun f b
substituteFun f (Log2 a)  = Log2 <$> substituteFun f a

-- | Try to evaluate an expression fully or partially and return either a single
--   Natural number or a simplified expression.
--
--   We try to be very liberal here: using various arithmetic laws to postpone
--   underflow errors if possible.
evaluate :: Exp a b -> Either (Exp a b) Natural
evaluate (N n) = Right n
evaluate (V v) = Left (V v)
evaluate (F t) = Left (F t)
evaluate (a :+ b) = case (evaluate a, evaluate b) of
  (x, Right 0)         -> x
  (Right 0, y)         -> y
  (Right x, Right y) -> Right (x + y)
  -- postpone evaluation of minus using commutativity
  (Left (x1 :- x2), Left (y1 :- y2))
                     -> evaluate ((x1 :+ y1) :- (x2 :+ y2))
  (Left (x1 :- x2), Right y)
                     -> evaluate ((x1 :+ N y) :- x2)
  (Right x, Left (y1 :- y2))
                     -> evaluate ((N x :+ y1) :- y2)
  (Left (x1 :- x2), Left y)
                     -> evaluate ((x1 :+ y) :- x2)
  (Left x, Left (y1 :- y2))
                     -> evaluate ((x :+ y1) :- y2)
  -- can do nothing else
  (Left x,  Right y) -> Left (x :+ N y)
  -- move computed numbers to the right
  (Right x, Left y)  -> evaluate (y :+ N x)
  (Left x,  Left y)  -> Left (x :+ y)

evaluate (a :- b) = case (evaluate a, evaluate b) of
  (x, Right 0)       -> x
  (Right x, Right y)
    | x >= y         -> Right (x - y)
    | otherwise      -> Left (N x :- N y)
  -- join nested minus
  (Right x, Left (y1 :- y2))
                     -> evaluate ((N x :+ y2) :- y1)
  (Left (x1 :- x2), Right y)
                     -> evaluate (x1 :- (x2 :+ N y))
  (Left (x1 :+ N x2), Right y)
    | x2 >= y        -> evaluate (x1 :+ N (x2 - y))
    | otherwise      -> evaluate (x1 :- N (y - x2))
  (Left x, Left (y1 :- y2))
                     -> evaluate ((x :+ y2) :- y1)
  (Left (x1 :- x2), Left y)
                     -> evaluate (x1 :- (x2 :+ y))
  -- can do nothing else
  (Left x,  Right y) -> Left (x :- N y)
  (Right x, Left y)  -> Left (N x :- y)
  (Left x,  Left y)  -> Left (x :- y)

evaluate (a :* b) = case (evaluate a, evaluate b) of
  (_, Right 0)         -> Right 0
  (x, Right 1)         -> x
  (Right 0, _)         -> Right 0
  (Right 1, y)         -> y
  (Right x, Right y)   -> Right (x * y)
  -- postpone minus using distributivity
  (Left (x1 :- x2), _) -> evaluate (x1 :* b :- x2 :* b)
  (_, Left (y1 :- y2)) -> evaluate (a :* y1 :- a :* y2)
  -- can do nothing else
  (Left x,  Right y)   -> Left (x :* N y)
  (Right x, Left y)    -> evaluate (y :* N x)
  (Left x,  Left y)    -> Left (x :* y)

evaluate (a :^ b) = case (evaluate a, evaluate b) of
  (Right 0, _)         -> Right 0
  (Right 1, _)         -> Right 1
  (_, Right 0)         -> Right 1
  (x, Right 1)         -> x
  (Right x, Right y)   -> Right (x ^ y)
  -- expand power expr so that later we can compute minus
  (Left x@(_ :- _), Right n)
    | (n', c) <- divMod n 2
                       -> evaluate (x :^ N n' :* x :^ N (n' + c))
  -- can do nothing else
  (Left x,  Right y)   -> Left (x :^ N y)
  (Right x, Left y)    -> Left (N x :^ y)
  (Left x,  Left y)    -> Left (x :^ y)

evaluate (Div a b) = case (evaluate a, evaluate b) of
  (Right 0, _)       -> Right 0
  (Right x, Right y)
    | y > 0          -> Right (div x y)
    | otherwise      -> Left (Div (N x) (N y))
  (Left x,  Right y) -> Left (Div x (N y))
  (Right x, Left y)  -> Left (Div (N x) y)
  (Left x,  Left y)  -> Left (Div x y)

evaluate (Mod a b) = case (evaluate a, evaluate b) of
  (Right 0, _)       -> Right 0
  (Right x, Right y)
    | y > 0          -> Right (mod x y)
    | otherwise      -> Left (Mod (N x) (N y))
  (Left x,  Right y) -> Left (Mod x (N y))
  (Right x, Left y)  -> Left (Mod (N x) y)
  (Left x,  Left y)  -> Left (Mod x y)

evaluate (Max a b) = case (evaluate a, evaluate b) of
  (Right 0, y)       -> y
  (x, Right 0)       -> x
  (Right x, Right y) -> Right (max x y)

  (Left x,  Right y) -> Left (Max x (N y))
  (Right x, Left y)  -> evaluate (Max y (N x))
  (Left x,  Left y)  -> Left (Max x y)

evaluate (Min a b) = case (evaluate a, evaluate b) of
  (Right 0, _)       -> Right 0
  (_, Right 0)       -> Right 0
  (Right x, Right y) -> Right (min x y)
  (Left x,  Right y) -> Left (Min x (N y))
  (Right x, Left y)  -> evaluate (Min y (N x))
  (Left x,  Left y)  -> Left (Min x y)

evaluate (Log2 a)  = case evaluate a of
  Right 0 -> Left  (Log2 (N 0))
  Right x -> Right (log2Nat x)
  Left e  -> Left  (Log2 e)
