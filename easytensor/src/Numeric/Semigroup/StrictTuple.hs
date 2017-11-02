{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Semigroup.StrictTuple
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This module defines a set of tuple data types to substitute normal lazy tuples.
-- The reason is that @Monoid@ instances of normal tuples are lazy,
-- which makes folds with arithmetic operations leak memory.
-- @Semigroup@ and @Monoid@ instances of these tuples are strict in all their arguments.
--
-- Using tuple types defined here together with @Numeric.Semigroup.foldMap'@,
-- one can combine multiple monoidal fold structures in a single pass over foldable container:
--
-- >> foldMap' (T3 <$> Max <*> Sum <*> minMax) $ take 100000000 ([1..] :: [Int])
--
-- The example above runs in constant space, which would not happen with normal
--  GHC tuples due to strictness properties of their `mappend` implementations
--  (tuple arguments are not enforced).
-----------------------------------------------------------------------------
module Numeric.Semigroup.StrictTuple
    ( T0 (..), T1 (..), T2 (..), T3 (..), T4 (..)
    , T5 (..), T6 (..), T7 (..), T8 (..), T9 (..)
    , StrictTuple (..)
    , strictT0, strictT1, strictT2, strictT3, strictT4
    , strictT5, strictT6, strictT7, strictT8, strictT9
    ) where

import           Data.Bifunctor
import           Data.Coerce       (coerce)
import           Data.Data
import           GHC.Generics
import           Numeric.Semigroup

data T0 = T0
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic)
newtype T1 a = T1 a
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T2 a b = T2 a b
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T3 a b c = T3 a b c
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T4 a b c d = T4 a b c d
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T5 a b c d e = T5 a b c d e
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T6 a b c d e f = T6 a b c d e f
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T7 a b c d e f g = T7 a b c d e f g
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T8 a b c d e f g h = T8 a b c d e f g h
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)
data T9 a b c d e f g h i = T9 a b c d e f g h i
  deriving (Eq, Show, Read, Ord, Data, Typeable, Generic, Generic1)

strictT0 :: T0
strictT0 = T0
strictT1 :: a -> T1 a
strictT1 !a = T1 a
strictT2 :: a -> b -> T2 a b
strictT2 !a !b = T2 a b
strictT3 :: a -> b -> c -> T3 a b c
strictT3 !a !b !c = T3 a b c
strictT4 :: a -> b -> c -> d -> T4 a b c d
strictT4 !a !b !c !d = T4 a b c d
strictT5 :: a -> b -> c -> d -> e -> T5 a b c d e
strictT5 !a !b !c !d !e = T5 a b c d e
strictT6 :: a -> b -> c -> d -> e -> f -> T6 a b c d e f
strictT6 !a !b !c !d !e !f = T6 a b c d e f
strictT7 :: a -> b -> c -> d -> e -> f -> g -> T7 a b c d e f g
strictT7 !a !b !c !d !e !f !g = T7 a b c d e f g
strictT8 :: a -> b -> c -> d -> e -> f -> g -> h -> T8 a b c d e f g h
strictT8 !a !b !c !d !e !f !g !h = T8 a b c d e f g h
strictT9 :: a -> b -> c -> d -> e -> f -> g -> h -> i -> T9 a b c d e f g h i
strictT9 !a !b !c !d !e !f !g !h !i = T9 a b c d e f g h i

instance Semigroup T0 where
    _ <> _ = T0
instance Semigroup a => Semigroup (T1 a) where
    (<>) = coerce ((<>) :: a -> a -> a)
instance ( Semigroup a
         , Semigroup b
         ) => Semigroup (T2 a b) where
    (T2 !ax !bx) <> (T2 !ay !by) = strictT2 (ax <> ay) (bx <> by)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         ) => Semigroup (T3 a b c) where
    (T3 !ax !bx !cx) <> (T3 !ay !by !cy)
      = strictT3 (ax <> ay) (bx <> by) (cx <> cy)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         ) => Semigroup (T4 a b c d) where
    (T4 !ax !bx !cx !dx) <> (T4 !ay !by !cy !dy)
      = strictT4 (ax <> ay) (bx <> by) (cx <> cy) (dx <> dy)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         , Semigroup e
         ) => Semigroup (T5 a b c d e) where
    (T5 !ax !bx !cx !dx !ex) <> (T5 !ay !by !cy !dy !ey)
      = strictT5 (ax <> ay) (bx <> by) (cx <> cy) (dx <> dy) (ex <> ey)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         , Semigroup e
         , Semigroup f
         ) => Semigroup (T6 a b c d e f) where
    (T6 !ax !bx !cx !dx !ex !fx) <> (T6 !ay !by !cy !dy !ey !fy)
      = strictT6 (ax <> ay) (bx <> by) (cx <> cy) (dx <> dy) (ex <> ey) (fx <> fy)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         , Semigroup e
         , Semigroup f
         , Semigroup g
         ) => Semigroup (T7 a b c d e f g) where
    (T7 !ax !bx !cx !dx !ex !fx !gx) <> (T7 !ay !by !cy !dy !ey !fy !gy)
      = strictT7 (ax <> ay) (bx <> by) (cx <> cy) (dx <> dy) (ex <> ey)
           (fx <> fy) (gx <> gy)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         , Semigroup e
         , Semigroup f
         , Semigroup g
         , Semigroup h
         ) => Semigroup (T8 a b c d e f g h) where
    (T8 !ax !bx !cx !dx !ex !fx !gx !hx) <> (T8 !ay !by !cy !dy !ey !fy !gy !hy)
      = strictT8 (ax <> ay) (bx <> by) (cx <> cy) (dx <> dy) (ex <> ey)
           (fx <> fy) (gx <> gy) (hx <> hy)
instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         , Semigroup e
         , Semigroup f
         , Semigroup g
         , Semigroup h
         , Semigroup i
         ) => Semigroup (T9 a b c d e f g h i) where
    (T9 !ax !bx !cx !dx !ex !fx !gx !hx !ix) <> (T9 !ay !by !cy !dy !ey !fy !gy !hy !iy)
      = strictT9 (ax <> ay) (bx <> by) (cx <> cy) (dx <> dy) (ex <> ey)
           (fx <> fy) (gx <> gy) (hx <> hy) (ix <> iy)



instance Monoid T0 where
    mempty = strictT0
    mappend T0 T0 = strictT0
instance Monoid a => Monoid (T1 a) where
    mempty = strictT1 mempty
    mappend = coerce (mappend :: a -> a -> a)
instance ( Monoid a
         , Monoid b
         ) => Monoid (T2 a b) where
    mempty = strictT2 mempty mempty
    mappend (T2 !ax !bx) (T2 !ay !by) = strictT2 (mappend ax ay) (mappend bx by)
instance ( Monoid a
         , Monoid b
         , Monoid c
         ) => Monoid (T3 a b c) where
    mempty = strictT3 mempty mempty mempty
    mappend (T3 !ax !bx !cx) (T3 !ay !by !cy)
      = strictT3 (mappend ax ay) (mappend bx by) (mappend cx cy)
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         ) => Monoid (T4 a b c d) where
    mempty = strictT4 mempty mempty mempty mempty
    mappend (T4 !ax !bx !cx !dx) (T4 !ay !by !cy !dy)
      = strictT4 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy)
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         ) => Monoid (T5 a b c d e) where
    mempty = strictT5 mempty mempty mempty mempty mempty
    mappend (T5 !ax !bx !cx !dx !ex) (T5 !ay !by !cy !dy !ey)
      = strictT5 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         ) => Monoid (T6 a b c d e f) where
    mempty = strictT6 mempty mempty mempty mempty mempty mempty
    mappend (T6 !ax !bx !cx !dx !ex !fx) (T6 !ay !by !cy !dy !ey !fy)
      = strictT6 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey) (mappend fx fy)
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         , Monoid g
         ) => Monoid (T7 a b c d e f g) where
    mempty = strictT7 mempty mempty mempty mempty mempty mempty mempty
    mappend (T7 !ax !bx !cx !dx !ex !fx !gx) (T7 !ay !by !cy !dy !ey !fy !gy)
      = strictT7 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
           (mappend fx fy) (mappend gx gy)
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         , Monoid g
         , Monoid h
         ) => Monoid (T8 a b c d e f g h) where
    mempty = strictT8 mempty mempty mempty mempty mempty mempty mempty mempty
    mappend (T8 !ax !bx !cx !dx !ex !fx !gx !hx) (T8 !ay !by !cy !dy !ey !fy !gy !hy)
      = strictT8 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
           (mappend fx fy) (mappend gx gy) (mappend hx hy)
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         , Monoid g
         , Monoid h
         , Monoid i
         ) => Monoid (T9 a b c d e f g h i) where
    mempty = strictT9 mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend (T9 !ax !bx !cx !dx !ex !fx !gx !hx !ix) (T9 !ay !by !cy !dy !ey !fy !gy !hy !iy)
      = strictT9 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
           (mappend fx fy) (mappend gx gy) (mappend hx hy) (mappend ix iy)


instance Functor T1 where
    fmap = coerce
instance Functor (T2 a) where
    fmap fun ~(T2 a b) = T2 a (fun b)
instance Functor (T3 a b) where
    fmap fun ~(T3 a b c) = T3 a b (fun c)
instance Functor (T4 a b c) where
    fmap fun ~(T4 a b c d) = T4 a b c (fun d)
instance Functor (T5 a b c d) where
    fmap fun ~(T5 a b c d e) = T5 a b c d (fun e)
instance Functor (T6 a b c d e) where
    fmap fun ~(T6 a b c d e f) = T6 a b c d e (fun f)
instance Functor (T7 a b c d e f) where
    fmap fun ~(T7 a b c d e f g) = T7 a b c d e f (fun g)
instance Functor (T8 a b c d e f g) where
    fmap fun ~(T8 a b c d e f g h) = T8 a b c d e f g (fun h)
instance Functor (T9 a b c d e f g h) where
    fmap fun ~(T9 a b c d e f g h i) = T9 a b c d e f g h (fun i)

instance Applicative T1 where
    pure = T1
    (<*>) = coerce
instance ( Monoid a
         ) => Applicative (T2 a) where
    pure = T2 mempty
    (T2 !ax fun) <*> (T2 !ay val)
      = ($ fun val)
     <$> strictT2 (mappend ax ay) id
instance ( Monoid a
         , Monoid b
         ) => Applicative (T3 a b) where
    pure = T3 mempty mempty
    (T3 !ax !bx fun) <*> (T3 !ay !by val)
      = ($ fun val)
     <$> strictT3 (mappend ax ay) (mappend bx by) id
instance ( Monoid a
         , Monoid b
         , Monoid c
         ) => Applicative (T4 a b c) where
    pure = T4 mempty mempty mempty
    (T4 !ax !bx !cx fun) <*> (T4 !ay !by !cy val)
      = ($ fun val)
     <$> strictT4 (mappend ax ay) (mappend bx by) (mappend cx cy) id
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         ) => Applicative (T5 a b c d) where
    pure = T5 mempty mempty mempty mempty
    (T5 !ax !bx !cx !dx fun) <*> (T5 !ay !by !cy !dy val)
      = ($ fun val)
     <$> strictT5 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) id
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         ) => Applicative (T6 a b c d e) where
    pure = T6 mempty mempty mempty mempty mempty
    (T6 !ax !bx !cx !dx !ex fun) <*> (T6 !ay !by !cy !dy !ey val)
      = ($ fun val)
     <$> strictT6 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey) id
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         ) => Applicative (T7 a b c d e f) where
    pure = T7 mempty mempty mempty mempty mempty mempty
    (T7 !ax !bx !cx !dx !ex !fx fun) <*> (T7 !ay !by !cy !dy !ey !fy val)
      = ($ fun val)
     <$> strictT7 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
                  (mappend fx fy) id
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         , Monoid g
         ) => Applicative (T8 a b c d e f g) where
    pure = T8 mempty mempty mempty mempty mempty mempty mempty
    (T8 !ax !bx !cx !dx !ex !fx !gx fun) <*> (T8 !ay !by !cy !dy !ey !fy !gy val)
      = ($ fun val)
     <$> strictT8 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
                  (mappend fx fy) (mappend gx gy) id
instance ( Monoid a
         , Monoid b
         , Monoid c
         , Monoid d
         , Monoid e
         , Monoid f
         , Monoid g
         , Monoid h
         ) => Applicative (T9 a b c d e f g h) where
    pure = T9 mempty mempty mempty mempty mempty mempty mempty mempty
    (T9 !ax !bx !cx !dx !ex !fx !gx !hx fun) <*> (T9 !ay !by !cy !dy !ey !fy !gy !hy val)
      = ($ fun val)
     <$> strictT9 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
                  (mappend fx fy) (mappend gx gy) (mappend hx hy) id



instance Monad T1 where
    m >>= k = k (coerce m)
instance ( Monoid a
         ) => Monad (T2 a) where
    (T2 !ax x) >>= k = case k x of
      (T2 !ay val) -> ($ val) <$>
         strictT2 (mappend ax ay) id
instance ( Monoid a, Monoid b
         ) => Monad (T3 a b) where
    (T3 !ax !bx x) >>= k = case k x of
      (T3 !ay !by val) -> ($ val) <$>
         strictT3 (mappend ax ay) (mappend bx by) id
instance ( Monoid a, Monoid b, Monoid c
         ) => Monad (T4 a b c) where
    (T4 !ax !bx !cx x) >>= k = case k x of
      (T4 !ay !by !cy val) -> ($ val) <$>
         strictT4 (mappend ax ay) (mappend bx by) (mappend cx cy) id
instance ( Monoid a, Monoid b, Monoid c, Monoid d
         ) => Monad (T5 a b c d) where
    (T5 !ax !bx !cx !dx x) >>= k = case k x of
      (T5 !ay !by !cy !dy val) -> ($ val) <$>
         strictT5 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) id
instance ( Monoid a, Monoid b, Monoid c, Monoid d
         , Monoid e
         ) => Monad (T6 a b c d e) where
    (T6 !ax !bx !cx !dx !ex x) >>= k = case k x of
      (T6 !ay !by !cy !dy !ey val) -> ($ val) <$>
         strictT6 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey) id
instance ( Monoid a, Monoid b, Monoid c, Monoid d
         , Monoid e, Monoid f
         ) => Monad (T7 a b c d e f) where
    (T7 !ax !bx !cx !dx !ex !fx x) >>= k = case k x of
      (T7 !ay !by !cy !dy !ey !fy val) -> ($ val) <$>
         strictT7 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
                  (mappend fx fy) id
instance ( Monoid a, Monoid b, Monoid c, Monoid d
         , Monoid e, Monoid f, Monoid g
         ) => Monad (T8 a b c d e f g) where
    (T8 !ax !bx !cx !dx !ex !fx !gx x) >>= k = case k x of
      (T8 !ay !by !cy !dy !ey !fy !gy val) -> ($ val) <$>
         strictT8 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
                  (mappend fx fy) (mappend gx gy) id
instance ( Monoid a, Monoid b, Monoid c, Monoid d
         , Monoid e, Monoid f, Monoid g, Monoid h
         ) => Monad (T9 a b c d e f g h) where
    (T9 !ax !bx !cx !dx !ex !fx !gx !hx x) >>= k = case k x of
      (T9 !ay !by !cy !dy !ey !fy !gy !hy val) -> ($ val) <$>
         strictT9 (mappend ax ay) (mappend bx by) (mappend cx cy) (mappend dx dy) (mappend ex ey)
                  (mappend fx fy) (mappend gx gy) (mappend hx hy) id

instance Bounded T0 where
    minBound = T0
    maxBound = T0
instance ( Bounded a
         ) => Bounded (T1 a) where
    minBound = strictT1 minBound
    maxBound = strictT1 maxBound
instance ( Bounded a, Bounded b
         ) => Bounded (T2 a b) where
    minBound = strictT2 minBound minBound
    maxBound = strictT2 maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c
         ) => Bounded (T3 a b c) where
    minBound = strictT3 minBound minBound minBound
    maxBound = strictT3 maxBound maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c, Bounded d
         ) => Bounded (T4 a b c d) where
    minBound = strictT4 minBound minBound minBound minBound
    maxBound = strictT4 maxBound maxBound maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c, Bounded d
         , Bounded e
         ) => Bounded (T5 a b c d e) where
    minBound = strictT5 minBound minBound minBound minBound minBound
    maxBound = strictT5 maxBound maxBound maxBound maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c, Bounded d
         , Bounded e, Bounded f
         ) => Bounded (T6 a b c d e f) where
    minBound = strictT6 minBound minBound minBound minBound minBound minBound
    maxBound = strictT6 maxBound maxBound maxBound maxBound maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c, Bounded d
         , Bounded e, Bounded f, Bounded g
         ) => Bounded (T7 a b c d e f g) where
    minBound = strictT7 minBound minBound minBound minBound minBound minBound minBound
    maxBound = strictT7 maxBound maxBound maxBound maxBound maxBound maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c, Bounded d
         , Bounded e, Bounded f, Bounded g, Bounded h
         ) => Bounded (T8 a b c d e f g h) where
    minBound = strictT8 minBound minBound minBound minBound minBound minBound minBound minBound
    maxBound = strictT8 maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound
instance ( Bounded a, Bounded b, Bounded c, Bounded d
         , Bounded e, Bounded f, Bounded g, Bounded h, Bounded i
         ) => Bounded (T9 a b c d e f g h i) where
    minBound = strictT9 minBound minBound minBound minBound minBound minBound minBound minBound minBound
    maxBound = strictT9 maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound maxBound

instance Foldable T1 where
    foldMap = coerce
    foldr f z y = f (coerce y) z
instance Foldable (T2 a) where
    foldMap f ~(T2 _ x) = f x
    foldr f z ~(T2 _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T3 a b) where
    foldMap f ~(T3 _ _ x) = f x
    foldr f z ~(T3 _ _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T4 a b c) where
    foldMap f ~(T4 _ _ _ x) = f x
    foldr f z ~(T4 _ _ _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T5 a b c e) where
    foldMap f ~(T5 _ _ _ _ x) = f x
    foldr f z ~(T5 _ _ _ _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T6 a b c d e) where
    foldMap f ~(T6 _ _ _ _ _ x) = f x
    foldr f z ~(T6 _ _ _ _ _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T7 a b c d e f) where
    foldMap f ~(T7 _ _ _ _ _ _ x) = f x
    foldr f z ~(T7 _ _ _ _ _ _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T8 a b c d e f g) where
    foldMap f ~(T8 _ _ _ _ _ _ _ x) = f x
    foldr f z ~(T8 _ _ _ _ _ _ _ x) = f x z
    length _ = 1
    null _ = False
instance Foldable (T9 a b c d e f g h) where
    foldMap f ~(T9 _ _ _ _ _ _ _ _ x) = f x
    foldr f z ~(T9 _ _ _ _ _ _ _ _ x) = f x z
    length _ = 1
    null _ = False

instance Traversable T1 where
    traverse f = fmap T1 . coerce f
instance Traversable (T2 a) where
    traverse fun ~(T2 a b) = T2 a <$> fun b
instance Traversable (T3 a b) where
    traverse fun ~(T3 a b c) = T3 a b <$> fun c
instance Traversable (T4 a b c) where
    traverse fun ~(T4 a b c d) = T4 a b c <$> fun d
instance Traversable (T5 a b c d) where
    traverse fun ~(T5 a b c d e) = T5 a b c d <$> fun e
instance Traversable (T6 a b c d e) where
    traverse fun ~(T6 a b c d e f) = T6 a b c d e <$> fun f
instance Traversable (T7 a b c d e f) where
    traverse fun ~(T7 a b c d e f g) = T7 a b c d e f <$> fun g
instance Traversable (T8 a b c d e f g) where
    traverse fun ~(T8 a b c d e f g h) = T8 a b c d e f g <$> fun h
instance Traversable (T9 a b c d e f g h) where
    traverse fun ~(T9 a b c d e f g h i) = T9 a b c d e f g h <$> fun i


instance Bifunctor T2 where
    first  funA ~(T2 a b) = T2 (funA a) b
    second funB ~(T2 a b) = T2 a (funB b)
    bimap  funA funB ~(T2 a b) = T2 (funA a) (funB b)
instance Bifunctor (T3 a) where
    first  funA ~(T3 a b c) = T3 a (funA b) c
    second funB ~(T3 a b c) = T3 a b (funB c)
    bimap  funA funB ~(T3 a b c) = T3 a (funA b) (funB c)
instance Bifunctor (T4 a b) where
    first  funA ~(T4 a b c d) = T4 a b (funA c) d
    second funB ~(T4 a b c d) = T4 a b c (funB d)
    bimap  funA funB ~(T4 a b c d) = T4 a b (funA c) (funB d)
instance Bifunctor (T5 a b c) where
    first  funA ~(T5 a b c d e) = T5 a b c (funA d) e
    second funB ~(T5 a b c d e) = T5 a b c d (funB e)
    bimap  funA funB ~(T5 a b c d e) = T5 a b c (funA d) (funB e)
instance Bifunctor (T6 a b c d) where
    first  funA ~(T6 a b c d e f) = T6 a b c d (funA e) f
    second funB ~(T6 a b c d e f) = T6 a b c d e (funB f)
    bimap  funA funB ~(T6 a b c d e f) = T6 a b c d (funA e) (funB f)
instance Bifunctor (T7 a b c d e) where
    first  funA ~(T7 a b c d e f g) = T7 a b c d e (funA f) g
    second funB ~(T7 a b c d e f g) = T7 a b c d e f (funB g)
    bimap  funA funB ~(T7 a b c d e f g) = T7 a b c d e (funA f) (funB g)
instance Bifunctor (T8 a b c d e f) where
    first  funA ~(T8 a b c d e f g h) = T8 a b c d e f (funA g) h
    second funB ~(T8 a b c d e f g h) = T8 a b c d e f g (funB h)
    bimap  funA funB ~(T8 a b c d e f g h) = T8 a b c d e f (funA g) (funB h)
instance Bifunctor (T9 a b c d e f g) where
    first  funA ~(T9 a b c d e f g h i) = T9 a b c d e f g (funA h) i
    second funB ~(T9 a b c d e f g h i) = T9 a b c d e f g h (funB i)
    bimap  funA funB ~(T9 a b c d e f g h i) = T9 a b c d e f g (funA h) (funB i)


class StrictTuple a b | a -> b, b -> a where
    toStrictTuple :: a -> b
    fromStrictTuple :: b -> a

instance StrictTuple () T0 where
    toStrictTuple () = T0
    fromStrictTuple T0 = ()
-- instance StrictTuple a (T1 a) where
--     toStrictTuple !a = T1 a
--     fromStrictTuple (T1 !a) = a
instance StrictTuple (a,b) (T2 a b) where
    toStrictTuple (!a,!b) = T2 a b
    fromStrictTuple (T2 !a !b) = (a,b)
instance StrictTuple (a,b,c) (T3 a b c) where
    toStrictTuple (!a,!b,!c) = T3 a b c
    fromStrictTuple (T3 !a !b !c) = (a,b,c)
instance StrictTuple (a,b,c,d) (T4 a b c d) where
    toStrictTuple (!a,!b,!c,!d) = T4 a b c d
    fromStrictTuple (T4 !a !b !c !d) = (a,b,c,d)
instance StrictTuple (a,b,c,d,e) (T5 a b c d e) where
    toStrictTuple (!a,!b,!c,!d,!e) = T5 a b c d e
    fromStrictTuple (T5 !a !b !c !d !e) = (a,b,c,d,e)
instance StrictTuple (a,b,c,d,e,f) (T6 a b c d e f) where
    toStrictTuple (!a,!b,!c,!d,!e,!f) = T6 a b c d e f
    fromStrictTuple (T6 !a !b !c !d !e !f) = (a,b,c,d,e,f)
instance StrictTuple (a,b,c,d,e,f,g) (T7 a b c d e f g) where
    toStrictTuple (!a,!b,!c,!d,!e,!f,!g) = T7 a b c d e f g
    fromStrictTuple (T7 !a !b !c !d !e !f !g) = (a,b,c,d,e,f,g)
instance StrictTuple (a,b,c,d,e,f,g,h) (T8 a b c d e f g h) where
    toStrictTuple (!a,!b,!c,!d,!e,!f,!g,!h) = T8 a b c d e f g h
    fromStrictTuple (T8 !a !b !c !d !e !f !g !h) = (a,b,c,d,e,f,g,h)
instance StrictTuple (a,b,c,d,e,f,g,h,i) (T9 a b c d e f g h i) where
    toStrictTuple (!a,!b,!c,!d,!e,!f,!g,!h,!i) = T9 a b c d e f g h i
    fromStrictTuple (T9 !a !b !c !d !e !f !g !h !i) = (a,b,c,d,e,f,g,h,i)
