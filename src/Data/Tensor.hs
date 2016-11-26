{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tensor
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Data.Tensor where

import GHC.TypeLits
import GHC.Prim
import Data.Proxy



data Dim (ds :: [Nat]) = Dim

class Dimensions (ds :: [Nat]) where
  dims :: Dim ds -> [Int]



headDim :: Dim (d ': ds) -> Proxy d
headDim _ = Proxy

tailDim :: Dim (d ': ds) -> Dim ds
tailDim _ = Dim


instance Dimensions '[] where
  dims _ = []

instance (KnownNat d, Dimensions ds) => Dimensions (d ': ds) where
  dims x = (fromIntegral . natVal $ headDim x) : dims (tailDim x)


printCrazy :: Dimensions d => Dim d -> String
printCrazy d = show $ dims d

contraDimsType :: Tensor t n m -> Dim n
contraDimsType _ = Dim

coDimsType :: Tensor t n m -> Dim m
coDimsType _ = Dim

contraDims :: Dimensions n => Tensor t n m -> [Int]
contraDims = dims . contraDimsType

coDims :: Dimensions m => Tensor t n m -> [Int]
coDims = dims . coDimsType


type Vec2 = Tensor Double '[2] '[]


vec2 :: Double -> Double -> Vec2
vec2 x y = T10 $ Vector2 x y


class TensorCalculus t (ns :: [Nat]) (ms :: [Nat]) where
  data Tensor t ns ms
  type TensorStore t ns ms
  -- | Add a contravariant rank
  infixr 5 .<.
  (.<.) :: Tensor t ns ms -> Tensor t ns ms -> Tensor t (2 ': ns) ms
  -- | Add a covariant rank
  infixr 5 .>.
  (.>.) :: Tensor t ns ms -> Tensor t ns ms -> Tensor t ns (2 ': ms)
  -- | Append dimension of the first contravariant rank
  infixr 5 .<
  (.<)  :: AppendDim (TensorStore t ns ms) (TensorStore t (nb ': ns) ms) (TensorStore t ((nb + 1) ': ns) ms)
        => Tensor t ns ms -> Tensor t (nb ': ns) ms -> Tensor t ((nb + 1) ': ns) ms
  -- | Append dimension of the first covariant rank
  infixr 5 .>
  (.>)  :: AppendDim (TensorStore t ns ms) (TensorStore t ns (mb ': ms)) (TensorStore t ns ((mb + 1) ': ms))
        => Tensor t ns ms -> Tensor t ns (mb ': ms) -> Tensor t ns ((mb + 1) ': ms)

-- AppendDim (Tensor t ns ms) (Tensor t ns (mb ': ms)) (Tensor t ns ((mb + 1) ': ms))


instance TensorCalculus t '[] '[] where
  newtype Tensor t '[] '[] = T00 t deriving (Bounded, Enum, Eq, Integral, Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat, Show)
  type TensorStore t '[] '[] = t
  T00 a .<. T00 b = T10 $ Vector2 a b
  T00 a .>. T00 b = T01 $ Vector2 a b
  T00 a .<  T10 b = T10 $ appendDim a b
  T00 a .>  T01 b = T01 $ appendDim a b
instance TensorCalculus t '[n] '[] where
  newtype Tensor t '[n] '[] = T10 (SomeVector t n)
  type TensorStore t '[n] '[] = SomeVector t n
--  contraV (T00 a) (T00 b) = T10 $ Vector2 a b
instance TensorCalculus t '[] '[m] where
  newtype Tensor t '[] '[m] = T01 (SomeVector t m)
  type TensorStore t '[] '[m] = SomeVector t m
instance TensorCalculus t '[n1, n2] '[] where
  newtype Tensor t '[n1, n2] '[] = T20 (SomeMatrix t n1 n2)
  type TensorStore t '[n1, n2] '[] = SomeMatrix t n1 n2
instance TensorCalculus t '[n] '[m] where
  newtype Tensor t '[n] '[m] = T11 (SomeMatrix t n m)
  type TensorStore t '[n] '[m] = SomeMatrix t n m
instance TensorCalculus t '[] '[m1,m2] where
  newtype Tensor t '[] '[m1,m2] = T02 (SomeMatrix t m1 m2)
  type TensorStore t '[] '[m1,m2] = SomeMatrix t m1 m2
instance TensorCalculus t (n1 ': n2 ': n3 ': ns) '[] where
  newtype Tensor t (n1 ': n2 ': n3 ': ns) '[] = Tn0 (NDArray t) deriving Show
  type TensorStore t (n1 ': n2 ': n3 ': ns) '[] = NDArray t
instance TensorCalculus t (n1 ': n2 ': n3 ': ns) '[m0] where
  newtype Tensor t (n1 ': n2 ': n3 ': ns) '[m0] = Tn1 (NDArray t) deriving Show
  type TensorStore t (n1 ': n2 ': n3 ': ns) '[m0] = NDArray t
instance TensorCalculus t (n1 ': n2 ': n3 ': ns) '[m0,m1] where
  newtype Tensor t (n1 ': n2 ': n3 ': ns) '[m0, m1] = Tn2 (NDArray t) deriving Show
  type TensorStore t (n1 ': n2 ': n3 ': ns) '[m0, m1] = NDArray t
instance TensorCalculus t '[] (m1 ': m2 ': m3 ': ms) where
  newtype Tensor t '[] (m1 ': m2 ': m3 ': ms) = T0m (NDArray t) deriving Show
  type TensorStore t '[] (m1 ': m2 ': m3 ': ms) = NDArray t
instance TensorCalculus t '[n1] (m1 ': m2 ': m3 ': ms) where
  newtype Tensor t '[n1] (m1 ': m2 ': m3 ': ms) = T1m (NDArray t) deriving Show
  type TensorStore t '[n1] (m1 ': m2 ': m3 ': ms) = NDArray t
instance TensorCalculus t '[n1, n2] (m1 ': m2 ': m3 ': ms) where
  newtype Tensor t '[n1, n2] (m1 ': m2 ': m3 ': ms) = T2m (NDArray t) deriving Show
  type TensorStore t '[n1, n2] (m1 ': m2 ': m3 ': ms) = NDArray t
instance TensorCalculus t (n1 ': n2 ': n3 ': ns) (m1 ': m2 ': m3 ': ms) where
  newtype Tensor t (n1 ': n2 ': n3 ': ns) (m1 ': m2 ': m3 ': ms) = Tnm (NDArray t) deriving Show
  type TensorStore t (n1 ': n2 ': n3 ': ns) (m1 ': m2 ': m3 ': ms) = NDArray t



deriving instance Show (SomeVector t n) => Show (Tensor t '[n] '[])
deriving instance Plus (SomeVector t n) => Plus (Tensor t '[n] '[])
deriving instance Show (SomeVector t m) => Show (Tensor t '[] '[m])
deriving instance Plus (SomeVector t m) => Plus (Tensor t '[] '[m])
deriving instance Show (SomeMatrix t n0 n1) => Show (Tensor t '[n0, n1] '[])
deriving instance Show (SomeMatrix t n0 m0) => Show (Tensor t '[n0] '[m0])
deriving instance Show (SomeMatrix t m0 m1) => Show (Tensor t '[] '[m0,m1])


--contraV :: Tensor t ns ms -> Tensor t ns ms -> Tensor t (2 ':. ns) ms
--contraV


class Plus a where
  plus :: a -> a -> a

instance Num t => Plus (Vector1 t) where
  plus (Vector1 a) (Vector1 b) = Vector1 (a+b)

instance Num t => Plus (Vector2 t) where
  plus (Vector2 a1 a2) (Vector2 b1 b2) = Vector2 (a1+b1) (a2+b2)

instance Num t => Plus (Vector3 t) where
  plus (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a1+b1) (a2+b2) (a3+b3)

instance Num t => Plus (Vector4 t) where
  plus (Vector4 a1 a2 a3 a4) (Vector4 b1 b2 b3 b4) = Vector4 (a1+b1) (a2+b2) (a3+b3) (a4+b4)

instance Num t => Plus (VectorN t n) where
  plus (VectorN as) (VectorN bs) = VectorN $ zipWith (+) as bs




newtype Vector1 t = Vector1 t
  deriving Show
data    Vector2 t = Vector2 t t
  deriving Show
data    Vector3 t = Vector3 t t t
  deriving Show
data    Vector4 t = Vector4 t t t t
  deriving Show
newtype VectorN t (n::Nat) = VectorN [t]
  deriving Show

newtype Matrix1x1 t = Matrix1x1 t
  deriving Show
data    Matrix2x2 t = Matrix2x2 t t t t
  deriving Show
newtype MatrixNxM t (n::Nat) (m::Nat) = MatrixNxM [[t]]
  deriving Show


data NDArray t = NDArray ByteArray#

instance Show (NDArray t) where
  show _ = "Big array"

--data TensorContraOnly t (ns :: [Nat]) = TContra Addr#
--data TensorCoOnly     t (ns :: [Nat]) = TCo Addr#
--data TensorCo         t (ns :: [Nat]) (ms :: [Nat]) = T Addr#

type family SomeVector t (n :: Nat) = v | v -> t n where
  SomeVector t 1 = Vector1 t
  SomeVector t 2 = Vector2 t
  SomeVector t 3 = Vector3 t
  SomeVector t 4 = Vector4 t
  SomeVector t n = VectorN t n


type family SomeMatrix t (n :: Nat) (m :: Nat) = v | v -> t n m where
  SomeMatrix t 1 1 = Matrix1x1 t
  SomeMatrix t 2 2 = Matrix2x2 t
  SomeMatrix t n m = MatrixNxM t n m


class AppendDim a b c | a b -> c where
  appendDim :: a -> b -> c

instance AppendDim t (Vector2 t) (Vector3 t) where
  appendDim a (Vector2 b1 b2) = Vector3 a b1 b2
instance AppendDim t (Vector3 t) (Vector4 t) where
  appendDim a (Vector3 b1 b2 b3) = Vector4 a b1 b2 b3
instance AppendDim t (Vector4 t) (VectorN t 5) where
  appendDim a (Vector4 b1 b2 b3 b4) = VectorN [a,b1,b2,b3,b4]
instance (m ~ (n+1), 5 <= m, 4 <= n) => AppendDim t (VectorN t n) (VectorN t m) where
  appendDim a (VectorN bs) = VectorN $ a : bs


--class VectorOps a b c | a b -> c where
--  appendVecs :: a -> b -> c
--
--
--instance VectorOps (Vector1 t) (Vector1 t) (Vector2 t) where
--  appendVecs (Vector1 a) (Vector1 b) = Vector2 a b
--instance VectorOps (Vector2 t) (Vector1 t) (Vector3 t) where
--  appendVecs (Vector2 a1 a2) (Vector1 b) = Vector3 a1 a2 b
--instance VectorOps (Vector3 t) (Vector1 t) (Vector4 t) where
--  appendVecs (Vector3 a1 a2 a3) (Vector1 b) = Vector4 a1 a2 a3 b
--instance VectorOps (Vector4 t) (Vector1 t) (VectorN t 5) where
--  appendVecs (Vector4 a1 a2 a3 a4) (Vector1 b) = VectorN [a1,a2,a3,a4,b]
--instance m ~ (n+1) => VectorOps (VectorN t n) (Vector1 t) (VectorN t m) where
--  appendVecs (VectorN as) (Vector1 b) = VectorN $ as ++ [b]
--
--instance VectorOps (Vector1 t) (Vector2 t) (Vector3 t) where
--  appendVecs (Vector1 a) (Vector2 b1 b2) = Vector3 a b1 b2
--instance VectorOps (Vector2 t) (Vector2 t) (Vector4 t) where
--  appendVecs (Vector2 a1 a2) (Vector2 b1 b2) = Vector4 a1 a2 b1 b2
--instance VectorOps (Vector3 t) (Vector2 t) (VectorN t 5) where
--  appendVecs (Vector3 a1 a2 a3) (Vector2 b1 b2) = VectorN [a1,a2,a3,b1,b2]
--instance VectorOps (Vector4 t) (Vector2 t) (VectorN t 6) where
--  appendVecs (Vector4 a1 a2 a3 a4) (Vector2 b1 b2) = VectorN [a1,a2,a3,a4,b1,b2]
--instance m ~ (n+2) => VectorOps (VectorN t n) (Vector2 t) (VectorN t m) where
--  appendVecs (VectorN as) (Vector2 b1 b2) = VectorN $ as ++ [b1,b2]
--
--instance VectorOps (Vector1 t) (Vector3 t) (Vector4 t) where
--  appendVecs (Vector1 a) (Vector3 b1 b2 b3) = Vector4 a b1 b2 b3
--instance VectorOps (Vector2 t) (Vector3 t) (VectorN t 5) where
--  appendVecs (Vector2 a1 a2) (Vector3 b1 b2 b3) = VectorN [a1,a2,b1,b2,b3]
--instance VectorOps (Vector3 t) (Vector3 t) (VectorN t 6) where
--  appendVecs (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = VectorN [a1,a2,a3,b1,b2,b3]
--instance VectorOps (Vector4 t) (Vector3 t) (VectorN t 7) where
--  appendVecs (Vector4 a1 a2 a3 a4) (Vector3 b1 b2 b3) = VectorN [a1,a2,a3,a4,b1,b2,b3]
--instance m ~ (n+3) => VectorOps (VectorN t n) (Vector3 t) (VectorN t m) where
--  appendVecs (VectorN as) (Vector3 b1 b2 b3) = VectorN $ as ++ [b1,b2,b3]
--
--instance VectorOps (Vector1 t) (Vector4 t) (VectorN t 5) where
--  appendVecs (Vector1 a) (Vector4 b1 b2 b3 b4) = VectorN [a,b1,b2,b3,b4]
--instance VectorOps (Vector2 t) (Vector4 t) (VectorN t 6) where
--  appendVecs (Vector2 a1 a2) (Vector4 b1 b2 b3 b4) = VectorN [a1,a2,b1,b2,b3,b4]
--instance VectorOps (Vector3 t) (Vector4 t) (VectorN t 7) where
--  appendVecs (Vector3 a1 a2 a3) (Vector4 b1 b2 b3 b4) = VectorN [a1,a2,a3,b1,b2,b3,b4]
--instance VectorOps (Vector4 t) (Vector4 t) (VectorN t 8) where
--  appendVecs (Vector4 a1 a2 a3 a4) (Vector4 b1 b2 b3 b4) = VectorN [a1,a2,a3,a4,b1,b2,b3,b4]
--instance m ~ (n+4) => VectorOps (VectorN t n) (Vector4 t) (VectorN t m) where
--  appendVecs (VectorN as) (Vector4 b1 b2 b3 b4) = VectorN $ as ++ [b1,b2,b3,b4]
--
--instance k ~ (m+1) => VectorOps (Vector1 t) (VectorN t m) (VectorN t k) where
--  appendVecs (Vector1 a) (VectorN bs) = VectorN $ a : bs
--instance k ~ (m+2) => VectorOps (Vector2 t) (VectorN t m) (VectorN t 6) where
--  appendVecs (Vector2 a1 a2) (VectorN bs) = VectorN $ a1 : a2 : bs
--instance k ~ (m+3) => VectorOps (Vector3 t) (VectorN t m) (VectorN t 7) where
--  appendVecs (Vector3 a1 a2 a3) (VectorN bs) = VectorN $ a1 : a2 : a3 : bs
--instance k ~ (m+4) => VectorOps (Vector4 t) (VectorN t m) (VectorN t 8) where
--  appendVecs (Vector4 a1 a2 a3 a4) (VectorN bs) = VectorN $ a1 : a2 : a3 : a4 : bs
--instance k ~ (m+n) => VectorOps (VectorN t n) (VectorN t m) (VectorN t m) where
--  appendVecs (VectorN as) (VectorN bs) = VectorN $ as ++ bs

