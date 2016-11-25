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
--import GHC.Prim
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




class TensorCalculus t (n :: [Nat]) (m :: [Nat]) where
  data Tensor t n m


instance TensorCalculus t '[] '[] where
  newtype Tensor t '[] '[] = Scalar t
    deriving Show

--instance TensorCalculus t '[1] '[] where
--  data Tensor t '[1] '[] = Vector1 t
--

instance TensorCalculus t '[n] '[] where
  data Tensor t '[n] '[] = Vector (SomeVector t n)

deriving instance Show (SomeVector t n) => Show (Tensor t '[n] '[])


instance TensorCalculus t '[] '[m] where
  data Tensor t '[] '[m] = CoVector (SomeVector t m)

deriving instance Show (SomeVector t m) => Show (Tensor t '[] '[m])

instance TensorCalculus t '[n0, n1] '[] where
  data Tensor t '[n0, n1] '[] = ContraContraMatrix (SomeMatrix t n0 n1)


deriving instance Show (SomeMatrix t n0 n1) => Show (Tensor t '[n0, n1] '[])

instance TensorCalculus t '[n0] '[m0] where
  data Tensor t '[n0] '[m0] = ContraCoMatrix (SomeMatrix t n0 m0)

deriving instance Show (SomeMatrix t n0 m0) => Show (Tensor t '[n0] '[m0])

instance TensorCalculus t '[] '[m0,m1] where
  data Tensor t '[] '[m0,m1] = CoCoMatrix (SomeMatrix t m0 m1)

deriving instance Show (SomeMatrix t m0 m1) => Show (Tensor t '[] '[m0,m1])

instance TensorCalculus t (n0 ': n1 ': n2 ': ns) '[] where
  data Tensor t (n0 ': n1 ': n2 ': ns) '[] = ContraTensor
    deriving Show

instance TensorCalculus t '[] (m0 ': m1 ': m2 ': ms) where
  data Tensor t '[] (m0 ': m1 ': m2 ': ms) = CoTensor
    deriving Show

instance TensorCalculus t (n0 ': n1 ': n2 ': ns) (m0 ': m1 ': m2 ': ms) where
  data Tensor t (n0 ': n1 ': n2 ': ns) (m0 ': m1 ': m2 ': ms) = Tensor
    deriving Show



--newtype Scalar t = Scalar t
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

