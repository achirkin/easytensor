{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.EasyTensor
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.EasyTensor
  ( Tensor (..)
  , Numeric.EasyTensor.prod
  ) where

import GHC.TypeLits
import GHC.Prim
import Data.Proxy

import Numeric.Vector as V
import Numeric.Vector.Family
import Numeric.Matrix as M
import Numeric.Matrix.Family
import Numeric.Commons


newtype Tensor t n m = Tensor (TT t n m)
instance Show (TT t n m) => Show (Tensor t n m) where
  show (Tensor t) = show t
deriving instance Eq (TT t n m) => Eq (Tensor t n m)
deriving instance Ord (TT t n m) => Ord (Tensor t n m)
deriving instance Num (TT t n m) => Num (Tensor t n m)
deriving instance Fractional (TT t n m) => Fractional (Tensor t n m)
deriving instance Floating (TT t n m) => Floating (Tensor t n m)
deriving instance PrimBytes (TT t n m) => PrimBytes (Tensor t n m)




newtype Scalar t = Scalar t
  deriving (Bounded, Enum, Eq, Integral, Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"

newtype CoVector t n = CoVector (Vector t n)
instance Show (Vector t n) => Show (CoVector t n) where
  show (CoVector t) = show t
deriving instance Eq (Vector t n) => Eq (CoVector t n)
deriving instance Ord (Vector t n) => Ord (CoVector t n)
deriving instance Num (Vector t n) => Num (CoVector t n)
deriving instance Fractional (Vector t n) => Fractional (CoVector t n)
deriving instance Floating (Vector t n) => Floating (CoVector t n)
deriving instance PrimBytes (Vector t n) => PrimBytes (CoVector t n)


newtype ContraVector t n = ContraVector (Vector t n)
instance Show (Vector t n) => Show (ContraVector t n) where
  show (ContraVector t) = show t ++ "'"
deriving instance Eq (Vector t n) => Eq (ContraVector t n)
deriving instance Ord (Vector t n) => Ord (ContraVector t n)
deriving instance Num (Vector t n) => Num (ContraVector t n)
deriving instance Fractional (Vector t n) => Fractional (ContraVector t n)
deriving instance Floating (Vector t n) => Floating (ContraVector t n)
deriving instance PrimBytes (Vector t n) => PrimBytes (ContraVector t n)

newtype MatrixT t n m = MatrixT (Matrix t n m)
instance Show (Matrix t n m) => Show (MatrixT t n m) where
  show (MatrixT t) = show t
deriving instance Eq (Matrix t n m) => Eq (MatrixT t n m)
deriving instance Ord (Matrix t n m) => Ord (MatrixT t n m)
deriving instance Num (Matrix t n m) => Num (MatrixT t n m)
deriving instance Fractional (Matrix t n m) => Fractional (MatrixT t n m)
deriving instance Floating (Matrix t n m) => Floating (MatrixT t n m)
deriving instance PrimBytes (Matrix t n m) => PrimBytes (MatrixT t n m)



type family TT t (n :: Nat) (m :: Nat) = v | v -> t n m where
  TT t 1 1 = Scalar t
  TT t n 1 = ContraVector t n
  TT t 1 m = CoVector t m
  TT t n m = MatrixT t n m



prod :: (MatrixProduct (TT t n m) (TT t m k) (TT t n k))
     => Tensor t n m -> Tensor t m k -> Tensor t n k
prod (Tensor a) (Tensor b) = Tensor $ M.prod a b

instance (MatrixProduct (Vector t n) t (Vector t n))
      => MatrixProduct (ContraVector t n) (Scalar t) (ContraVector t n) where
  prod (ContraVector a) (Scalar b) = ContraVector (M.prod a b)

instance (MatrixProduct (Vector t n) t (Vector t n))
      => MatrixProduct (Scalar t) (CoVector t n) (CoVector t n) where
  prod (Scalar b) (CoVector a) = CoVector (M.prod a b)

instance (VectorCalculus (Vector t n) t n)
      => MatrixProduct (CoVector t n) (ContraVector t n) (Scalar t) where
  prod (CoVector a) (ContraVector b) = Scalar $ V.dot a b

instance (MatrixProduct (Vector t n) (Vector t n) (Matrix t n n))
      => MatrixProduct (ContraVector t n) (CoVector t n) (MatrixT t n n) where
  prod (ContraVector a) (CoVector b) = MatrixT $ M.prod a b

instance (MatrixProduct (Matrix t n m) (Vector t m) (Vector t n))
      => MatrixProduct (MatrixT t n m) (ContraVector t m) (ContraVector t n) where
  prod (MatrixT a) (ContraVector b) = ContraVector $ M.prod a b

instance (MatrixProduct (Vector t m) (Matrix t m k) (Vector t k))
      => MatrixProduct (CoVector t m) (MatrixT t m k) (CoVector t k) where
  prod (CoVector a) (MatrixT b) = CoVector $ M.prod a b

instance (MatrixProduct (Matrix t n m) (Matrix t m k) (Matrix t n k))
      => MatrixProduct (MatrixT t n m) (MatrixT t m k) (MatrixT t n k) where
  prod (MatrixT a) (MatrixT b) = MatrixT $ M.prod a b


