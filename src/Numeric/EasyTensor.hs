{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
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
  , (<:>)
  -- * Type abbreviations
  , Mat, Vec, Vec'
  , Vec2f, Vec3f, Vec4f
  , Vec2f', Vec3f', Vec4f'
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  -- * Simplified type constructors
  , scalar, vec2, vec3, vec4, vec2', vec3', vec4'
  -- * Some low-dimensional operations
  , det2, det2', cross
  ) where

import GHC.Base (runRW#)
import GHC.TypeLits
import GHC.Prim
-- import Data.Proxy

import qualified Numeric.Vector as V
-- import Numeric.Vector.Family
import qualified Numeric.Matrix as M
-- import Numeric.Matrix.Family
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

newtype CoVector t n = CoVector (V.Vector t n)
instance Show (V.Vector t n) => Show (CoVector t n) where
  show (CoVector t) = show t
deriving instance Eq (V.Vector t n) => Eq (CoVector t n)
deriving instance Ord (V.Vector t n) => Ord (CoVector t n)
deriving instance Num (V.Vector t n) => Num (CoVector t n)
deriving instance Fractional (V.Vector t n) => Fractional (CoVector t n)
deriving instance Floating (V.Vector t n) => Floating (CoVector t n)
deriving instance PrimBytes (V.Vector t n) => PrimBytes (CoVector t n)

newtype ContraVector t n = ContraVector (V.Vector t n)
instance Show (V.Vector t n) => Show (ContraVector t n) where
  show (ContraVector t) = show t ++ "'"
deriving instance Eq (V.Vector t n) => Eq (ContraVector t n)
deriving instance Ord (V.Vector t n) => Ord (ContraVector t n)
deriving instance Num (V.Vector t n) => Num (ContraVector t n)
deriving instance Fractional (V.Vector t n) => Fractional (ContraVector t n)
deriving instance Floating (V.Vector t n) => Floating (ContraVector t n)
deriving instance PrimBytes (V.Vector t n) => PrimBytes (ContraVector t n)

newtype Matrix t n m = Matrix (M.Matrix t n m)
instance Show (M.Matrix t n m) => Show (Matrix t n m) where
  show (Matrix t) = show t
deriving instance Eq (M.Matrix t n m) => Eq (Matrix t n m)
deriving instance Ord (M.Matrix t n m) => Ord (Matrix t n m)
deriving instance Num (M.Matrix t n m) => Num (Matrix t n m)
deriving instance Fractional (M.Matrix t n m) => Fractional (Matrix t n m)
deriving instance Floating (M.Matrix t n m) => Floating (Matrix t n m)
deriving instance PrimBytes (M.Matrix t n m) => PrimBytes (Matrix t n m)



type family TT t (n :: Nat) (m :: Nat) = v | v -> t n m where
  TT t 1 1 = Scalar t
  TT t n 1 = ContraVector t n
  TT t 1 m = CoVector t m
  TT t n m = Matrix t n m



prod :: (M.MatrixProduct (TT t n m) (TT t m k) (TT t n k))
     => Tensor t n m -> Tensor t m k -> Tensor t n k
prod (Tensor a) (Tensor b) = Tensor $ M.prod a b

instance (M.MatrixProduct (V.Vector t n) t (V.Vector t n))
      => M.MatrixProduct (ContraVector t n) (Scalar t) (ContraVector t n) where
  prod (ContraVector a) (Scalar b) = ContraVector (M.prod a b)

instance (M.MatrixProduct (V.Vector t n) t (V.Vector t n))
      => M.MatrixProduct (Scalar t) (CoVector t n) (CoVector t n) where
  prod (Scalar b) (CoVector a) = CoVector (M.prod a b)

instance (V.VectorCalculus (V.Vector t n) t n)
      => M.MatrixProduct (CoVector t n) (ContraVector t n) (Scalar t) where
  prod (CoVector a) (ContraVector b) = Scalar $ V.dot a b

instance (M.MatrixProduct (V.Vector t n) (V.Vector t n) (M.Matrix t n n))
      => M.MatrixProduct (ContraVector t n) (CoVector t n) (Matrix t n n) where
  prod (ContraVector a) (CoVector b) = Matrix $ M.prod a b

instance (M.MatrixProduct (M.Matrix t n m) (V.Vector t m) (V.Vector t n))
      => M.MatrixProduct (Matrix t n m) (ContraVector t m) (ContraVector t n) where
  prod (Matrix a) (ContraVector b) = ContraVector $ M.prod a b

instance (M.MatrixProduct (V.Vector t m) (M.Matrix t m k) (V.Vector t k))
      => M.MatrixProduct (CoVector t m) (Matrix t m k) (CoVector t k) where
  prod (CoVector a) (Matrix b) = CoVector $ M.prod a b

instance (M.MatrixProduct (M.Matrix t n m) (M.Matrix t m k) (M.Matrix t n k))
      => M.MatrixProduct (Matrix t n m) (Matrix t m k) (Matrix t n k) where
  prod (Matrix a) (Matrix b) = Matrix $ M.prod a b


-- | Append one vector to another, adding up their dimensionality
(<:>) :: ( PrimBytes (Tensor t k n)
         , PrimBytes (Tensor t k m)
         , PrimBytes (Tensor t k (n + m))
         )
        => Tensor t k n -> Tensor t k m -> Tensor t k (n + m)
a <:> b = case (# toBytes a, toBytes b, byteSize a, byteSize b #) of
  (# arr1, arr2, n, m #) -> case runRW#
     ( \s0 -> case newByteArray# (n +# m) s0 of
         (# s1, marr #) -> case copyByteArray# arr1 0# marr 0# n s1 of
           s2 -> case copyByteArray# arr2 0# marr n m s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> fromBytes r
infixl 5 <:>

-- Simple types

type Mat t n m = Tensor t n m
type Vec t n = Tensor t n 1
type Vec' t m = Tensor t 1 m


-- Even Sympler types

type Vec2f = Tensor Float 2 1
type Vec3f = Tensor Float 3 1
type Vec4f = Tensor Float 4 1
type Vec2f' = Tensor Float 1 2
type Vec3f' = Tensor Float 1 3
type Vec4f' = Tensor Float 1 4


type Mat22f = Tensor Float 2 2
type Mat32f = Tensor Float 3 2
type Mat42f = Tensor Float 4 2
type Mat23f = Tensor Float 2 3
type Mat33f = Tensor Float 3 3
type Mat43f = Tensor Float 4 3
type Mat24f = Tensor Float 2 4
type Mat34f = Tensor Float 3 4
type Mat44f = Tensor Float 4 4


-- construct tensors

scalar :: t -> Tensor t 1 1
scalar = Tensor . Scalar

vec2 :: V.Vector2D t => t -> t -> Tensor t 2 1
vec2 a b = Tensor . ContraVector $ V.vec2 a b

vec2' :: V.Vector2D t => t -> t -> Tensor t 1 2
vec2' a b = Tensor . CoVector $ V.vec2 a b

vec3 :: V.Vector3D t => t -> t -> t -> Tensor t 3 1
vec3 a b c = Tensor . ContraVector $ V.vec3 a b c

vec3' :: V.Vector3D t => t -> t -> t -> Tensor t 1 3
vec3' a b c = Tensor . CoVector $ V.vec3 a b c

vec4 :: V.Vector4D t => t -> t -> t -> t -> Tensor t 4 1
vec4 a b c d = Tensor . ContraVector $ V.vec4 a b c d

vec4' :: V.Vector4D t => t -> t -> t -> t -> Tensor t 1 4
vec4' a b c d = Tensor . CoVector $ V.vec4 a b c d




det2 :: V.Vector2D t => Tensor t 2 1 -> Tensor t 2 1 -> t
det2 (Tensor (ContraVector a)) (Tensor (ContraVector b)) = V.det2 a b

det2' :: V.Vector2D t => Tensor t 1 2 -> Tensor t 1 2 -> t
det2' (Tensor (CoVector a)) (Tensor (CoVector b)) = V.det2 a b

cross :: V.Vector3D t => Tensor t 3 1 -> Tensor t 3 1 -> Tensor t 3 1
cross (Tensor (ContraVector a)) (Tensor (ContraVector b)) = Tensor . ContraVector $ V.cross a b
