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
-- Module      :  Numeric.Tensor
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Tensor
  ( Tensor ()
  , module Numeric.Dimensions
  , module Numeric.NDArray.Family
  , module Numeric.Commons
  ) where

import GHC.TypeLits
--import GHC.Prim
--import Data.Proxy



-- import Numeric.NDArray.Base.Float ()
import Numeric.Dimensions
-- import Numeric.NDArray.Family
import Numeric.Commons


newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral, Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes, FloatBytes, DoubleBytes, IntBytes, WordBytes)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"

newtype NDA t ds = NDA (NDArray t ds)
instance Show (NDArray t ds) => Show (NDA t ds) where
  show (NDA t) = show t
deriving instance Eq (NDArray t ds) => Eq (NDA t ds)
deriving instance Ord (NDArray t ds) => Ord (NDA t ds)
deriving instance Num (NDArray t ds) => Num (NDA t ds)
deriving instance Fractional (NDArray t ds) => Fractional (NDA t ds)
deriving instance Floating (NDArray t ds) => Floating (NDA t ds)
deriving instance PrimBytes (NDArray t ds) => PrimBytes (NDA t ds)
deriving instance FloatBytes (NDArray t ds) => FloatBytes (NDA t ds)
deriving instance DoubleBytes (NDArray t ds) => DoubleBytes (NDA t ds)
deriving instance IntBytes (NDArray t ds) => IntBytes (NDA t ds)
deriving instance WordBytes (NDArray t ds) => WordBytes (NDA t ds)
--deriving instance V.VectorCalculus t n (NDArray t ds) => V.VectorCalculus t n (NDA t ds)

type family TT t (ds :: [Nat]) = v | v -> t ds where
  TT t '[] = Scalar t
  TT t ds  = NDA t ds


newtype Tensor t (ns :: [Nat]) (ms :: [Nat]) = Tensor { _unT :: TT t (ns ++ Reverse ms) }
instance Show (TT t (ns ++ Reverse ms)) => Show (Tensor t ns ms) where
  show (Tensor t) = show t
deriving instance Eq (TT t (ns ++ Reverse ms)) => Eq (Tensor t ns ms)
deriving instance Ord (TT t (ns ++ Reverse ms)) => Ord (Tensor t ns ms)
deriving instance Num (TT t (ns ++ Reverse ms)) => Num (Tensor t ns ms)
deriving instance Fractional (TT t (ns ++ Reverse ms)) => Fractional (Tensor t ns ms)
deriving instance Floating (TT t (ns ++ Reverse ms)) => Floating (Tensor t ns ms)
deriving instance Bounded (TT t (ns ++ Reverse ms)) => Bounded (Tensor t ns ms)
deriving instance Enum (TT t (ns ++ Reverse ms)) => Enum (Tensor t ns ms)
deriving instance Integral (TT t (ns ++ Reverse ms)) => Integral (Tensor t ns ms)
deriving instance Real (TT t (ns ++ Reverse ms)) => Real (Tensor t ns ms)
deriving instance RealFrac (TT t (ns ++ Reverse ms)) => RealFrac (Tensor t ns ms)
deriving instance RealFloat (TT t (ns ++ Reverse ms)) => RealFloat (Tensor t ns ms)
