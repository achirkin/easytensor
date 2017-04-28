-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Inference.KnownNatFuns
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This module provides functions that serve as evidence for being KnownNat
-- for certain type families.
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}
module Numeric.Dimensions.Inference.KnownNatFuns
  (SNat' (..), funAdd, funRem, funMul, funPow) where

import GHC.TypeLits
import Data.Proxy

-- | This newtype declaration has the same representation
--   as Integer type and as hidden SNat type from GHC.TypeLits
newtype SNat' (n :: Nat) = SNat' Integer


funAdd :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' (a + b)
funAdd = SNat' (natVal (Proxy @a) + natVal (Proxy @b))

funRem :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' (a - b)
funRem = SNat' (natVal (Proxy @a) - natVal (Proxy @b))

funMul :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' (a * b)
funMul = SNat' (natVal (Proxy @a) * natVal (Proxy @b))

funPow :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' (a ^ b)
funPow = SNat' (natVal (Proxy @a) ^ natVal (Proxy @b))
