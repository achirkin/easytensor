{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeInType      #-}
{-# LANGUAGE TypeOperators   #-}
-- This module recursively depends on Numeric.TypedList.
-- I thought hs-boot is better than orphan instances.
module Numeric.Dimensions.Dim ( Dim, dimVal, minusDimM ) where
import Data.Type.Lits (type (-), Nat)
newtype Dim (x :: k) = DimSing Word
type role Dim nominal
dimVal :: forall x . Dim x -> Word
minusDimM :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Maybe (Dim (n - m))
