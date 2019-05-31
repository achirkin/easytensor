{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}
-- This module recursively depends on Numeric.TypedList.
-- I thought hs-boot is better than orphan instances.
module Numeric.Dimensions.Dim ( Dim, Nat, dimVal, minusDimM ) where
import Data.Kind      (Type)
import Data.Type.Lits (type (-), Nat)
newtype Dim (x :: k) = DimSing Word
dimVal :: forall (k :: Type) (x :: k) . Dim (x :: k) -> Word
minusDimM :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Maybe (Dim (n - m))
