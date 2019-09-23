{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
#endif

module Numeric.DataFrame.Internal.Backend
  ( Backend (..), BackendFamily, KnownBackend ()
  , inferKnownBackend, inferPrimElem
    -- * Auto-deriving instances
  , inferEq, inferOrd
  , inferProductOrder, inferPONonTransitive, inferPOPartial
  , inferBounded, inferNum
  , inferFractional, inferFloating
  , inferPrimBytes, inferPrimArray
  ) where

import           Data.Kind                            (Type)
import           Numeric.DataFrame.Internal.PrimArray (PrimArray)
import           Numeric.Dimensions                   (Dict, Dimensions, Nat)
import           Numeric.PrimBytes                    (PrimBytes)
import           Numeric.ProductOrd                   (ProductOrder)
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial

import {-# SOURCE #-} Numeric.DataFrame.Internal.Backend.Family (BackendFamily)
import {-# SOURCE #-} qualified Numeric.DataFrame.Internal.Backend.Family as Impl
                                                                                   (KnownBackend)

class Impl.KnownBackend t ds (BackendFamily t ds)
   => KnownBackend (t :: Type) (ds :: [Nat])
instance Impl.KnownBackend t ds (BackendFamily t ds)
   => KnownBackend (t :: Type) (ds :: [Nat])

newtype Backend (i :: Type) (t :: Type) (ds :: [Nat]) (backend :: Type)
    = Backend { _getBackend :: backend }
type role Backend phantom phantom phantom representational

inferKnownBackend
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, Dimensions ds)
  => Dict (KnownBackend t ds)

inferPrimElem
  :: forall (t :: Type) (d :: Nat) (ds :: [Nat]) (i :: Type)
   . KnownBackend t (d ': ds)
  => Backend i t (d ': ds) (BackendFamily t (d ': ds)) -> Dict (PrimBytes t)

inferEq
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Eq t, Impl.KnownBackend t ds b)
  => Dict (Eq (Backend i t ds b))

inferOrd
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Backend i t ds b))

inferProductOrder
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (ProductOrder (Backend i t ds b))

inferPONonTransitive
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (NonTransitive.ProductOrd (Backend i t ds b)))

inferPOPartial
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Partial.ProductOrd (Backend i t ds b)))

inferBounded
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Bounded t, Impl.KnownBackend t ds b)
  => Dict (Bounded (Backend i t ds b))

inferNum
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Num t, Impl.KnownBackend t ds b)
  => Dict (Num (Backend i t ds b))

inferFractional
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Fractional t, Impl.KnownBackend t ds b)
  => Dict (Fractional (Backend i t ds b))

inferFloating
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Floating t, Impl.KnownBackend t ds b)
  => Dict (Floating (Backend i t ds b))

inferPrimBytes
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (PrimBytes t, Dimensions ds, Impl.KnownBackend t ds b)
  => Dict (PrimBytes (Backend i t ds b))

inferPrimArray
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (PrimBytes t, Impl.KnownBackend t ds b)
  => Dict (PrimArray t (Backend i t ds b))
