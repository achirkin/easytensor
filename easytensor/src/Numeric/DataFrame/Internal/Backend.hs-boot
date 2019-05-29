{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeOperators         #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
#endif

module Numeric.DataFrame.Internal.Backend
  ( DFBackend, Backend (..), BackendFamily, KnownBackend ()
  , inferKnownBackend, inferPrimElem
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

-- | Implementation behind the DataFrame
type DFBackend (t :: Type) (ds :: [Nat]) = Backend t ds (BackendFamily t ds)

-- | Backend resolver:
--   Use this constraint to find any class instances defined for all DataFrame implementations,
--   e.g. @Num@, @PrimBytes@, etc.
type KnownBackend (t :: Type) (ds :: [Nat]) = Impl.KnownBackend t ds (BackendFamily t ds)

-- | A newtype wrapper for all DataFrame implementations.
--   I need two layers of wrappers to provide default overlappable instances to
--   all type classes using KnownBackend mechanics.
--   Type arguments are redundant here;
--   nevertheless, they improve readability of error messages.
newtype Backend (t :: Type) (ds :: [Nat]) (backend :: Type)
    = Backend { _getBackend :: backend }
type role Backend phantom phantom representational

inferKnownBackend
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, Dimensions ds)
  => Dict (KnownBackend t ds)

inferPrimElem
  :: forall (t :: Type) (d :: Nat) (ds :: [Nat])
   . KnownBackend t (d ': ds)
  => DFBackend t (d ': ds) -> Dict (PrimBytes t)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Eq t, Impl.KnownBackend t ds b)
  => Eq (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => ProductOrder (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (NonTransitive.ProductOrd (Backend t ds b))

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (Partial.ProductOrd (Backend t ds b))

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Bounded t, Impl.KnownBackend t ds b)
  => Bounded (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Num t, Impl.KnownBackend t ds b)
  => Num (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Fractional t, Impl.KnownBackend t ds b)
  => Fractional (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Floating t, Impl.KnownBackend t ds b)
  => Floating (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Show t, Dimensions ds, Impl.KnownBackend t ds b)
  => Show (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . ( PrimBytes t
    , Dimensions ds
    , Impl.KnownBackend t ds b
    )
  => PrimBytes (Backend t ds b)

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . ( PrimBytes t
    , Impl.KnownBackend t ds b
    )
  => PrimArray t (Backend t ds b)
