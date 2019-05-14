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

module Numeric.DataFrame.Internal.Backend
  ( DFBackend, Backend (..), BackendFamily, KnownBackend
  , inferKnownBackend, inferPrimElem
  ) where

import Data.Kind                            (Type)
import Numeric.DataFrame.Internal.PrimArray (PrimArray)
import Numeric.Dimensions                   (Dict, Dimensions, Nat)
import Numeric.PrimBytes                    (PrimBytes)


import {-# SOURCE #-} Numeric.DataFrame.Internal.Backend.Family (BackendFamily, KnownBackend)

-- | Implementation behind the DataFrame
type DFBackend (t :: Type) (ds :: [Nat]) = Backend t ds (BackendFamily t ds)
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
  :: forall (t :: Type) (ds :: [Nat])
   . KnownBackend t ds
  => DFBackend t ds -> Maybe (Dict (PrimBytes t))


instance {-# INCOHERENT #-}
    forall t ds b
  . (Eq t, KnownBackend t ds)
  => Eq (Backend t ds b) where


instance {-# INCOHERENT #-}
    forall t ds b
  . (Ord t, KnownBackend t ds)
  => Ord (Backend t ds b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . (Bounded t, KnownBackend t ds)
  => Bounded (Backend t ds b) where


instance {-# INCOHERENT #-}
    forall t ds b
  . (Num t, KnownBackend t ds)
  => Num (Backend t ds b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . (Fractional t, KnownBackend t ds)
  => Fractional (Backend t ds b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . (Floating t, KnownBackend t ds)
  => Floating (Backend t ds b) where


instance {-# INCOHERENT #-}
    forall t ds b
  . (Show t, Dimensions ds, KnownBackend t ds)
  => Show (Backend t ds b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . ( PrimBytes t
    , Dimensions ds
    , KnownBackend t ds
    )
  => PrimBytes (Backend t ds b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . ( PrimBytes t
    , KnownBackend t ds
    )
  => PrimArray t (Backend t ds b) where
