{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Numeric.DataFrame.Internal.Backend
  ( DFBackend, Backend (), BackendFamily, KnownBackend ()
  , inferKnownBackend, inferPrimElem
  ) where

import Data.Kind                            (Type)
import GHC.TypeLits                         (ErrorMessage (..), TypeError)
import Numeric.DataFrame.Internal.PrimArray (PrimArray)
import Numeric.Dimensions                   (Dict, Dimensions, Nat)
import Numeric.PrimBytes                    (PrimBytes)


import {-# SOURCE #-} Numeric.DataFrame.Internal.Backend.Family (BackendFamily,
                                                                 KnownBackend)


-- | Implementation behind the DataFrame
type DFBackend (t :: Type) (ds :: [Nat]) = Backend t ds () (BackendFamily t ds)
-- | A newtype wrapper for all DataFrame implementations.
--   I need two layers of wrappers to provide default overlappable instances to
--   all type classes using KnownBackend mechanics.
--   Type arguments are redundant here;
--   nevertheless, they improve readability of error messages.
newtype Backend (t :: Type) (ds :: [Nat]) (i :: Type) (backend :: Type)
    = Backend { _getBackend :: backend }
type role Backend phantom phantom nominal representational

inferKnownBackend
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, Dimensions ds)
  => Dict (KnownBackend t ds)

inferPrimElem
  :: forall (t :: Type) (ds :: [Nat])
   . KnownBackend t ds
  => Maybe (Dict (PrimBytes t))


instance {-# INCOHERENT #-}
    forall t ds b
  . (Eq t, KnownBackend t ds)
  => Eq (Backend t ds () b) where


instance {-# INCOHERENT #-}
    forall t ds b
  . (Ord t, KnownBackend t ds)
  => Ord (Backend t ds () b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . (Bounded t, KnownBackend t ds)
  => Bounded (Backend t ds () b) where


instance {-# INCOHERENT #-}
    forall t ds b
  . (Num t, KnownBackend t ds)
  => Num (Backend t ds () b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . (Fractional t, KnownBackend t ds)
  => Fractional (Backend t ds () b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . (Floating t, KnownBackend t ds)
  => Floating (Backend t ds () b) where


instance {-# INCOHERENT #-}
    forall t ds b
  . (Show t, Dimensions ds, KnownBackend t ds)
  => Show (Backend t ds () b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . ( PrimBytes t
    , Dimensions ds
    , KnownBackend t ds
    )
  => PrimBytes (Backend t ds () b) where

instance {-# INCOHERENT #-}
    forall t ds b
  . ( PrimBytes t
    , KnownBackend t ds
    )
  => PrimArray t (Backend t ds () b) where

instance {-# OVERLAPPABLE #-}
         TypeError (ErrorMsg (Eq (Backend t ds i b)))
         => Eq (Backend t ds i b) where

instance {-# OVERLAPPABLE #-}
         ( Eq (Backend t ds i b)
         , TypeError (ErrorMsg (Ord (Backend t ds i b)))
         )
         => Ord (Backend t ds i b) where

instance {-# OVERLAPPABLE #-}
         TypeError (ErrorMsg (Bounded (Backend t ds i b)))
         => Bounded (Backend t ds i b) where

instance {-# OVERLAPPABLE #-}
         TypeError (ErrorMsg (Num (Backend t ds i b)))
         => Num (Backend t ds i b) where

instance {-# OVERLAPPABLE #-}
         ( Num (Backend t ds i b)
         , TypeError (ErrorMsg (Fractional (Backend t ds i b)))
         )
         => Fractional (Backend t ds i b) where

instance {-# OVERLAPPABLE #-}
         ( Fractional (Backend t ds i b)
         , TypeError (ErrorMsg (Floating (Backend t ds i b)))
         )
         => Floating (Backend t ds i b) where


instance {-# OVERLAPPABLE #-}
         TypeError (ErrorMsg (Show (Backend t ds i b)))
         => Show (Backend t ds i b) where

instance {-# OVERLAPPABLE #-}
         TypeError (ErrorMsg (PrimBytes (Backend t ds i b)))
         => PrimBytes (Backend t ds i b) where

type ErrorMsg i = 'Text "No instance for " ':<>: 'ShowType i
            ':$$: 'Text "Perhaps, you need to find an instance of KnownBackend"
            ':$$: 'Text "Make this type more concrete."
