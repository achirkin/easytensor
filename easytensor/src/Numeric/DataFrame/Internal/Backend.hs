{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
#else
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
#endif
{-
This module contains all generic and specific instances for all backend
(implementation) types. Its hs-boot counterpart provides only generic instances
(that do not depend on BackendFamily).
Import this module only at the root of the module hierarchy, where the specific
implementation-dependent modules do not need to depend on it.
But do not forget to import this module in the places that will be imported by
the user (such as Numeric.DataFrame). So that the user can benefit from the
compile-time knowledge of the specific backend implementation.

By design the import chain is as follows:

NDI.Backend.Family.hs-boot            NDI.Backend.Family
         \                                   |
    NDI.Backend.hs-boot               NDI.Backend (instances only)
          \                                 /
     NDI.BackendI                          /
            \                             /
    Numeric.DataFrame.Type               /
                    \                   /
                     \                 /
                     Numeric.DataFrame

 -}
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


import Data.Constraint
import Data.Constraint.Deriving
import Data.Constraint.Unsafe
import Data.Kind                (Type)
import Unsafe.Coerce            (unsafeCoerce)

import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial

import           Numeric.DataFrame.Internal.Backend.Family (BackendFamily)
import qualified Numeric.DataFrame.Internal.Backend.Family as Impl (KnownBackend,
                                                                    inferBackendInstance,
                                                                    inferKnownBackend,
                                                                    inferPrimArray,
                                                                    inferPrimElem)


-- | Backend resolver:
--   Use this constraint to find any class instances defined for all DataFrame implementations,
--   e.g. @Num@, @PrimBytes@, etc.
class Impl.KnownBackend t ds (BackendFamily t ds)
   => KnownBackend (t :: Type) (ds :: [Nat])
instance Impl.KnownBackend t ds (BackendFamily t ds)
   => KnownBackend (t :: Type) (ds :: [Nat])

-- | A newtype wrapper for all DataFrame implementations.
--   I need two layers of wrappers to provide default overlappable instances to
--   all type classes using KnownBackend mechanics.
--   Type arguments are redundant here;
--   nevertheless, they improve readability of error messages.
newtype Backend (i :: Type) (t :: Type) (ds :: [Nat]) (backend :: Type)
    = Backend { _getBackend :: backend }
type role Backend phantom phantom phantom representational
type instance DeriveContext (Backend i t ds b) = b ~ BackendFamily t ds
-- When typechecker knows what the fourth parameter @b@ is, no instances overlap,
-- because @b@ is always always a concrete type.
-- The "dynamic instances" derived via ToInstance have to be incoherent,
-- because @BackendFamily t ds@ as a type family (before resolving it)
--  is no more concrete than just @b@.
{-# ANN type Backend (DeriveAll' NoOverlap ["KnownBackend"]) #-}



inferKnownBackend
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, Dimensions ds)
  => Dict (KnownBackend t ds)
inferKnownBackend
  = case Impl.inferKnownBackend @t @ds @(BackendFamily t ds) of
      Dict -> Dict

inferPrimElem
  :: forall (t :: Type) (d :: Nat) (ds :: [Nat]) (i :: Type)
   . KnownBackend t (d ': ds)
  => Backend i t (d ': ds) (BackendFamily t (d ': ds)) -> Dict (PrimBytes t)
inferPrimElem = Impl.inferPrimElem @t @d @ds . _getBackend



inferEq
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Eq t, Impl.KnownBackend t ds b)
  => Dict (Eq (Backend i t ds b))
inferEq
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferOrd
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Backend i t ds b))
inferOrd
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferProductOrder
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (ProductOrder (Backend i t ds b))
inferProductOrder
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferPONonTransitive
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (NonTransitive.ProductOrd (Backend i t ds b)))
inferPONonTransitive
    = mapDict (unsafeDerive NonTransitive.ProductOrd)
    . mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferPOPartial
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Partial.ProductOrd (Backend i t ds b)))
inferPOPartial
    = mapDict (unsafeDerive Partial.ProductOrd)
    . mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferBounded
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Bounded t, Impl.KnownBackend t ds b)
  => Dict (Bounded (Backend i t ds b))
inferBounded
    -- Here is an ugly trick:
    --  We don't have instance of @Bounded Float@ and @Bounded Double@.
    --  However, we want to have an instance @Bounded t => Bounded (Backend t ds b)@.
    --  Some specialized implementations (e.g. FloatX4) require explicit
    --    constraints @Bounded Float@ or @Bounded Double@.
    --  To satisfy these, I pull @Bounded t@ convincing the compiler that @t ~ Float@.
    --  This should work fine even if a user implements instances of Bounded for
    --    the floating types, because we know that @t ~ Float@ or @t ~ Double@
    --    automatically whenever this instance is called for these types.
  | Dict <- (case (unsafeCoerce (Dict @(t ~ t)) :: Dict (t ~ Float)) of
               Dict -> Dict) :: Dict (Bounded Float)
  , Dict <- (case (unsafeCoerce (Dict @(t ~ t)) :: Dict (t ~ Double)) of
               Dict -> Dict) :: Dict (Bounded Double)
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferNum
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Num t, Impl.KnownBackend t ds b)
  => Dict (Num (Backend i t ds b))
inferNum
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferFractional
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Fractional t, Impl.KnownBackend t ds b)
  => Dict (Fractional (Backend i t ds b))
inferFractional
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferFloating
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (Floating t, Impl.KnownBackend t ds b)
  => Dict (Floating (Backend i t ds b))
inferFloating
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined


inferPrimBytes
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (PrimBytes t, Dimensions ds, Impl.KnownBackend t ds b)
  => Dict (PrimBytes (Backend i t ds b))
inferPrimBytes
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferPrimArray
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
   . (PrimBytes t, Impl.KnownBackend t ds b)
  => Dict (PrimArray t (Backend i t ds b))
inferPrimArray
    = mapDict toBackend
    . mapDict (Sub (Impl.inferPrimArray @t @ds))
    $ inferDeriveContext @t @ds @b undefined


-- This is the rule that cannot be encoded in the type system, but enforced
-- as an invariant: Backend t ds b implies DeriveContext t ds b
inferDeriveContext :: forall (t :: Type) (ds :: [Nat]) (b :: Type) (i :: Type)
                    . Backend i t ds b
                   -> Dict (DeriveContext (Backend i t ds b))
inferDeriveContext _ = unsafeCoerce (Dict :: Dict (b ~ b))
{-# INLINE inferDeriveContext #-}

-- Backend is the newtype wrapper over b.
-- It has the same represenation and I expect it to have the same instance behavior.
toBackend :: forall c t ds b i . c b :- c (Backend i t ds b)
toBackend = unsafeDerive Backend
{-# INLINE toBackend #-}
