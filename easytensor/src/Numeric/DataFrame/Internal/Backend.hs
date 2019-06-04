{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
#else
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
#endif

module Numeric.DataFrame.Internal.Backend
  ( DFBackend, Backend (..), BackendFamily, KnownBackend ()
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
import           Numeric.DataFrame.Internal.BackendI       (I)

-- | Implementation behind the DataFrame
type DFBackend (t :: Type) (ds :: [Nat]) = Backend I t ds (BackendFamily t ds)

-- | Backend resolver:
--   Use this constraint to find any class instances defined for all DataFrame implementations,
--   e.g. @Num@, @PrimBytes@, etc.
type KnownBackend (t :: Type) (ds :: [Nat]) = Impl.KnownBackend t ds (BackendFamily t ds)

-- | A newtype wrapper for all DataFrame implementations.
--   I need two layers of wrappers to provide default overlappable instances to
--   all type classes using KnownBackend mechanics.
--   Type arguments are redundant here;
--   nevertheless, they improve readability of error messages.
newtype Backend (i :: Type) (t :: Type) (ds :: [Nat]) (backend :: Type)
    = Backend { _getBackend :: backend }
type role Backend phantom phantom phantom representational
type instance DeriveContext (Backend I t ds b) = b ~ BackendFamily t ds
-- When typechecker knows what the third parameter @b@ is, no instances overlap,
-- because @b@ is always always a concrete type.
-- The "dynamic instances" derived via ToInstance have to be incoherent,
-- because @BackendFamily t ds@ as a type family (before resolving it)
--  is no more conrete than just @b@.
{-# ANN type Backend (DeriveAll' NoOverlap ["KnownBackend"]) #-}



inferKnownBackend
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, Dimensions ds)
  => Dict (KnownBackend t ds)
inferKnownBackend
  = Impl.inferKnownBackend @t @ds @(BackendFamily t ds)

inferPrimElem
  :: forall (t :: Type) (d :: Nat) (ds :: [Nat])
   . KnownBackend t (d ': ds)
  => DFBackend t (d ': ds) -> Dict (PrimBytes t)
inferPrimElem = Impl.inferPrimElem @t @d @ds . _getBackend



inferEq
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Eq t, Impl.KnownBackend t ds b)
  => Dict (Eq (Backend I t ds b))
inferEq
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferOrd
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Backend I t ds b))
inferOrd
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferProductOrder
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (ProductOrder (Backend I t ds b))
inferProductOrder
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferPONonTransitive
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (NonTransitive.ProductOrd (Backend I t ds b)))
inferPONonTransitive
    = mapDict (unsafeDerive NonTransitive.ProductOrd)
    . mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferPOPartial
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Partial.ProductOrd (Backend I t ds b)))
inferPOPartial
    = mapDict (unsafeDerive Partial.ProductOrd)
    . mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferBounded
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Bounded t, Impl.KnownBackend t ds b)
  => Dict (Bounded (Backend I t ds b))
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
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Num t, Impl.KnownBackend t ds b)
  => Dict (Num (Backend I t ds b))
inferNum
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferFractional
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Fractional t, Impl.KnownBackend t ds b)
  => Dict (Fractional (Backend I t ds b))
inferFractional
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferFloating
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Floating t, Impl.KnownBackend t ds b)
  => Dict (Floating (Backend I t ds b))
inferFloating
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined


inferPrimBytes
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (PrimBytes t, Dimensions ds, Impl.KnownBackend t ds b)
  => Dict (PrimBytes (Backend I t ds b))
inferPrimBytes
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

inferPrimArray
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (PrimBytes t, Impl.KnownBackend t ds b)
  => Dict (PrimArray t (Backend I t ds b))
inferPrimArray
    = mapDict toBackend
    . mapDict (Sub (Impl.inferPrimArray @t @ds))
    $ inferDeriveContext @t @ds @b undefined


-- This is the rule that cannot be encoded in the type system, but enforced
-- as an invariant: Backend t ds b implies DeriveContext t ds b
inferDeriveContext :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
                    . Backend I t ds b
                   -> Dict (DeriveContext (Backend I t ds b))
inferDeriveContext _ = unsafeCoerce (Dict :: Dict (b ~ b))
{-# INLINE inferDeriveContext #-}

-- Backend is the newtype wrapper over b.
-- It has the same represenation and I expect it to have the same instance behavior.
toBackend :: forall c t ds b . c b :- c (Backend I t ds b)
toBackend = unsafeDerive Backend
{-# INLINE toBackend #-}
