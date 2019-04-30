{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}

module Numeric.DataFrame.Internal.Backend
  ( DFBackend
  , Backend (), BackendFamily, KnownBackend (), BackendSing ()
  , Impl.inferKnownBackend
  , Impl.inferPrimElem
    -- auto-derived:
  , inferEq, inferOrd, inferBounded, inferNum
  , inferFractional, inferFloating
  , inferPrimBytes, inferPrimArray
  , inferShow
  ) where


import           Data.Constraint
import           Data.Constraint.Deriving
import           Data.Constraint.Unsafe
import           GHC.Base

import           Numeric.DataFrame.Internal.Backend.Family (BackendFamily,
                                                            BackendSing,
                                                            KnownBackend)
import qualified Numeric.DataFrame.Internal.Backend.Family as Impl
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.Dimensions
import           Numeric.PrimBytes

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
type instance DeriveContext (Backend t ds b) = b ~ BackendFamily t ds
{-# ANN type Backend DeriveAll #-}

{-# ANN inferEq (ToInstance Incoherent) #-}
inferEq :: forall t ds b
         . (Eq t, KnownBackend t ds)
        => Dict (Eq (Backend t ds b))
inferEq
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferOrd (ToInstance Incoherent) #-}
inferOrd :: forall t ds b
            . (Ord t, KnownBackend t ds)
           => Dict (Ord (Backend t ds b))
inferOrd
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferBounded (ToInstance Incoherent) #-}
inferBounded :: forall t ds b
              . (Bounded t, KnownBackend t ds)
             => Dict (Bounded (Backend t ds b))
inferBounded
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferNum (ToInstance Incoherent) #-}
inferNum :: forall t ds b
          . (Num t, KnownBackend t ds)
         => Dict (Num (Backend t ds b))
inferNum
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferFractional (ToInstance Incoherent) #-}
inferFractional :: forall t ds b
                 . (Fractional t, KnownBackend t ds)
                => Dict (Fractional (Backend t ds b))
inferFractional
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferFloating (ToInstance Incoherent) #-}
inferFloating :: forall t ds b
               . (Floating t, KnownBackend t ds)
              => Dict (Floating (Backend t ds b))
inferFloating
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferShow (ToInstance Incoherent) #-}
inferShow :: forall t ds b
           . (Show t, Dimensions ds, KnownBackend t ds)
          => Dict (Show (Backend t ds b))
inferShow
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPrimBytes (ToInstance Incoherent) #-}
inferPrimBytes :: forall t ds b
                . ( PrimBytes t
                  , Dimensions ds
                  , KnownBackend t ds
                  )
               => Dict ( PrimBytes (Backend t ds b))
inferPrimBytes
    = mapDict toBackend
    . mapDict (Sub Impl.inferBackendInstance)
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPrimArray (ToInstance Incoherent) #-}
inferPrimArray :: forall t ds b
                . ( PrimBytes t
                  , KnownBackend t ds
                  )
               => Dict ( PrimArray t (Backend t ds b))
inferPrimArray
    = mapDict toBackend
    . mapDict (Sub Impl.inferPrimArray)
    $ inferDeriveContext @t @ds @b undefined


-- This is the rule that cannot be encoded in the type system, but enforced
-- as an invariant: Backend t ds b implies DeriveContext t ds b
inferDeriveContext :: Backend t ds b
                   -> Dict (DeriveContext (Backend t ds b))
inferDeriveContext _ = unsafeCoerce# (Dict :: Dict (b ~ b) )
{-# INLINE inferDeriveContext #-}

-- Backend is the newtype wrapper over b.
-- It has the same represenation and I expect it to have the same instance behavior.
toBackend :: forall c t n b . c b :- c (Backend t n b)
toBackend = unsafeDerive Backend
{-# INLINE toBackend #-}
