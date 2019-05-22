{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#endif

module Numeric.DataFrame.Internal.Backend
  ( DFBackend, Backend (..), BackendFamily, KnownBackend ()
  , inferKnownBackend, inferPrimElem
    -- auto-derived (will be removed by the plugin):
#if !(defined(__HADDOCK__) || defined(__HADDOCK_VERSION__))
  , inferEq, inferOrd
  , inferProductOrder, inferPONonTransitive, inferPOPartial
  , inferBounded, inferNum
  , inferFractional, inferFloating
  , inferPrimBytes, inferPrimArray
  , inferShow
#endif
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
type instance DeriveContext (Backend t ds b) = b ~ BackendFamily t ds
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
  :: forall (t :: Type) (ds :: [Nat])
   . KnownBackend t ds
  => DFBackend t ds -> Maybe (Dict (PrimBytes t))
inferPrimElem = Impl.inferPrimElem @t @ds . _getBackend



{-# ANN inferEq (ToInstance Incoherent) #-}
inferEq :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
         . (Eq t, Impl.KnownBackend t ds b)
        => Dict (Eq (Backend t ds b))
inferEq
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferOrd (ToInstance Incoherent) #-}
inferOrd :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
            . (Ord t, Impl.KnownBackend t ds b)
           => Dict (Ord (Backend t ds b))
inferOrd
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferProductOrder (ToInstance Incoherent) #-}
inferProductOrder
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (ProductOrder (Backend t ds b))
inferProductOrder
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPONonTransitive (ToInstance Incoherent) #-}
inferPONonTransitive
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (NonTransitive.ProductOrd (Backend t ds b)))
inferPONonTransitive
    = mapDict (unsafeDerive NonTransitive.ProductOrd)
    . mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPOPartial (ToInstance Incoherent) #-}
inferPOPartial
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Partial.ProductOrd (Backend t ds b)))
inferPOPartial
    = mapDict (unsafeDerive Partial.ProductOrd)
    . mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferBounded (ToInstance Incoherent) #-}
inferBounded :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
              . (Bounded t, Impl.KnownBackend t ds b)
             => Dict (Bounded (Backend t ds b))
inferBounded
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferNum (ToInstance Incoherent) #-}
inferNum :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
          . (Num t, Impl.KnownBackend t ds b)
         => Dict (Num (Backend t ds b))
inferNum
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferFractional (ToInstance Incoherent) #-}
inferFractional :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
                 . (Fractional t, Impl.KnownBackend t ds b)
                => Dict (Fractional (Backend t ds b))
inferFractional
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferFloating (ToInstance Incoherent) #-}
inferFloating :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
               . (Floating t, Impl.KnownBackend t ds b)
              => Dict (Floating (Backend t ds b))
inferFloating
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferShow (ToInstance Incoherent) #-}
inferShow :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
           . (Show t, Dimensions ds, Impl.KnownBackend t ds b)
          => Dict (Show (Backend t ds b))
inferShow
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPrimBytes (ToInstance Incoherent) #-}
inferPrimBytes :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
                . ( PrimBytes t
                  , Dimensions ds
                  , Impl.KnownBackend t ds b
                  )
               => Dict ( PrimBytes (Backend t ds b))
inferPrimBytes
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPrimArray (ToInstance Incoherent) #-}
inferPrimArray :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
                . ( PrimBytes t
                  , Impl.KnownBackend t ds b
                  )
               => Dict ( PrimArray t (Backend t ds b))
inferPrimArray
    = mapDict toBackend
    . mapDict (Sub (Impl.inferPrimArray @t @ds))
    $ inferDeriveContext @t @ds @b undefined


-- This is the rule that cannot be encoded in the type system, but enforced
-- as an invariant: Backend t ds b implies DeriveContext t ds b
inferDeriveContext :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
                    . Backend t ds b
                   -> Dict (DeriveContext (Backend t ds b))
inferDeriveContext _ = unsafeCoerce (Dict :: Dict (b ~ b))
{-# INLINE inferDeriveContext #-}

-- Backend is the newtype wrapper over b.
-- It has the same represenation and I expect it to have the same instance behavior.
toBackend :: forall c t ds b . c b :- c (Backend t ds b)
toBackend = unsafeDerive Backend
{-# INLINE toBackend #-}


{-
  The instances below are bumb stubs.
  I need them to make GHC happy in the presence of my ToInstance plugin and
  recursive modules, because I cannot use the plugin in Backend.hs-boot.
  Without this, instances of these classes would not be available in modules,
  which import Backend.hs as SOURCE.
 -}

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Eq t, Impl.KnownBackend t ds b)
  => Eq (Backend t ds b) where
    (==) = undefined
    (/=) = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (Backend t ds b) where
    compare = undefined
    (<) = undefined
    (<=) = undefined
    (>) = undefined
    (>=) = undefined
    max = undefined
    min = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => ProductOrder (Backend t ds b) where
    cmp = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (NonTransitive.ProductOrd (Backend t ds b)) where
    compare = undefined
    (<) = undefined
    (<=) = undefined
    (>) = undefined
    (>=) = undefined
    max = undefined
    min = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (Partial.ProductOrd (Backend t ds b)) where
    compare = undefined
    (<) = undefined
    (<=) = undefined
    (>) = undefined
    (>=) = undefined
    max = undefined
    min = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Bounded t, Impl.KnownBackend t ds b)
  => Bounded (Backend t ds b) where
    maxBound = undefined
    minBound = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Num t, Impl.KnownBackend t ds b)
  => Num (Backend t ds b) where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Fractional t, Impl.KnownBackend t ds b)
  => Fractional (Backend t ds b) where
    fromRational = undefined
    recip = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Floating t, Impl.KnownBackend t ds b)
  => Floating (Backend t ds b) where
    pi = undefined
    exp = undefined
    log = undefined
    sqrt = undefined
    (**) = undefined
    logBase = undefined
    sin = undefined
    cos = undefined
    tan = undefined
    asin = undefined
    acos = undefined
    atan = undefined
    sinh = undefined
    cosh = undefined
    tanh = undefined
    asinh = undefined
    acosh = undefined
    atanh = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Show t, Dimensions ds, Impl.KnownBackend t ds b)
  => Show (Backend t ds b) where
    show = undefined
    showsPrec = undefined
    showList = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . ( PrimBytes t
    , Dimensions ds
    , Impl.KnownBackend t ds b
    )
  => PrimBytes (Backend t ds b) where
    getBytes   = undefined
    fromBytes  = undefined
    readBytes  = undefined
    writeBytes = undefined
    readAddr   = undefined
    writeAddr  = undefined
    byteSize   = undefined
    byteAlign  = undefined
    byteOffset = undefined
    indexArray = undefined
    readArray  = undefined
    writeArray = undefined

instance {-# INCOHERENT #-}
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . ( PrimBytes t
    , Impl.KnownBackend t ds b
    )
  => PrimArray t (Backend t ds b) where
    broadcast = undefined
    ix# = undefined
    gen# = undefined
    upd# = undefined
    offsetElems = undefined
    uniqueOrCumulDims = undefined
    fromElems = undefined
