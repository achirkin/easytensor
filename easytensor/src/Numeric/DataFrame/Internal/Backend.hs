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
-- {-# OPTIONS_GHC -fplugin-opt Data.Constraint.Deriving:dump-instances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Numeric.DataFrame.Internal.Backend
  ( DFBackend, Backend (..), BackendFamily, KnownBackend ()
  , inferKnownBackend, inferPrimElem
    -- auto-derived (will be removed by the plugin):
  , inferEq, inferOrd, inferBounded, inferNum
  , inferFractional, inferFloating
  , inferPrimBytes, inferPrimArray
  , inferShow
  ) where


import Data.Constraint
import Data.Constraint.Deriving
import Data.Constraint.Unsafe
import Data.Kind                (Type)
import Unsafe.Coerce            (unsafeCoerce)

import Numeric.DataFrame.Internal.PrimArray
import Numeric.Dimensions
import Numeric.PrimBytes


import           Numeric.DataFrame.Internal.Backend.Family (BackendFamily, KnownBackend)
import qualified Numeric.DataFrame.Internal.Backend.Family as Impl (inferBackendInstance,
                                                                    inferKnownBackend,
                                                                    inferPrimArray,
                                                                    inferPrimElem)

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
{-# ANN type Backend (DeriveAllBut ["KnownBackend"]) #-}



inferKnownBackend
  :: forall (t :: Type) (ds :: [Nat])
   . (PrimBytes t, Dimensions ds)
  => Dict (KnownBackend t ds)
inferKnownBackend
  = Impl.inferKnownBackend @t @ds

inferPrimElem
  :: forall (t :: Type) (ds :: [Nat])
   . KnownBackend t ds
  => DFBackend t ds -> Maybe (Dict (PrimBytes t))
inferPrimElem = Impl.inferPrimElem @t @ds . _getBackend



{-# ANN inferEq (ToInstance Incoherent) #-}
inferEq :: forall t ds b
         . (Eq t, KnownBackend t ds)
        => Dict (Eq (Backend t ds b))
inferEq
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferOrd (ToInstance Incoherent) #-}
inferOrd :: forall t ds b
            . (Ord t, KnownBackend t ds)
           => Dict (Ord (Backend t ds b))
inferOrd
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferBounded (ToInstance Incoherent) #-}
inferBounded :: forall t ds b
              . (Bounded t, KnownBackend t ds)
             => Dict (Bounded (Backend t ds b))
inferBounded
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferNum (ToInstance Incoherent) #-}
inferNum :: forall t ds b
          . (Num t, KnownBackend t ds)
         => Dict (Num (Backend t ds b))
inferNum
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferFractional (ToInstance Incoherent) #-}
inferFractional :: forall t ds b
                 . (Fractional t, KnownBackend t ds)
                => Dict (Fractional (Backend t ds b))
inferFractional
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferFloating (ToInstance Incoherent) #-}
inferFloating :: forall t ds b
               . (Floating t, KnownBackend t ds)
              => Dict (Floating (Backend t ds b))
inferFloating
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferShow (ToInstance Incoherent) #-}
inferShow :: forall t ds b
           . (Show t, Dimensions ds, KnownBackend t ds)
          => Dict (Show (Backend t ds b))
inferShow
    = mapDict toBackend
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
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
    . mapDict (Sub (Impl.inferBackendInstance @t @ds))
    $ inferDeriveContext @t @ds @b undefined

{-# ANN inferPrimArray (ToInstance Incoherent) #-}
inferPrimArray :: forall t ds b
                . ( PrimBytes t
                  , KnownBackend t ds
                  )
               => Dict ( PrimArray t (Backend t ds b))
inferPrimArray
    = mapDict toBackend
    . mapDict (Sub (Impl.inferPrimArray @t @ds))
    $ inferDeriveContext @t @ds @b undefined


-- This is the rule that cannot be encoded in the type system, but enforced
-- as an invariant: Backend t ds b implies DeriveContext t ds b
inferDeriveContext :: forall t ds b
                    . Backend t ds b
                   -> Dict (DeriveContext (Backend t ds b))
inferDeriveContext _ = unsafeCoerce (Dict :: Dict (b ~ b))
{-# INLINE inferDeriveContext #-}

-- Backend is the newtype wrapper over b.
-- It has the same represenation and I expect it to have the same instance behavior.
toBackend :: forall c t ds b . c b :- c (Backend t ds b)
toBackend = unsafeDerive Backend
{-# INLINE toBackend #-}



instance {-# INCOHERENT #-}
                forall t ds b
         . (Eq t, KnownBackend t ds)
        => Eq (Backend t ds b) where
  (==) = error "Eq (Backend t ds i b){==}: instance not found."
  (/=) = error "Eq (Backend t ds i b){/=}: instance not found."

instance {-# INCOHERENT #-}
                forall t ds b
            . (Ord t, KnownBackend t ds)
           => Ord (Backend t ds b) where
   compare = undefined

instance {-# INCOHERENT #-}
                forall t ds b
              . (Bounded t, KnownBackend t ds)
             => Bounded (Backend t ds b) where
   maxBound = undefined
   minBound = undefined


instance {-# INCOHERENT #-}
                forall t ds b
          . (Num t, KnownBackend t ds)
         => Num (Backend t ds b) where
   (+) = error "Num (Backend t ds i b){+}: instance not found."
   (-) = error "Num (Backend t ds i b){-}: instance not found."
   (*) = error "Num (Backend t ds i b){*}: instance not found."
   negate = error "Num (Backend t ds i b){negate}: instance not found."
   abs = error "Num (Backend t ds i b){abs}: instance not found."
   signum = error "Num (Backend t ds i b){signum}: instance not found."
   fromInteger = error "Num (Backend t ds i b){fromInteger}: instance not found."

instance {-# INCOHERENT #-}
                forall t ds b
                 . (Fractional t, KnownBackend t ds)
                => Fractional (Backend t ds b) where
   fromRational = undefined
   recip = undefined

instance {-# INCOHERENT #-}
                forall t ds b
               . (Floating t, KnownBackend t ds)
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
                forall t ds b
           . (Show t, Dimensions ds, KnownBackend t ds)
          => Show (Backend t ds b) where
    show = undefined

instance {-# INCOHERENT #-}
                forall t ds b
                . ( PrimBytes t
                  , Dimensions ds
                  , KnownBackend t ds
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
                forall t ds b
                . ( PrimBytes t
                  , KnownBackend t ds
                  )
               => PrimArray t (Backend t ds b) where
    broadcast = undefined
    ix# = undefined
    gen# = undefined
    upd# = undefined
    offsetElems = undefined
    uniqueOrCumulDims = undefined
    fromElems = undefined
