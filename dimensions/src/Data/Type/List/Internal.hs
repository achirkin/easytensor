{-# OPTIONS_HADDOCK hide, prune     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ExplicitNamespaces      #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.Type.List.Internal
  ( UnreachableConstraint (..)
  , type (=->) (..), runMagic, unsafeEqTypes
  ) where

import Data.Constraint      (Dict (..))
import Data.Constraint.Bare
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce        (unsafeCoerce)


{- |
This class should have the same runtime representation as its parameter ctx.
The point is to make sure GHC won't segfault if it mixes up `UnreachableConstraint ctx`
with `ctx` itself:

  If GHC mistakenly recognizes `UnreachableConstraint ctx` dictionary as
   `ctx` dictionary, a call to this dictionary should return an error
    as defined by `unreachable` function rather than the panic.

Note, I must not put `ctx` in the superclass position to prevent GHC from
trying to use it.
 -}
class UnreachableConstraint (ctx :: Constraint) (msg :: Symbol) where
  unreachable :: BareConstraint ctx

instance KnownSymbol msg
      => UnreachableConstraint ctx msg where
  unreachable = error $ "Unreachable constraint:: " ++ symbolVal (Proxy @msg)


newtype c =-> r = Magic (c => r)
infixr 0 =->

runMagic :: (c =-> r) -> BareConstraint c -> r
runMagic = unsafeCoerce
{-# NOINLINE runMagic #-}


unsafeEqTypes :: forall k (a :: k) (b :: k)
               . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce (Dict :: Dict (a ~ a))
