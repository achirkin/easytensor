{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Numeric.DataFrame.Internal.Backend.Family
  ( BackendFamily, KnownBackend ()
  ) where

import Data.Kind    (Type)
import GHC.TypeLits (Nat)


type family BackendFamily (t :: Type) (ds :: [Nat]) = (v :: Type) | v -> t ds where ..
class KnownBackend (t :: Type) (ds :: [Nat]) (backend :: Type)
