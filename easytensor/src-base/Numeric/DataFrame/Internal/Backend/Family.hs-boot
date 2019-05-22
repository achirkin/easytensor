{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Numeric.DataFrame.Internal.Backend.Family
  ( BackendFamily, KnownBackend ()
  ) where

import Data.Kind    (Type)
import GHC.TypeLits (Nat)


type family BackendFamily (t :: Type) (ds :: [Nat]) = (v :: Type) | v -> t ds where ..
class KnownBackend (t :: Type) (ds :: [Nat]) (backend :: Type)

-- When compiled for haddock, some of the modules complain about missing KnownBackend
-- instance. To workaround this, I add an instance stub.
--
-- Also note, these may be related issues:
--
-- https://github.com/haskell/haddock/issues/680
-- https://github.com/haskell/cabal/issues/4513
--
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
instance KnownBackend t ds b
#endif
