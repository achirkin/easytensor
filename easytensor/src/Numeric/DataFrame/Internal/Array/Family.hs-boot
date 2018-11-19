{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Numeric.DataFrame.Internal.Array.Family
  ( Array
  ) where

import GHC.Base (Nat, Type)

type family Array (t :: Type) (ds :: [Nat]) = (v :: Type) | v -> t ds where ..
