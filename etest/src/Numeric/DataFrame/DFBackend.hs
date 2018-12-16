{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# OPTIONS_GHC -fplugin Numeric.DataFrame.TcPlugin #-}

module Numeric.DataFrame.DFBackend
  ( DFBackend(..)
  )
where


import           Data.Semigroup
import           GHC.Base

import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.Dimensions




type role DFBackend phantom phantom representational
-- I need two layers of wrappers to provide default overlappable instances to
-- all type classes using KnownBackend mechanics.
-- Type arguments are redundant here;
-- nevertheless, they improve readability of error messages.
newtype DFBackend (t :: Type) (n :: Nat) (backend :: Type)
  = DFBackend { _getBackend :: backend }
type instance DataElemType (DFBackend t _ _) = t
type instance DataDims (DFBackend _  n _) = n

-- Note, deriving KnownBackend goes in a not intuitive way:
-- DFBackend t n b ==> DataFrame t n ==> Backend t n;
-- this way, I may be able to not expose DFBackend in user error messages.
instance KnownBackend (DataFrame t n) => KnownBackend (DFBackend t n b) where
  bSing = unsafeCoerce# (bSing :: BackendSing (DataFrame t n))
  {-# INLINE bSing #-}


instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Eq t ) => Eq (DFBackend t n b)
instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Ord t ) => Ord (DFBackend t n b)
instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Show t ) => Show (DFBackend t n b)
instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Num t ) => Semigroup (DFBackend t n b)
instance
   {-# OVERLAPPABLE #-}
   ( KnownBackend (DataFrame t n), Num t, KnownDim n) => Monoid (DFBackend t n b)
