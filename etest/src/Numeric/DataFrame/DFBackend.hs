{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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
  , inferEq, inferOrd, inferShow, inferSemigroup, inferMonoid
  )
where


import           Data.Semigroup
import           GHC.Base

import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.Dimensions
import           Numeric.Type.Evidence.Internal
import           Unsafe.Coerce

import           Numeric.DataFrame.TcPlugin


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


-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), Eq t ) => Eq (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), Ord t ) => Ord (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), Show t ) => Show (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), Num t ) => Semigroup (DFBackend t n b)
-- deriving instance
--    {-# OVERLAPPABLE #-}
--    ( KnownBackend (DataFrame t n), Num t, KnownDim n) => Monoid (DFBackend t n b)

{-# ANN inferEq ToInstanceOverlappable #-}
inferEq :: forall t n b . ( KnownBackend (DataFrame t n), Eq t) => BareConstraint (Eq (DFBackend t n b))
inferEq = case inferBase @t @n @b undefined of
  E -> toDFBackend @Eq @t @n @b $ inferBackendInstance 

{-# ANN inferShow ToInstanceOverlappable #-}
inferShow :: forall t n b . ( KnownBackend (DataFrame t n), Show t)
          => BareConstraint (Show (DFBackend t n b))
inferShow = case inferBase @t @n @b undefined of
  E -> toDFBackend @Show @t @n @b $ inferBackendInstance 


{-# ANN inferOrd ToInstanceOverlappable #-}
inferOrd :: forall t n b . ( KnownBackend (DataFrame t n), Ord t)
          => BareConstraint (Ord (DFBackend t n b))
inferOrd = case inferBase @t @n @b undefined of
  E -> toDFBackend @Ord @t @n @b $ inferBackendInstance 

{-# ANN inferSemigroup ToInstanceOverlappable #-}
inferSemigroup :: forall t n b . ( KnownBackend (DataFrame t n), Num t)
          => BareConstraint (Semigroup (DFBackend t n b))
inferSemigroup = case inferBase @t @n @b undefined of
  E -> toDFBackend @Semigroup @t @n @b $ inferBackendInstance 


{-# ANN inferMonoid ToInstanceOverlappable #-}
inferMonoid :: forall t n b . ( KnownBackend (DataFrame t n), Num t, KnownDim n)
          => BareConstraint (Monoid (DFBackend t n b))
inferMonoid = case inferBase @t @n @b undefined of
  E -> toDFBackend @Monoid @t @n @b $ inferBackendInstance 




inferBase :: forall t n b .
             ( KnownBackend (DataFrame t n) )
          => DFBackend t n b
          -> Evidence
          ( b ~ Backend t n
          , t ~ DataElemType b
          , n ~ DataDims b
          , KnownBackend b
          )
inferBase d = case inferBackendTypes d of
  E -> case inferKnownBackendDF d of
    E -> E
{-# INLINE inferBase #-}

inferBackendTypes :: DFBackend t n b -> Evidence (b ~ Backend t n, t ~ DataElemType b, n ~ DataDims b)
inferBackendTypes _ = unsafeCoerce
  (E :: Evidence (b ~ b, t ~ t, n ~ n) )
{-# INLINE inferBackendTypes #-}

inferKnownBackendDF :: forall t n b . KnownBackend (DataFrame t n)
                    => DFBackend t n b -> Evidence (KnownBackend b)
inferKnownBackendDF _ = unsafeCoerce
  (E :: Evidence (KnownBackend (DataFrame t n)))
{-# INLINE inferKnownBackendDF #-}

toDFBackend :: forall c t n b . BareConstraint (c b) -> BareConstraint (c (DFBackend t n b))
toDFBackend = unsafeCoerce
{-# INLINE toDFBackend #-}
