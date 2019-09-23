{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
#else
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
#endif
{-
The purpose of this module is to provide incoherent instances of common type classes
for the type @Backend@.
It imports @Backend.hs-boot@ module and thus does not depend on the actual
implementation of @BackendFamily@.
Import this module instead of @Backend.hs-boot@ in the places that must not depend
on @BackendFamily@, such as @Numeric.DataFrame.Type@.

By design the import chain is as follows:

NDI.Backend.Family.hs-boot            NDI.Backend.Family
         \                                   |
    NDI.Backend.hs-boot               NDI.Backend (instances only)
          \                                 /
     NDI.BackendI                          /
            \                             /
    Numeric.DataFrame.Type               /
                    \                   /
                     \                 /
                     Numeric.DataFrame

 -}
module Numeric.DataFrame.Internal.BackendI
  ( DFBackend, Backend (..), Backend.BackendFamily, Backend.KnownBackend ()
  , Backend.inferKnownBackend, Backend.inferPrimElem
#if !defined(__HADDOCK__) && !defined(__HADDOCK_VERSION__)
  , inferEq, inferOrd
  , inferProductOrder, inferPONonTransitive, inferPOPartial
  , inferBounded, inferNum
  , inferFractional, inferFloating
  , inferPrimBytes, inferPrimArray
#endif
  ) where


#if !defined(__HADDOCK__) && !defined(__HADDOCK_VERSION__)
import Data.Constraint          (Dict)
import Data.Constraint.Deriving (OverlapMode (..), ToInstance (..))
#endif
import           Data.Kind                            (Type)
import           Numeric.DataFrame.Internal.PrimArray (PrimArray)
import           Numeric.Dimensions                   (Dimensions, Nat)
import           Numeric.PrimBytes                    (PrimBytes)
import           Numeric.ProductOrd                   (ProductOrder)
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial

import {-# SOURCE #-} Numeric.DataFrame.Internal.Backend        (Backend)
import {-# SOURCE #-} qualified Numeric.DataFrame.Internal.Backend        as Backend
import {-# SOURCE #-} qualified Numeric.DataFrame.Internal.Backend.Family as Impl



-- | Implementation behind the DataFrame
type DFBackend (t :: Type) (ds :: [Nat]) = Backend I t ds (Impl.BackendFamily t ds)

{- | The instance keeper for the `Backend` type.

  Using this data as a tag to the `Backend` type allows to define `Backend` instances
  in two different modules without any orphans.

 -}
data I



-- The instances below are bumb stubs needed only to compile haddock
#if !defined(__HADDOCK__) && !defined(__HADDOCK_VERSION__)

{-# ANN inferEq (ToInstance Incoherent) #-}
inferEq
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Eq t, Impl.KnownBackend t ds b)
  => Dict (Eq (Backend I t ds b))
inferEq = Backend.inferEq @t @ds @b

{-# ANN inferOrd (ToInstance Incoherent) #-}
inferOrd
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Backend I t ds b))
inferOrd = Backend.inferOrd @t @ds @b

{-# ANN inferProductOrder (ToInstance Incoherent) #-}
inferProductOrder
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (ProductOrder (Backend I t ds b))
inferProductOrder = Backend.inferProductOrder @t @ds @b

{-# ANN inferPONonTransitive (ToInstance Incoherent) #-}
inferPONonTransitive
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (NonTransitive.ProductOrd (Backend I t ds b)))
inferPONonTransitive = Backend.inferPONonTransitive @t @ds @b

{-# ANN inferPOPartial (ToInstance Incoherent) #-}
inferPOPartial
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Ord t, Impl.KnownBackend t ds b)
  => Dict (Ord (Partial.ProductOrd (Backend I t ds b)))
inferPOPartial = Backend.inferPOPartial @t @ds @b

{-# ANN inferBounded (ToInstance Incoherent) #-}
inferBounded
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Bounded t, Impl.KnownBackend t ds b)
  => Dict (Bounded (Backend I t ds b))
inferBounded = Backend.inferBounded @t @ds @b

{-# ANN inferNum (ToInstance Incoherent) #-}
inferNum
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Num t, Impl.KnownBackend t ds b)
  => Dict (Num (Backend I t ds b))
inferNum = Backend.inferNum @t @ds @b

{-# ANN inferFractional (ToInstance Incoherent) #-}
inferFractional
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Fractional t, Impl.KnownBackend t ds b)
  => Dict (Fractional (Backend I t ds b))
inferFractional = Backend.inferFractional @t @ds @b

{-# ANN inferFloating (ToInstance Incoherent) #-}
inferFloating
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (Floating t, Impl.KnownBackend t ds b)
  => Dict (Floating (Backend I t ds b))
inferFloating = Backend.inferFloating @t @ds @b

{-# ANN inferPrimBytes (ToInstance Incoherent) #-}
inferPrimBytes
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (PrimBytes t, Dimensions ds, Impl.KnownBackend t ds b)
  => Dict (PrimBytes (Backend I t ds b))
inferPrimBytes = Backend.inferPrimBytes @t @ds @b

{-# ANN inferPrimArray (ToInstance Incoherent) #-}
inferPrimArray
  :: forall (t :: Type) (ds :: [Nat]) (b :: Type)
   . (PrimBytes t, Impl.KnownBackend t ds b)
  => Dict (PrimArray t (Backend I t ds b))
inferPrimArray = Backend.inferPrimArray @t @ds @b

#else

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Eq t, Impl.KnownBackend t ds b)
  => Eq (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => ProductOrder (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (NonTransitive.ProductOrd (Backend I t ds b))

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Ord t, Impl.KnownBackend t ds b)
  => Ord (Partial.ProductOrd (Backend I t ds b))

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Bounded t, Impl.KnownBackend t ds b)
  => Bounded (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Num t, Impl.KnownBackend t ds b)
  => Num (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Fractional t, Impl.KnownBackend t ds b)
  => Fractional (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . (Floating t, Impl.KnownBackend t ds b)
  => Floating (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . ( PrimBytes t
    , Dimensions ds
    , Impl.KnownBackend t ds b
    )
  => PrimBytes (Backend I t ds b)

instance
    forall (t :: Type) (ds :: [Nat]) (b :: Type)
  . ( PrimBytes t
    , Impl.KnownBackend t ds b
    )
  => PrimArray t (Backend I t ds b)

#endif
