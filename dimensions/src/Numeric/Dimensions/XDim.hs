{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.XDim
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Some dimensions in a type-level dimension list may by not known at compile time.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.XDim
  ( -- * Data types
    XDim (..), xdim, xDimVal
    -- * Constraints
  , XDimensions (..)
  ) where


import           Data.Maybe              (isJust)
import           GHC.Exts                (unsafeCoerce#)

import           Numeric.Dimensions.Dim
import           Numeric.TypeLits


-- | Similar to SomeNat, hide some dimensions under an existential constructor.
--   In contrast to SomeDim, it preserves the order of dimensions,
--   and it can keep some of the dimensions in the list static
--   while making other dimensions known only at runtime.
data XDim (xns :: [XNat])
  = forall ns . ( FixedDim xns ns ~ ns
                , FixedXDim xns ns ~ xns
                ) => XDim (Dim ns)


class XDimensions (xds :: [XNat]) where
    wrapDim :: FixedXDim xds ds ~ xds => Dim ds -> Dim xds


instance XDimensions '[] where
    wrapDim D = D
    {-# INLINE wrapDim #-}

instance XDimensions xs => XDimensions (XN m ': xs) where
  wrapDim ((d@Dn :: Dim d) :* ds)
    | Evidence <- unsafeEqEvidence @(m <=? d) @'True
    = Dx d :* wrapDim ds

instance XDimensions xs => XDimensions (N n ': xs) where
  wrapDim ((Dn :: Dim d) :* ds)
    | Evidence <- unsafeEqEvidence @n @d
    = Dn @d :* wrapDim ds


-- | Loose compile-time information about dimensionalities
xdim :: forall (ds :: [Nat]) (xds :: [XNat])
      . ( Dimensions ds
        , XDimensions xds
        , FixedXDim xds ds ~ xds) => Dim xds
xdim = wrapDim @xds @ds (dim @ds)
{-# INLINE xdim #-}



-- | Construct dimensionality at runtime
xDimVal :: Dim (xns :: [XNat]) -> XDim xns
xDimVal D = XDim D
xDimVal ((Dn :: Dim n) :* ds) = case xDimVal ds of
  XDim ps -> XDim (Dn @n :* ps)
xDimVal (Dx d :* ds) = case xDimVal ds of
  XDim ps -> XDim (d :* ps)


instance Show (XDim xns) where
    show (XDim p) = 'X' : show p

instance Eq (XDim xds) where
    XDim as == XDim bs = isJust $ sameDim as bs

instance Ord (XDim xds) where
    compare (XDim as) (XDim bs) = compareDim as bs


unsafeEqEvidence :: forall x y . Evidence (x ~ y)
unsafeEqEvidence = unsafeCoerce# (Evidence @())
{-# INLINE unsafeEqEvidence #-}
