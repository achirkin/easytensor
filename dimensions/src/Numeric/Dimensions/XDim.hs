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
import           Data.Type.Equality      ((:~:)(..))
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
    wrapDim :: Dim (ds :: [Nat]) -> Maybe (Dim xds)


instance XDimensions '[] where
    wrapDim D = Just D
    wrapDim _ = Nothing
    {-# INLINE wrapDim #-}

instance (XDimensions xs, KnownDim m) => XDimensions (XN m ': xs) where
    wrapDim D = Nothing
    wrapDim ((d@Dn :: Dim d) :* ds) =
      if dimVal d >= dimVal' @m
      then case (wrapDim @xs ds, unsafeEqEvidence @(m <=? d) @'True) of
            (Just xds, Evidence) -> Just (Dx d :* xds)
            (Nothing, _) -> Nothing
      else Nothing

instance (XDimensions xs, KnownDim n) => XDimensions (N n ': xs) where
  wrapDim D = Nothing
  wrapDim ((Dn :: Dim d) :* ds) =
    if dimVal' @d == dimVal' @n
    then case (wrapDim @xs ds, unsafeEqEvidence @n @d) of
          (Just xds, Evidence) -> Just (Dn @d :* xds)
          (Nothing, _) -> Nothing
    else Nothing


-- | Loose compile-time information about dimensionalities
xdim :: forall (ds :: [Nat]) (xds :: [XNat])
      . ( Dimensions ds
        , XDimensions xds) => Maybe (Dim xds)
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
unsafeEqEvidence = case (unsafeCoerce# Refl :: x :~: y) of Refl -> Evidence
{-# INLINE unsafeEqEvidence #-}
