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
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE RoleAnnotations           #-}
-- {-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Dim
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type Idx that enumerates through multiple dimensions.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-- Some of the type-level list operations are implemented using type families
--   and weirdly duplicated for kinds k,Nat,XNat:
--   This duplication is needed to workaround some GHC bugs (panic with "no skolem info")
-----------------------------------------------------------------------------

module Numeric.Dimensions.Dim
  ( -- * Data types
    XNat, XN, N
  , Dim (..)
    -- * Constraints
  , KnownDim (..), KnownDims, dimVal
  , SomeDim (..), someDimVal, someDimsVal
  , Dimensions (..)
  ) where


-- import           Control.Arrow           (first)
-- import           Data.Maybe              (isJust)
-- import           Data.Type.Equality      ((:~:) (..))
-- import           GHC.Exts                (Constraint, IsList (..), Proxy#,
--                                           State#, proxy#)
-- import           GHC.TypeLits            (type (+), type (-), type (<=),
--                                           type (<=?), ErrorMessage (..),
--                                           KnownNat, Nat, SomeNat (..),
--                                           TypeError, natVal, natVal', sameNat,
--                                           someNatVal)
import GHC.Exts
import GHC.Types

import Numeric.TypeLits
import           Numeric.Dimensions.List


-- | Either known or unknown at compile-time natural number
data XNat = XN | N Nat
-- | Unknown natural number
type XN = 'XN
-- | Known natural number
type N (n::Nat) = 'N n


-- | Type-level dimensionality
data Dim (ns :: k) where
  -- | Zero-rank dimensionality - scalar
  D   :: Dim '[]
  -- | List-like concatenation of dimensionality
  (:*) :: {-# UNPACK #-} !(Dim n) -> Dim ns -> Dim (ConsDim n ns)
  -- | Proxy-like constructor
  Dn   :: forall (n :: Nat) . KnownDim n => Dim (n :: Nat)
  -- | Nat known at runtime packed into existential constructor
  Dx   :: forall (n :: Nat) . KnownDim n => {-# UNPACK #-} !(Dim n) -> Dim XN
infixr 5 :*

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDims = forall (ns :: [Nat]) . KnownDims ns => SomeDims (Dim ns)


-- | Get value of type-level dim at runtime.
--   Gives a product of all dimensions if is a list
dimVal :: Dim x -> Int
dimVal (Dn :: Dim m) = dimVal' @m
dimVal (Dx (Dn :: Dim m)) = dimVal' @m
dimVal D = 1
dimVal (d :* ds) = dimVal d * dimVal ds
{-# INLINE dimVal #-}

-- | Convert a list of ints into unknown type-level Dimensions list
someDimsVal :: [Int] -> Maybe SomeDims
someDimsVal []             = Just $ SomeDims D
someDimsVal (x:xs) | 2 > x = Nothing
                   | otherwise = do
  SomeDim (_ :: Proxy# n) <- someDimVal x
  SomeDims ps <- someDimsVal xs
  return $ SomeDims (Dn @n :* ps)


-- | Unify usage of XNat and Nat.
--   This is useful in function and type definitions.
--   Mainly used in the definition of Dim.
type family ConsDim (x :: l) (xs :: [k]) = (ys :: [k]) | ys -> x xs l where
    ConsDim (x :: Nat) (xs :: [Nat])  = x   ': xs
    ConsDim (x :: Nat) (xs :: [XNat]) = N x ': xs
    ConsDim  XN        (xs :: [XNat]) = XN  ': xs


dimList :: Dim ds -> String
dimList  D        = ""
dimList  d@Dn     = show (dimVal d)
dimList  (Dx d)   = show (dimVal d)
dimList (d :* D)  = show (dimVal d)
dimList (d :* ds) = show (dimVal d) ++ ' ':dimList ds




instance Show (Dim ds) where
    show D      = "Dim Ø"
    show ds     = "Dim " ++ dimList ds

instance Show SomeDims where
    show (SomeDims p) = "Some" ++ show p

class Dimensions (ds :: [Nat]) where
    -- | Dimensionality of our space
    dim :: Dim ds

instance Dimensions '[] where
    dim = D
    {-# INLINE dim #-}

instance (KnownDim d, Dimensions ds) => Dimensions (d ': ds) where
    dim = Dn :* dim
    {-# INLINE dim #-}



inferDimensions :: forall (ds :: [Nat])
                 . (KnownDims ds, FiniteList ds)
                => Evidence (Dimensions ds)
inferDimensions = case tList @Nat @ds of
  TLEmpty -> Evidence
  TLCons _ (_ :: TypeList ds') -> case inferDimensions @ds' of
    Evidence -> Evidence
{-# INLINE inferDimensions #-}

inferDimKnownDims :: forall (ds :: [Nat])
                   . Dimensions ds
                  => Evidence (KnownDims ds)
inferDimKnownDims = inferDimKnownDims' (dim @ds)
  where
    inferDimKnownDims' :: forall (ns :: [Nat]) . Dim ns -> Evidence (KnownDims ns)
    inferDimKnownDims' D = Evidence
    inferDimKnownDims' (Dn :* ds) = case inferDimKnownDims' ds of Evidence -> Evidence
    inferDimKnownDims' _ = error "Impossible Dim constructor"
{-# INLINE inferDimKnownDims #-}

inferDimFiniteList :: forall (ds :: [Nat])
                    . Dimensions ds
                   => Evidence (FiniteList ds)
inferDimFiniteList = inferDimFiniteList' (dim @ds)
  where
    inferDimFiniteList' :: forall (ns :: [Nat]) . Dim ns -> Evidence (FiniteList ns)
    inferDimFiniteList' D = Evidence
    inferDimFiniteList' (Dn :* ds) = case inferDimFiniteList' ds of Evidence -> Evidence
    inferDimFiniteList' _ = error "Impossible Dim constructor"
{-# INLINE inferDimFiniteList #-}



inferTailDimensions :: forall (ds :: [Nat])
                    . Dimensions ds
                    => Evidence (Dimensions (Tail ds))
inferTailDimensions = case dim @ds of
    D -> error "Tail dimensions error -- empty Dim list"
    Dn :* ds' -> reifyDims ds' Evidence
    _ -> error "Tail dimensions error -- impossible Dim constructor"








-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDims :: forall r (ds :: [Nat]) . Dim ds -> ( Dimensions ds => r) -> r
reifyDims ds k = unsafeCoerce# (MagicDims k :: MagicDims ds r) ds
{-# INLINE reifyDims #-}
newtype MagicDims ds r = MagicDims (Dimensions ds => r)
