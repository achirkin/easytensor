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
import GHC.TypeLits

import           Unsafe.Coerce           (unsafeCoerce)

--import           Numeric.Dimensions.List


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
  (:*) :: {-# UNPACK #-} !(Dim n) -> {-# UNPACK #-} !(Dim ns) -> Dim (ConsDim n ns)
  -- | Proxy-like constructor
  Dn   :: forall (n :: Nat) . KnownDim n => Dim (n :: Nat)
  -- | Nat known at runtime packed into existential constructor
  Dx   :: forall (n :: Nat) . KnownDim n => {-# UNPACK #-} !(Dim n) -> Dim XN
infixr 5 :*

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDim = forall (ns :: [Nat]) . KnownDims ns => SomeDim (Dim ns)

-- | This class gives the int associated with a type-level natural.
--   Valid known dim must be not less than 2.
class KnownDim (n :: Nat) where
    -- | Get value of type-level dim at runtime
    dimVal' :: Int

-- | A constraint family that makes sure all subdimensions are known.
type family KnownDims (ns :: [Nat]) :: Constraint where
    KnownDims '[] = ()
    KnownDims (x ': xs) = ( KnownDim x, KnownDims xs )



instance {-# OVERLAPPABLE #-} (KnownNat n, 21 <= n) => KnownDim n where
    {-# INLINE dimVal' #-}
    dimVal' = fromInteger (natVal' (proxy# :: Proxy# n))

instance {-# OVERLAPPING #-} KnownDim 2  where { {-# INLINE dimVal' #-}; dimVal' = 2 }
instance {-# OVERLAPPING #-} KnownDim 3  where { {-# INLINE dimVal' #-}; dimVal' = 3 }
instance {-# OVERLAPPING #-} KnownDim 4  where { {-# INLINE dimVal' #-}; dimVal' = 4 }
instance {-# OVERLAPPING #-} KnownDim 5  where { {-# INLINE dimVal' #-}; dimVal' = 5 }
instance {-# OVERLAPPING #-} KnownDim 6  where { {-# INLINE dimVal' #-}; dimVal' = 6 }
instance {-# OVERLAPPING #-} KnownDim 7  where { {-# INLINE dimVal' #-}; dimVal' = 7 }
instance {-# OVERLAPPING #-} KnownDim 8  where { {-# INLINE dimVal' #-}; dimVal' = 8 }
instance {-# OVERLAPPING #-} KnownDim 9  where { {-# INLINE dimVal' #-}; dimVal' = 9 }
instance {-# OVERLAPPING #-} KnownDim 10 where { {-# INLINE dimVal' #-}; dimVal' = 10 }
instance {-# OVERLAPPING #-} KnownDim 11 where { {-# INLINE dimVal' #-}; dimVal' = 11 }
instance {-# OVERLAPPING #-} KnownDim 12 where { {-# INLINE dimVal' #-}; dimVal' = 12 }
instance {-# OVERLAPPING #-} KnownDim 13 where { {-# INLINE dimVal' #-}; dimVal' = 13 }
instance {-# OVERLAPPING #-} KnownDim 14 where { {-# INLINE dimVal' #-}; dimVal' = 14 }
instance {-# OVERLAPPING #-} KnownDim 15 where { {-# INLINE dimVal' #-}; dimVal' = 15 }
instance {-# OVERLAPPING #-} KnownDim 16 where { {-# INLINE dimVal' #-}; dimVal' = 16 }
instance {-# OVERLAPPING #-} KnownDim 17 where { {-# INLINE dimVal' #-}; dimVal' = 17 }
instance {-# OVERLAPPING #-} KnownDim 18 where { {-# INLINE dimVal' #-}; dimVal' = 18 }
instance {-# OVERLAPPING #-} KnownDim 19 where { {-# INLINE dimVal' #-}; dimVal' = 19 }
instance {-# OVERLAPPING #-} KnownDim 20 where { {-# INLINE dimVal' #-}; dimVal' = 20 }


-- | Get value of type-level dim at runtime.
--   Gives a product of all dimensions if is a list
dimVal :: Dim x -> Int
dimVal (Dn :: Dim m) = dimVal' @m
dimVal (Dx (Dn :: Dim m)) = dimVal' @m
dimVal D = 1
dimVal (d :* ds) = dimVal d * dimVal ds
{-# INLINE dimVal #-}

-- | Similar to `someNatVal`, but for a single dimension
someDimVal :: Int -> Maybe (Dim XN)
someDimVal x | 2 > x     = Nothing
             | otherwise = Just (reifyDim x $ \(_ :: Proxy# n) -> Dx (Dn @n))

-- | Convert a list of ints into unknown type-level Dimensions list
someDimsVal :: [Int] -> Maybe SomeDim
someDimsVal []             = Just $ SomeDim D
someDimsVal (x:xs) | 2 > x = Nothing
                   | otherwise = do
  SomeDim ps <- someDimsVal xs
  return $ reifyDim x $ \(_ :: Proxy# n) -> SomeDim (Dn @n :* ps)


-- | Unify usage of XNat and Nat.
--   This is useful in function and type definitions.
--   Mainly used in the definition of Dim.
type family ConsDim (x :: l) (xs :: [k]) = (ys :: [k]) | ys -> x xs l where
    ConsDim (x :: Nat) (xs :: [Nat])  = x   ': xs
    ConsDim (x :: Nat) (xs :: [XNat]) = N x ': xs
    ConsDim  XN        (xs :: [XNat]) = XN  ': xs


-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall r . Int -> (forall (n :: Nat) . KnownDim n => Proxy# n -> r) -> r
reifyDim n k = unsafeCoerce (MagicDim k :: MagicDim r) n proxy#
newtype MagicDim r = MagicDim (forall (n :: Nat) . KnownDim n => Proxy# n -> r)

instance Show (Dim ds) where
    show D      = "Dim Ø"
    show ds     = "Dim " ++ dimList ds

dimList :: Dim ds -> String
dimList  D        = ""
dimList  d@Dn     = show (dimVal d)
dimList  (Dx d)   = show (dimVal d)
dimList (d :* D)  = show (dimVal d)
dimList (d :* ds) = show (dimVal d) ++ ' ':dimList ds


instance Show SomeDim where
    show (SomeDim p) = "Some" ++ show p


class Dimensions (ds :: [Nat]) where
    -- | Dimensionality of our space
    dim :: Dim ds

instance Dimensions '[] where
    dim = D
    {-# INLINE dim #-}

instance (KnownDim d, Dimensions ds) => Dimensions (d ': ds) where
    dim = Dn :* dim
    {-# INLINE dim #-}
