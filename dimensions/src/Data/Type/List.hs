{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Provides type-level operations on lists.
--
--
--------------------------------------------------------------------------------

module Data.Type.List
  ( -- * Basic operations
    type (++), type (+:), type (:+)
  , Empty, Cons, Snoc, Head
  , Tail, Last, Init, Concat
  , StripPrefix, StripSuffix
  , Reverse, Take, Drop, Length
    -- * Operations on elements
  , All, Map, UnMap, Elem
    -- * Classes that simplify inference of type equalities
  , SnocList, ReverseList, ConcatList
  , inferStripSuffix, inferStripPrefix, inferConcat
    -- * Data.Typeable
  , inferTypeableCons
  ) where

import Data.Constraint         (Constraint, Dict (..))
import Data.Kind               (Type)
import Data.Type.List.Classes
import Data.Type.List.Families
import Data.Type.Lits
import Type.Reflection

-- | Empty list, same as @'[]@.
type Empty = '[]

-- | Appending a list, represents an @Op@ counterpart of @(':)@.
type Cons (a :: k) (as :: [k])
    = a ': as

-- | @Take n xs@ returns the prefix of a list of length @max n (length xs)@.
type family Take (n :: Nat) (xs :: [k]) :: [k] where
    Take 0  _        = '[]
    Take n (x ': xs) = x ': Take (n - 1) xs
    Take _ '[]       = '[]

-- | @Drop n xs@ drops up to @n@ elements of @xs@.
type family Drop (n :: Nat) (xs :: [k]) :: [k] where
    Drop 0  xs       = xs
    Drop n (_ ': xs) = Drop (n - 1) xs
    Drop _ '[]       = '[]

-- | Number of elements in a list.
type family Length (xs :: [k]) :: Nat where
    Length '[]       = 0
    Length (x ': xs) = Length xs + 1

-- | Synonym for a type-level @Cons@.
type (a :: k) :+ (as :: [k]) = a ': as
infixr 5 :+

-- | Synonym for a type-level @Snoc@.
type (ns :: [k]) +: (n :: k) = Snoc ns n
infixl 6 +:

-- | Infix-style synonym for concatenation
type (as :: [k]) ++ (bs :: [k]) = Concat as bs
infixr 5 ++

-- | All elements of a type list must satisfy the same constraint.
type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    All _ '[]       = ()
    All f (x ': xs) = (f x, All f xs)

-- | Map a functor over the elements of a type list.
type family Map (f :: a -> b) (xs :: [a]) :: [b] where
    Map f '[]       = '[]
    Map f (x ': xs) = f x ': Map f xs

-- | Unmap a functor over the elements of a type list.
type family UnMap (f :: a -> b) (xs :: [b]) :: [a] where
    UnMap f '[]         = '[]
    UnMap f (f x ': ys) = x ': UnMap f ys

-- | Check if an item is a member of a list.
type family Elem (x :: k) (xs :: [k]) :: Constraint where
    Elem x (x ': xs) = ()
    Elem x (_ ': xs) = Elem x xs

-- | Given a @Typeable@ list, infer this constraint for its parts.
inferTypeableCons :: forall (k :: Type) (ys :: [k]) (x :: k) (xs :: [k])
                   . (Typeable ys, ys ~ (x ': xs))
                  => Dict (Typeable x, Typeable xs)
inferTypeableCons = case typeRep @ys of
  App _ xRep `App` xsRep
    -> case ( withTypeable xRep  (Dict @(Typeable x))
            , withTypeable xsRep (Dict @(Typeable xs))
            ) of
        (Dict, Dict) -> Dict
