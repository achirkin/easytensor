{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE GADTs, PolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FunctionalDependencies     #-}
{-# LANGUAGE ConstraintKinds      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides type-level operations on lists.
-- Is better when used together with a plugin from Numeric.Dimensions.Inference
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.List
  ( type (++), Length
  , type (:+), type (+:), SnocI, Head, Tail
  , List (..), Cons, Snoc, Reverse, Take, Drop, Concat, Suffix, Prefix
  , EvalList, EvalCons, ToList, SimplifyList
  , ListHead, ListTail, ListLast, ListInit
  , idempSimplifyList, normalSimplifyList
  , ToListNat, EvalConsNat
  ) where

import GHC.TypeLits
import Data.Type.Equality
import Unsafe.Coerce


-- | Synonym for a type-level cons
--     (injective, since this is just a synonym for the list constructor)
type (a :: k) :+ (as :: [k]) = a ': as
infixr 5 :+

-- | List concatenation
type (as :: [k]) ++ (bs :: [k]) = EvalList ('Concat (ToList as) (ToList bs))
infixr 5 ++


-- | Type-level list operations
data List k
  = Empty
  | Cons k (List k)
  | Snoc (List k) k
  | Concat (List k) (List k)
  | Reverse (List k)
  | Drop Nat (List k)
  | Take Nat (List k)
  | Suffix (List k) (List k)
  | Prefix (List k) (List k)


-- | Transform haskell list into a type-level list operations type List
type family ToList (xs :: [k]) = (ys :: List k) | ys -> xs where
    ToList ('[] :: [Nat]) = ('Empty :: List Nat)
    ToList ('[] :: [k]) = ('Empty :: List k)
    ToList (x ': xs :: [Nat]) = ('Cons x (ToListNat xs) :: List Nat)
    ToList (x ': xs) = 'Cons x (ToList xs)

type family ToListNat (xs :: [Nat]) = (ys :: List Nat) | ys -> xs where
    ToListNat ('[] :: [Nat]) = ('Empty :: List Nat)
    ToListNat (x ': xs :: [Nat]) = ('Cons x (ToListNat xs) :: List Nat)



-- | Evaluate a type-level operations List type into a lifted haskell list
type EvalList xs = EvalCons (SimplifyList xs)

-- | Evaluate a List into haskel list with a strong assumption that
--   the list consist only of 'Cons constructors.
type family EvalCons (xs :: List k) = (ys :: [k]) |  ys -> xs where
    EvalCons ('Empty :: List Nat) = ('[] :: [Nat])
    EvalCons ('Empty :: List k) = ('[] :: [k])
    EvalCons ('Cons x xs :: List Nat) = x ': EvalConsNat xs
    EvalCons ('Cons x xs) = x ': EvalCons xs

type family EvalConsNat (xs :: List k) = (ys :: [k]) |  ys -> xs where
    EvalConsNat ('Empty :: List Nat) = ('[] :: [Nat])
    EvalConsNat ('Cons x xs :: List Nat) = x ': EvalConsNat xs



-- x :: Proxy (EvalList (Concat '[2,6] (Drop 1 (Reverse '[2,7,89,4]))))
-- x = _

-- | This function must guarantee that result of evaluation is
--   either 'Empty or 'Cons
type family SimplifyList (xs :: List k) :: List k where
    SimplifyList 'Empty       = 'Empty
    SimplifyList ('Cons x xs) = 'Cons x (SimplifyList xs)

    SimplifyList ('Snoc 'Empty x)       = 'Cons x 'Empty
    SimplifyList ('Snoc ('Cons x xs) y) = 'Cons x (SimplifyList ('Snoc xs y))
    SimplifyList ('Snoc xs y)           = SimplifyList ('Snoc (SimplifyList xs) y)

    SimplifyList ('Concat ('Take n xs) ('Drop n xs)) = SimplifyList xs
    SimplifyList ('Concat 'Empty xs)                 = SimplifyList xs
    SimplifyList ('Concat xs 'Empty)                 = SimplifyList xs
    SimplifyList ('Concat ('Cons x xs) ys)           = 'Cons x (SimplifyList ('Concat xs ys))
    SimplifyList ('Concat ('Prefix bs asbs) ('Suffix ('Prefix bs asbs) asbs)) = SimplifyList asbs
    SimplifyList ('Concat ('Prefix ('Suffix as asbs) asbs) ('Suffix as asbs)) = SimplifyList asbs
    SimplifyList ('Concat xs ys)                     = SimplifyList ('Concat (SimplifyList xs) ys)

    SimplifyList ('Reverse 'Empty)          = 'Empty
    SimplifyList ('Reverse ('Concat xs ys)) = SimplifyList ('Concat ('Reverse ys) ('Reverse xs))
    SimplifyList ('Reverse ('Reverse xs))   = SimplifyList xs
    SimplifyList ('Reverse ('Snoc xs x))    = 'Cons x (SimplifyList ('Reverse xs))
    SimplifyList ('Reverse ('Cons x xs))    = SimplifyList ('Snoc ('Reverse xs) x)
    SimplifyList ('Reverse xs)              = SimplifyList ('Reverse (SimplifyList xs))

    SimplifyList ('Drop 0 xs)           = SimplifyList xs
    SimplifyList ('Drop n 'Empty)       = 'Empty
    SimplifyList ('Drop n ('Cons x xs)) = SimplifyList ('Drop (n-1) xs)
    SimplifyList ('Drop n xs)           = SimplifyList ('Drop n (SimplifyList xs))

    SimplifyList ('Take 0 _)            = 'Empty
    SimplifyList ('Take n 'Empty)       = 'Empty
    SimplifyList ('Take n ('Cons x xs)) = 'Cons x (SimplifyList ('Take (n-1) xs))
    SimplifyList ('Take n xs)           = SimplifyList ('Take n (SimplifyList xs))


    SimplifyList ('Suffix 'Empty xs)
        = SimplifyList xs
    SimplifyList ('Suffix xs xs)
        = 'Empty

    SimplifyList ('Suffix ('Cons _ _) 'Empty)
        = TypeError ( 'Text "Lhs Suffix/Prefix parameter cannot have more elements than its rhs parameter" )
    SimplifyList ('Suffix ('Cons _ as) ('Cons _ asbs))
        = SimplifyList ('Suffix as asbs)
    SimplifyList ('Suffix ('Cons _ as) ('Concat _ asbs))
        = SimplifyList ('Suffix as asbs)
    SimplifyList ('Suffix ('Cons _ as) ('Reverse ('Snoc asbs _)))
        = SimplifyList ('Suffix as ('Reverse asbs))
    SimplifyList ('Suffix ('Cons a as) asbs)
        = SimplifyList ('Suffix ('Cons a as) (SimplifyList asbs))

    SimplifyList ('Suffix ('Snoc _ _) 'Empty)
        = TypeError ( 'Text "Lhs Suffix/Prefix parameter cannot have more elements than its rhs parameter" )

    SimplifyList ('Suffix ('Reverse ('Snoc as _)) ('Cons _ asbs))
        = SimplifyList ('Suffix ('Reverse as) asbs)
    SimplifyList ('Suffix ('Reverse ('Snoc as _)) ('Reverse ('Snoc asbs _)))
        = SimplifyList ('Suffix ('Reverse as) ('Reverse asbs))
    SimplifyList ('Suffix ('Take n asbs) asbs)
        = SimplifyList ('Drop n asbs)
    SimplifyList ('Suffix ('Reverse ('Drop n asbs)) ('Reverse asbs))
        = SimplifyList ('Reverse ('Take n asbs))
    SimplifyList ('Suffix as ('Concat as bs))
        = SimplifyList bs
    SimplifyList ('Suffix ('Prefix bs asbs) asbs)
        = SimplifyList bs
    SimplifyList ('Suffix as asbs)
        = SimplifyList ('Suffix (SimplifyList as) asbs)

    SimplifyList ('Prefix 'Empty asbs)
        = SimplifyList asbs
    SimplifyList ('Prefix xs xs)
        = 'Empty
    SimplifyList ('Prefix ('Cons _ _) 'Empty)
        = TypeError ( 'Text "Lhs Suffix/Prefix parameter cannot have more elements than its rhs parameter" )
    SimplifyList ('Prefix ('Snoc _ _) 'Empty)
        = TypeError ( 'Text "Lhs Suffix/Prefix parameter cannot have more elements than its rhs parameter" )
    SimplifyList ('Prefix ('Snoc as _) ('Snoc asbs _))
        = SimplifyList ('Prefix as asbs)
    SimplifyList ('Prefix ('Reverse ('Cons _ as)) ('Snoc asbs _))
        = SimplifyList ('Prefix ('Reverse as) asbs)
    SimplifyList ('Prefix ('Snoc as _) ('Reverse ('Cons _ asbs)))
        = SimplifyList ('Prefix as ('Reverse asbs))
    SimplifyList ('Prefix ('Reverse ('Cons _ as))  ('Reverse ('Cons _ asbs)))
        = SimplifyList ('Prefix ('Reverse as) ('Reverse asbs))
    SimplifyList ('Prefix ('Drop n asbs) asbs)
        = SimplifyList ('Take n asbs)
    SimplifyList ('Prefix ('Reverse ('Take n asbs)) ('Reverse asbs))
        = SimplifyList ('Reverse ('Drop n asbs))
    SimplifyList ('Prefix bs ('Concat as bs))
        = SimplifyList as
    SimplifyList ('Prefix ('Suffix as asbs) asbs)
        = SimplifyList as
    SimplifyList ('Prefix bs asbs)
        = SimplifyList ('Reverse ('Suffix ('Reverse bs) ('Reverse asbs)))


-- | SimplifyList is an idempotent operation
idempSimplifyList :: p (xs :: List k) -> SimplifyList xs :~: SimplifyList (SimplifyList xs)
idempSimplifyList _ = unsafeCoerce Refl

-- | Result of SimplifyList operation
normalSimplifyList :: p (xs :: [k]) -> ToList xs :~: SimplifyList (ToList xs)
normalSimplifyList _ = unsafeCoerce Refl



type family ListHead (xs :: List k) :: k where
    ListHead 'Empty = TypeError ('Text "Empty type-level list!")
    ListHead ('Take 0 _) = TypeError ('Text "Empty type-level list!")
    ListHead ('Take _ xs) = ListHead xs
    ListHead ('Cons h _) = h
    ListHead xs = ListHead (SimplifyList xs)

type family ListLast (xs :: List k) :: k where
    ListLast 'Empty = TypeError ('Text "Empty type-level list!")
    ListLast ('Take 0 _) = TypeError ('Text "Empty type-level list!")
    ListLast ('Snoc _ x) = x
    ListLast ('Reverse xs) = ListHead xs
    ListLast ('Cons x xs) = ListHead ('Reverse ('Cons x xs))
    ListLast xs = ListLast (SimplifyList xs)

type family ListTail (xs :: List k) :: List k where
    ListTail 'Empty = TypeError ('Text "Empty type-level list!")
    ListTail ('Take 0 _) = TypeError ('Text "Empty type-level list!")
    ListTail ('Cons _ xs) = xs
    ListTail ('Reverse ('Snoc xs _)) = 'Reverse xs
    ListTail xs = ListTail (SimplifyList xs)

type family ListInit (xs :: List k) :: List k where
    ListInit 'Empty = TypeError ('Text "Empty type-level list!")
    ListInit ('Take 0 _) = TypeError ('Text "Empty type-level list!")
    ListInit ('Snoc xs _) = xs
    ListInit ('Reverse ('Cons _ xs)) = 'Reverse xs
    ListInit ('Cons x xs) = 'Reverse (ListTail ('Reverse ('Cons x xs)))
    ListInit xs = ListInit (SimplifyList xs)

-- x :: Proxy (EvalList ( ToList '[1,7,2,8] ))
-- x :: Proxy (EvalList ( ListInit ('Reverse ('Drop 2 (ToList '[1,7,2,8]))) ))
-- x = _


type Cons = 'Cons
type Snoc = 'Snoc
type Concat = 'Concat
type Reverse = 'Reverse
type Drop = 'Drop
type Take = 'Take
type Suffix = 'Suffix
type Prefix = 'Prefix


--------------------------------------------------------------------------------
-- Tricks to make some type-level operations injective
--------------------------------------------------------------------------------


-- x :: Proxy (GetSinkList (SinkFirst '[1,2,3,4]))
-- x = _


-- | Synonym for a type-level snoc (injective!)
type (ns :: [k]) +: (n :: k) = GetSinkList (SinkFirst (n ': ns))
-- type family (ns :: [k]) +: (n :: k) = (nsn :: [k]) | nsn -> ns n where
--   xs +: x = GetListCons (SinkSnoc xs x)
infixl 5 +:
type SnocI (ns :: [k]) (n :: k) = GetSinkList (SinkFirst (n ': ns))


data SinkList k = SLEmpty | SLSingle k | SLCons k [k]

type family SinkFirst (xs :: [k]) = (ys :: SinkList k) | ys -> xs where
  SinkFirst ('[] :: [Nat])  = ('SLEmpty :: SinkList Nat)
  SinkFirst ('[] :: [k])    = ('SLEmpty :: SinkList k)
  SinkFirst ('[x] :: [Nat])  = ('SLSingle x :: SinkList Nat)
  SinkFirst ('[x] :: [k])    = ('SLSingle x :: SinkList k)
  SinkFirst (y ': x ': xs) = 'SLCons x (GetSinkListNat (SinkFirstNat xs y))
  SinkFirst (y ': x ': xs) = 'SLCons x (GetSinkList (SinkFirstK xs y))

type SinkFirstNat (ns :: [Nat]) (n :: Nat) = SinkFirst (n ': ns)
type family GetSinkListNat (xs :: SinkList Nat) = (ys :: [Nat]) | ys -> xs where
  GetSinkListNat 'SLEmpty = '[]
  GetSinkListNat ('SLSingle x) = '[x]
  GetSinkListNat ('SLCons y (x ': xs)) = y ': x ': xs


type SinkFirstK (ns :: [k]) (n :: k) = SinkFirst (n ': ns)
type family GetSinkList (xs :: SinkList k) = (ys :: [k]) | ys -> xs where
  GetSinkList ('SLEmpty :: SinkList k) = ('[] :: [k])
  GetSinkList ('SLSingle x) = '[x]
  GetSinkList ('SLCons y (x ': xs)) = y ': x ': xs






type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x
  Head '[]       = TypeError ( 'Text
    "Head -- empty type-level list."
   )

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[]       = TypeError ( 'Text
    "Tail -- empty type-level list."
   )

type family Length (as :: l) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + Length as
  Length (xs :: List k) = Length (EvalList xs)
