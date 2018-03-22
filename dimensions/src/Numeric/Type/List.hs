{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Type.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides type-level operations on lists.
--
--
--------------------------------------------------------------------------------

module Numeric.Type.List
  ( -- * Basic operations
    type (++), type (+:), type (:+)
  , Empty, Cons, Snoc, Head
  , Tail, Init, Last, Concat, Reverse, Take, Drop, Length
  , All, Map
    -- * Working with concatenations
  , Suffix, Prefix, IsPrefix, IsSuffix
    -- * Term level functions
  , ConcatList
  ) where

import           GHC.Exts
import           GHC.TypeLits


-- | Synonym for a type-level cons
--     (injective, since this is just a synonym for the list constructor)
type (a :: k) :+ (as :: [k]) = a ': as
infixr 5 :+
-- | Prefix-style synonym for cons
type Cons (n :: k) (ns :: [k]) = n :+ ns

-- | Synonym for a type-level snoc (injective!)
type (ns :: [k]) +: (n :: k) = Snoc ns n
infixl 5 +:
-- | Prefix-style synonym for snoc
type Snoc (ns :: [k]) (n :: k) = GetSnoc k (DoSnoc k ns n)


-- | List concatenation
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    (++) '[] bs = bs
    (++) as '[] = as
    (++) (a :+ as) bs = a :+ (as ++ bs)
infixr 5 ++

-- | Prefix-style synonym for concatenation
type Concat (as :: [k]) (bs :: [k]) = as ++ bs


-- | Reverse a type-level list (injective!)
type Reverse (xs :: [k]) = Reversed k (DoReverse k xs)


-- | Synonym for an empty type-level list
type Empty = '[]


type family Take (n::Nat) (xs :: [k]) :: [k] where
    Take _ '[] = '[]
    Take 0 xs = '[]
    Take n (x :+ xs) = x :+ Take (n-1) xs


type family Drop (n::Nat) (xs :: [k]) :: [k] where
    Drop _ '[] = '[]
    Drop 0 xs = xs
    Drop n (x :+ xs) = Drop (n-1) xs

type family Suffix (as :: [k]) (asbs :: [k]) :: [k] where
    Suffix '[] bs = bs
    Suffix as as = '[]
    Suffix (_ :+ as) (_ :+ asbs) = Suffix as asbs

type family Prefix (bs :: [k]) (asbs :: [k]) :: [k] where
    Prefix '[] as = as
    Prefix bs bs = '[]
    Prefix bs asbs = Take (Length asbs - Length bs) asbs


type family IsPrefix (as :: [k]) (asbs :: [k]) :: Bool where
    IsPrefix '[] _ = 'True
    IsPrefix (a :+ as) (a :+ asbs) = IsPrefix as asbs
    IsPrefix as as = 'True
    IsPrefix _ _= 'False

type family IsSuffix (as :: [k]) (asbs :: [k]) :: Bool where
    IsSuffix '[] _ = 'True
    IsSuffix bs bs = 'True
    IsSuffix bs (_ :+ sbs) = IsSuffix bs sbs
    IsSuffix _ _ = 'False


type family Head (xs :: [k]) :: k where
    Head (x :+ xs)  = x
    Head (KEmpty k) = TypeError ( 'Text
      "Head: empty type-level list."
      ':$$: FamErrMsg k
     )

type family Tail (xs :: [k]) :: [k] where
    Tail (x :+ xs)  = xs
    Tail (KEmpty k) = TypeError ( 'Text
      "Tail: empty type-level list."
      ':$$: FamErrMsg k
     )

type family Init (xs :: [k]) :: [k] where
    Init '[x]       = '[]
    Init (x :+ xs)  = x :+ Init xs
    Init (KEmpty k) = TypeError ( 'Text
      "Init: empty type-level list."
      ':$$: FamErrMsg k
     )

type family Last (xs :: [k]) :: k where
    Last '[x]       = x
    Last (x :+ xs)  = Last xs
    Last (KEmpty k) = TypeError ( 'Text
      "Last: empty type-level list."
      ':$$: FamErrMsg k
     )

type family Length (xs :: [k]) :: Nat where
    Length '[] = 0
    Length (_ ': xs) = 1 + Length xs


type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    All _ '[] = ()
    All f (x ': xs) = (f x, All f xs)

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs


-- | Represent a triple of lists forming a relation `as ++ bs ~ asbs`
class ( asbs ~ Concat as bs
      , as   ~ Prefix bs asbs
      , bs   ~ Suffix as asbs
      , IsSuffix bs asbs ~ 'True
      , IsPrefix as asbs ~ 'True
      ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])
        | as bs -> asbs
        , as asbs -> bs
        , bs asbs -> as

instance ( asbs ~ Concat as bs
         , as   ~ Prefix bs asbs
         , bs   ~ Suffix as asbs
         , IsSuffix bs asbs ~ 'True
         , IsPrefix as asbs ~ 'True
         ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])



type FamErrMsg k
  = 'Text "Type-level error occured when operating on a list of kind "
    ':<>: 'ShowType [k] ':<>: 'Text "."

--------------------------------------------------------------------------------
---- Tricks to make some type-level operations injective
--------------------------------------------------------------------------------



-- | A special data type that can have either a single element,
--   or more than two.
--   This feature is not enforced in the type system - this is just a way to make injective Snoc.
data Snocing k    = SSingle k | SCons [k]
type SSingle k x  = 'SSingle (x :: k)
type SCons k xs   = 'SCons (xs :: [k])
type KCons k x xs = (x :: k) ': (xs :: [k])
type KEmpty k     = ('[] :: [k])
type KSingle k x  = ('[x] :: [k])

type family DoSnoc k (xs :: [k]) (z::k) = (ys :: Snocing k) | ys -> xs z where
    DoSnoc k '[]       x      = SSingle k x
    DoSnoc k (KCons k x xs) y =
      (SCons k (KCons k x (GetSnoc k (DoSnoc k xs y)) :: [k]) :: Snocing k)


type family GetSnoc k (xs :: Snocing k) = (ys :: [k]) | ys -> xs where
    GetSnoc k (SSingle k x) = KSingle k x
    GetSnoc k (SCons k (KCons k y (KCons k x xs))) =
      KCons k y (KCons k x xs)

-- | Another data type to make Reverse injective.
data Reversing k    = REmpty | RReverse [k]
type REmpty    k    = 'REmpty
type RReverse  k xs = 'RReverse (xs :: [k])



type family Reversed k (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
    Reversed k (REmpty k) = KEmpty k
    Reversed k (RReverse k (KCons k x xs)) = KCons k x xs


type family DoReverse k (as :: [k]) = (rs :: Reversing k) | rs -> as where
    DoReverse k '[]  = REmpty k
    DoReverse k (KCons k a as) =
      RReverse k (GetSnoc k (DoSnoc k (Reversed k (DoReverse k as)) a))
