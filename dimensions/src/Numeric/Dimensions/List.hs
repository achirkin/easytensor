-- {-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
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
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides type-level operations on lists.
--
--------------------------------------------------------------------------------

module Numeric.Dimensions.List
  ( type (++), type (:+), type (+:)
  , Empty, Cons, Snoc, Head
  , Tail, Init, Last, Concat, Reverse, Take, Drop, Suffix, Prefix
  , IsPrefix, IsSuffix
  , ConcatList (..), FiniteList (..), TypeList (..)
  , inferConcat, inferSuffix, inferPrefix, ConcatEvidence, FiniteListEvidence
  , NonEmptyListEvidence, inferNonEmptyList
  , inferKnownLength
  , inferTailFiniteList, inferConcatFiniteList
  , inferPrefixFiniteList, inferSuffixFiniteList
  , inferSnocFiniteList, inferInitFiniteList
  , inferTakeNFiniteList, inferDropNFiniteList, inferReverseFiniteList
  ) where

import           Data.Proxy       (Proxy (..))
import           Numeric.TypeLits
import           Unsafe.Coerce    (unsafeCoerce)

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
type Snoc (ns :: [k]) (n :: k) = GetSnoc (DoSnoc ns n)


-- | List concatenation
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    (++) '[] bs = bs
    (++) as '[] = as
    (++) (a :+ as) bs = a :+ (as ++ bs)
infixr 5 ++

-- | Prefix-style synonym for concatenation
type Concat (as :: [k]) (bs :: [k]) = as ++ bs


-- | Reverse a type-level list (injective!)
type Reverse (xs :: [k]) = Reversed (DoReverse xs)


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
    Head (x :+ xs) = x
    Head '[]       = TypeError ( 'Text
      "Head -- empty type-level list."
     )

type family Tail (xs :: [k]) :: [k] where
    Tail (x :+ xs) = xs
    Tail '[]       = TypeError ( 'Text
      "Tail -- empty type-level list."
     )

type family Init (xs :: [k]) :: [k] where
    Init '[x] = '[]
    Init (x :+ xs) = x :+ Init xs
    Init '[]       = TypeError ( 'Text
      "Init -- empty type-level list."
     )

type family Last (xs :: [k]) :: k where
    Last '[x] = x
    Last (x :+ xs) = Last xs
    Last '[]       = TypeError ( 'Text
      "Last -- empty type-level list."
     )


type NonEmptyList xs = ( xs ~ (Head xs :+ Tail xs)
                       , xs ~ (Init xs +: Last xs)
                       , IsPrefix  (Init xs) xs ~ 'True
                       , IsPrefix '[Head xs] xs ~ 'True
                       , IsSuffix  (Tail xs) xs ~ 'True
                       , IsSuffix '[Last xs] xs ~ 'True
                       , Suffix '[Head xs] xs ~   Tail xs
                       , Suffix  (Init xs) xs ~ '[Last xs]
                       , Prefix '[Last xs] xs ~   Init xs
                       , Prefix  (Tail xs) xs ~ '[Head xs]
                       , Prefix  (Tail xs) ('[Head xs] ++ Tail xs) ~ '[Head xs]
                       , Suffix  (Init xs) (Init xs ++ '[Last xs]) ~ '[Last xs]
                       , IsPrefix '[Head xs] ('[Head xs] ++ Tail xs) ~ 'True
                       , IsSuffix '[Last xs] (Init xs ++ '[Last xs]) ~ 'True
                       , xs ~ ('[Head xs] ++ Tail xs)
                       , xs ~ (Init xs ++ '[Last xs])
                       )

-- | Represent a triple of lists forming a relation `as ++ bs ~ asbs`
class ( asbs ~ Concat as bs
      , as   ~ Prefix bs asbs
      , bs   ~ Suffix as asbs
      , IsSuffix bs asbs ~ 'True
      , IsPrefix as asbs ~ 'True
      ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])
        | as bs -> asbs
        , as asbs -> bs
        , bs asbs -> as where
    tlPrefix :: ConcatEvidence as bs asbs -> Proxy as
    tlSuffix :: ConcatEvidence as bs asbs -> Proxy bs
    tlConcat :: ConcatEvidence as bs asbs -> Proxy asbs

instance ( asbs ~ Concat as bs
         , as   ~ Prefix bs asbs
         , bs   ~ Suffix as asbs
         , IsSuffix bs asbs ~ 'True
         , IsPrefix as asbs ~ 'True
         ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k]) where
    tlPrefix _ = Proxy
    {-# INLINE tlPrefix #-}
    tlSuffix _ = Proxy
    {-# INLINE tlSuffix #-}
    tlConcat _ = Proxy
    {-# INLINE tlConcat #-}


-- | Type level list, used together with FiniteList typeclass
data TypeList (xs :: [k]) where
    TLEmpty :: TypeList '[]
    TLCons  :: FiniteList xs => !(Proxy# x) -> TypeList xs -> TypeList (x :+ xs)

-- | Type-level list that is known to be finite.
--   Basically, provides means to get its length and term-level rep (via TypeList)
class FiniteList (xs :: [k]) where
    -- | Length of a type-level list at type level
    type Length xs :: Nat
    -- | Length of a type-level list at term level
    order :: Int
    -- | Get type-level constructed list
    tList :: TypeList xs



instance FiniteList ('[] :: [k]) where
    type Length '[] = 0
    order = 0
    {-# INLINE order #-}
    tList = TLEmpty
    {-# INLINE tList #-}

instance FiniteList xs => FiniteList (x :+ xs :: [k]) where
    type Length (x :+ xs) = Length xs + 1
    order = 1 + order @k @xs
    {-# INLINE order #-}
    tList = TLCons proxy# (tList @k @xs)
    {-# INLINE tList #-}



unsafeEqEvidence :: forall x y . Evidence (x ~ y)
unsafeEqEvidence = unsafeCoerce (Evidence @())
{-# INLINE unsafeEqEvidence #-}

-- | Length of a finite list is known and equal to `order` of the list
inferKnownLength :: forall xs . FiniteList xs => Evidence (KnownDim (Length xs))
inferKnownLength = reifyDim (order @_ @xs) $
      \(_ :: Proxy# n) -> unsafeCoerce (Evidence @(KnownDim n))
{-# INLINE inferKnownLength #-}


-- | Tail of the list is also known list
inferTailFiniteList :: forall xs . FiniteList xs => Evidence (FiniteList (Tail xs))
inferTailFiniteList = case tList @_ @xs of
  TLEmpty    -> error "Tail of FiniteList -- empty type-level list"
  TLCons _ _ -> Evidence
{-# INLINE inferTailFiniteList #-}

-- | Infer that concatenation is also finite
inferConcatFiniteList :: forall as bs
                       . (FiniteList as, FiniteList bs)
                      => Evidence (FiniteList (as ++ bs))
inferConcatFiniteList = case tList @_ @as of
  TLEmpty -> Evidence
  TLCons (_ :: Proxy# a) (_ :: TypeList as') -> case inferConcatFiniteList @as' @bs of
      Evidence -> case unsafeEqEvidence @((a ': as') ++ bs) @(a ': (as' ++ bs)) of
        Evidence -> Evidence
{-# INLINE inferConcatFiniteList #-}


-- | Infer that prefix is also finite
inferPrefixFiniteList :: forall bs asbs
                       . (IsSuffix bs asbs ~ 'True, FiniteList bs, FiniteList asbs)
                      => Evidence (FiniteList (Prefix bs asbs))
inferPrefixFiniteList = reifyDim (order @_ @asbs - order @_ @bs) $
      \(_ :: Proxy# n) -> unsafeCoerce (inferTakeNFiniteList @n @asbs)
{-# INLINE inferPrefixFiniteList #-}

-- | Infer that suffix is also finite
inferSuffixFiniteList :: forall as asbs
                       . (IsPrefix as asbs ~ 'True, FiniteList as, FiniteList asbs)
                      => Evidence (FiniteList (Suffix as asbs))
inferSuffixFiniteList = case tList @_ @as of
  TLEmpty -> Evidence
  TLCons _ (_ :: TypeList as') -> case tList @_ @asbs of
    TLCons _ (_ :: TypeList asbs') -> case unsafeEqEvidence @(IsPrefix as' asbs') @'True
                                  `sumEvs` unsafeEqEvidence @(Suffix as' asbs') @(Suffix as asbs) of
      Evidence -> inferSuffixFiniteList @as' @asbs'
{-# INLINE inferSuffixFiniteList #-}

-- | Make snoc almost as good as cons
inferSnocFiniteList :: forall xs z
                     . FiniteList xs
                    => Evidence (FiniteList (xs +: z))
inferSnocFiniteList = case tList @_ @xs of
  TLEmpty -> Evidence
  TLCons (_ :: Proxy# x) (_ :: TypeList xs') -> case inferSnocFiniteList @xs' @z
                                            `sumEvs` unsafeEqEvidence @(x :+ (xs' +: z)) @(xs +: z) of
    Evidence -> Evidence
{-# INLINE inferSnocFiniteList #-}

-- | Init of the list is also known list
inferInitFiniteList :: forall xs
                     . FiniteList xs
                    => Evidence (FiniteList (Init xs))
inferInitFiniteList = case tList @_ @xs of
  TLEmpty -> error "Init of FiniteList -- empty type-level list"
  TLCons _ TLEmpty -> Evidence
  TLCons _ (TLCons _ _ :: TypeList xs') -> case inferInitFiniteList @xs' of
    Evidence -> Evidence
{-# INLINE inferInitFiniteList #-}

-- | Take KnownDim of the list is also known list
inferTakeNFiniteList :: forall n xs
                      . (KnownDim n, FiniteList xs)
                     => Evidence (FiniteList (Take n xs))
inferTakeNFiniteList = case magic (dimVal' @n) (tList @_ @xs) of
      TLEmpty    -> Evidence
      TLCons _ _ -> Evidence
    where
      magic :: forall ns . Int -> TypeList ns -> TypeList (Take n ns)
      magic _ TLEmpty = TLEmpty
      magic 0 _ = unsafeCoerce TLEmpty
      magic n (TLCons p tl) = unsafeCoerce $ TLCons p (unsafeCoerce $ magic (n-1) tl :: TypeList (Tail ns))
{-# INLINE inferTakeNFiniteList #-}

-- | Drop KnownDim of the list is also known list
inferDropNFiniteList :: forall n xs
                      . (KnownDim n, FiniteList xs)
                     => Evidence (FiniteList (Drop n xs))
inferDropNFiniteList = case magic (dimVal' @n) (tList @_ @xs) of
      TLEmpty    -> Evidence
      TLCons _ _ -> Evidence
    where
      magic :: forall ns . Int -> TypeList ns -> TypeList (Drop n ns)
      magic _ TLEmpty       = TLEmpty
      magic 0 tl            = unsafeCoerce tl
      magic n (TLCons _ tl) = unsafeCoerce $ magic (n-1) tl
{-# INLINE inferDropNFiniteList #-}

-- | Reverse of the list is also known list
inferReverseFiniteList :: forall xs . FiniteList xs => Evidence (FiniteList (Reverse xs))
inferReverseFiniteList = case magic (tList @_ @xs) (unsafeCoerce TLEmpty) of
      TLEmpty    -> Evidence
      TLCons _ _ -> Evidence
    where
      magic :: forall ns . TypeList ns -> TypeList (Reverse ns) -> TypeList (Reverse ns)
      magic TLEmpty xs = xs
      magic (TLCons p sx) TLEmpty = magic (unsafeCoerce sx :: TypeList ns)
                                          (unsafeCoerce (TLCons p TLEmpty) :: TypeList (Reverse ns))
      magic (TLCons p sx) xs@(TLCons _ _) = magic (unsafeCoerce sx :: TypeList ns)
                                                  (unsafeCoerce (TLCons p xs) :: TypeList (Reverse ns))
{-# INLINE inferReverseFiniteList #-}


--------------------------------------------------------------------------------
---- Constructing evidence for our constraints
--------------------------------------------------------------------------------

-- | Various kinds of proofs to make sure we infer list operations and constrains
--   over simple cons ar snoc
type NonEmptyListEvidence xs
  = Evidence (NonEmptyList xs)

-- | Pattern-matching on the constructor of this type
--   brings an evidence that `as ++ bs ~ asbs`
type ConcatEvidence (as :: [k]) (bs :: [k]) (asbs :: [k])
  = Evidence ( asbs ~ Concat as bs
    , as   ~ Prefix bs asbs
    , bs   ~ Suffix as asbs
    , IsSuffix bs asbs ~ 'True
    , IsPrefix as asbs ~ 'True
    )

-- | Pattern-matching on the constructor of this type
--   brings an evidence that the type-level parameter list is finite
type FiniteListEvidence (xs :: [k])
  = Evidence (FiniteList xs)


-- | Various kinds of proofs to make sure we infer list operations and constrains
--   over cons constructor
inferNonEmptyList ::  forall x xs . NonEmptyListEvidence (x ': xs)
inferNonEmptyList = unsafeCoerce (Evidence :: NonEmptyListEvidence '[x])

-- | Any two type-level lists can be concatenated,
--   so we just fool the compiler by unsafeCoercing proxy-like data type.
inferConcat :: forall as bs . ConcatEvidence as bs (as ++ bs)
inferConcat = unsafeCoerce (Evidence :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
{-# INLINE inferConcat #-}


-- | `as` being prefix of `asbs` is enough to infer some concatenation relations
--   so we just fool the compiler by unsafeCoercing proxy-like data type.
inferSuffix :: forall as asbs
             . IsPrefix as asbs ~ 'True
            => ConcatEvidence as (Suffix as asbs) asbs
inferSuffix = unsafeCoerce (Evidence :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
{-# INLINE inferSuffix #-}


-- | `bs` being suffix of `asbs` is enough to infer some concatenation relations
--   so we just fool the compiler by unsafeCoercing proxy-like data type.
inferPrefix :: forall bs asbs
             . IsSuffix bs asbs ~ 'True
            => ConcatEvidence (Prefix bs asbs) bs asbs
inferPrefix = unsafeCoerce (Evidence :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
{-# INLINE inferPrefix #-}


--------------------------------------------------------------------------------
---- Tricks to make some type-level operations injective
--------------------------------------------------------------------------------


-- | A special data type that can have either a single element,
--   or more than two.
--   This feature is not enforced in the type system - this is just a way to make injective Snoc.
data Snocing k = SSingle k | Snocing [k]

type family DoSnoc (xs :: [k]) (z::k) = (ys :: Snocing k) | ys -> xs z where
    DoSnoc '[]       x = 'SSingle x
    DoSnoc (x :+ xs) y = 'Snocing (x :+ GetSnoc (DoSnoc xs y))

type family GetSnoc (xs :: Snocing k) = (ys :: [k]) | ys -> xs where
    GetSnoc ('SSingle x) = '[x]
    GetSnoc ('Snocing (y :+ x :+ xs)) = y :+ x :+ xs

-- | Another data type to make Reverse injective.
data Reversing k = REmpty | Reversing [k]

type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
    Reversed 'REmpty = '[]
    Reversed ('Reversing (x :+ xs)) = x :+ xs

type family DoReverse (as :: [k]) = (rs :: Reversing k) | rs -> as where
    DoReverse '[]  = 'REmpty
    DoReverse (a :+ as) = 'Reversing (Reversed (DoReverse as) +: a)
