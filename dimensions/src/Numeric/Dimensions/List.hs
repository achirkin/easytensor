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
{-# LANGUAGE CPP                       #-}
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
-- * Note for GHC 8.0
-- Due to <https://ghc.haskell.org/trac/ghc/ticket/13538 GHC issue #13538>
-- some complex type families could not be truly kind-polymorphic before GHC 8.2,
-- thus I specialized them to work only on `[Nat]` and `[XNat]`.
--
--------------------------------------------------------------------------------

module Numeric.Dimensions.List
  ( -- * Basic operations
    type (++), type (:+), type (+:)
  , Empty, Cons, Snoc, Head
  , Tail, Init, Last, Concat, Reverse, Take, Drop
    -- * Working with concatenations
  , Suffix, Prefix, IsPrefix, IsSuffix
    -- * Term level functions
  , ConcatList (..), FiniteList (..), TypeList (..)
    -- * Term level inference of type-level functions
  , inferConcat, inferSuffix, inferPrefix, ConcatEvidence, FiniteListEvidence
  , inferKnownLength
  , inferTailFiniteList, inferConcatFiniteList
  , inferPrefixFiniteList, inferSuffixFiniteList
  , inferSnocFiniteList, inferInitFiniteList
  , inferTakeNFiniteList, inferDropNFiniteList, inferReverseFiniteList
  ) where

import           Data.Proxy       (Proxy (..))
import           Data.Type.Equality      ((:~:)(..))
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
    TLCons  :: FiniteList xs => !(Proxy# x) -> !(TypeList xs) -> TypeList (x :+ xs)

instance Show (TypeList xs) where
    show TLEmpty = "TLEmpty"
    show (TLCons _ xs) = "TLCons " ++ show xs

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
unsafeEqEvidence = case (unsafeCoerce Refl :: x :~: y) of Refl -> Evidence
{-# INLINE unsafeEqEvidence #-}

-- | Length of a finite list is known and equal to `order` of the list
inferKnownLength :: forall xs . FiniteList xs => Evidence (KnownDim (Length xs))
inferKnownLength = reifyDim (order @_ @xs) f
  where
    f :: forall n . KnownDim n => Proxy# n -> Evidence (KnownDim (Length xs))
    f _ = unsafeCoerce (Evidence @(KnownDim n))
{-# INLINE inferKnownLength #-}


-- | Tail of the list is also known list
inferTailFiniteList :: forall xs . FiniteList xs => Maybe (Evidence (FiniteList (Tail xs)))
inferTailFiniteList = case tList @_ @xs of
  TLEmpty    -> Nothing
  TLCons _ _ -> Just Evidence
{-# INLINE inferTailFiniteList #-}

-- | Infer that concatenation is also finite
inferConcatFiniteList :: forall as bs
                       . (FiniteList as, FiniteList bs)
                      => Evidence (FiniteList (as ++ bs))
inferConcatFiniteList = case tList @_ @as of
  TLEmpty -> Evidence
  TLCons _ (_ :: TypeList as') -> case inferConcatFiniteList @as' @bs of
      Evidence -> case unsafeEqEvidence @(as ++ bs) @(Head as ': (as' ++ bs)) of
        Evidence -> Evidence
{-# INLINE inferConcatFiniteList #-}


-- | Infer that prefix is also finite
inferPrefixFiniteList :: forall bs asbs
                       . (IsSuffix bs asbs ~ 'True, FiniteList bs, FiniteList asbs)
                      => Evidence (FiniteList (Prefix bs asbs))
inferPrefixFiniteList = reifyDim (order @_ @asbs - order @_ @bs) f
  where
    f :: forall n . KnownDim n => Proxy# n -> Evidence (FiniteList (Prefix bs asbs))
    f _ = unsafeCoerce (inferTakeNFiniteList @n @asbs)
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
  TLCons _ (_ :: TypeList xs') -> case inferSnocFiniteList @xs' @z
                              `sumEvs` unsafeEqEvidence @(Head xs :+ (xs' +: z)) @(xs +: z) of
    Evidence -> Evidence
{-# INLINE inferSnocFiniteList #-}

-- | Init of the list is also known list
inferInitFiniteList :: forall xs
                     . FiniteList xs
                    => Maybe (Evidence (FiniteList (Init xs)))
inferInitFiniteList = case tList @_ @xs of
  TLEmpty -> Nothing
  TLCons _ TLEmpty -> Just Evidence
  TLCons _ (TLCons _ _ :: TypeList xs') -> case inferInitFiniteList @xs' of
    Just Evidence -> Just Evidence
    Nothing -> Nothing
{-# INLINE inferInitFiniteList #-}

-- | Take KnownDim of the list is also known list
inferTakeNFiniteList :: forall n xs
                      . (KnownDim n, FiniteList xs)
                     => Evidence (FiniteList (Take n xs))
inferTakeNFiniteList = magic @n @xs (dimVal' @n) (tList @_ @xs)
    where
      magic :: forall m ns . Int -> TypeList ns -> Evidence (FiniteList (Take m ns))
      magic _ TLEmpty = Evidence
      magic 0 _ = case unsafeEqEvidence @(Take m ns) @'[] of
              Evidence -> Evidence
      magic n (TLCons _ tl) = case unsafeEqEvidence @(Head ns ': Take (m-1) (Tail ns)) @(Take m ns) of
              Evidence -> case magic @(m-1) @(Tail ns) (n-1) tl of
                Evidence -> Evidence
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
inferReverseFiniteList = case magic (tList @_ @xs) TLEmpty of
      TLEmpty    -> Evidence
      TLCons _ _ -> Evidence
    where
      magic :: forall (ns :: [k]) (bs :: [k])
             . FiniteList bs
            => TypeList ns -> TypeList bs -> TypeList (Reverse ns)
      magic TLEmpty xs = unsafeCoerce xs
      magic (TLCons p sx) xs = magic (unsafeCoerce sx :: TypeList ns) (TLCons p xs)
{-# INLINE inferReverseFiniteList #-}


--------------------------------------------------------------------------------
---- Constructing evidence for our constraints
--------------------------------------------------------------------------------

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
#if __GLASGOW_HASKELL__ >= 802
    DoSnoc (x :+ xs :: [k]) (y :: k) = ('Snocing (x :+ GetSnoc (DoSnoc xs y) :: [k]) :: Snocing k)
#else
    DoSnoc (x :+ xs :: [Nat]) (y :: Nat) = ('Snocing (x :+ GetSnoc (DoSnoc xs y) :: [Nat]) :: Snocing Nat)
    DoSnoc (x :+ xs :: [XNat]) (y :: XNat) = ('Snocing (x :+ GetSnoc (DoSnoc xs y) :: [XNat]) :: Snocing XNat)
#endif

type family GetSnoc (xs :: Snocing k) = (ys :: [k]) | ys -> xs where
    GetSnoc ('SSingle x) = '[x]
#if __GLASGOW_HASKELL__ >= 802
    GetSnoc ('Snocing (y :+ x :+ xs)) = y :+ x :+ xs
#else
    GetSnoc ('Snocing (y :+ x :+ xs) :: Snocing Nat) = (y :+ x :+ xs :: [Nat])
    GetSnoc ('Snocing (y :+ x :+ xs) :: Snocing XNat) = (y :+ x :+ xs :: [XNat])
#endif

-- | Another data type to make Reverse injective.
data Reversing k = REmpty | Reversing [k]

type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
    Reversed 'REmpty = '[]
#if __GLASGOW_HASKELL__ >= 802
    Reversed ('Reversing (x :+ xs)) = x :+ xs
#else
    Reversed ('Reversing (x :+ xs) :: Reversing Nat) = (x :+ xs :: [Nat])
    Reversed ('Reversing (x :+ xs) :: Reversing XNat) = (x :+ xs :: [XNat])
#endif


type family DoReverse (as :: [k]) = (rs :: Reversing k) | rs -> as where
    DoReverse '[]  = 'REmpty
#if __GLASGOW_HASKELL__ >= 802
    DoReverse (a :+ as) = 'Reversing (Reversed (DoReverse as) +: a)
#else
    DoReverse (a :+ as :: [Nat]) = ('Reversing (Reversed (DoReverse as) +: a :: [Nat]) :: Reversing Nat)
    DoReverse (a :+ as :: [XNat]) = ('Reversing (Reversed (DoReverse as) +: a :: [XNat]) :: Reversing XNat)
#endif
