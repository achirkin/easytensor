{-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
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
  , inferConcat, inferSuffix, inferPrefix, ConcatEvidence (..), FiniteListEvidence (..)
  , NonEmptyListEvidence (..), inferNonEmptyList
  ) where

import           Data.Proxy         (Proxy (..))
import           Data.Type.Equality ((:~:)(..))
import           GHC.TypeLits       (Nat, KnownNat, type(-), type(+), natVal
                                    ,TypeError, ErrorMessage(..))
import           GHC.Types          (Type)
import           Unsafe.Coerce      (unsafeCoerce)

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
    TLCons :: FiniteList xs => Proxy x -> TypeList xs -> TypeList (x :+ xs)

-- | Type-level list that is known to be finite.
--   Basically, provides means to get its length and term-level rep (via TypeList)
class KnownNat (Length xs) => FiniteList (xs :: [k]) where
    -- | Length of a type-level list at type level
    type Length xs :: Nat
    -- | Length of a type-level list at term level
    order :: t xs -> Int
    -- | Get type-level constructed list
    tList :: t xs -> TypeList xs
    -- | Infer that concatenation is also finite
    inferConcatFiniteList :: forall (bs :: [k]) (p :: [k] -> Type) (q :: [k] -> Type)
                          . FiniteList bs
                         => p xs
                         -> q bs
                         -> FiniteListEvidence (xs ++ bs)
    -- | Infer that prefix is also finite
    inferPrefixFiniteList :: (IsSuffix bs xs ~ 'True, FiniteList bs)
                         => p bs
                         -> q xs
                         -> FiniteListEvidence (Prefix bs xs)
    -- | Infer that suffix is also finite
    inferSuffixFiniteList :: (IsPrefix as xs ~ 'True, FiniteList as)
                         => p as
                         -> q xs
                         -> FiniteListEvidence (Suffix as xs)
    -- | Make snoc almost as good as cons
    inferSnocFiniteList :: p xs -> q z -> FiniteListEvidence (xs +: z)
    -- | Init of the list is also known list
    inferInitFiniteList :: p xs -> FiniteListEvidence (Init xs)
    -- | Tail of the list is also known list
    inferTailFiniteList :: p xs -> FiniteListEvidence (Tail xs)
    -- | Take KnownNat of the list is also known list
    inferTakeNFiniteList :: KnownNat n => p n -> q xs -> FiniteListEvidence (Take n xs)
    -- | Drop KnownNat of the list is also known list
    inferDropNFiniteList :: KnownNat n => p n -> q xs -> FiniteListEvidence (Drop n xs)
    -- | Reverse of the list is also known list
    inferReverseFiniteList :: q xs -> FiniteListEvidence (Reverse xs)



instance FiniteList ('[] :: [k]) where
    type Length '[] = 0
    order _ = 0
    {-# INLINE order #-}
    tList _ = TLEmpty
    {-# INLINE tList #-}
    inferConcatFiniteList _ (_ :: q bs) = FiniteListEvidence :: FiniteListEvidence bs
    {-# INLINE inferConcatFiniteList #-}
    inferPrefixFiniteList (_ :: p bs) _
      | Refl <- unsafeCoerce Refl :: bs :~: '[] = FiniteListEvidence
    {-# INLINE inferPrefixFiniteList #-}
    inferSuffixFiniteList (_ :: p as) _
      | Refl <- unsafeCoerce Refl :: as :~: '[] = FiniteListEvidence
    {-# INLINE inferSuffixFiniteList #-}
    inferSnocFiniteList _ _ = FiniteListEvidence
    {-# INLINE inferSnocFiniteList #-}
    inferInitFiniteList _ = error "Init -- empty type-level list"
    {-# INLINE inferInitFiniteList #-}
    inferTailFiniteList _ = error "Tail -- empty type-level list"
    {-# INLINE inferTailFiniteList #-}
    inferTakeNFiniteList _ _ = FiniteListEvidence
    {-# INLINE inferTakeNFiniteList #-}
    inferDropNFiniteList _ _ = FiniteListEvidence
    {-# INLINE inferDropNFiniteList #-}
    inferReverseFiniteList _ = FiniteListEvidence
    {-# INLINE inferReverseFiniteList #-}

instance FiniteList xs => FiniteList (x :+ xs :: [k]) where
    type Length (x :+ xs) = Length xs + 1
    order _ = fromInteger (natVal (Proxy @(Length (x :+ xs))))
    {-# INLINE order #-}
    tList _ = TLCons Proxy (tList (Proxy :: Proxy xs))
    {-# INLINE tList #-}
    inferConcatFiniteList _ (pbs :: p bs)
      | FiniteListEvidence  <- inferConcatFiniteList (Proxy @xs) pbs
      , Refl <- unsafeCoerce Refl :: x :+ (xs ++ bs) :~: ((x :+ xs) ++ bs)
      = FiniteListEvidence :: FiniteListEvidence (x :+ (xs ++ bs))
    {-# INLINE inferConcatFiniteList #-}
    inferPrefixFiniteList (_ :: p bs) xs
      | Refl <- unsafeCoerce Refl :: Prefix bs (x :+ xs) :~: Take (Length (x :+ xs) - Length bs) (x :+ xs)
      = inferTakeNFiniteList (Proxy @(Length (x :+ xs) - Length bs)) xs
    {-# INLINE inferPrefixFiniteList #-}
    inferSuffixFiniteList (pas :: p as) _
      | TLEmpty <- tList pas
        = FiniteListEvidence
      | TLCons _ (pas' :: TypeList as') <- tList pas
      , Refl <- unsafeCoerce Refl :: IsPrefix as' xs :~: 'True
      , Refl <- unsafeCoerce Refl :: Suffix as' xs :~: Suffix as (x :+ xs)
      , FiniteListEvidence <- inferSuffixFiniteList pas' (Proxy @xs)
        = FiniteListEvidence :: FiniteListEvidence (Suffix as' xs)
      | otherwise = error "inferSuffixFiniteList: TypeList failed to pattern match"
    {-# INLINE inferSuffixFiniteList #-}
    inferSnocFiniteList _ (q :: q z)
      | FiniteListEvidence <- inferSnocFiniteList (Proxy @xs) q
      , Refl <- unsafeCoerce Refl :: (x :+ (xs +: z)) :~: ((x :+ xs) +: z)
      = FiniteListEvidence :: FiniteListEvidence (x :+ (xs +: z))
    {-# INLINE inferSnocFiniteList #-}
    inferInitFiniteList _ = case tList (Proxy @xs) of
        TLEmpty -> FiniteListEvidence
        TLCons _ _ -> case inferInitFiniteList (Proxy @xs) of
          FiniteListEvidence -> FiniteListEvidence :: FiniteListEvidence (x :+ Init xs)
    {-# INLINE inferInitFiniteList #-}
    inferTailFiniteList _ = FiniteListEvidence
    {-# INLINE inferTailFiniteList #-}
    inferTakeNFiniteList (pn :: p n) _
      | 0 <- natVal pn
      , Refl <- unsafeCoerce Refl :: Take n (x :+ xs) :~: '[]
        = FiniteListEvidence :: FiniteListEvidence '[]
      | otherwise
      , Refl <- unsafeCoerce Refl :: Take n (x :+ xs) :~: (x :+ Take (n-1) xs)
      , FiniteListEvidence <- inferTakeNFiniteList (Proxy @(n-1)) (Proxy @xs)
        = FiniteListEvidence :: FiniteListEvidence (x :+ Take (n-1) xs)
    {-# INLINE inferTakeNFiniteList #-}
    inferDropNFiniteList (pn :: p n) _
      | 0 <- natVal pn
      , Refl <- unsafeCoerce Refl :: Drop n (x :+ xs) :~: (x :+ xs)
        = FiniteListEvidence :: FiniteListEvidence (x :+ xs)
      | otherwise
      , Refl <- unsafeCoerce Refl :: Drop n (x :+ xs) :~: Drop (n-1) xs
      , FiniteListEvidence <- inferDropNFiniteList (Proxy @(n-1)) (Proxy @xs)
        = FiniteListEvidence :: FiniteListEvidence (Drop (n-1) xs)
    {-# INLINE inferDropNFiniteList #-}
    inferReverseFiniteList _
      | FiniteListEvidence <- inferReverseFiniteList (Proxy @xs)
      , FiniteListEvidence <- inferSnocFiniteList (Proxy @(Reverse xs)) (Proxy @x)
      , Refl <- unsafeCoerce Refl :: Reverse (x :+ xs) :~: (Reverse xs +: x)
       = FiniteListEvidence :: FiniteListEvidence (Reverse xs +: x)
    {-# INLINE inferReverseFiniteList #-}


--------------------------------------------------------------------------------
---- Constructing evidence for our constraints
--------------------------------------------------------------------------------

-- | Various kinds of proofs to make sure we infer list operations and constrains
--   over simple cons ar snoc
data NonEmptyListEvidence xs
  = NonEmptyList xs => NonEmptyListEvidence

-- | Pattern-matching on the constructor of this type
--   brings an evidence that `as ++ bs ~ asbs`
data ConcatEvidence (as :: [k]) (bs :: [k]) (asbs :: [k])
  = ( asbs ~ Concat as bs
    , as   ~ Prefix bs asbs
    , bs   ~ Suffix as asbs
    , IsSuffix bs asbs ~ 'True
    , IsPrefix as asbs ~ 'True
    ) => ConcatEvidence

-- | Pattern-matching on the constructor of this type
--   brings an evidence that the type-level parameter list is finite
data FiniteListEvidence (xs :: [k])
  = FiniteList xs => FiniteListEvidence


-- | Various kinds of proofs to make sure we infer list operations and constrains
--   over cons constructor
inferNonEmptyList ::  forall x xs . NonEmptyListEvidence (x ': xs)
inferNonEmptyList = unsafeCoerce (NonEmptyListEvidence @'[x])

-- | Any two type-level lists can be concatenated,
--   so we just fool the compiler by unsafeCoercing proxy-like data type.
inferConcat :: p as -> q bs -> ConcatEvidence as bs (as ++ bs)
inferConcat _ _ = unsafeCoerce (ConcatEvidence :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
{-# INLINE inferConcat #-}


-- | `as` being prefix of `asbs` is enough to infer some concatenation relations
--   so we just fool the compiler by unsafeCoercing proxy-like data type.
inferSuffix :: IsPrefix as asbs ~ 'True
            => p as -> q asbs -> ConcatEvidence as (Suffix as asbs) asbs
inferSuffix _ _ = unsafeCoerce (ConcatEvidence :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
{-# INLINE inferSuffix #-}


-- | `bs` being suffix of `asbs` is enough to infer some concatenation relations
--   so we just fool the compiler by unsafeCoercing proxy-like data type.
inferPrefix :: IsSuffix bs asbs ~ 'True
            => p bs -> q asbs -> ConcatEvidence (Prefix bs asbs) bs asbs
inferPrefix _ _ = unsafeCoerce (ConcatEvidence :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
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
