{-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- The following extensions are needed for ConcatDim typeclass
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- The following extensions are needed for KnownList typeclass
{-# LANGUAGE ScopedTypeVariables, GADTs #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides type-level operations on lists.
--
--------------------------------------------------------------------------------

module Numeric.Dimensions.List
  ( type (++), Length
  , type (:+), type (+:)
  , Empty, Cons, Snoc, Head
  , Tail, Init, Last, Concat, Reverse, Take, Drop, Suffix, Prefix
  , IsPrefix, IsSuffix
  , ConcatList (..), KnownList (..), TypeList (..)
  , inferConcat, inferSuffix, inferPrefix, ConcatEvidence, KnownListEvidence
  ) where

import GHC.TypeLits
import GHC.Types (Type)
import Data.Proxy (Proxy (..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Equality

-- | Synonym for a type-level cons
--     (injective, since this is just a synonym for the list constructor)
type (a :: k) :+ (as :: [k]) = a ': as
infixr 5 :+
-- | Prefix-style synonym for cons
type Cons (n :: k) (ns :: [k]) = n ': ns

-- | Synonym for a type-level snoc (injective!)
type (ns :: [k]) +: (n :: k) = Snoc ns n
infixl 5 +:
-- | Prefix-style synonym for snoc
type Snoc (ns :: [k]) (n :: k) = GetSnoc (DoSnoc ns n)


-- | List concatenation
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    (++) '[] bs = bs
    (++) as '[] = as
    (++) (a ': as) bs = a ': (as ++ bs)
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
    Take n (x ': xs) = x ': Take (n-1) xs


type family Drop (n::Nat) (xs :: [k]) :: [k] where
    Drop _ '[] = '[]
    Drop 0 xs = xs
    Drop n (x ': xs) = Drop (n-1) xs

type family Suffix (as :: [k]) (asbs :: [k]) :: [k] where
    Suffix '[] bs = bs
    Suffix as as = '[]
    Suffix (_ ': as) (_' : asbs) = Suffix as asbs

type family Prefix (bs :: [k]) (asbs :: [k]) :: [k] where
    Prefix '[] as = as
    Prefix bs bs = '[]
    Prefix bs asbs = Take (Length asbs - Length bs) asbs


type family IsPrefix (as :: [k]) (asbs :: [k]) :: Bool where
    IsPrefix '[] _ = 'True
    IsPrefix (a ': as) (a ': asbs) = IsPrefix as asbs
    IsPrefix as as = 'True
    IsPrefix _ _= 'False

type family IsSuffix (as :: [k]) (asbs :: [k]) :: Bool where
    IsSuffix '[] _ = 'True
    IsSuffix bs bs = 'True
    IsSuffix bs (_ ': sbs) = IsSuffix bs sbs
    IsSuffix _ _ = 'False


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

type family Init (xs :: [k]) :: [k] where
    Init '[x] = '[]
    Init (x ': xs) = x ': Init xs
    Init '[]       = TypeError ( 'Text
      "Init -- empty type-level list."
     )

type family Last (xs :: [k]) :: k where
    Last '[x] = x
    Last (x ': xs) = Last xs
    Last '[]       = TypeError ( 'Text
      "Last -- empty type-level list."
     )


-- | I unroll list inside Length defenition
--   in order to have a little bit more chances for inferring KnownNat
type family Length (as :: [k]) :: Nat where
    Length '[] = 0
    Length '[_] = 1
    Length '[_,_] = 2
    Length '[_,_,_] = 3
    Length '[_,_,_,_] = 4
    Length '[_,_,_,_,_] = 5
    Length '[_,_,_,_,_,_] = 6
    Length '[_,_,_,_,_,_,_] = 7
    Length '[_,_,_,_,_,_,_,_] = 8
    Length '[_,_,_,_,_,_,_,_,_] = 9
    Length (a ': as) = 1 + Length as



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


-- | Type level list, used together with KnownList typeclass
data TypeList (xs :: [k]) where
    TLEmpty :: TypeList '[]
    TLCons :: KnownList xs => Proxy x -> TypeList xs -> TypeList (x ': xs)

class KnownList (xs :: [k]) where
    -- | Length of a type-leve list
    order :: t xs -> Int
    -- | Get type-level constructed list
    tList :: t xs -> TypeList xs
    -- | Infer that concatenation is also finite
    inferConcatKnownList :: forall (bs :: [k]) (p :: [k] -> Type) (q :: [k] -> Type)
                          . KnownList bs
                         => p xs
                         -> q bs
                         -> KnownListEvidence (xs ++ bs)
    -- | Infer that prefix is also finite
    inferPrefixKnownList :: (IsSuffix bs xs ~ 'True, KnownList bs)
                         => p bs
                         -> q xs
                         -> KnownListEvidence (Prefix bs xs)
    -- | Infer that suffix is also finite
    inferSuffixKnownList :: (IsPrefix as xs ~ 'True, KnownList as)
                         => p as
                         -> q xs
                         -> KnownListEvidence (Suffix as xs)
    -- | Make snoc almost as good as cons
    inferSnocKnownList :: p xs -> q z -> KnownListEvidence (xs +: z)
    -- | Init of the list is also known list
    inferInitKnownList :: p (x ': xs) -> KnownListEvidence (Init (x ': xs))
    -- | Tail of the list is also known list
    inferTailKnownList :: p xs -> KnownListEvidence (Tail xs)
    inferTakeNKnownList :: KnownNat n => p n -> q xs -> KnownListEvidence (Take n xs)



instance KnownList ('[] :: [k]) where
  order _ = 0
  {-# INLINE order #-}
  tList _ = TLEmpty
  {-# INLINE tList #-}
  inferConcatKnownList _ (_ :: q bs) = KnownListEvidence :: KnownListEvidence bs
  {-# INLINE inferConcatKnownList #-}
  inferPrefixKnownList (_ :: p bs) _
    | Refl <- unsafeCoerce Refl :: bs :~: '[] = KnownListEvidence
  {-# INLINE inferPrefixKnownList #-}
  inferSuffixKnownList (_ :: p as) _
    | Refl <- unsafeCoerce Refl :: as :~: '[] = KnownListEvidence
  {-# INLINE inferSuffixKnownList #-}
  inferSnocKnownList _ _ = KnownListEvidence
  {-# INLINE inferSnocKnownList #-}
  inferInitKnownList _ = KnownListEvidence
  {-# INLINE inferInitKnownList #-}
  inferTailKnownList _ = error "Tail -- empty type-level list"
  {-# INLINE inferTailKnownList #-}
  inferTakeNKnownList _ _ = KnownListEvidence
  {-# INLINE inferTakeNKnownList #-}

instance KnownList xs => KnownList (x ': xs :: [k]) where
  order _ = order (Proxy :: Proxy xs) + 1
  {-# INLINE order #-}
  tList _ = TLCons Proxy (tList (Proxy :: Proxy xs))
  {-# INLINE tList #-}
  inferConcatKnownList _ (pbs :: p bs)
    | KnownListEvidence  <- inferConcatKnownList (Proxy @xs) pbs
    , Refl <- unsafeCoerce Refl :: (x ': (xs ++ bs)) :~: ((x ': xs) ++ bs)
    = KnownListEvidence :: KnownListEvidence (x ': (xs ++ bs))
  {-# INLINE inferConcatKnownList #-}
  -- inferPrefixKnownList (pbs :: p bs) _
  --   | TLEmpty <- tList pbs
  --   = KnownListEvidence :: KnownListEvidence (x ': (xs ++ bs))
  -- {-# INLINE inferPrefixKnownList #-}
  inferSuffixKnownList (pas :: p as) _
    | TLEmpty <- tList pas
      = KnownListEvidence
    | TLCons _ (pas' :: TypeList as') <- tList pas
    , Refl <- unsafeCoerce Refl :: IsPrefix as' xs :~: 'True
    , Refl <- unsafeCoerce Refl :: Suffix as' xs :~: Suffix as (x ': xs)
    , KnownListEvidence <- inferSuffixKnownList pas' (Proxy @xs)
      = KnownListEvidence :: KnownListEvidence (Suffix as' xs)
    | otherwise = error "inferSuffixKnownList: TypeList failed to pattern match"
  {-# INLINE inferSuffixKnownList #-}
  inferSnocKnownList _ (q :: q z)
    | KnownListEvidence <- inferSnocKnownList (Proxy @xs) q
    , Refl <- unsafeCoerce Refl :: (x ': (xs +: z)) :~: ((x ': xs) +: z)
    = KnownListEvidence :: KnownListEvidence (x ': (xs +: z))
  {-# INLINE inferSnocKnownList #-}
  inferInitKnownList (_ :: p (x0 ': x ': xs))
    | KnownListEvidence <- inferInitKnownList (Proxy @(x ': xs))
    = KnownListEvidence :: KnownListEvidence (x0 ': Init (x ': xs))
  {-# INLINE inferInitKnownList #-}
  inferTailKnownList _ = KnownListEvidence
  {-# INLINE inferTailKnownList #-}
  inferTakeNKnownList (pn :: p n) _
    | 0 <- natVal pn
    , Refl <- unsafeCoerce Refl :: Take n (x ': xs) :~: (x ': xs)
      = KnownListEvidence :: KnownListEvidence (x ': xs)
    | otherwise
    , Refl <- unsafeCoerce Refl :: Take n (x ': xs) :~: (x ': Take (n-1) xs)
    , KnownListEvidence <- inferTakeNKnownList (Proxy @(n-1)) (Proxy @xs)
      = KnownListEvidence :: KnownListEvidence (x ': Take (n-1) xs)
  {-# INLINE inferTakeNKnownList #-}



--------------------------------------------------------------------------------
---- Constructing evidence for our constraints
--------------------------------------------------------------------------------

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
data KnownListEvidence (xs :: [k])
  = KnownList xs => KnownListEvidence


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
    DoSnoc (x ': xs) y = 'Snocing (x ': (GetSnoc (DoSnoc xs y)))

type family GetSnoc (xs :: Snocing k) = (ys :: [k]) | ys -> xs where
    GetSnoc ('SSingle x) = '[x]
    GetSnoc ('Snocing (y ': x ': xs)) = y ': x ': xs

-- | Another data type to make Reverse injective.
data Reversing k = REmpty | Reversing [k]

type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
    Reversed 'REmpty = '[]
    Reversed ('Reversing (x ': xs)) = x ': xs

type family DoReverse (as :: [k]) = (rs :: Reversing k) | rs -> as where
    DoReverse '[]  = 'REmpty
    DoReverse (a ': as) = 'Reversing ((Reversed (DoReverse as)) +: a)
