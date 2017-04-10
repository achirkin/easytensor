{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- The following extensions are needed for ConcatDim typeclass
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- The following extensions are needed for KnownList typeclass
{-# LANGUAGE ScopedTypeVariables, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides type-level operations on lists.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.List
  ( type (++), Length
  , type (:+), type (+:)
  , Empty, Cons, Snoc, Head
  , Tail, Init, Last, Concat, Reverse, Take, Drop, Suffix, Prefix
  , IsPrefix, IsSuffix
  , ConcatList (..), KnownList (..), TypeList (..)
  ) where

import GHC.TypeLits
import Data.Proxy (Proxy (..))

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
type Snoc (ns :: [k]) (n :: k) = GetSinkList (SinkFirst (n ': ns))


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


type family IsPrefix (as :: [k]) (asbs :: [k]) where
    IsPrefix '[] _ = 'True
    IsPrefix as as = 'True
    IsPrefix (a':as) (a':asbs) = IsPrefix as asbs
    IsPrefix _ _= 'False

type family IsSuffix (as :: [k]) (asbs :: [k]) where
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
      ) => ConcatList (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
        | as bs -> asbs
        , as asbs -> bs
        , bs asbs -> as where
    tlPrefix :: p bs -> q asbs -> Proxy as
    tlSuffix :: p as -> q asbs -> Proxy bs
    tlConcat :: p as -> q bs   -> Proxy asbs

instance ( asbs ~ Concat as bs
         , as   ~ Prefix bs asbs
         , bs   ~ Suffix as asbs
         , IsSuffix bs asbs ~ 'True
         , IsPrefix as asbs ~ 'True
         ) => ConcatList (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where
    tlPrefix _ _ = Proxy
    {-# INLINE tlPrefix #-}
    tlSuffix _ _ = Proxy
    {-# INLINE tlSuffix #-}
    tlConcat _ _ = Proxy
    {-# INLINE tlConcat #-}


-- | Type level list, used together with KnownList typeclass
data TypeList (xs :: [k]) where
    TLEmpty :: TypeList '[]
    TLCons :: Proxy x -> TypeList xs -> TypeList (x ': xs)

class KnownList (xs :: [k]) where
    -- | Length of a type-leve list
    order :: t xs -> Int
    -- | Get type-level constructed list
    tList :: t xs -> TypeList xs

instance KnownList ('[] :: [k]) where
  order _ = 0
  {-# INLINE order #-}
  tList _ = TLEmpty
  {-# INLINE tList #-}

instance KnownList xs => KnownList (x ': xs :: [k]) where
  order _ = order (Proxy :: Proxy xs) + 1
  {-# INLINE order #-}
  tList _ = TLCons Proxy (tList (Proxy :: Proxy xs))
  {-# INLINE tList #-}


----------------------------------------------------------------------------------
---- Tricks to make some type-level operations injective
----------------------------------------------------------------------------------


data SinkList k = SLEmpty | SLSingle k | SLCons k [k]

type family SinkFirst (xs :: [k]) = (ys :: SinkList k) | ys -> xs where
    SinkFirst ('[] :: [k])    = ('SLEmpty :: SinkList k)
    SinkFirst ('[x] :: [k])    = ('SLSingle x :: SinkList k)
    SinkFirst (y ': x ': xs) = 'SLCons x (GetSinkList (SinkFirst (y ': xs)))

type family GetSinkList (xs :: SinkList k) = (ys :: [k]) | ys -> xs where
    GetSinkList ('SLEmpty :: SinkList k) = ('[] :: [k])
    GetSinkList ('SLSingle x) = '[x]
    GetSinkList ('SLCons y (x ': xs)) = y ': x ': xs

data Reversing k = REmpty | Reversing [k]

type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
    Reversed 'REmpty = '[]
    Reversed ('Reversing (x ': xs)) = x ': xs

type family DoReverse (as :: [k]) = (rs :: Reversing k) | rs -> as where
    DoReverse '[]  = 'REmpty
    DoReverse (a ': as) = 'Reversing ((Reversed (DoReverse as)) +: a)



