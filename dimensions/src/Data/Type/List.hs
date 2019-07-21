{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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
  , All, Map, Elem
    -- * Classes that simplify inference of type equalities
  , SnocList, ReverseList, ConcatList
  , evStripSuffix, evStripPrefix, evConcat
    -- * Data.Typeable
  , inferTypeableCons
  ) where

import Data.Constraint         ((:-) (..), Constraint, Dict (..))
import Data.Type.List.Internal (Snoc)
import Data.Type.Lits
import GHC.Base                (Type)
import Type.Reflection
import Unsafe.Coerce           (unsafeCoerce)

-- | Empty list, same as @'[]@.
type Empty = '[]

-- | Appending a list, represents an @Op@ counterpart of @(':)@.
type Cons (a :: k) (as :: [k])
    = a ': as

-- | Extract the first element of a list, which must be non-empty.
type family Head (xs :: [k]) :: k where
    Head (x ': _)     = x

-- | Extract the elements after the head of a list, which must be non-empty.
type family Tail (xs :: [k]) :: [k] where
    Tail (_ ': xs)    = xs

-- | Extract the last element of a list, which must be non-empty.
type family Last (xs :: [k]) :: k where
    Last '[x]         = x
    Last (_ ': xs)    = Last xs

-- | Extract all but last elements of a list, which must be non-empty.
type family Init (xs :: [k]) :: [k] where
    Init '[x]         = '[]
    Init (x ': xs)    = x ': Init xs

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

-- | Append two lists.
type family Concat (as :: [k]) (bs :: [k]) :: [k] where
    Concat  as       '[]       = as -- "incoherent instance"
    Concat '[]        bs       = bs
    Concat (a ': as)  bs       = a ': Concat as bs

-- | Remove prefix @as@ from a list @asbs@ if @as@ is a prefix; fail otherwise.
type family StripPrefix (as :: [k]) (asbs :: [k]) :: [k] where
    StripPrefix  as        as         = '[] -- "incoherent instance"
    StripPrefix '[]        bs         = bs
    StripPrefix (a ': as) (a ': asbs) = StripPrefix as asbs

-- | Remove suffix @bs@ from a list @asbs@ if @bs@ is a suffix; fail otherwise.
type family StripSuffix (bs :: [k]) (asbs :: [k]) :: [k] where
    StripSuffix '[]        as         = as -- "incoherent instance"
    StripSuffix  bs        bs         = '[]
    StripSuffix  bs       (a ': asbs) = a ': StripSuffix bs asbs

-- | Returns the elements of a list in reverse order.
type Reverse (xs :: [k]) = Reverse' xs ('[] :: [k])

-- | A helper function for `Reverse`.
type family Reverse' (xs :: [k]) (ts :: [k]) :: [k] where
    Reverse' (x ': xs) ts = Reverse' xs (x ': ts)
    Reverse' '[]       ts = ts

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

-- | Check if an item is a member of a list.
type family Elem (x :: k) (xs :: [k]) :: Constraint where
    Elem x (x ': xs) = ()
    Elem x (_ ': xs) = Elem x xs

-- | Represent a decomposition of a list by appending an element to its end.
class
    (bs ~ Snoc as a, as ~ Init bs, a ~ Last bs)
      => SnocList (as :: [k]) (a :: k) (bs :: [k])
            | as a -> bs, bs -> as a, as -> k, a -> a, bs -> k

instance SnocList '[] a '[a]
-- instance SnocList as z bs => SnocList (a ': as) z (a ': bs)

-- | Represent two lists that are `Reverse` of each other
class
    (as ~ Reverse bs, bs ~ Reverse as)
      => ReverseList (as :: [k]) (bs :: [k])
            | as -> bs, bs -> as, as -> k, bs -> k

-- | Represent a triple of lists forming a relation @(as ++ bs) ~ asbs@
class
    ( asbs ~ Concat as bs
    , as   ~ StripSuffix bs asbs
    , bs   ~ StripPrefix as asbs
    ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])
            | as bs -> asbs, as asbs -> bs, bs asbs -> as
            , as -> k, bs -> k, asbs -> k

instance {-# INCOHERENT #-} ConcatList as '[] as
instance ConcatList '[] bs bs
-- instance ConcatList as bs asbs => ConcatList (a ': as) bs (a ': asbs)

-- | Derive @ConcatList@ given @Concat@
evConcat :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
          . (asbs ~ Concat as bs) :- ConcatList as bs asbs
evConcat = Sub $ unsafeCoerce
  ( Dict :: Dict
    ( asbs ~ Concat as bs
    , as   ~ as
    , bs   ~ bs
    )
  )

-- | Derive @ConcatList@ given @StripSuffix@
evStripSuffix :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
               . (as ~ StripSuffix bs asbs) :- ConcatList as bs asbs
evStripSuffix = Sub $ unsafeCoerce
  ( Dict :: Dict
    ( asbs ~ asbs
    , as   ~ StripSuffix bs asbs
    , bs   ~ bs
    )
  )

-- | Derive @ConcatList@ given @StripPrefix@
evStripPrefix :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
               . (bs ~ StripPrefix as asbs) :- ConcatList as bs asbs
evStripPrefix = Sub $ unsafeCoerce
  ( Dict :: Dict
    ( asbs ~ asbs
    , as   ~ as
    , bs   ~ StripPrefix as asbs
    )
  )

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
