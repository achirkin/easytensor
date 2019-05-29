{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
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
  , All, Map
    -- * Concatenation and its evidence
  , ConcatList, evStripSuffix, evStripPrefix, evConcat
    -- * Data.Typeable
  , inferTypeableCons
  ) where

import Data.Constraint         ((:-) (..), Constraint, Dict (..))
import Data.Type.List.Internal (Snoc)
import GHC.Base                (Type)
import GHC.TypeLits
import Type.Reflection
import Unsafe.Coerce           (unsafeCoerce)

-- | Empty list, same as @'[]@.
type Empty = '[]

-- | Appending a list, represents an @Op@ counterpart of @(':)@.
type Cons (a :: k) (as :: [k])
    = a ': as

-- | Extract the first element of a list, which must be non-empty.
type family Head (xs :: [k]) :: k where
    Head ('[] :: [k]) = TypeError ( ListError k "Head: empty type-level list." )
    Head (x ': _)     = x

-- | Extract the elements after the head of a list, which must be non-empty.
type family Tail (xs :: [k]) :: [k] where
    Tail ('[] :: [k]) = TypeError ( ListError k "Tail: empty type-level list." )
    Tail (_ ': xs)    = xs

-- | Extract the last element of a list, which must be non-empty.
type family Last (xs :: [k]) :: k where
    Last ('[] :: [k]) = TypeError ( ListError k "Last: empty type-level list." )
    Last '[x]         = x
    Last (_ ': xs)    = Last xs

-- | Extract all but last elements of a list, which must be non-empty.
type family Init (xs :: [k]) :: [k] where
    Init ('[] :: [k]) = TypeError ( ListError k "Init: empty type-level list." )
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
    Concat '[]        bs       = bs
    Concat  as       '[]       = as
    Concat (a ': as)  bs       = a ': Concat as bs

-- | Remove prefix @as@ from a list @asbs@ if @as@ is a prefix; fail otherwise.
type family StripPrefix (as :: [k]) (asbs :: [k]) :: [k] where
    StripPrefix  as        as         = '[]
    StripPrefix '[]        asbs       = asbs
    StripPrefix (a ': as) (a ': asbs) = StripPrefix as asbs
    StripPrefix (a ': as) (b ': asbs)
      = TypeError
        ( 'Text "StripPrefix: the first argument is not a prefix of the second."
        ':$$:
          'Text "Failed prefix: " ':<>: 'ShowType (a ': as)
        ':$$:
          'Text "Second argument: " ':<>: 'ShowType (b ': asbs)
        )
    StripPrefix (a ': as) '[]
      = TypeError
        ( 'Text "StripPrefix: the first argument is longer than the second."
        ':$$:
          'Text "Failed prefix: " ':<>: 'ShowType (a ': as)
        ':$$:
          'Text "The reduced second argument is empty."
        )

-- | Remove suffix @bs@ from a list @asbs@ if @bs@ is a suffix; fail otherwise.
type family StripSuffix (bs :: [k]) (asbs :: [k]) :: [k] where
    StripSuffix  bs        bs         = '[]
    StripSuffix '[]        asbs       = asbs
    StripSuffix (a ': bs) (b ': bs)
      = TypeError
        ( 'Text "StripSuffix: the first argument is not a suffix of the second."
        ':$$:
          'Text "Failed match: "
            ':<>: 'ShowType ((a ': bs) ~ (b ': bs))
        )
    StripSuffix (b ': bs) '[]
      = TypeError
        ( 'Text "StripSuffix: the first argument is longer than the second."
        ':$$:
          'Text "Failed suffix: " ':<>: 'ShowType (b ': bs)
        ':$$:
          'Text "The reduced second argument is empty."
        )
    StripSuffix  bs       (a ': asbs) = a ': StripSuffix bs asbs

-- | Returns the elements of a list in reverse order.
type family Reverse (xs :: [k]) :: [k] where
    -- Note: the goal is not to write a fast implementation,
    --       but make it easier for the type checker to simplify things.
    --       This is only going to be executed during compile time,
    --       so there is no real performance impact.
    Reverse '[] = '[]
    Reverse (x ': xs) = Snoc (Reverse xs) x

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
    All _ '[] = ()
    All f (x ': xs) = (f x, All f xs)

-- | Map a functor over the elements of a type list.
type family Map (f :: a -> b) (xs :: [a]) :: [b] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

type ListError k t
    = 'Text t ':$$:
    ( 'Text "Type-level error occured when operating on a list of kind "
      ':<>: 'ShowType [k] ':<>: 'Text "."
    )

-- | Represent a triple of lists forming a relation @(as ++ bs) ~ asbs@
type ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k]) =
    ( asbs ~ Concat as bs
    , as   ~ StripSuffix bs asbs
    , bs   ~ StripPrefix as asbs
    )

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
