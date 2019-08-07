{-# OPTIONS_HADDOCK hide, prune     #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Type.List.Families
  ( Head, Tail
  , Snoc, Init, Last, Reverse
  , StripSuffix, StripPrefix, Concat
    -- * Internals
  , List (..), RunList, Snoc', Reverse'
  ) where


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
type family Init (xs :: [k]) = (ys :: [k]) | ys -> k where
    Init ('[x] :: [k]) = ('[] :: [k])
    Init (x ': xs)     = x ': Init xs

-- | Appending a list on the other side (injective).
type Snoc (xs :: [k]) (x :: k) = (RunList (Snoc' xs x :: List k) :: [k])

-- | Reverse elements of a list (injective).
type Reverse (xs :: [k]) = (RunList (Reverse' xs :: List k) :: [k])

-- | A helper data type that makes possible injective `Snoc` and `Reverse` families.
--
--   It assures GHC type checker that the `Snoc` operation on a non-empty list
--   yields a list that contains at least two elements.
data List k
  = Empty
  | Single k
  | TwoOrMore [k]
    -- ^ An important invariant: the argument list contains at least two elements.

type family RunList (xs :: List k) = (ys :: [k]) | ys -> k xs where
    RunList ('Empty :: List k)          = ('[] :: [k])
    RunList ('Single x)                 = '[x]
    RunList ('TwoOrMore (x ': y ': xs)) = x ': y ': xs

type family Snoc' (xs :: [k]) (x :: k) = (ys :: List k) | ys -> k xs x where
    Snoc' '[]       y = 'Single y
    Snoc' (x ': xs) y = 'TwoOrMore (x ': RunList (Snoc' xs y))

type family Reverse' (xs :: [k]) = (ys :: List k) | ys -> k xs where
    Reverse' ('[] :: [k])   = ('Empty :: List k)
    Reverse' '[x]           = 'Single x
    Reverse' (y ': x ': xs) = 'TwoOrMore (Snoc (RunList (Reverse' (x ': xs))) y)


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
