{-# OPTIONS_HADDOCK hide, prune     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
module Data.Type.List.Internal
  ( Snoc, Init, Last, Reverse
  , StripSuffix, StripPrefix, Concat
    -- * Classes that simplify inference of type equalities
  , SnocList, ReverseList, ConcatList
  , inferStripSuffix, inferStripPrefix, inferConcat
    -- auto-generating instances
  , consInstSnocList, consInstReverseList, consInstConcatList
  , incohInstReverseList
  ) where

import Data.Constraint          (Dict (..))
import Data.Constraint.Deriving
import GHC.Base                 (Type)
import Unsafe.Coerce            (unsafeCoerce)

-- | Extract the last element of a list, which must be non-empty.
type family Last (xs :: [k]) :: k where
    Last '[x]         = x
    Last (_ ': xs)    = Last xs

-- | Extract all but last elements of a list, which must be non-empty.
type family Init (xs :: [k]) :: [k] where
    Init '[x]         = '[]
    Init (x ': xs)    = x ': Init xs

-- | Appending a list on the other side (injective).
type Snoc (xs :: [k]) (x :: k) = RunList (Snoc' xs x)

-- | Reverse elements of a list (injective).
type Reverse (xs :: [k]) = RunList (Reverse' xs)

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
    RunList  'Empty                     = '[]
    RunList ('Single x)                 = '[x]
    RunList ('TwoOrMore (x ': y ': xs)) = x ': y ': xs

type family Snoc' (xs :: [k]) (x :: k) = (ys :: List k) | ys -> k xs x where
    Snoc' '[]       y = 'Single y
    Snoc' (x ': xs) y = 'TwoOrMore (x ': RunList (Snoc' xs y))

type family Reverse' (xs :: [k]) = (ys :: List k) | ys -> k xs where
    Reverse' '[]            = 'Empty
    Reverse' '[x]           = 'Single x
    Reverse' (y ': x ': xs) = 'TwoOrMore (Snoc (RunList (Reverse' (x ': xs))) y)



-- | Represent a decomposition of a list by appending an element to its end.
class
    (bs ~ Snoc as a, as ~ Init bs, a ~ Last bs)
      => SnocList (as :: [k]) (a :: k) (bs :: [k])
            | as a -> bs, bs -> as a, as -> k, a -> a, bs -> k

{-# ANN defineSnocList ClassDict #-}
defineSnocList :: forall (k :: Type) (as :: [k]) (a :: k) (bs :: [k])
                . (bs ~ Snoc as a, as ~ Init bs, a ~ Last bs)
               => Dict (SnocList as a bs)
defineSnocList = defineSnocList

instance SnocList '[] a '[a]
-- instance SnocList as z bs => SnocList (a ': as) z (a ': bs)
{-# ANN consInstSnocList (ToInstance NoOverlap) #-}
consInstSnocList :: forall (k :: Type) (as :: [k]) (z :: k) (bs :: [k]) (a :: k) (b :: k)
                  . SnocList as z (b ': bs)
                 => Dict (SnocList (a ': as) z (a ': b ': bs))
consInstSnocList
  | Dict <- unsafeEqTypes @_ @(a ': b ': bs) @(Snoc (a ': as) z)
  , Dict <- unsafeEqTypes @_ @(a ': as) @(Init (a ': b ': bs))
  , Dict <- unsafeEqTypes @_ @z @(Last (a ': b ': bs))
    = defineSnocList @k @(a ': as) @z @(a ': b ': bs)

-- | Represent two lists that are `Reverse` of each other
class
    (as ~ Reverse bs, bs ~ Reverse as, ReverseList bs as)
      => ReverseList (as :: [k]) (bs :: [k])
            | as -> bs, bs -> as, as -> k, bs -> k

{-# ANN defineReverseList ClassDict #-}
defineReverseList :: forall (k :: Type) (as :: [k]) (bs :: [k])
                   . (as ~ Reverse bs, bs ~ Reverse as, ReverseList bs as)
                  => Dict (ReverseList as bs)
defineReverseList = defineReverseList

instance ReverseList ('[] :: [k]) ('[] :: [k])
-- instance (ReverseList as bs', SnocList bs' a (b ': bs))
--          => ReverseList (a ': as) (b ': bs)
{-# ANN consInstReverseList (ToInstance NoOverlap) #-}
consInstReverseList :: forall (k :: Type)
                              (a :: k) (as :: [k]) (b :: k) (bs :: [k])
                              (bs' :: [k])
                     . (ReverseList as bs', SnocList bs' a (b ': bs))
                    => Dict (ReverseList (a ': as) (b ': bs))
consInstReverseList
  | Dict <- unsafeEqTypes @_ @(a ': as) @(Reverse (b ': bs))
  , Dict <- unsafeEqTypes @_ @(b ': bs) @(Reverse (a ': as))
    = let d1 = f d2
          d2 = g d1

          f :: Dict (ReverseList (b ': bs) (a ': as))
            -> Dict (ReverseList (a ': as) (b ': bs))
          f Dict = defineReverseList @k @(a ': as) @(b ': bs)
          g :: Dict (ReverseList (a ': as) (b ': bs))
            -> Dict (ReverseList (b ': bs) (a ': as))
          g Dict = defineReverseList @k @(b ': bs) @(a ': as)
      in  d1

{-# ANN incohInstReverseList (ToInstance Incoherent) #-}
incohInstReverseList :: forall (k :: Type) (as :: [k]) (bs :: [k])
                      . bs ~ Reverse as
                     => Dict (ReverseList as bs)
incohInstReverseList
  | Dict <- unsafeEqTypes @_ @as @(Reverse bs)
    = let d1 = f d2
          d2 = g d1
          f :: Dict (ReverseList bs as) -> Dict (ReverseList as bs)
          f Dict = defineReverseList @k @as @bs
          g :: Dict (ReverseList as bs) -> Dict (ReverseList bs as)
          g Dict = defineReverseList @k @bs @as
      in  d1



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

-- | Represent a triple of lists forming a relation @(as ++ bs) ~ asbs@
--
--   NB: functional dependency @bs asbs -> as@ does not seem to be possible,
--       because dependency check happens before constraints check and does not
--       take constraints into account.
class
    ( asbs ~ Concat as bs
    , as   ~ StripSuffix bs asbs
    , bs   ~ StripPrefix as asbs
    ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])
            | as bs -> asbs, as asbs -> bs --, bs asbs -> as
            , as -> k, bs -> k, asbs -> k

{-# ANN defineConcatList ClassDict #-}
defineConcatList :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                  . ( asbs ~ Concat as bs
                    , as   ~ StripSuffix bs asbs
                    , bs   ~ StripPrefix as asbs
                    )
                 => Dict (ConcatList as bs asbs)
defineConcatList = defineConcatList

instance {-# INCOHERENT #-} ConcatList as '[] as
instance ConcatList '[] bs bs
-- I have to implement next instance via the deriving plugin to let GHC know
-- that equality constraints are satisfied.
-- instance ConcatList  as bs asbs => ConcatList (a ': as) bs (a ': asbs)
{-# ANN consInstConcatList (ToInstance NoOverlap) #-}
consInstConcatList :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) (a :: k)
                    . ConcatList as bs asbs
                   => Dict (ConcatList (a ': as) bs (a ': asbs))
consInstConcatList
  | Dict <- unsafeEqTypes @_ @(a ': as)   @(StripSuffix bs (a ': asbs))
  , Dict <- unsafeEqTypes @_ @bs          @(StripPrefix (a ': as) (a ': asbs))
  , Dict <- unsafeEqTypes @_ @(a ': asbs) @(Concat (a ': as) bs)
    = defineConcatList @k @(a ': as) @bs @(a ': asbs)

-- | Derive @ConcatList@ given @Concat@
inferConcat :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
             . asbs ~ Concat as bs
            => Dict (ConcatList as bs asbs)
inferConcat
  | Dict <- unsafeEqTypes @_ @as @(StripSuffix bs asbs)
  , Dict <- unsafeEqTypes @_ @bs @(StripPrefix as asbs)
    = defineConcatList

-- | Derive @ConcatList@ given @StripSuffix@
inferStripSuffix :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                  . as ~ StripSuffix bs asbs
                 => Dict (ConcatList as bs asbs)
inferStripSuffix
  | Dict <- unsafeEqTypes @_ @bs @(StripPrefix as asbs)
  , Dict <- unsafeEqTypes @_ @asbs @(Concat as bs)
    = defineConcatList

-- | Derive @ConcatList@ given @StripPrefix@
inferStripPrefix :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                  . bs ~ StripPrefix as asbs
                 => Dict (ConcatList as bs asbs)
inferStripPrefix
  | Dict <- unsafeEqTypes @_ @as @(StripSuffix bs asbs)
  , Dict <- unsafeEqTypes @_ @asbs @(Concat as bs)
    = defineConcatList

unsafeEqTypes :: forall k (a :: k) (b :: k)
               . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce (Dict :: Dict (a ~ a))
