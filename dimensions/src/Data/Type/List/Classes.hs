{-# OPTIONS_HADDOCK hide, prune     #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
module Data.Type.List.Classes
  ( -- * Classes that simplify inference of type equalities
    SnocList, ReverseList, ConcatList
  , inferStripSuffix, inferStripPrefix, inferConcat
    -- auto-generating instances
  , nilInstSnocList, consInstSnocList, incohInstSnocList
  , consInstReverseList, incohInstReverseList
  , nilInstConcatList, consInstConcatList, incohInstConcatList
  ) where

import Data.Constraint          (Dict (..))
import Data.Constraint.Bare
import Data.Constraint.Deriving
import Data.Kind
import Data.Type.Equality
import Unsafe.Coerce            (unsafeCoerce)

import Data.Type.List.Families
import Data.Type.List.Internal


-- | Represent a decomposition of a list by appending an element to its end.
class
    ( bs ~ Snoc as a, as ~ Init bs, a ~ Last bs
    , SnocListCtx as a bs, ConcatList as '[a] bs)
      => SnocList (as :: [k]) (a :: k) (bs :: [k])
            | as a -> bs, bs -> as a, as -> k, a -> a, bs -> k where

-- | Represent two lists that are `Reverse` of each other
class
    ( as ~ Reverse bs, bs ~ Reverse as, ReverseList bs as
    , ReverseListCtx as bs)
      => ReverseList (as :: [k]) (bs :: [k])
            | as -> bs, bs -> as, as -> k, bs -> k

-- | Represent a triple of lists forming a relation @(as ++ bs) ~ asbs@
--
--   NB: functional dependency @bs asbs -> as@ does not seem to be possible,
--       because dependency checking happens before constraints checking
--        and does not take constraints into account.
class
    ( asbs ~ Concat as bs
    , as   ~ StripSuffix bs asbs
    , bs   ~ StripPrefix as asbs
    , ConcatListCtx1 as bs asbs
    , ConcatListCtx2 as bs asbs (bs == asbs)
    ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])
            | as bs -> asbs, as asbs -> bs --, bs asbs -> as
            , as -> k, bs -> k, asbs -> k




{- Type-dependent constraints - XxxCtx type familes

Extra "given" constraints provided by class instances via superclass mechanics
  (the constraints that depend on the expansion of the type variables)


These type families give very handy evidence when you know the structure of
the type variables (e.g. as ~ '[] or as ~ (a' ': 'as)).

A huge problem with this approach is that sometimes I need to provide these
constraints for incoherent instances when I don't know how the type variables expand.
To workaround this, I need to provide such constraint families that:

  1. Their runtime representation is the same for all parameter values
  2. If a constraint does not make sense for a given parameter expansion,
     replace it with a useless but harmless alternative
  3. XxxCtx must not be in loop with Xxx
     (otherwise, typechecker stack overflow occurs at the call site)

 -}


type family SnocListCtx (as :: [k]) (a :: k) (bs :: [k]) :: Constraint where
    SnocListCtx '[]       z bs =
      ( Head bs ~ z
      , Tail bs ~ '[]
      , bs ~ '[z]
      , UnreachableConstraint (SnocList (Tail '[]) (Head '[]) (Tail bs))
             "SnocListCtx '[] z bs -- SnocList (Tail '[]) (Head '[]) (Tail bs)"
      )
    SnocListCtx (a ': as) z bs =
      ( Head bs ~ a
      , Tail bs ~ Snoc as z
      , bs ~ (Head bs ': Head (Tail bs) ': Tail (Tail bs))
      , SnocList as z (Tail bs)
      )

type family ReverseListCtx (as :: [k]) (bs :: [k]) :: Constraint where
    ReverseListCtx '[]       bs =
      ( bs ~ '[]
      , UnreachableConstraint (ReverseList (Tail '[]) (Init bs))
             "ReverseListCtx '[] bs -- ReverseList (Tail '[]) (Init bs)"
      , UnreachableConstraint (SnocList (Init bs) (Head '[]) bs)
             "ReverseListCtx '[] bs -- SnocList (Init bs) (Head '[]) bs"
      )
    ReverseListCtx (a ': as) bs =
      ( bs ~ (Head bs ': Tail bs)
      , ReverseList as (Init bs)
      , SnocList (Init bs) a bs
      )

-- | Extra evidence provided by `ConcatList` in various cases
type family ConcatListCtx1 (as :: [k]) (bs :: [k]) (asbs :: [k]) :: Constraint where
    ConcatListCtx1 '[] bs asbs =
      ( asbs ~ bs
      , (bs == asbs) ~ 'True
      , UnreachableConstraint (Head '[] ~ Head asbs)
             "ConcatListCtx1 '[] bs asbs -- (Head '[] ~ Head asbs)"
      , UnreachableConstraint (ConcatList (Tail '[]) bs (Tail asbs))
             "ConcatListCtx1 '[] bs asbs -- ConcatList (Tail '[]) bs (Tail asbs)"
      , UnreachableConstraint (ConcatList (Init '[]) (Last '[] ': bs) asbs)
             "ConcatListCtx1 '[] bs asbs -- ConcatList (Init '[]) (Last '[] ': bs) asbs"
      )
    ConcatListCtx1 (a ': as) bs asbs =
      ( asbs ~ (a ': Tail asbs)
      , (bs == asbs) ~ 'False
      , a ~ Head asbs
      , ConcatList as bs (Tail asbs)
      , ConcatList (Init (a ': as)) (Last (a ': as) ': bs) asbs
      )
-- | Extra evidence provided by `ConcatList` in various cases
type family ConcatListCtx2 (as :: [k]) (bs :: [k]) (asbs :: [k]) (bsEq :: Bool) :: Constraint where
    ConcatListCtx2 as bs asbs 'True =
      ( as ~ '[]
      , bs ~ asbs
      , UnreachableConstraint (ConcatList (Tail as) bs (Tail asbs))
             "ConcatListCtx2 as bs asbs 'True -- ConcatList (Tail as) bs (Tail asbs)"
      )
    ConcatListCtx2 as bs (a ': asbs) 'False =
      ( as ~ (a ': Tail as)
      , a ~ Head as
      , ConcatList (Tail as) bs asbs
      )
    ConcatListCtx2 as bs '[] _ =
      ( as ~ '[]
      , bs ~ '[]
      , UnreachableConstraint (ConcatList (Tail as) bs (Tail '[]))
             "ConcatListCtx2 as bs '[] _ -- ConcatList (Tail as) bs (Tail '[])"
      )

{- Lookup class data constructors.

I use my plugin called ClassDict from constraints-deriving package.
It takes a data constructor for a given class and wraps it in Dict.

The signatures of the functions below are fully determined by the structure
of the corresponding type classes.
But as a user of the ClassDict API, I have to write these signatures by hand
  (otherwise the plugin displays a compile-time error with the correct signatures).

 -}

{-# ANN defineSnocList ClassDict #-}
defineSnocList :: forall (k :: Type) (as :: [k]) (a :: k) (bs :: [k])
                . ( bs ~ Snoc as a, as ~ Init bs, a ~ Last bs
                  , SnocListCtx as a bs
                  , ConcatList as '[a] bs
                  )
               => Dict (SnocList as a bs)
defineSnocList = defineSnocList

{-# ANN defineReverseList ClassDict #-}
defineReverseList :: forall (k :: Type) (as :: [k]) (bs :: [k])
                   . (as ~ Reverse bs, bs ~ Reverse as, ReverseList bs as, ReverseListCtx as bs)
                  => Dict (ReverseList as bs)
defineReverseList = defineReverseList

{-# ANN defineConcatList ClassDict #-}
defineConcatList :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                  . ( asbs ~ Concat as bs
                    , as   ~ StripSuffix bs asbs
                    , bs   ~ StripPrefix as asbs
                    , ConcatListCtx1 as bs asbs
                    , ConcatListCtx2 as bs asbs (bs == asbs)
                    )
                 => Dict (ConcatList as bs asbs)
defineConcatList = defineConcatList


{- Creating instances

The constraints defined about have a lot of recursive references to each other.
This makes defining instances using the standard syntax virtually impossible.

That is why I create most of the instances manually via the ClassDict+toInstance plugins.

Even on this way, there are some problems with passing dictionaries recursively;
if I just used `Dict` all the time and pattern match against it, I would get
a lot of <<loop>> or "unreachable instances" errors.
Instead, I use the `BareConstraint` abstract data type and pass it to `defineXxx`
functions as if constraints were vanilla values.

The functions below are helpers that break reference loops.
They also unsafely create all type equality constraints;
  it is very easy to introduce a bug here, because I bypass most of the typechecker.
 -}



unsafeBareSnocList :: forall (k :: Type) (as :: [k]) (z :: k) (bs :: [k])
                    . BareConstraint (SnocListCtx as z bs)
                   -> BareConstraint (SnocList as z bs)
unsafeBareSnocList = runMagic m
  where
    m :: SnocListCtx as z bs =-> BareConstraint (SnocList as z bs)
    m | Dict <- unsafeEqTypes @bs @(Snoc as z)
      , Dict <- unsafeEqTypes @as @(Init bs)
      , Dict <- unsafeEqTypes @z @(Last bs)
      , Dict <- unsafeEqTypes @bs @(Concat as '[z])
      , Dict <- inferConcat @as @'[z] @bs
      = Magic (dictToBare $ defineSnocList @k @as @z @bs)

unsafeBareReverseList :: forall (k :: Type) (as :: [k]) (bs :: [k])
                       . BareConstraint (ReverseList bs as)
                      -> BareConstraint (ReverseListCtx as bs)
                      -> BareConstraint (ReverseList as bs)
unsafeBareReverseList = runMagic . runMagic m
  where
    m :: ReverseList bs as
      =-> ReverseListCtx as bs
      =-> BareConstraint (ReverseList as bs)
    m | Dict <- unsafeEqTypes @as @(Reverse bs)
      , Dict <- unsafeEqTypes @bs @(Reverse as)
      = Magic (Magic (dictToBare $ defineReverseList @k @as @bs))

unsafeBareConcatList ::
     forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
  .  BareConstraint (ConcatListCtx1 as bs asbs)
  -> BareConstraint (ConcatListCtx2 as bs asbs (bs == asbs))
  -> BareConstraint (ConcatList as bs asbs)
unsafeBareConcatList = runMagic . runMagic m
  where
    m :: ConcatListCtx1 as bs asbs
      =-> ConcatListCtx2 as bs asbs (bs == asbs)
      =-> BareConstraint (ConcatList as bs asbs)
    m | Dict <- unsafeEqTypes @as @(StripSuffix bs asbs)
      , Dict <- unsafeEqTypes @bs @(StripPrefix as asbs)
      , Dict <- unsafeEqTypes @asbs @(Concat as bs)
      = Magic (Magic (dictToBare $ defineConcatList @k @as @bs @asbs))


unsafeBareSnocListCtx :: forall (k :: Type) (as :: [k]) (z :: k) (bs :: [k])
                       . BareConstraint (SnocList (Tail as) z (Tail bs))
                      -> BareConstraint (SnocListCtx as z bs)
unsafeBareSnocListCtx = runMagic m
  where
    m :: SnocList (Tail as) z (Tail bs) =-> BareConstraint (SnocListCtx as z bs)
    m = Magic (dictToBare d)
    d :: SnocList (Tail as) z (Tail bs) => Dict (SnocListCtx as z bs)
    d = unsafeCoerce $ Dict
      @(Head as ~ Head as, Tail bs ~ Tail bs, bs ~ bs, SnocList (Tail as) z (Tail bs))

unsafeBareReverseListCtx :: forall (k :: Type) (as :: [k]) (bs :: [k])
                          . BareConstraint (ReverseList (Tail as) (Init bs))
                         -> BareConstraint (ReverseListCtx as bs)
unsafeBareReverseListCtx
    = runMagic (runMagic m unsafeIncohBareSnocList)
  where
    m :: SnocList (Init bs) (Head as) bs
      =-> ReverseList (Tail as) (Init bs)
      =-> BareConstraint (ReverseListCtx as bs)
    m = Magic (Magic (dictToBare f))
    f :: ( SnocList (Init bs) (Last bs) bs
         , ReverseList (Tail as) (Init bs)
         )
      => Dict (ReverseListCtx as bs)
    f | Dict <- unsafeEqTypes @(Head as) @(Last bs)
      = unsafeCoerce (
            Dict @( bs ~ bs
                  , ReverseList (Tail as) (Init bs)
                  , SnocList (Init bs) (Head as) bs)
          )

unsafeBareConcatListCtx1 :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                          . BareConstraint (ConcatList (Tail as) bs (Tail asbs))
                         -> BareConstraint (ConcatList (Init as) (Last as ': bs) asbs)
                         -> BareConstraint (ConcatListCtx1 as bs asbs)
unsafeBareConcatListCtx1 = runMagic . runMagic m
  where
    m :: ConcatList (Tail as) bs (Tail asbs)
      =-> ConcatList (Init as) (Last as ': bs) asbs
      =-> BareConstraint (ConcatListCtx1 as bs asbs)
    m = Magic (Magic (dictToBare d))
    d :: ( ConcatList (Tail as) bs (Tail asbs)
         , ConcatList (Init as) (Last as ': bs) asbs)
      => Dict (ConcatListCtx1 as bs asbs)
    d = unsafeCoerce $ Dict
      @( asbs ~ asbs, (bs == asbs) ~ (bs == asbs)
       , Head asbs ~ Head asbs
       , ConcatList (Tail as) bs (Tail asbs)
       , ConcatList (Init as) (Last as ': bs) asbs
       )

unsafeBareConcatListCtx2 :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                          . BareConstraint (ConcatList (Tail as) bs (Tail asbs))
                         -> BareConstraint (ConcatListCtx2 as bs asbs (bs == asbs))
unsafeBareConcatListCtx2 = runMagic m
  where
    m :: ConcatList (Tail as) bs (Tail asbs)
      =-> BareConstraint (ConcatListCtx2 as bs asbs (bs == asbs))
    m = Magic (dictToBare d)
    d :: ConcatList (Tail as) bs (Tail asbs)
      => Dict (ConcatListCtx2 as bs asbs (bs == asbs))
    d = unsafeCoerce $ Dict
      @(as ~ as, Head as ~ Head as, ConcatList (Tail as) bs (Tail asbs))


{- Recursive bindings for generic incoherent instances

Since all three classes consist solely of some combination of equality constraints
(nested inside algebraic class constructors and constraint tuples),
one can construct instances  "from nothing".
These are the three functions below.

 -}

unsafeIncohBareSnocList :: forall (k :: Type) (as :: [k]) (z :: k) (bs :: [k])
                         . BareConstraint (SnocList as z bs)
unsafeIncohBareSnocList = unsafeBareSnocList $
    unsafeBareSnocListCtx @k @as @z @bs unsafeIncohBareSnocList

unsafeIncohBareReverseList :: forall (k :: Type) (as :: [k]) (bs :: [k])
                            . BareConstraint (ReverseList as bs)
unsafeIncohBareReverseList = unsafeBareReverseList @k @as @bs
    unsafeIncohBareReverseList
    (unsafeBareReverseListCtx @k @as @bs unsafeIncohBareReverseList)

unsafeIncohBareConcatList :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                           . BareConstraint (ConcatList as bs asbs)
unsafeIncohBareConcatList = unsafeBareConcatList @k @as @bs @asbs
    (unsafeBareConcatListCtx1 @k @as @bs @asbs unsafeIncohBareConcatList unsafeIncohBareConcatList)
    (unsafeBareConcatListCtx2 @k @as @bs @asbs unsafeIncohBareConcatList)




{- Declaring instances

The simplest instances can be created using the vanilla syntax;
the rest is derived via the three recursive definitions above.
 -}

-- instance SnocList '[] a '[a] where
{-# ANN nilInstSnocList (ToInstance NoOverlap) #-}
nilInstSnocList :: forall (k :: Type) (a :: k)
                 . Dict (SnocList '[] a '[a])
nilInstSnocList
  | Dict <- nilInstConcatList @k @'[a]
    = defineSnocList @k @'[] @a @'[a]

-- instance SnocList as z bs => SnocList (a ': as) z (a ': bs)
{-# ANN consInstSnocList (ToInstance NoOverlap) #-}
consInstSnocList :: forall (k :: Type) (as :: [k]) (z :: k) (bs :: [k]) (a :: k) (b :: k)
                  . SnocList as z (b ': bs)
                 => Dict (SnocList (a ': as) z (a ': b ': bs))
consInstSnocList
  | Dict <- unsafeEqTypes @(a ': b ': bs) @(Snoc (a ': as) z)
  , Dict <- unsafeEqTypes @(a ': as) @(Init (a ': b ': bs))
  , Dict <- unsafeEqTypes @z @(Last (a ': b ': bs))
  , Dict <- consInstConcatList @_ @as @'[z] @(b ': bs) @a
    = defineSnocList @k @(a ': as) @z @(a ': b ': bs)


-- instance {-# INCOHERENT #-} SnocList as z bs
{-# ANN incohInstSnocList (ToInstance Incoherent) #-}
incohInstSnocList :: forall (k :: Type) (as :: [k]) (z :: k) (bs :: [k])
                   . bs ~ Snoc as z
                  => Dict (SnocList as z bs)
incohInstSnocList = bareToDict $ unsafeIncohBareSnocList @k @as @z @bs


instance ReverseList ('[] :: [k]) ('[] :: [k])

-- instance (ReverseList as bs', SnocList bs' a (b ': bs))
--          => ReverseList (a ': as) (b ': bs)
{-# ANN consInstReverseList (ToInstance NoOverlap) #-}
consInstReverseList :: forall (k :: Type)
                              (a :: k) (as :: [k]) (b :: k) (bs :: [k])
                     . (ReverseList as (Init (b ': bs)), SnocList (Init (b ': bs)) a (b ': bs))
                    => Dict (ReverseList (a ': as) (b ': bs))
consInstReverseList = bareToDict d
  where
    d = runMagic m (rev d)

    m :: ReverseList (b ': bs) (a ': as)
      =-> BareConstraint (ReverseList (a ': as) (b ': bs))
    m = Magic f
    f :: ReverseList (b ': bs) (a ': as)
      => BareConstraint (ReverseList (a ': as) (b ': bs))
    f | Dict <- unsafeEqTypes @(a ': as) @(Reverse (b ': bs))
      , Dict <- unsafeEqTypes @(b ': bs) @(Reverse (a ': as))
      = dictToBare $ defineReverseList

    {- Since both classes, ReverseList and SnocList actually bear no runtime references
       to their parameters, the only thing that matters is the length of the list
         (the only parameter that affects the content of an instance).
       Thus, I can cast the class instances between the lists of the same lengths.
     -}
    rev :: BareConstraint (ReverseList (a ': as) (b ': bs))
        -> BareConstraint (ReverseList (b ': bs) (a ': as))
    rev = unsafeCoerce


{-# ANN incohInstReverseList (ToInstance Incoherent) #-}
incohInstReverseList :: forall (k :: Type) (as :: [k]) (bs :: [k])
                      . bs ~ Reverse as
                     => Dict (ReverseList as bs)
incohInstReverseList = bareToDict $ unsafeIncohBareReverseList @k @as @bs




-- instance {-# INCOHERENT #-} ConcatList as '[] as
{-# ANN incohInstConcatList (ToInstance Incoherent) #-}
incohInstConcatList :: forall (k :: Type) (as :: [k])
                     . Dict (ConcatList as ('[] :: [k]) as)
incohInstConcatList = bareToDict $ unsafeIncohBareConcatList @k @as @'[] @as

-- instance ConcatList '[] bs bs
{-# ANN nilInstConcatList (ToInstance NoOverlap) #-}
nilInstConcatList :: forall (k :: Type) (bs :: [k])
                    . Dict (ConcatList '[] bs bs)
nilInstConcatList
  | Dict <- unsafeEqTypes @(bs == bs) @'True
    = defineConcatList @k @'[] @bs @bs

-- instance ConcatList  as bs asbs => ConcatList (a ': as) bs (a ': asbs)
{-# ANN consInstConcatList (ToInstance NoOverlap) #-}
consInstConcatList :: forall (k :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k]) (a :: k)
                    . ConcatList as bs asbs
                   => Dict (ConcatList (a ': as) bs (a ': asbs))
consInstConcatList
  | Dict <- unsafeEqTypes @(bs == (a ': asbs)) @'False
  , Dict <- unsafeEqTypes @(a ': as)   @(StripSuffix bs (a ': asbs))
  , Dict <- unsafeEqTypes @bs          @(StripPrefix (a ': as) (a ': asbs))
  , Dict <- unsafeEqTypes @(a ': asbs) @(Concat (a ': as) bs)
    = let x :: ConcatList (Init (a : as)) (Last (a : as) : bs) (a : asbs)
            => Dict (ConcatList (a ': as) bs (a ': asbs))
          x = defineConcatList @k @(a ': as) @bs @(a ': asbs)
          m :: ConcatList (Init (a : as)) (Last (a : as) : bs) (a : asbs)
            =-> BareConstraint (ConcatList (a ': as) bs (a ': asbs))
          m = Magic (dictToBare x)

          shiftedCL :: BareConstraint
            (ConcatList (Init (a : as)) (Last (a : as) : bs) (a : asbs))
          shiftedCL = unsafeIncohBareConcatList
      in bareToDict $ runMagic m shiftedCL




-- | Derive @ConcatList@ given @Concat@
inferConcat :: forall as bs asbs
             . asbs ~ Concat as bs
            => Dict (ConcatList as bs asbs)
inferConcat = bareToDict $ unsafeIncohBareConcatList @_ @as @bs @asbs

-- | Derive @ConcatList@ given @StripSuffix@
inferStripSuffix :: forall as bs asbs
                  . as ~ StripSuffix bs asbs
                 => Dict (ConcatList as bs asbs)
inferStripSuffix = bareToDict $ unsafeIncohBareConcatList @_ @as @bs @asbs

-- | Derive @ConcatList@ given @StripPrefix@
inferStripPrefix :: forall as bs asbs
                  . bs ~ StripPrefix as asbs
                 => Dict (ConcatList as bs asbs)
inferStripPrefix = bareToDict $ unsafeIncohBareConcatList @_ @as @bs @asbs
