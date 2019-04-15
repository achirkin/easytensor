{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE GADTs   #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Type.ListAlt
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides type-level operations on lists.
--
--
--------------------------------------------------------------------------------

module Numeric.Type.ListAlt
  ( -- * Basic operations
    type (++), type (+:), type (:+)
  , Empty, Cons, Snoc, Head
  , Tail, Init, Last, Concat, Reverse, Take, Drop, Length
  , All, Map
    -- * Working with concatenations
  , StripSuffix, StripPrefix, IsPrefix, IsSuffix
    -- * Term level functions
  , ConcatList
  , Op (..), RunOp
  ) where

import           GHC.Exts
import           GHC.TypeLits

data Op x where
  Id          :: x -> Op x
  Empty       :: Op [x]
  Cons        :: Op x   -> Op [x] -> Op [x]
  Snoc        :: Op [x] -> Op x -> Op [x]
  Head        :: Op [x] -> Op x
  Tail        :: Op [x] -> Op [x]
  Last        :: Op [x] -> Op x
  Init        :: Op [x] -> Op [x]
  Take        :: Op Nat -> Op [x] -> Op [x]
  Drop        :: Op Nat -> Op [x] -> Op [x]
  Concat      :: Op [x] -> Op [x] -> Op [x]
  StripPrefix :: Op [x] -> Op [x] -> Op [x]
  StripSuffix :: Op [x] -> Op [x] -> Op [x]
  Reverse     :: Op [x] -> Op [x]
  Length      :: Op [x] -> Op Nat


{-
 `Op` expands the ordinary haskell list type with more constructors to
 enable GHC to simplify some operations that cancel each other and
 do other simplifications.

 Idea for an invariant: simplify '[] to 'Empty and ': to 'Cons always!

 Thus, never use 'IdOp directly; only use the IdOp wrapper 
 -}

type family Id (xs :: k) = (ys :: Op k) | ys -> xs where
  Id ('[] :: [k]) = ('Empty :: Op [k])
  Id (x ': xs)    = 'Cons (Id x) (Id xs)
  Id n            = 'Id n

{-
 NB:
   * 'IdOp should never appear on rhs here (use IdOp)
   * '[] and ': should never appear on lhs here

 NB: need to decide something about precedence
   (otherwise it is going to loop forever)
 -}
type family RunOp (xs :: Op k) :: k where
  -- Primitive ops: one level of pattern-matching
  RunOp ('Id xs) = xs
  RunOp ('Empty)  = '[]
  RunOp ('Cons x xs) = RunOp x ': RunOp xs

  -- The rest must be implemented in terms of the first three rules.
  -- In addition, I should try to simplify recursively all constructors except 'Id;

  RunOp ('Snoc 'Empty z)       = '[RunOp z]
  RunOp ('Snoc ('Cons x xs) z) = RunOp x ': RunOp ('Snoc xs z)
  RunOp ('Snoc ('Init xs) ('Last xs)) = RunOp xs
  RunOp ('Snoc xs x) = RunOp ('Snoc (Recur xs) x)

  RunOp ('Head ('Empty :: Op [k]))
    = TypeError ( MkFEM k "Head: empty type-level list." )
  RunOp ('Head ('Cons x _))  = RunOp x
  RunOp ('Head xs)           = RunOp ('Head (Recur xs))

  RunOp ('Tail ('Empty :: Op [k]))
    = TypeError ( MkFEM k "Tail: empty type-level list." )
  RunOp ('Tail ('Cons _ xs)) = RunOp xs
  RunOp ('Tail xs)           = RunOp ('Tail (Recur xs))
   
  RunOp ('Last ('Snoc _ x))  = RunOp x
  RunOp ('Last ('Empty :: Op [k]))
    = TypeError ( MkFEM k "Last: empty type-level list." )
  RunOp ('Last ('Cons x 'Empty)) = RunOp x
  RunOp ('Last ('Cons _ ('Cons x xs))) = RunOp ('Last ('Cons x xs))
  RunOp ('Last xs)           = RunOp ('Last (Recur xs))

  RunOp ('Init ('Snoc xs _)) = RunOp xs
  RunOp ('Init ('Empty :: Op [k]))
    = TypeError ( MkFEM k "Init: empty type-level list." )
  RunOp ('Init ('Cons x 'Empty)) = '[]
  RunOp ('Init ('Cons x ('Cons y ys))) = RunOp x ': RunOp ('Init ('Cons y ys))
  RunOp ('Init xs)           = RunOp ('Init (Recur xs))

  RunOp ('Take n 'Empty)             = '[]
  RunOp ('Take ('Id 0) _)            = '[]
  RunOp ('Take ('Length xs) xs)      = RunOp xs
  -- it's ok to use 'Id here because we know this is a Nat
  RunOp ('Take ('Id n) ('Cons x xs)) = RunOp x ': RunOp ('Take ('Id (n - 1)) xs)
  RunOp ('Take n xs)                 = RunOp ('Take (Recur n) (Recur xs))

  RunOp ('Drop n 'Empty)             = '[]
  RunOp ('Drop ('Length xs) xs)      = '[]
  RunOp ('Drop ('Id 0) xs)           = RunOp xs
  -- it's ok to use 'Id here because we know this is a Nat
  RunOp ('Drop ('Id n) ('Cons _ xs)) = RunOp ('Drop ('Id (n - 1)) xs)
  -- RunOp ('Drop ('Id n) ('Tail xs))   = RunOp ('Drop ('Id (n + 1)) xs) -- not exactly
  RunOp ('Drop n xs)                 = RunOp ('Drop (Recur n) (Recur xs))

  RunOp ('Concat 'Empty bs) = RunOp bs
  RunOp ('Concat as 'Empty) = RunOp as
  RunOp ('Concat ('Cons a as) bs) = RunOp ('Cons a ('Concat as bs))
  RunOp ('Concat as ('Snoc bs z)) = RunOp ('Snoc ('Concat as bs) z)
  RunOp ('Concat as bs)
    = RunOp ('Concat (Recur as) (Recur bs))
  
  RunOp ('StripPrefix 'Empty asbs) = RunOp asbs
  RunOp ('StripPrefix ('Cons a as) ('Cons a asbs)) = RunOp ('StripPrefix as asbs)
  RunOp ('StripPrefix as as) = '[]
  RunOp ('StripPrefix as ('Concat as bs)) = RunOp bs
  RunOp ('StripPrefix as asbs)
    = RunOp ('StripPrefix (Recur as) (Recur asbs))

  RunOp ('StripSuffix 'Empty asbs) = RunOp asbs
  RunOp ('StripSuffix ('Snoc bs z) ('Snoc asbs z)) = RunOp ('StripSuffix bs asbs)
  RunOp ('StripSuffix bs bs) = '[]
  RunOp ('StripSuffix bs ('Cons a asbs)) = RunOp a ': RunOp ('StripSuffix bs asbs)
  RunOp ('StripSuffix bs ('Concat as bs)) = RunOp as
  RunOp ('StripSuffix bs asbs) -- also very bad
    = RunOp ('StripSuffix (Recur bs) (Recur asbs))

  
  RunOp ('Reverse 'Empty)
    = RunOp 'Empty
  RunOp ('Reverse ('Cons x xs))
    = RunOp (('Snoc ('Reverse xs)) x)
  RunOp ('Reverse ('Snoc xs x))
    = RunOp ('Cons x ('Reverse xs))
  RunOp ('Reverse ('Tail xs))
    = RunOp ('Init ('Reverse xs))
  RunOp ('Reverse ('Init xs))
    = RunOp ('Tail ('Reverse xs))
  RunOp ('Reverse ('Concat as bs))
    = RunOp ('Concat ('Reverse bs) ('Reverse as))
  RunOp ('Reverse ('StripPrefix as asbs))
    = RunOp ('StripSuffix ('Reverse as) ('Reverse asbs))
  RunOp ('Reverse ('StripSuffix bs asbs))
    = RunOp ('StripPrefix ('Reverse bs) ('Reverse asbs))
  RunOp ('Reverse ('Reverse xs)) = RunOp xs
  RunOp ('Reverse xs) = RunOp ('Reverse (Recur xs))

  RunOp ('Length 'Empty) = 0
  RunOp ('Length ('Cons _ xs)) = RunOp ('Length xs) + 1
  RunOp ('Length ('Snoc xs _)) = RunOp ('Length xs) + 1
  RunOp ('Length ('Tail xs)) = RunOp ('Length xs) - 1
  RunOp ('Length ('Init xs)) = RunOp ('Length xs) - 1
  RunOp ('Length ('Concat as bs)) = RunOp ('Length as) + RunOp ('Length bs)
  RunOp ('Length ('StripPrefix as asbs))
    = RunOp ('Length asbs) - RunOp ('Length as)
  RunOp ('Length ('StripSuffix bs asbs))
    = RunOp ('Length asbs) - RunOp ('Length bs)
  RunOp ('Length ('Reverse xs)) = RunOp ('Length xs)
  

type Recur (x :: Op k) = Id (RunOp x)

data T (x :: k) = T deriving (Eq, Show, Ord, Read)

t1 = T :: T (StripSuffix '[21,26] ([1,2,4] ++ [16, 21, 26]))
t2 = T :: T (StripSuffix '[22,26] ([1,2,4] ++ [16, 21, 26]))


-- type family Recur (x :: Op k) :: Op k where
--   Recur ('Id x)      = Id x
--   Recur 'Empty       = 'Empty
--   Recur ('Cons x xs) = 'Cons (Recur x) (Recur xs)
--   Recur ('Snoc xs x) = Id (RunOp ('Snoc xs x))
--   Recur ('Head xs)   = Id (RunOp ('Snoc xs x))

-- Id          :: x -> Op x
-- Empty       :: Op [x]
-- Cons        :: Op x   -> Op [x] -> Op [x]
-- Snoc        :: Op [x] -> Op x -> Op [x]
-- Head        :: Op [x] -> Op x
-- Tail        :: Op [x] -> Op [x]
-- Last        :: Op [x] -> Op x
-- Init        :: Op [x] -> Op [x]
-- Take        :: Op Nat -> Op [x] -> Op [x]
-- Drop        :: Op Nat -> Op [x] -> Op [x]
-- Concat      :: Op [x] -> Op [x] -> Op [x]
-- StripPrefix :: Op [x] -> Op [x] -> Op [x]
-- StripSuffix :: Op [x] -> Op [x] -> Op [x]
-- Reverse     :: Op [x] -> Op [x]
-- Length      :: Op [x] -> Op Nat


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
type Snoc (xs :: [k]) (x :: k) = RunOp ('Snoc (Id xs) (Id x))


-- | Infix-style synonym for concatenation
type (as :: [k]) ++ (bs :: [k]) = Concat as bs
infixr 5 ++


-- | Synonym for an empty type-level list
type Empty = '[]
type Take (n::Nat) (xs :: [k]) = RunOp ('Take (Id n) (Id xs))
type Drop (n::Nat) (xs :: [k]) = RunOp ('Drop (Id n) (Id xs))
type Reverse (xs :: [k])       = RunOp ('Reverse (Id xs))
type Concat  (as :: [k]) (bs :: [k]) = RunOp ('Concat (Id as) (Id bs))
type StripPrefix (as :: [k]) (asbs :: [k]) = RunOp ('StripPrefix (Id as) (Id asbs))
type StripSuffix (bs :: [k]) (asbs :: [k]) = RunOp ('StripSuffix (Id bs) (Id asbs))

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


type Head (xs :: [k]) = RunOp ('Head (Id xs))
type Tail (xs :: [k]) = RunOp ('Tail (Id xs))
type Init (xs :: [k]) = RunOp ('Init (Id xs))
type Last (xs :: [k]) = RunOp ('Last (Id xs))
type Length (xs :: [k]) = RunOp ('Length (Id xs))

type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    All _ '[] = ()
    All f (x ': xs) = (f x, All f xs)

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs


-- | Represent a triple of lists forming a relation `as ++ bs ~ asbs`
class ( asbs ~ Concat as bs
      , as   ~ Prefix bs asbs
      , bs   ~ Suffix as asbs
      , IsSuffix bs asbs ~ 'True
      , IsPrefix as asbs ~ 'True
      ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])
        | as bs -> asbs
        , as asbs -> bs
        , bs asbs -> as

instance ( asbs ~ Concat as bs
         , as   ~ Prefix bs asbs
         , bs   ~ Suffix as asbs
         , IsSuffix bs asbs ~ 'True
         , IsPrefix as asbs ~ 'True
         ) => ConcatList (as :: [k]) (bs :: [k]) (asbs :: [k])

type MkFEM k t = 'Text t ':$$: FamErrMsg k

type FamErrMsg k
  = 'Text "Type-level error occured when operating on a list of kind "
    ':<>: 'ShowType [k] ':<>: 'Text "."
