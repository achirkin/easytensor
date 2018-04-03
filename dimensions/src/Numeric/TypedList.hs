{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.TypedList
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-- Provide a type-indexed heterogeneous list type @TypedList@.
-- Behind the facade, @TypedList@ is just a plain list of haskell pointers.
-- It is used to represent dimension lists, indices, and just flexible tuples.
--
-- Most of type-level functionality is implemented using GADT-like pattern synonyms.
-- Import this module qualified to use list-like functionality.
--
-----------------------------------------------------------------------------
module Numeric.TypedList
    ( TypedList (U, (:*), Empty, TypeList, Cons, Snoc, Reverse, Concat)
    , RepresentableList (..)
    , TypeList, types, order, order'
    , cons, snoc
    , Numeric.TypedList.reverse
    , Numeric.TypedList.take
    , Numeric.TypedList.drop
    , Numeric.TypedList.head
    , Numeric.TypedList.tail
    , Numeric.TypedList.last
    , Numeric.TypedList.init
    , Numeric.TypedList.splitAt
    , Numeric.TypedList.length
    , Numeric.TypedList.map
    , module Numeric.Type.List
    ) where

import           Control.Arrow         (first)
import           Data.Proxy
import           GHC.Base              (Type)
import           GHC.Exts

import           Numeric.Dim
import           Numeric.Type.Evidence
import           Numeric.Type.List


-- | Type-indexed list
newtype TypedList (f :: (k -> Type)) (xs :: [k]) = TypedList [Any]


-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE TypeList #-}
{-# COMPLETE U, (:*) #-}
{-# COMPLETE U, Cons #-}
{-# COMPLETE U, Snoc #-}
{-# COMPLETE Empty, (:*) #-}
{-# COMPLETE Empty, Cons #-}
{-# COMPLETE Empty, Snoc #-}
{-# COMPLETE Reverse #-}
{-# COMPLETE Concat #-}
#endif

-- | A list of type proxies
type TypeList (xs :: [k]) = TypedList Proxy xs

-- | Pattern matching against this causes `RepresentableList` instance
--   come into scope.
--   Also it allows constructing a term-level list out of a constraint.
pattern TypeList :: forall (xs :: [k])
                  . () => RepresentableList xs => TypeList xs
pattern TypeList <- (mkRTL -> E)
  where
    TypeList = tList @k @xs



-- | Zero-length type list
pattern U :: forall (f :: k -> Type) (xs :: [k])
           . () => (xs ~ '[]) => TypedList f xs
pattern U <- (patTL -> (Left (E, _)))
  where
    U = unsafeCoerce# []

-- | Zero-length type list; synonym to `U`.
pattern Empty :: forall (f :: k -> Type) (xs :: [k])
               . () => (xs ~ '[]) => TypedList f xs
pattern Empty = U

-- | Constructing a type-indexed list
pattern (:*) :: forall (y :: k) (ys :: [k]) (f :: k -> Type) (xs :: [k])
              . () => (xs ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f xs
pattern (:*) x xs <- (patTL @f @xs @y @ys -> (Right (E, (x, xs ))))
  where
    (:*) x xs = TypedList (unsafeCoerce# x : unsafeCoerce# xs)
infixr 5 :*

-- | Constructing a type-indexed list in the canonical way
pattern Cons :: forall (y :: k) (ys :: [k]) (f :: k -> Type) (xs :: [k])
              . () => (xs ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f xs
pattern Cons x xs <- (patTL @f @xs @y @ys -> (Right (E, (x, xs ))))
  where
    Cons = Numeric.TypedList.cons

-- | Constructing a type-indexed list from the other end
pattern Snoc :: forall (sy :: [k]) (y :: k) (f :: k -> Type) (xs :: [k])
              . () => (xs ~ (sy +: y)) => TypedList f sy -> f y -> TypedList f xs
pattern Snoc sx x <- (unsnocTL @f @xs @sy @y -> (Just (E, (sx, x))))
  where
    Snoc = Numeric.TypedList.snoc

-- | Reverse a typed list
pattern Reverse :: forall (sx :: [k]) (f :: k -> Type) (xs :: [k])
              . () => (xs ~ Reverse sx, sx ~ Reverse xs)
                   => TypedList f sx -> TypedList f xs
pattern Reverse sx <- (unreverseTL @f @xs @sx -> (E, sx))
  where
    Reverse = Numeric.TypedList.reverse

-- | Split a typed list into two.
--   Bring type-level evidence on relations between typelists into the scope.
pattern Concat :: forall (xs :: [k]) (ys :: [k]) (f :: k -> Type) (xsys :: [k])
                . KnownDim (Length xs)
                  => ConcatList xs ys xsys
                  => TypedList f xs -> TypedList f ys -> TypedList f xsys
pattern Concat xs ys <- (unconcatTL @f @xs @ys @xsys -> (E, (xs, ys)))
  where
    Concat (TypedList xs) (TypedList ys) = unsafeCoerce# (xs ++ ys)


cons :: f x -> TypedList f xs -> TypedList f (x :+ xs)
cons x xs = TypedList (unsafeCoerce# x : unsafeCoerce# xs)
{-# INLINE cons #-}

snoc :: TypedList f xs -> f x -> TypedList f (xs +: x)
snoc xs x = TypedList (unsafeCoerce# xs ++ [unsafeCoerce# x])
{-# INLINE snoc #-}

reverse :: TypedList f xs -> TypedList f (Reverse xs)
reverse (TypedList sx) = unsafeCoerce# (Prelude.reverse sx)
{-# INLINE reverse #-}

take :: Dim n -> TypedList f xs -> TypedList f (Take n xs)
take d (TypedList xs) = unsafeCoerce# (Prelude.take (intD d) xs)
{-# INLINE take #-}

drop :: Dim n -> TypedList f xs -> TypedList f (Drop n xs)
drop d (TypedList xs) = unsafeCoerce# (Prelude.drop (intD d) xs)
{-# INLINE drop #-}

head :: TypedList f xs -> f (Head xs)
head (TypedList xs) = unsafeCoerce# (Prelude.head xs)
{-# INLINE head #-}

tail :: TypedList f xs -> TypedList f (Tail xs)
tail (TypedList xs) = unsafeCoerce# (Prelude.tail xs)
{-# INLINE tail #-}

init :: TypedList f xs -> TypedList f (Init xs)
init (TypedList xs) = unsafeCoerce# (Prelude.init xs)
{-# INLINE init #-}

last :: TypedList f xs -> f (Head xs)
last (TypedList xs) = unsafeCoerce# (Prelude.last xs)
{-# INLINE last #-}

length :: TypedList f xs -> Dim (Length xs)
length = order
{-# INLINE length #-}

splitAt :: Dim n
        -> TypedList f xs
        -> (TypedList f (Take n xs), TypedList f (Drop n xs))
splitAt d (TypedList xs) = unsafeCoerce# (Prelude.splitAt (intD d) xs)
{-# INLINE splitAt #-}

-- | Map a function over contents of a typed list
map :: (forall a . f a -> g a)
    -> TypedList f xs
    -> TypedList g xs
map k (TypedList xs) = unsafeCoerce# (Prelude.map k' xs)
  where
    k' :: Any -> Any
    k' = unsafeCoerce# . k . unsafeCoerce#

-- | Get a constructible `TypeList` from any other `TypedList`;
--   Pattern matching agains the result brings `RepresentableList` constraint
--   into the scope:
--
--   > case types ts of TypeList -> ...
--
types :: TypedList f xs -> TypeList xs
types (TypedList xs) = unsafeCoerce# (Prelude.map (const Proxy) xs)


-- | Representable type lists.
--   Allows getting type information about list structure at runtime.
class RepresentableList (xs :: [k]) where
  -- | Get type-level constructed list
  tList :: TypeList xs

instance RepresentableList ('[] :: [k]) where
  tList = U

instance RepresentableList xs => RepresentableList (x ': xs :: [k]) where
  tList = Proxy @x :* tList @k @xs


order' :: forall xs . RepresentableList xs => Dim (Length xs)
order' = order (tList @_ @xs)

order :: TypedList f xs -> Dim (Length xs)
order (TypedList xs) = unsafeCoerce# (fromIntegral (Prelude.length xs) :: Word)





--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------


-- | This function does GHC's magic to convert user-supplied `tList` function
--   to create an instance of `RepresentableList` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyRepList :: forall xs r
              . TypeList xs
             -> (RepresentableList xs => r)
             -> r
reifyRepList tl k = unsafeCoerce# (MagicRepList k :: MagicRepList xs r) tl
{-# INLINE reifyRepList #-}
newtype MagicRepList xs r = MagicRepList (RepresentableList xs => r)


unreverseTL :: forall f xs sx
           . TypedList f xs
          -> (Evidence (xs ~ Reverse sx, sx ~ Reverse xs), TypedList f sx)
unreverseTL (TypedList xs)
  = case (unsafeCoerce# (E @(xs ~ xs, sx ~ sx))
           :: Evidence (xs ~ Reverse sx, sx ~ Reverse xs)
         ) of
      E -> (E, unsafeCoerce# (Prelude.reverse xs))
{-# INLINE unreverseTL #-}


mkRTL :: forall (xs :: [k])
       . TypeList xs
      -> Evidence (RepresentableList xs)
mkRTL xs = reifyRepList xs E
{-# INLINE mkRTL #-}

unsnocTL :: forall f xs sy y
        . TypedList f xs
       -> Maybe (Evidence (xs ~ (sy +: y)), (TypedList f sy, f y))
unsnocTL (TypedList []) = Nothing
unsnocTL (TypedList (x:xs))
  = case (unsafeCoerce# (E @(xs ~ xs)) :: Evidence (xs ~ (sy +: y))) of
      E -> Just (E, (unsafeCoerce# sy, unsafeCoerce# y))
  where
    (sy, y) = unsnoc x xs
    unsnoc t []     = ([], t)
    unsnoc t (z:zs) = first (t:) (unsnoc z zs)
{-# INLINE unsnocTL #-}


patTL :: forall f xs y ys
       . TypedList f xs
      -> Either (Evidence (xs ~ '[]), TypedList f '[])
                (Evidence (xs ~ (y ': ys)), (f y, TypedList f ys))
patTL (TypedList [])
  = case (unsafeCoerce# (E @(xs ~ xs)) :: Evidence (xs ~ '[])) of
      E -> Left (E, unsafeCoerce# [])
patTL (TypedList (x : xs))
  = case (unsafeCoerce# (E @(xs ~ xs)) :: Evidence (xs ~ (y ': ys))) of
      E -> Right (E, (unsafeCoerce# x, unsafeCoerce# xs))
{-# INLINE patTL #-}


unconcatTL :: forall f xs ys xsys
            . KnownDim (Length xs)
           => TypedList f xsys
           -> (Evidence (ConcatList xs ys xsys), (TypedList f xs, TypedList f ys))
unconcatTL (TypedList xsys)
  = case ( unsafeCoerce#
            (E :: ConcatEvidence ('[] :: [()]) ('[] :: [()]) ('[] :: [()]))
           :: ConcatEvidence xs ys xsys
         ) of
      E -> (E, (unsafeCoerce# xs, unsafeCoerce# ys))
  where
    (xs, ys) = Prelude.splitAt (fromIntegral (dimVal' @(Length xs))) xsys
{-# INLINE unconcatTL #-}


-- | Pattern-matching on the constructor of this type
--   brings an evidence that `as ++ bs ~ asbs`
type ConcatEvidence (as :: [k]) (bs :: [k]) (asbs :: [k])
  = Evidence
    ( asbs ~ Concat as bs
    , as   ~ Prefix bs asbs
    , bs   ~ Suffix as asbs
    , IsSuffix bs asbs ~ 'True
    , IsPrefix as asbs ~ 'True
    )

intD :: Dim n -> Int
intD = (fromIntegral :: Word -> Int) . unsafeCoerce#
