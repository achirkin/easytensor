{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.TypedList
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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
    ( TypedList (U, (:*), Empty, TypeList, EvList, Cons, Snoc, Reverse)
    , RepresentableList (..)
    , Dict1 (..), DictList
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
    , Numeric.TypedList.stripPrefix
    , Numeric.TypedList.stripSuffix
    , Numeric.TypedList.sameList
    , Numeric.TypedList.concat
    , Numeric.TypedList.length
    , Numeric.TypedList.map
    , module Data.Type.List
    ) where

import           Control.Arrow   (first)
import           Data.Constraint hiding ((***))
import           Data.Data
import           Data.Void
import           GHC.Base        (Type)
import           GHC.Exts
import           GHC.Generics    hiding (Infix, Prefix)
import           Type.Reflection (pattern App, withTypeable)
import qualified Type.Reflection as R

import Data.Type.List
import Numeric.Dim


-- | Type-indexed list
newtype TypedList (f :: (k -> Type)) (xs :: [k]) = TypedList [Any]
  deriving (Typeable)
{-# COMPLETE TypeList #-}
{-# COMPLETE EvList #-}
{-# COMPLETE U, (:*) #-}
{-# COMPLETE U, Cons #-}
{-# COMPLETE U, Snoc #-}
{-# COMPLETE Empty, (:*) #-}
{-# COMPLETE Empty, Cons #-}
{-# COMPLETE Empty, Snoc #-}
{-# COMPLETE Reverse #-}

withTypeableTail :: forall (k :: Type) (ys :: [k]) (x::k) (xs :: [k]) (r :: Type)
                  . (Typeable ys, ys ~ (x ': xs))
                 => (Typeable xs => r) -> r
withTypeableTail f = case R.typeRep @ys of
  App _ xsRep -> withTypeable xsRep f

instance (Typeable k, Typeable f, Typeable xs, All Data (Map f xs))
      => Data (TypedList (f :: (k -> Type)) (xs :: [k])) where
    gfoldl _ z U         = z U
    gfoldl k z (x :* xs) = withTypeableTail @_ @xs $ z (:*) `k` x `k` xs
    gunfold k z c = case constrIndex c of
        1 -> case unsafeEqTypes @[k] @xs @'[] of
               Dict -> z (unsafeCoerce# U)
        2 -> case unsafeEqTypes @[k] @xs @(Head xs ': Tail xs) of
               Dict -> withTypeableTail @_ @xs $ k (k (z (:*)))
        _ -> error "gunfold"
    toConstr U        = typedListConstrEmpty
    toConstr (_ :* _) = typedListConstrCons
    dataTypeOf _ = typedListDataType

typedListDataType :: DataType
typedListDataType = mkDataType
  "Numeric.TypedList.TypedList" [typedListConstrEmpty, typedListConstrCons]

typedListConstrEmpty :: Constr
typedListConstrEmpty = mkConstr typedListDataType "U" [] Prefix

typedListConstrCons :: Constr
typedListConstrCons = mkConstr typedListDataType ":*" [] Infix


type family TypedListRepLeft (xs :: [k]) :: (Type -> Type) where
    TypedListRepLeft '[]      = C1 ('MetaCons "U" 'PrefixI 'False) U1
    TypedListRepLeft (_ ': _) = Rec0 Void

type family TypedListRepRight (f :: (k -> Type)) (xs :: [k]) :: (Type -> Type) where
    TypedListRepRight _ '[]       = Rec0 Void
    TypedListRepRight f (x ': xs) = C1 ('MetaCons ":*" ('InfixI 'RightAssociative 5) 'False)
      ( S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
           (Rec0 (f x))
       :*:
        S1 ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
           (Rec0 (TypedList f xs))
      )

instance Generic (TypedList (f :: (k -> Type)) (xs :: [k])) where
    type Rep (TypedList f xs) = D1
          ('MetaData "TypedList" "Numeric.TypedList" "dimensions" 'False)
          ( TypedListRepLeft xs :+: TypedListRepRight f xs  )
    from U         = M1 (L1 (M1 U1))
    from (x :* xs) = M1 (R1 (M1 (M1 (K1 x) :*: M1 (K1 xs))))
    to (M1 (L1 _))
      | Dict <- unsafeEqTypes @[k] @xs @'[] = U
    to (M1 (R1 xxs))
      | Dict <- unsafeEqTypes @[k] @xs @(Head xs ': Tail xs)
      , M1 (M1 (K1 x) :*: M1 (K1 xs)) <- xxs = x :* xs


-- | A list of type proxies
type TypeList (xs :: [k]) = TypedList Proxy xs

-- | Same as `Dict`, but allows to separate constraint function from
--   the type it is applied to.
data Dict1 :: (k -> Constraint) -> k -> Type where
    Dict1 :: c a => Dict1 c a
    deriving Typeable

instance (Typeable k, Typeable p, Typeable a, p a)
      => Data (Dict1 (p :: k -> Constraint) (a :: k)) where
  gfoldl _ z Dict1 = z Dict1
  toConstr _ = dictConstr
  gunfold _ z c = case constrIndex c of
    1 -> z Dict1
    _ -> error "gunfold"
  dataTypeOf _ = dictDataType

dictConstr :: Constr
dictConstr = mkConstr dictDataType "Dict1" [] Prefix

dictDataType :: DataType
dictDataType = mkDataType "Numeric.TypedList.Dict1" [dictConstr]

deriving instance Eq (Dict1 (p :: k -> Constraint) (a :: k))
deriving instance Ord (Dict1 (p :: k -> Constraint) (a :: k))
deriving instance Show (Dict1 (p :: k -> Constraint) (a :: k))


-- | A list of dicts for the same constraint over several types.
type DictList (c :: k -> Constraint) (xs :: [k])
  = TypedList (Dict1 c) xs


-- | Pattern matching against this causes `RepresentableList` instance
--   come into scope.
--   Also it allows constructing a term-level list out of a constraint.
pattern TypeList :: forall (k :: Type) (xs :: [k])
                  . () => RepresentableList xs => TypeList xs
pattern TypeList <- (mkRTL -> Dict)
  where
    TypeList = tList @k @xs

-- | Pattern matching against this allows manipulating lists of constraints.
--   Useful when creating functions that change the shape of dimensions.
pattern EvList :: forall (k :: Type) (c :: k -> Constraint) (xs :: [k])
                . () => (All c xs, RepresentableList xs) => DictList c xs
pattern EvList <- (mkEVL -> Dict)
  where
    EvList = _evList (tList @k @xs)

-- | Zero-length type list
pattern U :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
           . () => (xs ~ '[]) => TypedList f xs
pattern U <- (patTL @f @xs -> PatCNil)
  where
    U = unsafeCoerce# []

-- | Zero-length type list; synonym to `U`.
pattern Empty :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
               . () => (xs ~ '[]) => TypedList f xs
pattern Empty = U

-- | Constructing a type-indexed list
pattern (:*) :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
              . ()
             => forall (y :: k) (ys :: [k])
              . (xs ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f xs
pattern (:*) x xs = Cons x xs
infixr 5 :*

-- | Constructing a type-indexed list in the canonical way
pattern Cons :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
              . ()
             => forall (y :: k) (ys :: [k])
              . (xs ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f xs
pattern Cons x xs <- (patTL @f @xs -> PatCons x xs)
  where
    Cons = Numeric.TypedList.cons

-- | Constructing a type-indexed list from the other end
pattern Snoc :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
              . ()
             => forall (sy :: [k]) (y :: k)
              . (xs ~ (sy +: y)) => TypedList f sy -> f y -> TypedList f xs
pattern Snoc sx x <- (unsnocTL @f @xs -> PatSnoc sx x)
  where
    Snoc = Numeric.TypedList.snoc

-- | Reverse a typed list
pattern Reverse :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
                 . ()
                => forall (sx :: [k])
                 . (xs ~ Reverse sx, sx ~ Reverse xs)
                => TypedList f sx -> TypedList f xs
pattern Reverse sx <- (unreverseTL @f @xs -> PatReverse sx)
  where
    Reverse = Numeric.TypedList.reverse

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

last :: TypedList f xs -> f (Last xs)
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

concat :: TypedList f xs
       -> TypedList f ys
       -> TypedList f (xs ++ ys)
concat (TypedList xs) (TypedList ys) = unsafeCoerce# (xs ++ ys)
{-# INLINE concat #-}

stripPrefix :: forall xs ys f
             . ( All Typeable xs, All Typeable ys, All Eq (Map f xs))
            => TypedList f xs
            -> TypedList f ys
            -> Maybe (TypedList f (StripPrefix xs ys))
stripPrefix U ys = Just ys
stripPrefix _ U  = Nothing
stripPrefix ((x :: f x) :* xs) ((y :: f y) :* ys)
  | Just Refl <- eqT @x @y
  , x == y       = unsafeCoerce# (stripPrefix xs ys)
  | otherwise    = Nothing
{-# INLINE stripPrefix #-}

stripSuffix :: forall xs ys f
             . ( All Typeable xs, All Typeable ys, All Eq (Map f xs))
            => TypedList f xs
            -> TypedList f ys
            -> Maybe (TypedList f (StripSuffix xs ys))
stripSuffix U ys = Just ys
stripSuffix _ U  = Nothing
stripSuffix xs ys
  | Just n <- order ys `minusDimM` order xs
  , (zs, xs') <- Numeric.TypedList.splitAt n ys
  , EvList <- Numeric.TypedList.drop n $ _evList @_ @Typeable ys
  , Just (Refl, True) <- sameList xs xs'
                 = Just (unsafeCoerce# zs)
  | otherwise    = Nothing
{-# INLINE stripSuffix #-}

-- | Returns two things at once:
--   (Evidence that types of lists match, value-level equality).
sameList :: ( All Typeable xs, All Typeable ys, All Eq (Map f xs))
         => TypedList f xs
         -> TypedList f ys
         -> Maybe (xs :~: ys, Bool)
sameList U U = Just (Refl, True)
sameList ((x :: f x) :* xs) ((y :: f y) :* ys)
  | Just Refl <- eqT @x @y
  , Just (Refl, b) <- sameList xs ys
    = Just (Refl, x == y && b)
  | otherwise
    = Nothing
sameList _ _ = Nothing

-- | Map a function over contents of a typed list
map :: (forall a . f a -> g a)
    -> TypedList f xs
    -> TypedList g xs
map k (TypedList xs) = unsafeCoerce# (Prelude.map k' xs)
  where
    k' :: Any -> Any
    k' = unsafeCoerce# . k . unsafeCoerce#
{-# INLINE map #-}

-- | Get a constructible `TypeList` from any other `TypedList`;
--   Pattern matching agains the result brings `RepresentableList` constraint
--   into the scope:
--
--   > case types ts of TypeList -> ...
--
types :: TypedList f xs -> TypeList xs
types (TypedList xs) = unsafeCoerce# (Prelude.map (const Proxy) xs)
{-# INLINE types #-}

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
{-# INLINE order' #-}

order :: TypedList f xs -> Dim (Length xs)
order (TypedList xs) = unsafeCoerce# (fromIntegral (Prelude.length xs) :: Word)
{-# INLINE order #-}




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

data PatReverse f xs
  = forall (sx :: [k]) . (xs ~ Reverse sx, sx ~ Reverse xs)
  => PatReverse (TypedList f sx)

unreverseTL :: forall f xs . TypedList f xs -> PatReverse f xs
unreverseTL (TypedList xs)
  = case (unsafeCoerce# (Dict @(xs ~ xs, xs ~ xs))
           :: Dict (xs ~ Reverse sx, sx ~ Reverse xs)
         ) of
      Dict -> PatReverse (unsafeCoerce# (Prelude.reverse xs))
{-# INLINE unreverseTL #-}


mkRTL :: forall (k :: Type) (xs :: [k])
       . TypeList xs
      -> Dict (RepresentableList xs)
mkRTL xs = reifyRepList xs Dict
{-# INLINE mkRTL #-}


data PatSnoc f xs where
  PatSNil :: PatSnoc f '[]
  PatSnoc :: TypedList f ys -> f y -> PatSnoc f (ys +: y)

unsnocTL :: forall f xs . TypedList f xs -> PatSnoc f xs
unsnocTL (TypedList [])
  = case unsafeEqTypes @_ @xs @'[] of
      Dict -> PatSNil
unsnocTL (TypedList (x:xs))
  = case unsafeEqTypes @_ @xs @(Init xs +: Last xs) of
      Dict -> PatSnoc (unsafeCoerce# sy) (unsafeCoerce# y)
  where
    (sy, y) = unsnoc x xs
    unsnoc :: Any -> [Any] -> ([Any], Any)
    unsnoc t []     = ([], t)
    unsnoc t (z:zs) = first (t:) (unsnoc z zs)
{-# INLINE unsnocTL #-}


data PatCons f xs where
  PatCNil :: PatCons f '[]
  PatCons :: f y -> TypedList f ys -> PatCons f (y ': ys)

patTL :: forall f xs . TypedList f xs -> PatCons f xs
patTL (TypedList [])
  = case unsafeEqTypes @_ @xs @'[] of
      Dict -> PatCNil
patTL (TypedList (x : xs))
  = case unsafeEqTypes @_ @xs  @(Head xs ': Tail xs) of
      Dict -> PatCons (unsafeCoerce# x) (unsafeCoerce# xs)
{-# INLINE patTL #-}

intD :: Dim n -> Int
intD = (fromIntegral :: Word -> Int) . unsafeCoerce#


mkEVL :: forall (k :: Type) (c :: k -> Constraint) (xs :: [k])
       . DictList c xs -> Dict (All c xs, RepresentableList xs)
mkEVL U              = Dict
mkEVL (Dict1 :* evs) = case mkEVL evs of Dict -> Dict


_evList :: forall (k :: Type) (c :: k -> Constraint) (xs :: [k]) (f :: (k -> Type))
        . All c xs => TypedList f xs -> DictList c xs
_evList U         = U
_evList (_ :* xs) = case _evList xs of evs -> Dict1 :* evs

unsafeEqTypes :: forall k (a :: k) (b :: k) . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce# (Dict :: Dict (a ~ a))
