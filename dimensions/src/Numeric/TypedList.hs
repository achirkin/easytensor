{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
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
    , TypeList, types, typeables, inferTypeableList
    , order, order'
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
      -- * Deriving Show and Read
    , typedListShowsPrecC, typedListShowsPrec
    , typedListReadPrec, withTypedListReadPrec
    ) where

import           Control.Arrow                   (first)
import           Data.Constraint                 hiding ((***))
import           Data.Data
import           Data.Type.List
import           Data.Type.List.Internal
import           Data.Type.Lits
import           Data.Void
import           GHC.Base                        (Type)
import           GHC.Exts
import           GHC.Generics                    hiding (Infix, Prefix)
import qualified Text.ParserCombinators.ReadPrec as Read
import qualified Text.Read                       as Read
import qualified Text.Read.Lex                   as Read
import qualified Type.Reflection                 as R
import           Unsafe.Coerce                   (unsafeCoerce)

import {-# SOURCE #-} Numeric.Dimensions.Dim (Dim, dimVal, minusDimM)

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

-- | Term-level structure of a @TypedList f xs@ is fully determined by its
--   type @Typeable xs@.
--   Thus, @gunfold@ does not use its last argument (@Constr@) at all,
--   relying on the structure of the type parameter.
instance (Typeable k, Typeable f, Typeable xs, All Data (Map f xs))
      => Data (TypedList (f :: (k -> Type)) (xs :: [k])) where
    gfoldl _ z U         = z U
    gfoldl k z (x :* xs) = case inferTypeableCons @xs of
      Dict -> z (:*) `k` x `k` xs
    gunfold k z _ = case typeables @xs of
        U      -> z U
        _ :* _ -> case inferTypeableCons @xs of Dict -> k (k (z (:*)))
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


type family TypedListRepNil (xs :: [k]) :: (Type -> Type) where
    TypedListRepNil '[]      = C1 ('MetaCons "U" 'PrefixI 'False) U1
    TypedListRepNil (_ ': _) = Rec0 Void

type family TypedListRepCons (f :: (k -> Type)) (xs :: [k]) :: (Type -> Type) where
    TypedListRepCons _ '[]
      = Rec0 Void
    TypedListRepCons f (x ': xs)
      = C1 ('MetaCons ":*" ('InfixI 'RightAssociative 5) 'False)
      ( S1 ('MetaSel 'Nothing 'NoSourceUnpackedness
                     'NoSourceStrictness 'DecidedLazy)
           (Rec0 (f x))
       :*:
        S1 ('MetaSel 'Nothing 'NoSourceUnpackedness
                     'NoSourceStrictness 'DecidedLazy)
           (Rec0 (TypedList f xs))
      )

instance Generic (TypedList (f :: (k -> Type)) (xs :: [k])) where
    type Rep (TypedList f xs) = D1
          ('MetaData "TypedList" "Numeric.TypedList" "dimensions" 'False)
          ( TypedListRepNil xs :+: TypedListRepCons f xs  )
    from U         = M1 (L1 (M1 U1))
    from (x :* xs) = M1 (R1 (M1 (M1 (K1 x) :*: M1 (K1 xs))))
    to (M1 (L1 _))
      | Dict <- unsafeEqTypes @xs @'[] = U
    to (M1 (R1 xxs))
      | Dict <- unsafeEqTypes @xs @(Head xs ': Tail xs)
      , M1 (M1 (K1 x) :*: M1 (K1 xs)) <- xxs = x :* xs

-- | A list of type proxies
type TypeList = (TypedList Proxy :: [k] -> Type)

-- | Same as `Dict`, but allows to separate constraint function from
--   the type it is applied to.
data Dict1 :: (k -> Constraint) -> k -> Type where
    Dict1 :: c a => Dict1 c a
    deriving Typeable

instance (Typeable k, Typeable p, Typeable a, p a)
      => Data (Dict1 (p :: k -> Constraint) (a :: k)) where
  gfoldl _ z Dict1 = z Dict1
  toConstr _ = dictConstr
  gunfold _ z _ = z Dict1
  dataTypeOf _ = dictDataType

dictConstr :: Constr
dictConstr = mkConstr dictDataType "Dict1" [] Prefix

dictDataType :: DataType
dictDataType = mkDataType "Numeric.TypedList.Dict1" [dictConstr]

deriving instance Eq (Dict1 (p :: k -> Constraint) (a :: k))
deriving instance Ord (Dict1 (p :: k -> Constraint) (a :: k))
deriving instance Show (Dict1 (p :: k -> Constraint) (a :: k))


-- | A list of dicts for the same constraint over several types.
type DictList (c :: k -> Constraint)
  = (TypedList (Dict1 c) :: [k] -> Type)


-- | Pattern matching against this causes `RepresentableList` instance
--   come into scope.
--   Also it allows constructing a term-level list out of a constraint.
pattern TypeList :: forall xs . () => RepresentableList xs => TypeList xs
pattern TypeList <- (mkRTL -> Dict)
  where
    TypeList = tList @xs

-- | Pattern matching against this allows manipulating lists of constraints.
--   Useful when creating functions that change the shape of dimensions.
pattern EvList :: forall c xs
                . () => (All c xs, RepresentableList xs) => DictList c xs
pattern EvList <- (mkEVL -> Dict)
  where
    EvList = _evList (tList @xs)

-- | Zero-length type list
pattern U :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
           . () => (xs ~ '[]) => TypedList f xs
pattern U <- (patTL @k @f @xs -> PatCNil)
  where
    U = coerce ([] :: [Any])

-- | Zero-length type list; synonym to `U`.
pattern Empty :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
               . () => (xs ~ '[]) => TypedList f xs
pattern Empty = U

-- | Constructing a type-indexed list
pattern (:*) :: forall f xs
              . ()
             => forall y ys
              . (xs ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f xs
pattern (:*) x xs = Cons x xs
infixr 5 :*

-- | Constructing a type-indexed list in the canonical way
pattern Cons :: forall f xs
              . ()
             => forall y ys
              . (xs ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f xs
pattern Cons x xs <- (patTL @_ @f @xs -> PatCons x xs)
  where
    Cons = Numeric.TypedList.cons

-- | Constructing a type-indexed list from the other end
pattern Snoc :: forall f xs . ()
             => forall sy y . SnocList sy y xs
             => TypedList f sy -> f y -> TypedList f xs
pattern Snoc sx x <- (unsnocTL @_ @f @xs -> PatSnoc sx x)
  where
    Snoc = Numeric.TypedList.snoc

-- | Reverse a typed list
pattern Reverse :: forall f xs . ()
                => forall sx . ReverseList xs sx
                => TypedList f sx -> TypedList f xs
pattern Reverse sx <- (unreverseTL @_ @f @xs -> PatReverse sx)
  where
    Reverse = Numeric.TypedList.reverse

-- | /O(1)/ append an element in front of a @TypedList@ (same as `(:)` for lists).
cons :: forall f x xs
      . f x -> TypedList f xs -> TypedList f (x :+ xs)
cons x xs = TypedList (unsafeCoerce x : coerce xs)
{-# INLINE cons #-}

-- | /O(n)/ append an element to the end of a @TypedList@.
snoc :: forall f xs x
      . TypedList f xs -> f x -> TypedList f (xs +: x)
snoc xs x = TypedList (coerce xs ++ [unsafeCoerce x])
{-# INLINE snoc #-}

-- | /O(n)/ return elements of a @TypedList@ in reverse order.
reverse :: forall f xs
         . TypedList f xs -> TypedList f (Reverse xs)
reverse = coerce (Prelude.reverse :: [Any] -> [Any])
{-# INLINE reverse #-}

-- | /O(1)/ Extract the first element of a @TypedList@, which must be non-empty.
head :: forall f xs
      . TypedList f xs -> f (Head xs)
head (TypedList xs) = unsafeCoerce (Prelude.head xs)
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a @TypedList@,
--   which must be non-empty.
tail :: forall f xs
      . TypedList f xs -> TypedList f (Tail xs)
tail = coerce (Prelude.tail :: [Any] -> [Any])
{-# INLINE tail #-}

-- | /O(n)/ Return all the elements of a @TypedList@ except the last one
--   (the list must be non-empty).
init :: forall f xs
      . TypedList f xs -> TypedList f (Init xs)
init = coerce (Prelude.init :: [Any] -> [Any])
{-# INLINE init #-}

-- | /O(n)/ Extract the last element of a @TypedList@, which must be non-empty.
last :: forall f xs
      . TypedList f xs -> f (Last xs)
last (TypedList xs) = unsafeCoerce (Prelude.last xs)
{-# INLINE last #-}

-- | /O(min(n,k))/ @take k xs@ returns a prefix of @xs@ of length @min(length xs, k)@.
--   It calls `Prelude.take` under the hood, so expect the same behavior.
take :: forall (n :: Nat) f xs
      . Dim n -> TypedList f xs -> TypedList f (Take n xs)
take = coerce (Prelude.take . dimValInt :: Dim n -> [Any] -> [Any])
{-# INLINE take #-}

-- | /O(min(n,k))/ @drop k xs@ returns a suffix of @xs@ of length @max(0, length xs - k)@.
--   It calls `Prelude.drop` under the hood, so expect the same behavior.
drop :: forall (n :: Nat) f xs
      . Dim n -> TypedList f xs -> TypedList f (Drop n xs)
drop = coerce (Prelude.drop . dimValInt :: Dim n -> [Any] -> [Any])
{-# INLINE drop #-}

-- | Return the number of elements in a @TypedList@ (same as `order`).
length :: forall f xs
       . TypedList f xs -> Dim (Length xs)
length = order
{-# INLINE length #-}


-- | /O(min(n,k))/ @splitAt k xs@ has the same effect as @('take' k xs, 'drop' k xs)@.
--   It calls `Prelude.splitAt` under the hood, so expect the same behavior.
splitAt :: forall (n :: Nat) f xs
         . Dim n
        -> TypedList f xs
        -> (TypedList f (Take n xs), TypedList f (Drop n xs))
splitAt = coerce (Prelude.splitAt . dimValInt :: Dim n -> [Any] -> ([Any], [Any]))
{-# INLINE splitAt #-}


-- | Return the number of elements in a type list @xs@ bound by a constraint
--   @RepresentableList xs@ (same as `order`, but takes no value arguments).
order' :: forall xs . RepresentableList xs => Dim (Length xs)
order' = order (tList @xs)
{-# INLINE order' #-}

-- | Return the number of elements in a @TypedList@ (same as `length`).
order :: forall f xs
       . TypedList f xs -> Dim (Length xs)
order = unsafeCoerce (fromIntegral . Prelude.length :: [Any] -> Word)
{-# INLINE order #-}

-- | Concat two @TypedList@s.
--   It calls `Prelude.(++)` under the hood, so expect the same behavior.
concat :: forall f xs ys
        . TypedList f xs
       -> TypedList f ys
       -> TypedList f (xs ++ ys)
concat = coerce ((++) :: [Any] -> [Any] -> [Any])
{-# INLINE concat #-}

-- | Drops the given prefix from a @TypedList@.
--   It returns 'Nothing' if the @TypedList@ does not start with the prefix
--   given, or 'Just' the @TypedList@ after the prefix, if it does.
--   It calls `Prelude.stripPrefix` under the hood, so expect the same behavior.
--
--   This function can be used to find the type-level evidence that one type-level
--   list is indeed a prefix of another.
stripPrefix :: forall f xs ys
             . ( All Typeable xs, All Typeable ys, All Eq (Map f xs))
            => TypedList f xs
            -> TypedList f ys
            -> Maybe (TypedList f (StripPrefix xs ys))
stripPrefix U ys = Just ys
stripPrefix _ U  = Nothing
stripPrefix ((x :: f x) :* xs) ((y :: f y) :* ys)
  | Just Refl <- eqT @x @y
  , x == y       = coerce (stripPrefix xs ys)
  | otherwise    = Nothing
{-# INLINE stripPrefix #-}

-- | Drops the given suffix from a @TypedList@.
--   It returns 'Nothing' if the @TypedList@ does not end with the suffix
--   given, or 'Just' the @TypedList@ before the suffix, if it does.
--
--   This function can be used to find the type-level evidence that one type-level
--   list is indeed a suffix of another.
stripSuffix :: forall f xs ys
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
                 = Just (coerce zs)
  | otherwise    = Nothing
{-# INLINE stripSuffix #-}

-- | Returns two things at once:
--   (Evidence that types of lists match, value-level equality).
sameList :: forall f xs ys
          . ( All Typeable xs, All Typeable ys, All Eq (Map f xs))
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
map :: forall f g xs
     . (forall a . f a -> g a)
    -> TypedList f xs
    -> TypedList g xs
map k = coerce (Prelude.map k')
  where
    k' :: Any -> Any
    k' = unsafeCoerce k
{-# INLINE map #-}

-- | Get a constructible `TypeList` from any other `TypedList`;
--   Pattern matching agains the result brings `RepresentableList` constraint
--   into the scope:
--
--   > case types ts of TypeList -> ...
--
types :: forall f xs
       . TypedList f xs -> TypeList xs
types = Numeric.TypedList.map (const Proxy)
{-# INLINE types #-}

-- | Construct a @TypeList xs@ if there is an instance of @Typeable xs@ around.
--
--   This way, you can always bring `RepresentableList` instance into the scope
--   if you have a `Typeable` instance.
--
typeables :: forall xs . Typeable xs => TypeList xs
typeables = case R.typeRep @xs of
    R.App (R.App _ (_ :: R.TypeRep (n :: k))) (txs :: R.TypeRep (ns :: ks))
      | Dict <- unsafeCoerce (Dict @(k ~ k, ks ~ ks))
                  :: Dict (k ~ KindOfEl xs, ks ~ KindOf xs)
      , Dict <- unsafeEqTypes @xs @(n ': ns)
      -> Proxy @n :* R.withTypeable txs (typeables @ns)
    R.Con _
      -> unsafeCoerce U
    r -> error ("typeables -- impossible typeRep: " ++ show r)
{-# INLINE typeables #-}

-- | If all elements of a @TypedList@ are @Typeable@,
--   then the list of these elements is also @Typeable@.
inferTypeableList :: forall f xs
                   . (Typeable (KindOfEl xs), All Typeable xs)
                  => TypedList f xs -> Dict (Typeable xs)
inferTypeableList U         = Dict
inferTypeableList (_ :* xs) = case inferTypeableList xs of Dict -> Dict

-- | Representable type lists.
--   Allows getting type information about list structure at runtime.
class RepresentableList xs where
  -- | Get type-level constructed list
  tList :: TypeList xs

instance RepresentableList ('[] :: [k]) where
  tList = U

instance RepresentableList xs => RepresentableList (x ': xs :: [k]) where
  tList = Proxy @x :* tList @xs

-- | Generic show function for a @TypedList@.
typedListShowsPrecC :: forall c f xs
                     . All c xs
                    => String
                       -- ^ Override cons symbol
                    -> ( forall x . c x => Int -> f x -> ShowS )
                       -- ^ How to show a single element
                    -> Int -> TypedList f xs -> ShowS
typedListShowsPrecC _ _ _ U = showChar 'U'
typedListShowsPrecC consS elShowsPrec p (x :* xs) = showParen (p >= 6)
    $ elShowsPrec 6 x
    . showChar ' ' . showString consS . showChar ' '
    . typedListShowsPrecC @c @f consS elShowsPrec 5 xs

-- | Generic show function for a @TypedList@.
typedListShowsPrec :: forall f xs
                    . ( forall x . Int -> f x -> ShowS )
                      -- ^ How to show a single element
                   -> Int -> TypedList f xs -> ShowS
typedListShowsPrec _ _ U = showChar 'U'
typedListShowsPrec elShowsPrec p (x :* xs) = showParen (p >= 6) $
    elShowsPrec 6 x . showString " :* " . typedListShowsPrec @f elShowsPrec 5 xs

-- | Generic read function for a @TypedList@.
--   Requires a "template" to enforce the structure of the type list.
typedListReadPrec :: forall c f xs g
                   . All c xs
                  => String
                     -- ^ Override cons symbol
                  -> ( forall x . c x => Read.ReadPrec (f x) )
                     -- ^ How to read a single element
                  -> TypedList g xs
                     -- ^ Enforce the type structure of the result
                  -> Read.ReadPrec (TypedList f xs)
typedListReadPrec _ _ U = Read.parens $ U <$ Read.lift (Read.expect $ Read.Ident "U")
typedListReadPrec consS elReadPrec (_ :* ts) = Read.parens . Read.prec 5 $ do
    x <- Read.step elReadPrec
    Read.lift . Read.expect $ Read.Symbol consS
    xs <- typedListReadPrec @c consS elReadPrec ts
    return (x :* xs)

-- | Generic read function for a @TypedList@ of unknown length.
withTypedListReadPrec :: forall f (r :: Type)
                       . (forall (z :: Type) .
                            ( forall x . f x -> z) -> Read.ReadPrec z )
                         -- ^ How to read a single element
                      -> (forall xs . TypedList f xs -> r )
                         -- ^ Consume the result
                      -> Read.ReadPrec r
withTypedListReadPrec withElReadPrec use = Read.parens $
    (use U <$ Read.lift (Read.expect $ Read.Ident "U"))
    Read.+++
    Read.prec 5 (do
      WithAnyTL withX <- Read.step $ withElReadPrec (\x -> WithAnyTL $ use . (x :*))
      Read.lift . Read.expect $ Read.Symbol ":*"
      withTypedListReadPrec @f @r withElReadPrec withX
    )

-- Workaround impredicative polymorphism
newtype WithAnyTL (f :: k -> Type) (r :: Type)
  = WithAnyTL (forall (xs :: [k]) . TypedList f xs -> r)

--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------


-- | This function does GHC's magic to convert user-supplied `tList` function
--   to create an instance of `RepresentableList` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyRepList :: forall (k :: Type) (xs :: [k]) (r :: Type)
              . TypeList xs
             -> (RepresentableList xs => r)
             -> r
reifyRepList tl k = unsafeCoerce (MagicRepList k :: MagicRepList xs r) tl
{-# INLINE reifyRepList #-}
newtype MagicRepList xs r = MagicRepList (RepresentableList xs => r)

data PatReverse (f :: k -> Type) (xs :: [k])
  = forall (sx :: [k]) . ReverseList xs sx => PatReverse (TypedList f sx)

unreverseTL :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
             . TypedList f xs -> PatReverse f xs
unreverseTL xs
  = case unsafeEqTypes @xs @(Reverse (Reverse xs)) of
      Dict -> PatReverse @k @f @xs @(Reverse xs) (Numeric.TypedList.reverse xs)
{-# INLINE unreverseTL #-}


mkRTL :: forall (k :: Type) (xs :: [k])
       . TypeList xs
      -> Dict (RepresentableList xs)
mkRTL xs = reifyRepList xs Dict
{-# INLINE mkRTL #-}


data PatSnoc (f :: k -> Type) (xs :: [k]) where
  PatSNil :: PatSnoc f '[]
  PatSnoc :: SnocList xs s xss => TypedList f xs -> f s -> PatSnoc f xss

unsnocTL :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
          . TypedList f xs -> PatSnoc f xs
unsnocTL (TypedList [])
  = case unsafeEqTypes @xs @'[] of
      Dict -> PatSNil
unsnocTL (TypedList (x:xs))
  = case unsafeEqTypes @xs @(Init xs +: Last xs) of
      Dict -> PatSnoc (coerce sy) (unsafeCoerce y)
  where
    (sy, y) = unsnoc x xs
    unsnoc :: Any -> [Any] -> ([Any], Any)
    unsnoc t []     = ([], t)
    unsnoc t (z:zs) = first (t:) (unsnoc z zs)
{-# INLINE unsnocTL #-}


data PatCons (f :: k -> Type) (xs :: [k]) where
  PatCNil :: PatCons f '[]
  PatCons :: f y -> TypedList f ys -> PatCons f (y ': ys)

patTL :: forall (k :: Type) (f :: k -> Type) (xs :: [k])
       . TypedList f xs -> PatCons f xs
patTL (TypedList [])
  = case unsafeEqTypes @xs @'[] of
      Dict -> PatCNil
patTL (TypedList (x : xs))
  = case unsafeEqTypes @xs  @(Head xs ': Tail xs) of
      Dict -> PatCons (unsafeCoerce x) (coerce xs)
{-# INLINE patTL #-}

mkEVL :: forall (k :: Type) (c :: k -> Constraint) (xs :: [k])
       . DictList c xs -> Dict (All c xs, RepresentableList xs)
mkEVL U              = Dict
mkEVL (Dict1 :* evs) = case mkEVL evs of Dict -> Dict


_evList :: forall (k :: Type) (c :: k -> Constraint) (xs :: [k]) (f :: (k -> Type))
        . All c xs => TypedList f xs -> DictList c xs
_evList U         = U
_evList (_ :* xs) = case _evList xs of evs -> Dict1 :* evs


dimValInt :: forall (k :: Type) (x :: k) . Dim x -> Int
dimValInt = fromIntegral . dimVal
