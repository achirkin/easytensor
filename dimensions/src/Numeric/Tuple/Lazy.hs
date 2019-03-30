{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Tuple.Lazy
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-----------------------------------------------------------------------------
module Numeric.Tuple.Lazy
    ( Id (..), Tuple
    , TypedList (U, (:*), (:$), (:!), Empty, TypeList, Cons, Snoc, Reverse)
    , (*$), ($*), (*!), (!*)
    ) where


import           Control.Arrow     (first)
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Bits         (Bits, FiniteBits)
import           Data.Coerce
import           Data.Data         (Data)
import           Data.Foldable
import           Data.Ix           (Ix)
import           Data.Monoid       as Mon (Monoid (..))
import           Data.Semigroup    as Sem (Semigroup (..))
import           Data.String       (IsString)
import           Foreign.Storable  (Storable)
import           GHC.Base          (Type)
import           GHC.Exts
import           GHC.Generics      (Generic, Generic1)
import qualified GHC.Read          as Read
import qualified Text.Read         as Read

import           Numeric.Type.List
import           Numeric.TypedList

-- | This is an almost complete copy of `Data.Functor.Identity`
--   by (c) Andy Gill 2001.
newtype Id a = Id { runId :: a }
    deriving ( Bits, Bounded, Data, Enum, Eq, FiniteBits, Floating, Fractional
             , Generic, Generic1, Integral, IsString, Ix, Monoid, Num, Ord
             , Real, RealFrac, RealFloat , Semigroup, Storable, Traversable)


instance (Read a) => Read (Id a) where
    readsPrec d = fmap (first Id) . readsPrec d

instance (Show a) => Show (Id a) where
    showsPrec d = showsPrec d . runId

instance Foldable Id where
    foldMap          = coerce
    elem             = (. runId) #. (==)
    foldl            = coerce
    foldl'           = coerce
    foldl1 _         = runId
    foldr f z (Id x) = f x z
    foldr'           = foldr
    foldr1 _         = runId
    length _         = 1
    maximum          = runId
    minimum          = runId
    null _           = False
    product          = runId
    sum              = runId
    toList (Id x)    = [x]

instance Functor Id where
    fmap     = coerce

instance Applicative Id where
    pure     = Id
    (<*>)    = coerce

instance Monad Id where
    m >>= k  = k (runId m)

instance MonadFix Id where
    mfix f   = Id (fix (runId . f))

instance MonadZip Id where
    mzipWith = coerce
    munzip   = coerce




-- | A tuple indexed by a list of types
type Tuple (xs :: [Type]) = TypedList Id xs

{-# COMPLETE U, (:$) #-}
{-# COMPLETE U, (:!) #-}
{-# COMPLETE Empty, (:$) #-}
{-# COMPLETE Empty, (:!) #-}


-- | Constructing a type-indexed list
pattern (:$) :: forall (xs :: [Type])
              . ()
             => forall (y :: Type) (ys :: [Type])
              . (xs ~ (y ': ys)) => y -> Tuple ys -> Tuple xs
pattern (:$) x xs <- (Id x :* xs)
  where
    (:$) = (*$)
infixr 5 :$

-- | Constructing a type-indexed list
pattern (:!) :: forall (xs :: [Type])
              . ()
             => forall (y :: Type) (ys :: [Type])
              . (xs ~ (y ': ys)) => y -> Tuple ys -> Tuple xs
pattern (:!) x xs <- (forceCons -> Id x :* xs)
  where
    (:!) = (*!)
infixr 5 :!


-- | Grow a tuple on the left O(1).
(*$) :: x -> Tuple xs -> Tuple (x :+ xs)
(*$) x xs = unsafeCoerce# (unsafeCoerce# x : unsafeCoerce# xs :: [Any])
{-# INLINE (*$) #-}
infixr 5 *$

-- | Grow a tuple on the left while evaluating arguments to WHNF O(1).
(*!) :: x -> Tuple xs -> Tuple (x :+ xs)
(*!) !x !xs = let !r = unsafeCoerce# x : unsafeCoerce# xs :: [Any]
              in unsafeCoerce# r
{-# INLINE (*!) #-}
infixr 5 *!

-- | Grow a tuple on the right.
--   Note, it traverses an element list inside O(n).
($*) :: Tuple xs -> x -> Tuple (xs +: x)
($*) xs x = unsafeCoerce# (unsafeCoerce# xs ++ [unsafeCoerce# x] :: [Any])
{-# INLINE ($*) #-}
infixl 5 $*

-- | Grow a tuple on the right while evaluating arguments to WHNF.
--   Note, it traverses an element list inside O(n).
(!*) :: Tuple xs -> x -> Tuple (xs +: x)
(!*) !xs !x = let !r = go (unsafeCoerce# x) (unsafeCoerce# xs) :: [Any]
                  go :: Any -> [Any] -> [Any]
                  go z []       = z `seq` [z]
                  go z (y : ys) = y `seq` y : go z ys
              in unsafeCoerce# r
{-# INLINE (!*) #-}
infixl 5 !*


instance All Semigroup xs => Sem.Semigroup (Tuple xs) where
    U <> U = U
    (x :$ xs) <> (y :$ ys) = (x <> y) *$ ( xs <> ys)

instance ( RepresentableList xs
         , All Semigroup xs
         , All Monoid xs) => Mon.Monoid (Tuple xs) where
    mempty = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Monoid ys => TypeList ys -> Tuple ys
        go U         = U
        go (_ :* xs) = mempty *$ go xs
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif


instance (RepresentableList xs, All Bounded xs) => Bounded (Tuple xs) where
    minBound = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Bounded ys => TypeList ys -> Tuple ys
        go U         = U
        go (_ :* xs) = minBound *$ go xs
    maxBound = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Bounded ys => TypeList ys -> Tuple ys
        go U         = U
        go (_ :* xs) = maxBound *$ go xs

instance All Eq xs => Eq (Tuple xs) where
    (==) U U                 = True
    (==) (x :* tx) (y :* ty) = x == y && tx == ty
    (/=) U U                 = False
    (/=) (x :* tx) (y :* ty) = x /= y || tx /= ty

-- | Ord instance of the Tuple implements inverse lexicorgaphic ordering.
--   That is, the last element in the tuple is the most significant one.
--
--   Note, this will never work on infinite-dimensional tuples!
instance (All Eq xs, All Ord xs) => Ord (Tuple xs) where
    compare U U                 = EQ
    compare (x :* tx) (y :* ty) = compare tx ty <> compare x y

instance All Show xs => Show (Tuple xs) where
    show U         = "U"
    show (x :* xs) = show x ++ " :* " ++ show xs
    showsPrec _ U = showString "U"
    showsPrec p (x :* xs) = showParen (p >= 5)
                          $ showsPrec 5 x
                          . showString " :* "
                          . showsPrec 5 xs

instance (RepresentableList xs, All Read xs) => Read (Tuple xs) where
    readPrec = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Read ys => TypeList ys -> Read.ReadPrec (Tuple ys)
        go U = U <$ Read.expectP (Read.Symbol "U")
        go (_ :* ts) = Read.parens $ Read.prec 5 $ do
          x <- Read.step Read.readPrec
          Read.expectP (Read.Symbol ":*")
          xs <- Read.step $ go ts
          return (x :* xs)



--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------


-- | Internal (non-exported) 'Coercible' helper for 'elem'
--
-- See Note [Function coercion] in "Data.Foldable" for more details.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce

forceCons :: Tuple xs -> Tuple xs
forceCons U            = U
forceCons (Id x :* xs) = x `seq` xs `seq` (Id x :* xs)
