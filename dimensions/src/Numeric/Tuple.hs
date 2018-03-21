{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Tuple
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
module Numeric.Tuple
    ( TypedList (U, (:*), (:*:)), Tuple
    , (!*!), (!*)
    ) where

-- import           GHC.Generics
import Data.Proxy
import           GHC.Prim
import           GHC.Types
import           GHC.TypeLits
import           Data.Functor.Identity
import           Data.Semigroup        (Monoid (..), Semigroup (..))
import qualified GHC.Read              as Read
-- import           Numeric.TypeLits
import qualified Text.Read             as Read

import Numeric.Dim
import Numeric.Type.List


type Tuple (xs :: [Type]) = TypedList Identity xs

-- | Enforce arguments before applying constructor
(!*!) :: x -> Tuple xs -> Tuple (x ': xs)
x !*! xs = x `seq` xs `seq` unsafeCoerce# x :* xs
{-# INLINE (!*!) #-}
infixr 5 !*!

-- | Enforce arguments before applying constructor
(!*) :: f x -> TypedList f xs -> TypedList f (x ': xs)
x !* xs = x `seq` xs `seq` x :* xs
{-# INLINE (!*) #-}
infixr 5 !*


pattern (:*:) :: x -> Tuple xs -> Tuple (x ': xs)
pattern (:*:) x xs = Identity x :* xs
infixr 5 :*:





-- inferRepTypeList :: Tuple a xs -> Evidence (RepTypeList xs)
-- inferRepTypeList U = Evidence
-- inferRepTypeList (_ :* xs)
--   | Evidence <- inferRepTypeList xs = Evidence


instance (All Semigroup xs) => Semigroup (Tuple xs) where
    U <> U = U
    (x :* xs) <> (y :* ys)
      =   (x `seq` y `seq` x <> y)
      !* (xs `seq` ys `seq` xs <> ys)

instance (RepresentableList xs, All Monoid xs) => Monoid (Tuple xs) where
    mempty = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Monoid ys => TypeList ys -> Tuple ys
        go U         = U
        go (_ :* xs) = mempty !* go xs
    mappend = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Monoid ys
            => TypeList ys
            -> Tuple ys
            -> Tuple ys
            -> Tuple ys
        go U _ _ = U
        go (_ :* ts) (x :* xs) (y :* ys) =
          (x `seq` y `seq` mappend x y) !* go ts xs ys


instance (RepresentableList xs, All Bounded xs) => Bounded (Tuple xs) where
    minBound = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Bounded ys => TypeList ys -> Tuple ys
        go U         = U
        go (_ :* xs) = minBound !* go xs
    maxBound = go (tList @Type @xs)
      where
        go :: forall (ys :: [Type])
            . All Bounded ys => TypeList ys -> Tuple ys
        go U         = U
        go (_ :* xs) = maxBound !* go xs

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






--
-- type Dims (xs :: [k]) = Tuple Dim xs
--
-- -- | Same as SomeNat, but for Dimensions:
-- --   Hide all information about Dimensions inside
-- data SomeDims = forall (ns :: [Nat]) . SomeDims (Dim ns)
