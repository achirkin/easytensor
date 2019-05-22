{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.ProductOrd
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Compare product types -- partial order.
--
-----------------------------------------------------------------------------
module Numeric.ProductOrd (ProductOrder (..), PartialOrdering (..), fromOrdering) where


import Data.Data
import Data.Kind      (Type)
import Data.Monoid    as Mon (Monoid (..))
import Data.Semigroup as Sem (Semigroup (..), stimesIdempotentMonoid)
import GHC.Generics

import Numeric.TypedList

-- | Partial order for comparing comparing product types
--     ([product order](https://en.wikipedia.org/wiki/Product_order)).
class ProductOrder a where
    -- | Same as `compare`, but may return @Incomparable@.
    cmp :: a -> a -> PartialOrdering

-- | Similar to `Ordering`, but may be @Incomparable@.
data PartialOrdering = PLT | PEQ | PGT | Incomparable
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )

-- | Extend `Ordering` with @Incomparable@ option.
fromOrdering :: Ordering -> PartialOrdering
fromOrdering LT = PLT
fromOrdering EQ = PEQ
fromOrdering GT = PGT
{-# INLINE fromOrdering #-}

instance Sem.Semigroup PartialOrdering where
    Incomparable <> _ = Incomparable
    _ <> Incomparable = Incomparable
    PLT <> PGT = Incomparable
    PGT <> PLT = Incomparable
    PLT <> _ = PLT
    PGT <> _ = PGT
    PEQ <> y = y

    stimes = stimesIdempotentMonoid

instance Mon.Monoid PartialOrdering where
    mempty = PEQ
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance All Ord (Map f xs)
      => ProductOrder (TypedList (f :: k -> Type) (xs :: [k])) where
    cmp U U                 = PEQ
    cmp (a :* as) (b :* bs) = fromOrdering (compare a b) <> cmp as bs


cmp' :: Ord a => a -> a -> PartialOrdering
cmp' a b = fromOrdering (compare a b)
{-# INLINE cmp' #-}

instance (Ord a1, Ord a2)
      => ProductOrder (a1, a2) where
    cmp (a1, a2)
        (b1, b2)
      =  cmp' a1 b1 <> cmp' a2 b2

instance (Ord a1, Ord a2, Ord a3)
      => ProductOrder (a1, a2, a3) where
    cmp (a1, a2, a3)
        (b1, b2, b3)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3

instance (Ord a1, Ord a2, Ord a3, Ord a4)
      => ProductOrder (a1, a2, a3, a4) where
    cmp (a1, a2, a3, a4)
        (b1, b2, b3, b4)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
      <> cmp' a4 b4

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5)
      => ProductOrder (a1, a2, a3, a4, a5) where
    cmp (a1, a2, a3, a4, a5)
        (b1, b2, b3, b4, b5)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
      <> cmp' a4 b4 <> cmp' a5 b5

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6)
      => ProductOrder (a1, a2, a3, a4, a5, a6) where
    cmp (a1, a2, a3, a4, a5, a6)
        (b1, b2, b3, b4, b5, b6)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
      <> cmp' a4 b4 <> cmp' a5 b5 <> cmp' a6 b6

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7)
      => ProductOrder (a1, a2, a3, a4, a5, a6, a7) where
    cmp (a1, a2, a3, a4, a5, a6, a7)
        (b1, b2, b3, b4, b5, b6, b7)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
      <> cmp' a4 b4 <> cmp' a5 b5 <> cmp' a6 b6
      <> cmp' a7 b7

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7, Ord a8)
      => ProductOrder (a1, a2, a3, a4, a5, a6, a7, a8) where
    cmp (a1, a2, a3, a4, a5, a6, a7, a8)
        (b1, b2, b3, b4, b5, b6, b7, b8)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
      <> cmp' a4 b4 <> cmp' a5 b5 <> cmp' a6 b6
      <> cmp' a7 b7 <> cmp' a8 b8

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7, Ord a8, Ord a9)
      => ProductOrder (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    cmp (a1, a2, a3, a4, a5, a6, a7, a8, a9)
        (b1, b2, b3, b4, b5, b6, b7, b8, b9)
      =  cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
      <> cmp' a4 b4 <> cmp' a5 b5 <> cmp' a6 b6
      <> cmp' a7 b7 <> cmp' a8 b8 <> cmp' a9 b9
