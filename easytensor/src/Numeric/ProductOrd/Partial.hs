{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.ProductOrd.Partial
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Compare product types using partial `Ord` instances:
--
-- *  if nor \( a > b \), nor \( b > a \), neither, \( a = b \),
--      then @compare a b == undefined@
--
-- To remind yourself that `ProductOrd` is partial, you may
--  import it qualified, e.g.
--
-- >  import qualified Numeric.ProductOrd.Partial as Partial
--
-----------------------------------------------------------------------------
module Numeric.ProductOrd.Partial (ProductOrd (..), toOrdering) where

import Control.Monad.Fix
import Control.Monad.Zip
import Data.Bits         (Bits, FiniteBits)
import Data.Coerce
import Data.Data
import Data.Foldable
import Data.Kind         (Type)
import Foreign.Storable  (Storable)
import GHC.Generics

import Numeric.ProductOrd
import Numeric.TypedList

{-|
  Redefine `Ord` instance for a type which is a cartesian product --
    as a partial __[product order](https://en.wikipedia.org/wiki/Product_order)__.

  Since vanilla Haskell `Ord` class is always about total order, @ProductOrd@
  instance is not particularly correct.
  However, it turns out to be very useful for comparing vector or tuple-like types.

  The implementation of `ProductOrd` in this module
  workarounds this by using a __partial @compare@ function in an `Eq` instance__:
  \[
    \neg (a > b) \land \neg (b > a) \land \neg (a = b)
      \Rightarrow \mathtt{compare\ a\ b == undefined}
  \]

  Another inconsistency with the Haskell Report is the `min` and `max` functions;
  these are simply element-wise minimum and maximum here.
  Thus, these instances preserve important properties like
    @min a b <= a && min a b <= b@, but do not preserve
  a property that @min a b == a || min a b == b@.

  All of this is really useful in geometry applications and for calculating things
  like [Pareto dominance](https://en.wikipedia.org/wiki/Pareto_efficiency),
  but should be used with care.
  In particular, never use @ProductOrd@ as a key to a @Set@ or a @Map@!
 -}
newtype ProductOrd a = ProductOrd { getProductOrd :: a }
  deriving ( Eq, Show, Read, Data, Typeable, Generic, Generic1
           , Num, Enum, Bounded, Floating, Fractional
           , Semigroup, Monoid, Storable, Traversable
           , Bits, FiniteBits)

deriving instance (Ord (ProductOrd a), Integral a)   => Integral (ProductOrd a)
deriving instance (Ord (ProductOrd a), Real a)       => Real (ProductOrd a)
deriving instance (Ord (ProductOrd a), RealFrac a)   => RealFrac (ProductOrd a)
deriving instance (Ord (ProductOrd a), RealFloat a)  => RealFloat (ProductOrd a)

instance Foldable ProductOrd where
    foldMap                  = coerce
    elem                     = k (==)
      where
        k :: (a -> a -> Bool) -> a -> ProductOrd a -> Bool
        k = coerce
    foldl                    = coerce
    foldl'                   = coerce
    foldl1 _                 = coerce
    foldr f z (ProductOrd x) = f x z
    foldr'                   = foldr
    foldr1 _                 = coerce
    length _                 = 1
    maximum                  = coerce
    minimum                  = coerce
    null _                   = False
    product                  = coerce
    sum                      = coerce
    toList (ProductOrd x)    = [x]

instance Functor ProductOrd where
    fmap  = coerce

instance Applicative ProductOrd where
    pure  = ProductOrd
    (<*>) = coerce

instance Monad ProductOrd where
    m >>= k = k (getProductOrd m)

instance MonadFix ProductOrd where
    mfix f   = ProductOrd (fix (getProductOrd . f))

instance MonadZip ProductOrd where
    mzipWith = coerce
    munzip   = coerce

instance {-# INCOHERENT #-}
         All Ord (Map f xs)
      => Eq (ProductOrd (TypedList (f :: k -> Type) (xs :: [k]))) where
    ProductOrd U == ProductOrd U = True
    ProductOrd (a :* as) == ProductOrd (b :* bs)
      = a == b && ProductOrd as == ProductOrd bs

instance All Ord (Map f xs)
      => Ord (ProductOrd (TypedList (f :: k -> Type) (xs :: [k]))) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd U >= ProductOrd U = True
    ProductOrd (a :* as) >= ProductOrd (b :* bs)
      = a >= b && ProductOrd as >= ProductOrd bs
    ProductOrd U <= ProductOrd U = True
    ProductOrd (a :* as) <= ProductOrd (b :* bs)
      = a <= b && ProductOrd as <= ProductOrd bs
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    min (ProductOrd U) (ProductOrd U) = ProductOrd U
    min (ProductOrd (a :* as)) (ProductOrd (b :* bs))
      = ProductOrd (min a b :* getProductOrd (min (ProductOrd as) (ProductOrd bs)))
    max (ProductOrd U) (ProductOrd U) = ProductOrd U
    max (ProductOrd (a :* as)) (ProductOrd (b :* bs))
      = ProductOrd (max a b :* getProductOrd (max (ProductOrd as) (ProductOrd bs)))

instance (Ord a1, Ord a2) => Ord (ProductOrd (a1, a2)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2) >= ProductOrd (b1, b2)
      = a1 >= b1 && a2 >= b2
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2) <= ProductOrd (b1, b2)
      = a1 <= b1 && a2 <= b2
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2)) (ProductOrd (b1, b2))
      = ProductOrd (min a1 b1, min a2 b2)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2)) (ProductOrd (b1, b2))
      = ProductOrd (max a1 b1, max a2 b2)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3) => Ord (ProductOrd (a1, a2, a3)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3) >= ProductOrd (b1, b2, b3)
      = a1 >= b1 && a2 >= b2 && a3 >= b3
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3) <= ProductOrd (b1, b2, b3)
      = a1 <= b1 && a2 <= b2 && a3 <= b3
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3)) (ProductOrd (b1, b2, b3))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3)) (ProductOrd (b1, b2, b3))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3, Ord a4)
      => Ord (ProductOrd (a1, a2, a3, a4)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3, a4) >= ProductOrd (b1, b2, b3, b4)
      = a1 >= b1 && a2 >= b2 && a3 >= b3 && a4 >= b4
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3, a4) <= ProductOrd (b1, b2, b3, b4)
      = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3, a4)) (ProductOrd (b1, b2, b3, b4))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3, min a4 b4)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3, a4)) (ProductOrd (b1, b2, b3, b4))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3, max a4 b4)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5)
      => Ord (ProductOrd (a1, a2, a3, a4, a5)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3, a4, a5) >= ProductOrd (b1, b2, b3, b4, b5)
      = a1 >= b1 && a2 >= b2 && a3 >= b3 && a4 >= b4 && a5 >= b5
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3, a4, a5) <= ProductOrd (b1, b2, b3, b4, b5)
      = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4 && a5 <= b5
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3, a4, a5)) (ProductOrd (b1, b2, b3, b4, b5))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3, min a4 b4, min a5 b5)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3, a4, a5)) (ProductOrd (b1, b2, b3, b4, b5))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3, max a4 b4, max a5 b5)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6)
      => Ord (ProductOrd (a1, a2, a3, a4, a5, a6)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6) >= ProductOrd (b1, b2, b3, b4, b5, b6)
      = a1 >= b1 && a2 >= b2 && a3 >= b3 && a4 >= b4 && a5 >= b5 && a6 >= b6
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6) <= ProductOrd (b1, b2, b3, b4, b5, b6)
      = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4 && a5 <= b5 && a6 <= b6
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3, a4, a5, a6)) (ProductOrd (b1, b2, b3, b4, b5, b6))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3, min a4 b4, min a5 b5, min a6 b6)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3, a4, a5, a6)) (ProductOrd (b1, b2, b3, b4, b5, b6))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3, max a4 b4, max a5 b5, max a6 b6)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7)
      => Ord (ProductOrd (a1, a2, a3, a4, a5, a6, a7)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6, a7) >= ProductOrd (b1, b2, b3, b4, b5, b6, b7)
      = a1 >= b1 && a2 >= b2 && a3 >= b3 && a4 >= b4 && a5 >= b5 && a6 >= b6 && a7 >= b7
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6, a7) <= ProductOrd (b1, b2, b3, b4, b5, b6, b7)
      = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4 && a5 <= b5 && a6 <= b6 && a7 <= b7
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3, a4, a5, a6, a7)) (ProductOrd (b1, b2, b3, b4, b5, b6, b7))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3, min a4 b4, min a5 b5, min a6 b6, min a7 b7)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3, a4, a5, a6, a7)) (ProductOrd (b1, b2, b3, b4, b5, b6, b7))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3, max a4 b4, max a5 b5, max a6 b6, max a7 b7)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7, Ord a8)
      => Ord (ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8) >= ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8)
      = a1 >= b1 && a2 >= b2 && a3 >= b3 && a4 >= b4 && a5 >= b5 && a6 >= b6 && a7 >= b7 && a8 >= b8
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8) <= ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8)
      = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4 && a5 <= b5 && a6 <= b6 && a7 <= b7 && a8 <= b8
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8)) (ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3, min a4 b4, min a5 b5, min a6 b6, min a7 b7, min a8 b8)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8)) (ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3, max a4 b4, max a5 b5, max a6 b6, max a7 b7, max a8 b8)
    {-# INLINE max #-}

instance (Ord a1, Ord a2, Ord a3, Ord a4, Ord a5, Ord a6, Ord a7, Ord a8, Ord a9)
      => Ord (ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8, a9)) where
    ProductOrd x > ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    ProductOrd x < ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8, a9) >= ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8, b9)
      = a1 >= b1 && a2 >= b2 && a3 >= b3 && a4 >= b4 && a5 >= b5 && a6 >= b6 && a7 >= b7 && a8 >= b8 && a9 >= b9
    {-# INLINE (>=) #-}
    ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8, a9) <= ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8, b9)
      = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4 && a5 <= b5 && a6 <= b6 && a7 <= b7 && a8 <= b8 && a9 <= b9
    {-# INLINE (<=) #-}
    compare (ProductOrd a) (ProductOrd b) = toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8, a9)) (ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8, b9))
      = ProductOrd (min a1 b1, min a2 b2, min a3 b3, min a4 b4, min a5 b5, min a6 b6, min a7 b7, min a8 b8, min a9 b9)
    {-# INLINE min #-}
    max (ProductOrd (a1, a2, a3, a4, a5, a6, a7, a8, a9)) (ProductOrd (b1, b2, b3, b4, b5, b6, b7, b8, b9))
      = ProductOrd (max a1 b1, max a2 b2, max a3 b3, max a4 b4, max a5 b5, max a6 b6, max a7 b7, max a8 b8, max a9 b9)
    {-# INLINE max #-}

-- | Treat `Incomparable` as error (partial function).
toOrdering :: PartialOrdering -> Ordering
toOrdering PLT = LT
toOrdering PGT = GT
toOrdering PEQ = EQ
toOrdering Incomparable = error "incomparable items (this is a partial function)"
