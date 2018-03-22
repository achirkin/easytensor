{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dim
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This module is based on `GHC.TypeLits` and re-exports its functionality.
-- It provides `KnownDim` class that is similar to `KnownNat`, but keeps
-- `Int`s instead of `Integer`s;
-- Also it provide `Dim` data family serving as a customized `Proxy` type.
-- A set of utility functions provide inference functionality, so
-- that `KnownDim` can be preserved over some type-level operations.
--
-----------------------------------------------------------------------------
module Numeric.Dim
  ( XNat (..), XN, N
  , Dim (D, Dn, Dx), SomeDim
  , KnownDim (..), dimVal, dimVal', someDimVal, sameDim
    -- * Simple Dim arithmetics
  , plusDim, minusDim, minusDimM, timesDim, powerDim
    -- * Re-export part of `GHC.TypeLits` for convenience
  , Nat, CmpNat, (<=), (+), (-), (*), (^)
  ) where


import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.Exts              (Constraint, Proxy#, proxy#,
                                        unsafeCoerce#)
import           GHC.TypeLits

import           Numeric.Type.Evidence


-- | Either known or unknown at compile-time natural number
data XNat = XN Nat | N Nat
-- | Unknown natural number, known to be not smaller than the given Nat
type XN (n::Nat) = 'XN n
-- | Known natural number
type N (n::Nat) = 'N n

-- | Same as `SomeNat`
type SomeDim = Dim ('XN 0)


-- | `Proxy` type to store type-level dimension info
newtype Dim (x :: k) = Dim Word
-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE D #-}
{-# COMPLETE Dn #-}
{-# COMPLETE Dx #-}
#endif

-- | Statically known `XNat`
pattern D :: forall (n :: Nat) . () => KnownDim n => Dim n
pattern D <- (dimEv -> E)
  where
    D = dim @n

-- | Statically known `XNat`
pattern Dn :: forall (n :: Nat) . () => KnownDim n => Dim n -> Dim (N n)
pattern Dn k <- (dimNEv -> (# E, k #))
  where
    Dn k = unsafeCoerce# k

-- | `XNat` that is unknown at compile time.
--   Same as `SomeNat`, but for a dimension:
--   Hide dimension size inside, but allow specifying its minimum possible value.
pattern Dx :: forall (m :: Nat) (n :: Nat)
            . () => (MinDim m n, KnownDim n) => Dim n -> Dim (XN m)
pattern Dx k <- (dimXNEv @m @n -> (# E, k #))
  where
    Dx k = unsafeCoerce# k

-- | This class gives the int associated with a type-level natural.
--   Valid known dim must be not less than 0.
class KnownDim (n :: Nat) where
    -- | Get value of type-level dim at runtime
    dim :: Dim n

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal :: Dim (x :: k) -> Word
dimVal = unsafeCoerce#
{-# INLINE dimVal #-}

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal' :: forall n . KnownDim n => Word
dimVal' = unsafeCoerce# (dim @n)
{-# INLINE dimVal' #-}

-- | Friendly error message if `m <= n` constraint is not satisfied
type family MinDim (m :: Nat) (n :: Nat) :: Constraint where
  MinDim m n =
    If (CmpNat m n == 'GT)
       (TypeError
         ('Text "Minimum Dim size constraint ("
            ':<>: 'ShowType m
            ':<>: 'Text " <= "
            ':<>: 'ShowType n
            ':<>: 'Text ") is not satisfied."
         ':$$: 'Text "Minimum Dim: " ':<>: 'ShowType m
         ':$$: 'Text " Actual Dim: " ':<>: 'ShowType n
         ) :: Constraint
       )
       (m <= n)

instance {-# OVERLAPPABLE #-} KnownNat n => KnownDim n where
    {-# INLINE dim #-}
    dim = Dim (fromInteger (natVal' (proxy# :: Proxy# n)))

instance {-# OVERLAPPING #-} KnownDim 0  where
  { {-# INLINE dim #-}; dim = Dim 0 }
instance {-# OVERLAPPING #-} KnownDim 1  where
  { {-# INLINE dim #-}; dim = Dim 1 }
instance {-# OVERLAPPING #-} KnownDim 2  where
  { {-# INLINE dim #-}; dim = Dim 2 }
instance {-# OVERLAPPING #-} KnownDim 3  where
  { {-# INLINE dim #-}; dim = Dim 3 }
instance {-# OVERLAPPING #-} KnownDim 4  where
  { {-# INLINE dim #-}; dim = Dim 4 }
instance {-# OVERLAPPING #-} KnownDim 5  where
  { {-# INLINE dim #-}; dim = Dim 5 }
instance {-# OVERLAPPING #-} KnownDim 6  where
  { {-# INLINE dim #-}; dim = Dim 6 }
instance {-# OVERLAPPING #-} KnownDim 7  where
  { {-# INLINE dim #-}; dim = Dim 7 }
instance {-# OVERLAPPING #-} KnownDim 8  where
  { {-# INLINE dim #-}; dim = Dim 8 }
instance {-# OVERLAPPING #-} KnownDim 9  where
  { {-# INLINE dim #-}; dim = Dim 9 }
instance {-# OVERLAPPING #-} KnownDim 10 where
  { {-# INLINE dim #-}; dim = Dim 10 }
instance {-# OVERLAPPING #-} KnownDim 11 where
  { {-# INLINE dim #-}; dim = Dim 11 }
instance {-# OVERLAPPING #-} KnownDim 12 where
  { {-# INLINE dim #-}; dim = Dim 12 }
instance {-# OVERLAPPING #-} KnownDim 13 where
  { {-# INLINE dim #-}; dim = Dim 13 }
instance {-# OVERLAPPING #-} KnownDim 14 where
  { {-# INLINE dim #-}; dim = Dim 14 }
instance {-# OVERLAPPING #-} KnownDim 15 where
  { {-# INLINE dim #-}; dim = Dim 15 }
instance {-# OVERLAPPING #-} KnownDim 16 where
  { {-# INLINE dim #-}; dim = Dim 16 }
instance {-# OVERLAPPING #-} KnownDim 17 where
  { {-# INLINE dim #-}; dim = Dim 17 }
instance {-# OVERLAPPING #-} KnownDim 18 where
  { {-# INLINE dim #-}; dim = Dim 18 }
instance {-# OVERLAPPING #-} KnownDim 19 where
  { {-# INLINE dim #-}; dim = Dim 19 }
instance {-# OVERLAPPING #-} KnownDim 20 where
  { {-# INLINE dim #-}; dim = Dim 20 }

-- | Similar to `someNatVal` from `GHC.TypeLits`.
someDimVal :: forall (m :: Nat) . KnownDim m => Word -> Maybe (Dim ('XN m))
someDimVal x | dimVal' @m > x = Nothing
             | otherwise      = Just (unsafeCoerce# x)
{-# INLINE someDimVal #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim :: forall (x :: Nat) (y :: Nat) p
         . (KnownDim x, KnownDim y)
         => p x -> p y -> Maybe (Evidence (x ~ y))
sameDim _ _
  | dimVal' @x == dimVal' @y
    = Just (unsafeCoerce# (E @(x ~ x)))
  | otherwise = Nothing


instance Eq (Dim (n :: Nat)) where
    _ == _ = True
    {-# INLINE (==) #-}

instance Eq (Dim (x :: XNat)) where
    Dim a == Dim b = a == b
    {-# INLINE (==) #-}

instance Ord (Dim (n :: Nat)) where
    compare _ _ = EQ
    {-# INLINE compare #-}

instance Ord (Dim (x :: XNat)) where
    compare (Dim a) (Dim b) = compare a b
    {-# INLINE compare #-}

instance Show (Dim x) where
    showsPrec p = showsPrec p . dimVal
    {-# INLINE showsPrec #-}

instance KnownDim m => Read (Dim ('XN m)) where
    readsPrec p xs = do (a,ys) <- readsPrec p xs
                        case someDimVal a of
                          Nothing -> []
                          Just n  -> [(n,ys)]




plusDim :: Dim n -> Dim m -> Dim (n + m)
plusDim (Dim a) (Dim b) = unsafeCoerce# (a + b)
{-# INLINE plusDim #-}

minusDim :: (<=) m n => Dim n -> Dim m -> Dim (n - m)
minusDim (Dim a) (Dim b) = unsafeCoerce# (a - b)
{-# INLINE minusDim #-}

minusDimM :: Dim n -> Dim m -> Maybe (Dim (n - m))
minusDimM (Dim a) (Dim b)
  | a >= b    = Just (unsafeCoerce# (a - b))
  | otherwise = Nothing
{-# INLINE minusDimM #-}

timesDim :: Dim n -> Dim m -> Dim ((*) n m)
timesDim (Dim a) (Dim b) = unsafeCoerce# (a * b)
{-# INLINE timesDim #-}

powerDim :: Dim n -> Dim m -> Dim ((^) n m)
powerDim (Dim a) (Dim b) = unsafeCoerce# (a ^ b)
{-# INLINE powerDim #-}


--------------------------------------------------------------------------------

-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall r . Word -> (forall (n :: Nat) . KnownDim n => Proxy# n -> r) -> r
reifyDim n k = unsafeCoerce# (MagicDim k :: MagicDim r) n proxy#
{-# INLINE reifyDim #-}
newtype MagicDim r = MagicDim (forall (n :: Nat) . KnownDim n => Proxy# n -> r)


dimEv :: forall n . Dim n -> Evidence (KnownDim n)
dimEv (Dim k) = reifyDim k f
  where
   f :: forall (k :: Nat)
      . KnownDim k => Proxy# k -> Evidence (KnownDim n)
   f _ = unsafeCoerce# (E @(KnownDim k))
{-# INLINE dimEv #-}

dimNEv :: forall n . Dim (N n) -> (# Evidence (KnownDim n), Dim n #)
dimNEv (Dim k) =
  (# reifyDim k f
   , unsafeCoerce# k
   #)
  where
    f :: forall (k :: Nat)
       . KnownDim k => Proxy# k -> Evidence (KnownDim n)
    f _ = unsafeCoerce# (E @(KnownDim k))
{-# INLINE dimNEv #-}

dimXNEv :: forall m n . Dim (XN m) -> (# Evidence (MinDim m n, KnownDim n), Dim n #)
dimXNEv (Dim k) =
  (# reifyDim k f
   , unsafeCoerce# k
   #)
  where
    f :: forall (k :: Nat)
       . KnownDim k => Proxy# k -> Evidence (MinDim m n, KnownDim n)
    f _ = unsafeCoerce# (E @(k <= k, KnownDim k))
{-# INLINE dimXNEv #-}
