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
{-# LANGUAGE Strict                #-}
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
-- Also it provides `Dim` data family serving as a customized `Proxy` type
-- and a singleton suitable for recovering an instance of the `KnownDim` class.
-- A set of utility functions provide inference functionality, so
-- that `KnownDim` can be preserved over some type-level operations.
--
-----------------------------------------------------------------------------
module Numeric.Dim
  ( XNat (..), XN, N
  , Dim (Dim, D, Dn, Dx), SomeDim
  , KnownDim (..)
  , dimVal, dimVal', someDimVal
  , sameDim, sameDim'
  , compareDim, compareDim'
  , constrain, relax
    -- * Simple Dim arithmetics
  , plusDim, minusDim, minusDimM, timesDim, powerDim
    -- * Re-export part of `GHC.TypeLits` for convenience
  , Nat, CmpNat, type (<=), type (+), type (-), type (*), type (^)
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
newtype Dim (x :: k) = DimSing Word
-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE D #-}
{-# COMPLETE Dn #-}
{-# COMPLETE Dx #-}
{-# COMPLETE Dim #-}
#endif


-- | Independently of the kind of type-level number,
--   construct an instance of `KnownDim` from it.
--
--   Match against this pattern to bring `KnownDim` instance into scope
--   when you don't know the kind of the @Dim@ parameter.
pattern Dim :: forall (n :: k) . () => KnownDim n => Dim n
pattern Dim <- (dimEv -> E)
  where
    Dim = dim @_ @n


-- | Statically known `XNat`
pattern D :: forall (n :: Nat) . () => KnownDim n => Dim n
pattern D <- (dimEv -> E)
  where
    D = dim @_ @n

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

-- | This class provides the `Dim` associated with a type-level natural.
class KnownDim (n :: k) where
    -- | Get value of type-level dim at runtime.
    --
    --   Note, this function is supposed to be used with @TypeApplications@,
    --   and the @KnownDim@ class has varying kind of the parameter;
    --   thus, the function has two type paremeters (kind and type of @n@).
    --   For example, you can type:
    --
    --   >>>:set -XTypeApplications
    --   >>>:set -XDataKinds
    --   >>>:t dim @Nat @3
    --   dim @Nat @3 :: Dim 3
    --
    --   >>>:set -XTypeOperators
    --   >>>:t dim @_ @(13 - 6)
    --   dim @_ @(13 - 6) :: Dim 7
    --
    --
    --   >>>:t dim @_ @(N 17)
    --   dim @_ @(N 17) :: Dim (N 17)
    --
    dim :: Dim n

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal :: Dim (x :: k) -> Word
dimVal = unsafeCoerce#
{-# INLINE dimVal #-}

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal' :: forall n . KnownDim n => Word
dimVal' = unsafeCoerce# (dim @_ @n)
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
    dim = DimSing (fromInteger (natVal' (proxy# :: Proxy# n)))

instance {-# OVERLAPPING #-} KnownDim 0  where
  { {-# INLINE dim #-}; dim = DimSing 0 }
instance {-# OVERLAPPING #-} KnownDim 1  where
  { {-# INLINE dim #-}; dim = DimSing 1 }
instance {-# OVERLAPPING #-} KnownDim 2  where
  { {-# INLINE dim #-}; dim = DimSing 2 }
instance {-# OVERLAPPING #-} KnownDim 3  where
  { {-# INLINE dim #-}; dim = DimSing 3 }
instance {-# OVERLAPPING #-} KnownDim 4  where
  { {-# INLINE dim #-}; dim = DimSing 4 }
instance {-# OVERLAPPING #-} KnownDim 5  where
  { {-# INLINE dim #-}; dim = DimSing 5 }
instance {-# OVERLAPPING #-} KnownDim 6  where
  { {-# INLINE dim #-}; dim = DimSing 6 }
instance {-# OVERLAPPING #-} KnownDim 7  where
  { {-# INLINE dim #-}; dim = DimSing 7 }
instance {-# OVERLAPPING #-} KnownDim 8  where
  { {-# INLINE dim #-}; dim = DimSing 8 }
instance {-# OVERLAPPING #-} KnownDim 9  where
  { {-# INLINE dim #-}; dim = DimSing 9 }
instance {-# OVERLAPPING #-} KnownDim 10 where
  { {-# INLINE dim #-}; dim = DimSing 10 }
instance {-# OVERLAPPING #-} KnownDim 11 where
  { {-# INLINE dim #-}; dim = DimSing 11 }
instance {-# OVERLAPPING #-} KnownDim 12 where
  { {-# INLINE dim #-}; dim = DimSing 12 }
instance {-# OVERLAPPING #-} KnownDim 13 where
  { {-# INLINE dim #-}; dim = DimSing 13 }
instance {-# OVERLAPPING #-} KnownDim 14 where
  { {-# INLINE dim #-}; dim = DimSing 14 }
instance {-# OVERLAPPING #-} KnownDim 15 where
  { {-# INLINE dim #-}; dim = DimSing 15 }
instance {-# OVERLAPPING #-} KnownDim 16 where
  { {-# INLINE dim #-}; dim = DimSing 16 }
instance {-# OVERLAPPING #-} KnownDim 17 where
  { {-# INLINE dim #-}; dim = DimSing 17 }
instance {-# OVERLAPPING #-} KnownDim 18 where
  { {-# INLINE dim #-}; dim = DimSing 18 }
instance {-# OVERLAPPING #-} KnownDim 19 where
  { {-# INLINE dim #-}; dim = DimSing 19 }
instance {-# OVERLAPPING #-} KnownDim 20 where
  { {-# INLINE dim #-}; dim = DimSing 20 }

instance KnownDim n => KnownDim ('N n) where
    {-# INLINE dim #-}
    dim = unsafeCoerce# (dim @Nat @n)

-- | Similar to `someNatVal` from `GHC.TypeLits`.
someDimVal :: Word -> SomeDim
someDimVal = unsafeCoerce#
{-# INLINE someDimVal #-}


-- | Change the minimum allowed size of a @Dim (XN x)@,
--   while testing if the value inside satisfies it.
constrain :: forall (m :: Nat) x . KnownDim m
          => Dim x -> Maybe (Dim (XN m))
constrain (DimSing x) | dimVal' @m > x = Nothing
                      | otherwise      = Just (unsafeCoerce# x)
{-# INLINE constrain #-}

-- | Decrease minimum allowed size of a @Dim (XN x)@.
relax :: forall (m :: Nat) (n :: Nat) . (m <= n) => Dim (XN n) -> Dim (XN m)
relax = unsafeCoerce#
{-# INLINE relax #-}


-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
--
--   Note, this version of the function behaves incorrectly with @Dim (XN x)@:
--   it compares only contained dim values, and ignores minimum value constraints.
--   As a result, GHC may infer minimum value contraints equality incorrectly.
sameDim :: forall x y
         . Dim x -> Dim y -> Maybe (Evidence (x ~ y))
sameDim (DimSing a) (DimSing b)
  | a == b    = Just (unsafeCoerce# (E @(x ~ x)))
  | otherwise = Nothing
{-# INLINE sameDim #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim' :: forall (x :: Nat) (y :: Nat) p q
          . (KnownDim x, KnownDim y)
         => p x -> q y -> Maybe (Evidence (x ~ y))
sameDim' _ _ = sameDim' (dim @Nat @x) (dim @Nat @y)
{-# INLINE sameDim' #-}

-- | Ordering of dimension values.
compareDim :: Dim a -> Dim b -> Ordering
compareDim = unsafeCoerce# (compare :: Word -> Word -> Ordering)
{-# INLINE compareDim #-}


-- | Ordering of dimension values.
compareDim' :: forall a b p q
             . (KnownDim a, KnownDim b) => p a -> q b -> Ordering
compareDim' _ _ = compareDim (dim @_ @a)  (dim @_ @b)
{-# INLINE compareDim' #-}


instance Eq (Dim (n :: Nat)) where
    _ == _ = True
    {-# INLINE (==) #-}

instance Eq (Dim (x :: XNat)) where
    DimSing a == DimSing b = a == b
    {-# INLINE (==) #-}

instance Ord (Dim (n :: Nat)) where
    compare _ _ = EQ
    {-# INLINE compare #-}

instance Ord (Dim (x :: XNat)) where
    compare = compareDim
    {-# INLINE compare #-}

instance Show (Dim x) where
    showsPrec p = showsPrec p . dimVal
    {-# INLINE showsPrec #-}

instance KnownDim m => Read (Dim ('XN m)) where
    readsPrec p xs = do (a,ys) <- readsPrec p xs
                        case constrain (someDimVal a) of
                          Nothing -> []
                          Just n  -> [(n,ys)]




plusDim :: Dim n -> Dim m -> Dim (n + m)
plusDim (DimSing a) (DimSing b) = unsafeCoerce# (a + b)
{-# INLINE plusDim #-}

minusDim :: (<=) m n => Dim n -> Dim m -> Dim (n - m)
minusDim (DimSing a) (DimSing b) = unsafeCoerce# (a - b)
{-# INLINE minusDim #-}

minusDimM :: Dim n -> Dim m -> Maybe (Dim (n - m))
minusDimM (DimSing a) (DimSing b)
  | a >= b    = Just (unsafeCoerce# (a - b))
  | otherwise = Nothing
{-# INLINE minusDimM #-}

timesDim :: Dim n -> Dim m -> Dim ((*) n m)
timesDim (DimSing a) (DimSing b) = unsafeCoerce# (a * b)
{-# INLINE timesDim #-}

powerDim :: Dim n -> Dim m -> Dim ((^) n m)
powerDim (DimSing a) (DimSing b) = unsafeCoerce# (a ^ b)
{-# INLINE powerDim #-}


--------------------------------------------------------------------------------

-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall r d . Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce# (MagicDim k :: MagicDim d r) d
{-# INLINE reifyDim #-}
newtype MagicDim d r = MagicDim (KnownDim d => r)

dimEv :: Dim d -> Evidence (KnownDim d)
dimEv d = reifyDim d E
{-# INLINE dimEv #-}

dimNEv :: forall n . Dim (N n) -> (# Evidence (KnownDim n), Dim n #)
dimNEv (DimSing k) =
  (# reifyDim (DimSing @Nat @n k) E
   , unsafeCoerce# k
   #)
{-# INLINE dimNEv #-}

dimXNEv :: forall m n . Dim (XN m) -> (# Evidence (MinDim m n, KnownDim n), Dim n #)
dimXNEv (DimSing k) =
  (# reifyDim (DimSing @Nat @n k) (unsafeCoerce# (E @(n <= n, KnownDim n)))
   , unsafeCoerce# k
   #)
{-# INLINE dimXNEv #-}
