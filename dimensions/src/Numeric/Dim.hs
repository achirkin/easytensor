{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
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
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
-- `Word`s instead of `Integer`s;
-- Also it provides `Dim` data type serving as a singleton
-- suitable for recovering an instance of the `KnownDim` class.
-- A set of utility functions provide inference functionality, so
-- that `KnownDim` can be preserved over some type-level operations.
--
-----------------------------------------------------------------------------
module Numeric.Dim
  ( -- * Type level numbers that can be unknown.
    XNat (..), XN, N, XNatType (..)
    -- * Term level dimension
  , Dim (Dim, D, Dn, Dx), SomeDim
  , KnownDim (..), KnownXNatType (..)
  , dimVal, dimVal', someDimVal
  , sameDim, sameDim'
  , compareDim, compareDim'
  , constrain, constrainBy, relax
    -- * Simple Dim arithmetics
    --
    --   The functions below create singleton values that work as a witness
    --   of `KnownDim` instance for type-level Nat operations.
    --   For example, to show that @(a + b)@ is a @KnownDim@, one writes:
    --
    --   > case plusDim dA dB of
    --   >   D -> ... -- here we know KnownDim ( a + b )
    --
    --   There is a bug and a feature in these functions though:
    --   they are implemented in terms of @Num Word@, which means that
    --   their results are subject to integer overflow.
    --   The good side is the confidence that they behave exactly as
    --   their @Word@ counterparts.
  , plusDim, minusDim, minusDimM, timesDim, powerDim
    -- * Re-export part of `GHC.TypeLits` for convenience
  , Nat, CmpNat, type (+), type (-), type (*), type (^)
  , MinDim, FixedDim, inferDimLE
    -- * Inferring kind of type-level dimension
  , KnownDimKind (..), DimKind (..)
  ) where


import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.Base           (Type)
import           GHC.Exts           (Constraint, Proxy#, proxy#, unsafeCoerce#)
import           GHC.TypeLits

import           Numeric.Type.Evidence


-- | Either known or unknown at compile-time natural number
data XNat = XN Nat | N Nat
-- | Unknown natural number, known to be not smaller than the given Nat
type XN (n::Nat) = 'XN n
-- | Known natural number
type N (n::Nat) = 'N n

-- | Find out whether @XNat@ is of known or constrained type.
data XNatType :: XNat -> Type where
  -- | Given @XNat@ is known
  Nt  :: XNatType ('N n)
  -- | Given @XNat@ is constrained unknown
  XNt :: XNatType ('XN m)

-- | Same as `SomeNat`
type SomeDim = Dim ('XN 0)

-- | Singleton type to store type-level dimension value.
--
--   On the one hand, it can be used to let type-inference system know
--   relations between type-level naturals.
--   On the other hand, this is just a newtype wrapper on the @Word@ type.
--
--   Usually, the type parameter of @Dim@ is either @Nat@ or @XNat@.
--   If dimensionality of your data is known in advance, use @Nat@;
--   if you know the size of some dimensions, but do not know the size
--   of others, use @XNat@s to represent them.
newtype Dim (x :: k) = DimSing Word
-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE D #-}
{-# COMPLETE Dn, Dx #-}
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


-- | Same as @Dim@ pattern, but constrained to @Nat@ kind.
pattern D :: forall (n :: Nat) . () => KnownDim n => Dim n
pattern D <- (dimEv -> E)
  where
    D = dim @_ @n

-- | Statically known `XNat`
pattern Dn :: forall (xn :: XNat) . KnownXNatType xn
           => forall (n :: Nat) . (KnownDim n, xn ~ 'N n) => Dim n -> Dim xn
pattern Dn k <- (dimXNEv (xNatType @xn) -> PatN k)
  where
    Dn k = unsafeCoerce# k

-- | `XNat` that is unknown at compile time.
--   Same as `SomeNat`, but for a dimension:
--   Hide dimension size inside, but allow specifying its minimum possible value.
pattern Dx :: forall (xn :: XNat) . KnownXNatType xn
           => forall (n :: Nat) (m :: Nat)
            . (KnownDim n, MinDim m n, xn ~ 'XN m) => Dim n -> Dim xn
pattern Dx k <- (dimXNEv (xNatType @xn) -> PatXN k)
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


-- | Find out the type of `XNat` constructor
class KnownXNatType (n :: XNat) where
  -- | Pattern-match against this to out the type of `XNat` constructor
  xNatType :: XNatType n

instance KnownXNatType ('N n) where
  xNatType = Nt
  {-# INLINE xNatType #-}

instance KnownXNatType ('XN n) where
  xNatType = XNt
  {-# INLINE xNatType #-}


-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal :: Dim (x :: k) -> Word
dimVal = unsafeCoerce#
{-# INLINE dimVal #-}

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal' :: forall n . KnownDim n => Word
dimVal' = unsafeCoerce# (dim @_ @n)
{-# INLINE dimVal' #-}

-- | Friendly error message if `m <= n` constraint is not satisfied.
--   Use this type family instead of @(<=)@ if possible
--   or try `inferDimLE` function as the last resort.
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

-- | Constraints given by an XNat type on possible values of a Nat hidden inside.
type family FixedDim (x :: XNat) (n :: Nat) :: Constraint where
  FixedDim ('N a)  b = a ~ b
  FixedDim ('XN m) b = MinDim m b

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

-- | `constrain` with explicitly-passed constraining @Dim@
--   to avoid @AllowAmbiguousTypes@.
constrainBy :: forall m x . Dim m -> Dim x -> Maybe (Dim (XN m))
constrainBy D = constrain @m
#if __GLASGOW_HASKELL__ < 802
constrainBy _ = error "Dim: Impossible pattern."
#endif

-- | Decrease minimum allowed size of a @Dim (XN x)@.
relax :: forall (m :: Nat) (n :: Nat) . (MinDim m n) => Dim (XN n) -> Dim (XN m)
relax = unsafeCoerce#
{-# INLINE relax #-}


-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
--
--   Note, this function works on @Nat@-indexed dimensions only,
--   because @Dim (XN x)@ does not have runtime evidence to infer @x@
--   and `KnownDim x` does not imply `KnownDim (XN x)`.
sameDim :: forall (x :: Nat) (y :: Nat)
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
sameDim' _ _ = sameDim (dim @Nat @x) (dim @Nat @y)
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

minusDim :: MinDim m n => Dim n -> Dim m -> Dim (n - m)
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


-- | @MinDim@ implies @(<=)@, but this fact is not so clear to GHC.
--   This function assures the type system that the relation takes place.
inferDimLE :: forall m n . MinDim m n => Evidence (m <= n)
inferDimLE = unsafeCoerce# (E @(n <= n))


-- | GADT to support `KnownDimKind` type class.
--   Match against its constructors to know if @k@ is @Nat@ or @XNat@
data DimKind :: Type -> Type where
    -- | Working on @Nat@.
    DimNat  :: DimKind Nat
    -- | Working on @XNat@.
    DimXNat :: DimKind XNat

-- | Figure out whether the type-level dimension is `Nat` or `XNat`.
--   Useful for generalized inference functions.
class KnownDimKind k where
    dimKind :: DimKind k

instance KnownDimKind Nat where
    dimKind = DimNat

instance KnownDimKind XNat where
    dimKind = DimXNat

--------------------------------------------------------------------------------

-- | This function does GHC's magic to convert user-supplied `dim` function
--   to create an instance of `KnownDim` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall r d . Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce# (MagicDim k :: MagicDim d r) d
{-# INLINE reifyDim #-}
newtype MagicDim d r = MagicDim (KnownDim d => r)

dimEv :: Dim d -> Evidence (KnownDim d)
dimEv d = reifyDim d E
{-# INLINE dimEv #-}

data PatXDim (xn :: XNat) where
  PatN :: KnownDim n => Dim n -> PatXDim ('N n)
  PatXN :: (KnownDim n, MinDim m n) => Dim n -> PatXDim ('XN m)

dimXNEv :: forall (xn :: XNat) . XNatType xn -> Dim xn -> PatXDim xn
dimXNEv Nt (DimSing k) = reifyDim dd (PatN dd)
  where
    dd = DimSing @Nat @_ k
dimXNEv XNt xn@(DimSing k) = reifyDim dd (f dd xn)
  where
    dd = DimSing @Nat @_ k
    f :: forall (d :: Nat) (m :: Nat)
       . KnownDim d => Dim d -> Dim ('XN m) -> PatXDim ('XN m)
    f d _ = case ( unsafeCoerce# (E @((CmpNat m m == 'GT) ~ 'False, m <= m))
                :: Evidence ((CmpNat m d == 'GT) ~ 'False, m <= d)
               ) of
      E -> PatXN d
{-# INLINE dimXNEv #-}
