{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
{-# LANGUAGE ViewPatterns              #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType              #-}
#endif
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Dim
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- This module provides `KnownDim` class that is similar to `KnownNat` from `GHC.TypeNats`,
-- but keeps `Word`s instead of `Natural`s;
-- Also it provides `Dim` data type serving as a singleton
-- suitable for recovering an instance of the `KnownDim` class.
-- A set of utility functions provide inference functionality, so
-- that `KnownDim` can be preserved over some type-level operations.
--
-- Provides a data type @Dims ds@ to keep dimension sizes
-- for multiple-dimensional data.
-- Higher indices go first, i.e. assumed enumeration
--          is i = i1*n1*n2*...*n(k-1) + ... + i(k-2)*n1*n2 + i(k-1)*n1 + ik
-- This corresponds to row-first layout of matrices and multidimenional arrays.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Dim
  ( -- * @Dim@: a @Nat@-indexed dimension
    -- ** Type level numbers that can be unknown.
    Nat, XNat (..), XN, N
  , DimType (..), KnownDimType(..), DimKind (..), KnownDimKind(..)
    -- ** Term level dimension
  , Dim ( D, Dn, Dx
        , D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13
        , D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25
        )
  , SomeDim
  , KnownDim (..), withKnownXDim
  , BoundedDim (..), minimalDim, ExactDim, FixedDim
  , dimVal, dimVal', typeableDim, someDimVal
  , sameDim, sameDim'
  , lessOrEqDim, lessOrEqDim'
  , compareDim, compareDim'
  , constrainBy, relax
    -- ** Simple Dim arithmetics
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
  , plusDim, minusDim, minusDimM, timesDim, powerDim, divDim, modDim, log2Dim
  , minDim, maxDim
    -- ** Re-export part of `Data.Type.Lits` for convenience
  , CmpNat, SOrdering (..), type (+), type (-), type (*), type (^), type (<=)
  , Min, Max
    -- * @Dims@: a list of dimensions
  , Dims, SomeDims (..), Dimensions (..), withKnownXDims
  , BoundedDims (..), DimsBound, minimalDims
  , ExactDims, FixedDims, inferFixedDims, inferExactFixedDims
  , TypedList ( Dims, XDims, KnownDims
              , U, (:*), Empty, TypeList, Cons, Snoc, Reverse)
  , typeableDims, inferTypeableDims
  , listDims, someDimsVal, totalDim, totalDim'
  , sameDims, sameDims'
  , inSpaceOf
  , stripPrefixDims, stripSuffixDims
    -- ** Re-export type list
  , RepresentableList (..), TypeList, types
  , order, order'
  , KindOf, KindOfEl
#if !(defined(__HADDOCK__) || defined(__HADDOCK_VERSION__))
    -- hide a plugin-related func
  , incohInstBoundedDims
#endif
  ) where


import           Data.Bits                (countLeadingZeros, finiteBitSize)
import           Data.Coerce
import           Data.Constraint
import           Data.Constraint.Bare
import           Data.Constraint.Deriving
import           Data.Data                hiding (TypeRep, typeRep,
                                           typeRepTyCon)
import           Data.Kind                (Type)
import qualified Data.List                (stripPrefix)
import           Data.Type.List
import           Data.Type.List.Internal
import           Data.Type.Lits
import           GHC.Exts                 (Proxy#, RuntimeRep, TYPE, proxy#, type (~~))
import qualified GHC.Generics             as G
import           Numeric.Natural          (Natural)
import           Numeric.TypedList
import qualified Text.Read                as Read
import qualified Text.Read.Lex            as Read
import           Type.Reflection
import           Unsafe.Coerce            (unsafeCoerce)

{-
COMPLETE pragmas on Dims and XDims let you avoid the boilerplate of catch-all cases
when pattern-matching only to get the evidence brought by Dims. Unfortunately,
there is a bug the pattern-match checker in GHC 8.10 and 9.0, which warns you incorrectly
about redundant or inacessible patterns in some rare cases.
To workaround those dangerous cases, I disable these pragmas for some compiler versions.
(i.e. it's better to place a redundant wildcard case than to have a partial function at runtime).

https://gitlab.haskell.org/ghc/ghc/-/issues/19622
 -}
#define IS_UNSOUND_MATCHING_810_900 (MIN_VERSION_GLASGOW_HASKELL(8,10,0,0) && !MIN_VERSION_GLASGOW_HASKELL(9,1,0,0))


-- | Either known or unknown at compile-time natural number
data XNat = XN Nat | N Nat
-- | Unknown natural number, known to be not smaller than the given Nat
type XN = 'XN
-- | Known natural number
type N = 'N

-- | GADT to support `KnownDimType` type class.
--   Find out if this type variable is a @Nat@ or @XNat@,
--   and whether @XNat@ is of known or constrained type.
data DimType (d :: k) where
    -- | This is a plain @Nat@
    DimTNat   :: DimType (n :: Nat)
    -- | Given @XNat@ is known
    DimTXNatN :: DimType (N n)
    -- | Given @XNat@ is constrained unknown
    DimTXNatX :: DimType (XN m)

-- | GADT to support `KnownDimKind` type class.
--   Match against its constructors to know if @k@ is @Nat@ or @XNat@
data DimKind (k :: Type) where
    -- | Working on @Nat@.
    DimKNat  :: DimKind Nat
    -- | Working on @XNat@.
    DimKXNat :: DimKind XNat

-- | Figure out whether the type-level dimension is `Nat`, or `N Nat`, or `XN Nat`.
class KnownDimType d where
    -- | Pattern-match against this to out the value (type) of the dim type variable.
    dimType :: DimType d

-- | Figure out whether the type-level dimension is `Nat` or `XNat`.
class KnownDimKind k where
    -- | Pattern-match against this to out the kind of the dim type variable.
    dimKind :: DimKind k

instance KnownDimType (n :: Nat) where
    dimType = DimTNat
    {-# INLINE dimType #-}

instance KnownDimType ('N n :: XNat) where
    dimType = DimTXNatN
    {-# INLINE dimType #-}

instance KnownDimType ('XN n :: XNat) where
    dimType = DimTXNatX
    {-# INLINE dimType #-}

instance KnownDimKind Nat where
    dimKind = DimKNat
    {-# INLINE dimKind #-}

instance KnownDimKind XNat where
    dimKind = DimKXNat
    {-# INLINE dimKind #-}

instance Class (KnownDimKind k) (KnownDimType (n :: k)) where
    cls = Sub $ case dimType @n of
      DimTNat   -> Dict
      DimTXNatN -> Dict
      DimTXNatX -> Dict

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
  deriving ( Typeable )

type role Dim nominal

-- | Same as `SomeNat`
type SomeDim = Dim (XN 0)

-- | Type-level dimensionality.
type Dims = (TypedList Dim :: [k] -> Type)

#define PLEASE_STYLISH_HASKELL \
  forall d . KnownDimType d => \
  (KindOf d ~ Nat, KnownDim d) => \
  Dim d

-- | Match against this pattern to bring `KnownDim` instance into scope.
pattern D :: PLEASE_STYLISH_HASKELL
pattern D <- (patDim (dimType @d) -> PatNat)
  where
    D = dim @d
#undef PLEASE_STYLISH_HASKELL


#define PLEASE_STYLISH_HASKELL \
  forall d . KnownDimType d => \
  forall (n :: Nat) . (d ~~ N n) => \
  Dim n -> Dim d

-- | Statically known `XNat`
pattern Dn :: PLEASE_STYLISH_HASKELL
pattern Dn k <- (patDim (dimType @d) -> PatXNatN k)
  where
    Dn k = coerce k
#undef PLEASE_STYLISH_HASKELL

#define PLEASE_STYLISH_HASKELL \
  forall d . KnownDimType d => \
  forall (m :: Nat) (n :: Nat) . (d ~~ XN m, m <= n) => \
  Dim n -> Dim d

-- | `XNat` that is unknown at compile time.
--   Same as `SomeNat`, but for a dimension:
--   Hide dimension size inside, but allow specifying its minimum possible value.
pattern Dx :: PLEASE_STYLISH_HASKELL
pattern Dx k <- (patDim (dimType @d) -> PatXNatX k)
  where
    Dx k = coerce k
#undef PLEASE_STYLISH_HASKELL

#if !IS_UNSOUND_MATCHING_810_900
{-# COMPLETE D #-}
{-# COMPLETE Dn, Dx #-}
#endif
{-# COMPLETE D, Dn, Dx #-}

-- | This class provides the `Dim` associated with a type-level natural.
--
--   Note, kind of the @KnownDim@ argument is usually @Nat@, because
--     it is impossible to create a unique @KnownDim (XN m)@ instance.
--   Nevertheless, you can have @KnownDim (N n)@, which is useful in some cases.
class KnownDim n where
    -- | Get value of type-level dim at runtime.
    --
    --   Note, this function is supposed to be used with @TypeApplications@.
    --   For example, you can type:
    --
    --   >>>:set -XTypeApplications
    --   >>>:set -XDataKinds
    --   >>>:t dim @3
    --   dim @3 :: Dim 3
    --
    --   >>>:set -XTypeOperators
    --   >>>:t dim @(13 - 6)
    --   dim @(13 - 6) :: Dim 7
    --
    dim :: Dim n

-- | Get a minimal or exact bound of a @Dim@.
--
--   To satisfy the @BoundedDim@ means to be equal to @N n@ or be not less than @XN m@.
class (KnownDimKind (KindOf d), KnownDimType d, KnownDim (DimBound d))
    => BoundedDim d where
    -- | Minimal or exact bound of a @Dim@.
    --   Useful for indexing: it is safe to index something by an index less than
    --   @DimBound n@ (for both @Nat@ and @Xnat@ indexed dims).
    type family DimBound d :: Nat
    -- | Get such a minimal @Dim (DimBound n)@, that @Dim n@ is guaranteed
    --   to be not less than @dimBound@ if @n ~ XN a@,
    --     otherwise, the return @Dim@ is the same as @n@.
    dimBound :: Dim (DimBound d)
    -- | If the runtime value of @Dim y@ satisfies @dimBound @x@,
    --   then coerce to @Dim x@. Otherwise, return @Nothing@.
    --
    --   To satisfy the @dimBound@ means to be equal to @N n@ or be not less than @XN m@.
    constrainDim :: forall y . Dim y -> Maybe (Dim d)

-- | Returns the minimal @Dim@ that satisfies the @BoundedDim@ constraint
--   (this is the exact @dim@ for @Nat@s and the minimal bound for @XNat@s).
minimalDim :: forall n . BoundedDim n => Dim n
minimalDim = coerce (dimBound @n)
{-# INLINE minimalDim #-}

instance KnownDim n => BoundedDim (n :: Nat) where
    type DimBound n = n
    dimBound = dim @n
    {-# INLINE dimBound #-}
    constrainDim (DimSing y)
       | dimVal' @n == y = Just (DimSing y)
       | otherwise       = Nothing
    {-# INLINE constrainDim #-}

instance KnownDim n => BoundedDim (N n) where
    type DimBound (N n) = n
    dimBound = dim @n
    {-# INLINE dimBound #-}
    constrainDim (DimSing y)
       | dimVal' @n == y = Just (DimSing y)
       | otherwise       = Nothing
    {-# INLINE constrainDim #-}

instance KnownDim m => BoundedDim (XN m) where
    type DimBound ('XN m) = m
    dimBound = dim @m
    {-# INLINE dimBound #-}
    constrainDim (DimSing y)
       | dimVal' @m <= y = Just (DimSing y)
       | otherwise       = Nothing
    {-# INLINE constrainDim #-}

-- | Similar to `natVal` from `GHC.TypeNats`, but returns `Word`.
dimVal :: forall x . Dim x -> Word
dimVal = coerce
{-# INLINE dimVal #-}

-- | Similar to `natVal` from `GHC.TypeNats`, but returns `Word`.
dimVal' :: forall n . KnownDim n => Word
dimVal' = coerce (dim @n)
{-# INLINE dimVal' #-}

-- | Construct a @Dim n@ if there is an instance of @Typeable n@ around.
--
--   Note: we can do this only for @Nat@-indexed dim, because the type @XN m@
--         does not have enough information to create a dim at runtime.
typeableDim :: forall (n :: Nat) . Typeable n => Dim n
{- YES, that's right. TyCon of a Nat is a string containing the Nat value.
   There simply no place in a TyCon to keep the Nat as a number
     (check GHC.Types for the definition of TyCon).

  Here is an excert from Data.Typeable.Internal:

  -- | Used to make `'Typeable' instance for things of kind Nat
  typeNatTypeRep :: KnownNat a => Proxy# a -> TypeRep a
  typeNatTypeRep p = typeLitTypeRep (show (natVal' p)) tcNat

 -}
typeableDim = DimSing . read . tyConName . typeRepTyCon $ typeRep @n
{-# INLINE typeableDim #-}

-- | Constraints given by an XNat type on possible values of a Nat hidden inside.
type family FixedDim (x :: XNat) (n :: Nat) :: Constraint where
    FixedDim ('N a)  b = a ~ b
    FixedDim ('XN m) b = m <= b

-- | This is either @Nat@, or a known @XNat@ (i.e. @N n@).
type family ExactDim (d :: k) :: Constraint where
    ExactDim (_ :: Nat)  = ()
    ExactDim (x :: XNat) = (x ~ N (DimBound x))

instance KnownNat n => KnownDim n where
    {-# INLINE dim #-}
    dim = DimSing (fromIntegral (natVal' (proxy# :: Proxy# n)))

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
instance {-# OVERLAPPING #-} KnownDim 21 where
  { {-# INLINE dim #-}; dim = DimSing 21 }
instance {-# OVERLAPPING #-} KnownDim 22 where
  { {-# INLINE dim #-}; dim = DimSing 22 }
instance {-# OVERLAPPING #-} KnownDim 23 where
  { {-# INLINE dim #-}; dim = DimSing 23 }
instance {-# OVERLAPPING #-} KnownDim 24 where
  { {-# INLINE dim #-}; dim = DimSing 24 }
instance {-# OVERLAPPING #-} KnownDim 25 where
  { {-# INLINE dim #-}; dim = DimSing 25 }

instance KnownDim n => KnownDim (N n) where
    {-# INLINE dim #-}
    dim = coerce (dim @n)

-- | If you have @KnownDim d@, then @d@ can only be @Nat@ or a known type of
--   @XNat@ (i.e. @N n@).
--   This function assures the type checker that this is indeed the case.
withKnownXDim :: forall (d :: XNat) (rep :: RuntimeRep) (r :: TYPE rep)
               . KnownDim d
              => ( (KnownDim (DimBound d), ExactDim d
                 , KnownDimType d, FixedDim d (DimBound d)) => r)
              -> r
withKnownXDim x
  | Dict <- unsafeEqTypes @d @(N (DimBound d))
    = reifyDim @Nat @(DimBound d) @rep @r (coerce (dim @d)) x
{-# INLINE withKnownXDim #-}

instance Class (KnownNat n) (KnownDim n) where
    cls = Sub $ reifyNat @_ @n (fromIntegral $ dimVal' @n) Dict

-- | Similar to `someNatVal` from `GHC.TypeNats`.
someDimVal :: Word -> SomeDim
someDimVal = coerce
{-# INLINE someDimVal #-}

-- | `constrainDim` with explicitly-passed constraining @Dim@
--   to avoid @AllowAmbiguousTypes@.
constrainBy :: forall x p y
             . BoundedDim x => p x -> Dim y -> Maybe (Dim x)
constrainBy = const (constrainDim @x @y)
{-# INLINE constrainBy #-}

-- | Decrease minimum allowed size of a @Dim (XN x)@.
relax :: forall (m :: Nat) (n :: Nat) . (<=) m n => Dim (XN n) -> Dim (XN m)
relax = coerce
{-# INLINE relax #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
--
--   Note, this function works on @Nat@-indexed dimensions only,
--   because @Dim (XN x)@ does not have runtime evidence to infer @x@
--   and `KnownDim x` does not imply `KnownDim (XN x)`.
sameDim :: forall (x :: Nat) (y :: Nat)
         . Dim x -> Dim y -> Maybe (Dict (x ~ y))
sameDim (DimSing a) (DimSing b)
  | a == b    = Just (unsafeEqTypes @x @y)
  | otherwise = Nothing
{-# INLINE sameDim #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim' :: forall (x :: Nat) (y :: Nat)
          . (KnownDim x, KnownDim y) => Maybe (Dict (x ~ y))
sameDim' = sameDim (dim @x) (dim @y)
{-# INLINE sameDim' #-}

-- | We either get evidence that @x@ is not greater than @y@, or Nothing.
lessOrEqDim :: forall (x :: Nat) (y :: Nat)
             . Dim x -> Dim y -> Maybe (Dict (x <= y))
lessOrEqDim a b = case compareDim a b of
  SLT -> Just Dict
  SEQ -> Just Dict
  SGT -> Nothing
{-# INLINE lessOrEqDim #-}

-- | We either get evidence that @x@ is not greater than @y@, or Nothing.
lessOrEqDim' :: forall (x :: Nat) (y :: Nat)
              . (KnownDim x, KnownDim y) => Maybe (Dict (x <= y))
lessOrEqDim' = lessOrEqDim (dim @x) (dim @y)
{-# INLINE lessOrEqDim' #-}

-- | Ordering of dimension values.
--
--   Note: `CmpNat` forces type parameters to kind `Nat`;
--         if you want to compare unknown `XNat`s, use `Ord` instance of `Dim`.
compareDim :: forall (a :: Nat) (b :: Nat)
            . Dim a -> Dim b -> SOrdering (CmpNat a b)
compareDim a b
  = case coerce (compare :: Word -> Word -> Ordering) a b of
    LT -> unsafeCoerce SLT
    EQ -> unsafeCoerce SEQ
    GT -> unsafeCoerce SGT
{-# INLINE compareDim #-}

-- | Ordering of dimension values.
--
--   Note: `CmpNat` forces type parameters to kind `Nat`;
--         if you want to compare unknown `XNat`s, use `Ord` instance of `Dim`.
compareDim' :: forall (a :: Nat) (b :: Nat)
             . (KnownDim a, KnownDim b) => SOrdering (CmpNat a b)
compareDim' = compareDim (dim @a)  (dim @b)
{-# INLINE compareDim' #-}

-- | Same as `Prelude.(+)`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (n + m)@.
plusDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim (n + m)
plusDim = coerce ((+) :: Word -> Word -> Word)
{-# INLINE plusDim #-}

-- | Same as `Prelude.(-)`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (n - m)@.
minusDim :: forall (n :: Nat) (m :: Nat) . (<=) m n => Dim n -> Dim m -> Dim (n - m)
minusDim = coerce ((-) :: Word -> Word -> Word)
{-# INLINE minusDim #-}

-- | Similar to `minusDim`, but returns @Nothing@ if the result would be negative.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (n - m)@.
minusDimM :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Maybe (Dim (n - m))
minusDimM (DimSing a) (DimSing b)
  | a >= b    = Just (coerce (a - b))
  | otherwise = Nothing
{-# INLINE minusDimM #-}

-- | Same as `Prelude.(*)`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (n * m)@.
timesDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim ((*) n m)
timesDim = coerce ((*) :: Word -> Word -> Word)
{-# INLINE timesDim #-}

-- | Same as `Prelude.(^)`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (n ^ m)@.
powerDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim ((^) n m)
powerDim = coerce ((^) :: Word -> Word -> Word)
{-# INLINE powerDim #-}

-- | Same as `Prelude.div`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (Div n m)@.
divDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim (Div n m)
divDim = coerce (div :: Word -> Word -> Word)

-- | Same as `Prelude.mod`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (Mod n m)@.
modDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim (Mod n m)
modDim = coerce (mod :: Word -> Word -> Word)

-- | Returns log base 2 (round down).
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (Log2 n)@.
log2Dim :: forall (n :: Nat) . Dim n -> Dim (Log2 n)
log2Dim (DimSing 0) = undefined
log2Dim (DimSing x) = DimSing . fromIntegral $ finiteBitSize x - 1 - countLeadingZeros x

-- | Same as `Prelude.min`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (Min n m)@.
minDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim (Min n m)
minDim = coerce (min :: Word -> Word -> Word)

-- | Same as `Prelude.max`.
--   Pattern-matching against the result would produce the evindence
--    @KnownDim (Max n m)@.
maxDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim (Max n m)
maxDim = coerce (max :: Word -> Word -> Word)



-- | Match @Dim n@ against a concrete @Nat@
pattern D0 :: forall (n :: Nat) . () => n ~ 0 => Dim n
pattern D0 <- (sameDim (D @0) -> Just Dict)
  where D0 = DimSing 0

-- | Match @Dim n@ against a concrete @Nat@
pattern D1 :: forall (n :: Nat) . () => n ~ 1 => Dim n
pattern D1 <- (sameDim (D @1) -> Just Dict)
  where D1 = DimSing 1

-- | Match @Dim n@ against a concrete @Nat@
pattern D2 :: forall (n :: Nat) . () => n ~ 2 => Dim n
pattern D2 <- (sameDim (D @2) -> Just Dict)
  where D2 = DimSing 2

-- | Match @Dim n@ against a concrete @Nat@
pattern D3 :: forall (n :: Nat) . () => n ~ 3 => Dim n
pattern D3 <- (sameDim (D @3) -> Just Dict)
  where D3 = DimSing 3

-- | Match @Dim n@ against a concrete @Nat@
pattern D4 :: forall (n :: Nat) . () => n ~ 4 => Dim n
pattern D4 <- (sameDim (D @4) -> Just Dict)
  where D4 = DimSing 4

-- | Match @Dim n@ against a concrete @Nat@
pattern D5 :: forall (n :: Nat) . () => n ~ 5 => Dim n
pattern D5 <- (sameDim (D @5) -> Just Dict)
  where D5 = DimSing 5

-- | Match @Dim n@ against a concrete @Nat@
pattern D6 :: forall (n :: Nat) . () => n ~ 6 => Dim n
pattern D6 <- (sameDim (D @6) -> Just Dict)
  where D6 = DimSing 6

-- | Match @Dim n@ against a concrete @Nat@
pattern D7 :: forall (n :: Nat) . () => n ~ 7 => Dim n
pattern D7 <- (sameDim (D @7) -> Just Dict)
  where D7 = DimSing 7

-- | Match @Dim n@ against a concrete @Nat@
pattern D8 :: forall (n :: Nat) . () => n ~ 8 => Dim n
pattern D8 <- (sameDim (D @8) -> Just Dict)
  where D8 = DimSing 8

-- | Match @Dim n@ against a concrete @Nat@
pattern D9 :: forall (n :: Nat) . () => n ~ 9 => Dim n
pattern D9 <- (sameDim (D @9) -> Just Dict)
  where D9 = DimSing 9

-- | Match @Dim n@ against a concrete @Nat@
pattern D10 :: forall (n :: Nat) . () => n ~ 10 => Dim n
pattern D10 <- (sameDim (D @10) -> Just Dict)
  where D10 = DimSing 10

-- | Match @Dim n@ against a concrete @Nat@
pattern D11 :: forall (n :: Nat) . () => n ~ 11 => Dim n
pattern D11 <- (sameDim (D @11) -> Just Dict)
  where D11 = DimSing 11

-- | Match @Dim n@ against a concrete @Nat@
pattern D12 :: forall (n :: Nat) . () => n ~ 12 => Dim n
pattern D12 <- (sameDim (D @12) -> Just Dict)
  where D12 = DimSing 12

-- | Match @Dim n@ against a concrete @Nat@
pattern D13 :: forall (n :: Nat) . () => n ~ 13 => Dim n
pattern D13 <- (sameDim (D @13) -> Just Dict)
  where D13 = DimSing 13

-- | Match @Dim n@ against a concrete @Nat@
pattern D14 :: forall (n :: Nat) . () => n ~ 14 => Dim n
pattern D14 <- (sameDim (D @14) -> Just Dict)
  where D14 = DimSing 14

-- | Match @Dim n@ against a concrete @Nat@
pattern D15 :: forall (n :: Nat) . () => n ~ 15 => Dim n
pattern D15 <- (sameDim (D @15) -> Just Dict)
  where D15 = DimSing 15

-- | Match @Dim n@ against a concrete @Nat@
pattern D16 :: forall (n :: Nat) . () => n ~ 16 => Dim n
pattern D16 <- (sameDim (D @16) -> Just Dict)
  where D16 = DimSing 16

-- | Match @Dim n@ against a concrete @Nat@
pattern D17 :: forall (n :: Nat) . () => n ~ 17 => Dim n
pattern D17 <- (sameDim (D @17) -> Just Dict)
  where D17 = DimSing 17

-- | Match @Dim n@ against a concrete @Nat@
pattern D18 :: forall (n :: Nat) . () => n ~ 18 => Dim n
pattern D18 <- (sameDim (D @18) -> Just Dict)
  where D18 = DimSing 18

-- | Match @Dim n@ against a concrete @Nat@
pattern D19 :: forall (n :: Nat) . () => n ~ 19 => Dim n
pattern D19 <- (sameDim (D @19) -> Just Dict)
  where D19 = DimSing 19

-- | Match @Dim n@ against a concrete @Nat@
pattern D20 :: forall (n :: Nat) . () => n ~ 20 => Dim n
pattern D20 <- (sameDim (D @20) -> Just Dict)
  where D20 = DimSing 20

-- | Match @Dim n@ against a concrete @Nat@
pattern D21 :: forall (n :: Nat) . () => n ~ 21 => Dim n
pattern D21 <- (sameDim (D @21) -> Just Dict)
  where D21 = DimSing 21

-- | Match @Dim n@ against a concrete @Nat@
pattern D22 :: forall (n :: Nat) . () => n ~ 22 => Dim n
pattern D22 <- (sameDim (D @22) -> Just Dict)
  where D22 = DimSing 22

-- | Match @Dim n@ against a concrete @Nat@
pattern D23 :: forall (n :: Nat) . () => n ~ 23 => Dim n
pattern D23 <- (sameDim (D @23) -> Just Dict)
  where D23 = DimSing 23

-- | Match @Dim n@ against a concrete @Nat@
pattern D24 :: forall (n :: Nat) . () => n ~ 24 => Dim n
pattern D24 <- (sameDim (D @24) -> Just Dict)
  where D24 = DimSing 24

-- | Match @Dim n@ against a concrete @Nat@
pattern D25 :: forall (n :: Nat) . () => n ~ 25 => Dim n
pattern D25 <- (sameDim (D @25) -> Just Dict)
  where D25 = DimSing 25


#define PLEASE_STYLISH_HASKELL \
  forall ds . KnownDimKind (KindOfEl ds) => \
  forall (ns :: [Nat]) . (ds ~~ ns, Dimensions ns) => \
  Dims ds

-- | @O(1)@ Pattern-matching against this constructor brings a `Dimensions`
--   instance into the scope.
--   Thus, you can do arbitrary operations on your dims and use this pattern
--   at any time to reconstruct the class instance at runtime.
pattern Dims :: PLEASE_STYLISH_HASKELL
pattern Dims <- (patDims (dimKind @(KindOfEl ds)) -> PatNats)
  where
    Dims = dims @ds
#undef PLEASE_STYLISH_HASKELL


#define PLEASE_STYLISH_HASKELL \
  forall ds . KnownDimKind (KindOfEl ds) => \
  forall (ns :: [Nat]) . (FixedDims ds ns) => \
  Dims ns -> Dims ds

-- | @O(n)@
--   Pattern-matching against this constructor reveals Nat-kinded list of dims,
--   pretending the dimensionality is known at compile time within the scope
--   of the pattern match.
--   This is the main recommended way to get `Dims` at runtime;
--   for example, reading a list of dimensions from a file.
pattern XDims :: PLEASE_STYLISH_HASKELL
pattern XDims ns <- (patDims (dimKind @(KindOfEl ds)) -> PatXNats ns)
  where
    XDims = unsafeCastTL
#undef PLEASE_STYLISH_HASKELL

-- | @O(Length ds)@ A heavy weapon against all sorts of type errors
pattern KnownDims :: forall (ds :: [Nat]) . ()
                  => ( All KnownDim ds, All BoundedDim ds
                     , RepresentableList ds, Dimensions ds)
                     => Dims ds
pattern KnownDims <- (patKDims -> PatKDims)
  where
    KnownDims = dims @ds

#if !IS_UNSOUND_MATCHING_810_900
{-# COMPLETE Dims #-}
{-# COMPLETE XDims #-}
#endif
{-# COMPLETE Dims, XDims #-}
{-# COMPLETE KnownDims #-}

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDims = forall (ns :: [Nat]) . SomeDims (Dims ns)

-- | Put runtime evidence of `Dims` value inside function constraints.
--   Similar to `KnownDim` or `KnownNat`, but for lists of numbers.
--
--   Note, kind of the @Dimensions@ list is usually @Nat@, restricted by
--   @KnownDim@ being also @Nat@-indexed
--     (it is impossible to create a unique @KnownDim (XN m)@ instance).
--   Nevertheless, you can have @KnownDim (N n)@, which is useful in some cases.
class Dimensions ds where
    -- | Get dimensionality of a space at runtime,
    --   represented as a list of `Dim`.
    --
    --   Note, this function is supposed to be used with @TypeApplications@.
    --   For example, you can type:
    --
    --   >>>:set -XTypeApplications
    --   >>>:set -XDataKinds
    --   >>>:t dims @'[17, 12]
    --   dims @'[17, 12] :: Dims '[17, 12]
    --
    --   >>>:t dims @'[]
    --   dims @'[] :: Dims '[]
    --
    --   >>>:t dims @(Tail '[3,2,5,7])
    --   dims @(Tail '[3,2,5,7]) :: Dims '[2, 5, 7]
    --
    dims :: Dims ds


instance Dimensions ('[] :: [k]) where
    dims = U
    {-# INLINE dims #-}

instance (KnownDim d, Dimensions ds) => Dimensions ((d ': ds) :: [k]) where
    dims = dim :* dims
    {-# INLINE dims #-}

-- | If you have @Dimensions ds@, then @ds@ can only be @[Nat]@ or a known type of
--   @[XNa]t@ (i.e. all @N n@).
--   This function assures the type checker that this is indeed the case.
withKnownXDims :: forall (ds :: [XNat]) (rep :: RuntimeRep) (r :: TYPE rep)
                . Dimensions ds
               => (( Dimensions (DimsBound ds), ExactDims ds
                   , All KnownDimType ds, FixedDims ds (DimsBound ds)) => r)
               -> r
withKnownXDims f
  | Dict <- unsafeEqTypes @ds @(Map 'N (DimsBound ds))
    = reifyDims @Nat @(DimsBound ds) dsN
        (withBareConstraint (dictToBare (inferExactFixedDims @ds dsN)) (\_ -> f) ())
  where
    dsN :: Dims (DimsBound ds)
    dsN = unsafeCastTL (dims @ds)
{-# INLINE withKnownXDims #-}
{-# ANN withKnownXDims "HLint: ignore Use const" #-}

-- | Minimal or exact bound of @Dims@.
--   This is a plural form of `DimBound`.
type family DimsBound (ds :: [k]) :: [Nat] where
    DimsBound (ns :: [Nat]) = ns
    DimsBound ('[] :: [XNat]) = '[]
    DimsBound (n ': ns) = DimBound n ': DimsBound ns

-- | Every dim in a list is either @Nat@, or a known @XNat@ (i.e. @N n@).
type family ExactDims (d :: [k]) :: Constraint where
    ExactDims (_  :: [Nat])  = ()
    ExactDims (xs :: [XNat]) = xs ~ Map 'N (DimsBound xs)

-- | This is a technical "helper" family that allows to infer BoundedDims
--   constraint on a tail of a list via the superclass relation.
type family BoundedDimsTail (ds :: [k]) where
    BoundedDimsTail '[] = UnreachableConstraint (BoundedDims (Tail '[]))
                           "BoundDimsTail '[] -- BoundedDims (Tail '[])"
    BoundedDimsTail (_ ': ns) = BoundedDims ns

-- | Get a minimal or exact bound of @Dims@.
--
--   This is a plural form of `BoundedDim`.
--
--   @BoundedDims@ is a somewhat weaker form of @Dimensions@:
--
--    * It is defined for both @[Nat]@ and @[XNat]@;
--    * Instance of @Dimensions ds@ always implies @BoundedDims ds@.
--
--   @BoundedDims@ is a powerful inference tool:
--     its instances do not require much, but it provides a lot via the superclass
--     constraints.
class ( KnownDimKind (KindOfEl ds), All BoundedDim ds, RepresentableList ds
      , Dimensions (DimsBound ds), BoundedDimsTail ds)
   => BoundedDims ds where
    -- | Plural form for `dimBound`
    dimsBound :: Dims (DimsBound ds)
    -- | Plural form for `constrainDim`.
    --
    --   Given a @Dims ys@, test if its runtime value satisfies constraints imposed by
    --   @BoundedDims xs@, and returns it back coerced to @Dims xs@ on success.
    --
    --   This function allows to guess safely individual dimension values,
    --   as well as the length of the dimension list.
    --   It returns @Nothing@ if @xs@ and @ys@ have different length or if any
    --   of the values in @ys@ are less than the corresponding values of @xs@.
    constrainDims :: forall ys . Dims ys -> Maybe (Dims ds)

-- | Minimal runtime @Dims ds@ value that satifies the constraints imposed by
--   the type signature of @Dims ds@
--   (this is the exact @dims@ for @Nat@s and the minimal bound for @XNat@s).
minimalDims :: forall ds . BoundedDims ds => Dims ds
minimalDims = unsafeCastTL (dimsBound @ds)


{-# ANN defineBoundedDims ClassDict #-}
defineBoundedDims ::
       forall (k :: Type) (ds :: [k])
     . ( KnownDimKind (KindOfEl ds), All BoundedDim ds, RepresentableList ds
       , Dimensions (DimsBound ds), BoundedDimsTail ds)
    => Dims (DimsBound ds)
    -> (forall (l :: Type) (ys :: [l]) . Dims ys -> Maybe (Dims ds))
    -> Dict (BoundedDims ds)
defineBoundedDims = defineBoundedDims

-- instance {-# INCOHERENT #-} Dimensions ns => BoundedDims (ns :: [k])
{-# ANN incohInstBoundedDims (ToInstance Incoherent) #-}
incohInstBoundedDims ::
       forall (k :: Type) (ds :: [k])
     . (Dimensions ds, KnownDimKind k) => Dict (BoundedDims ds)
incohInstBoundedDims
    = incohInstBoundedDims' @k @ds dims (inferAllBoundedDims ds)
  where
    ds = dims @ds

incohInstBoundedDims' ::
       forall (k :: Type) (ds :: [k])
     . KnownDimKind k
    => Dims ds
    -> Dict (All BoundedDim ds, RepresentableList ds)
    -> Dict (BoundedDims ds)
incohInstBoundedDims' ds Dict = case dimsBound' of
  Dims -> case ds of
    U   -> defineBoundedDims dimsBound' constrainDims'
    _ :* ds'
      | _ :* TypeList <- tList @ds
      , Dict <- incohInstBoundedDims' ds' Dict
        -> defineBoundedDims dimsBound' constrainDims'
#if __GLASGOW_HASKELL__ < 810
    _ -> error "incohInstBoundedDims': impossible pattern"
#endif
#if IS_UNSOUND_MATCHING_810_900
  _ -> error "incohInstBoundedDims': impossible pattern"
#endif
  where
    dimsBound' :: Dims (DimsBound ds)
    dimsBound' = unsafeCastTL ds
    constrainDims' :: forall (l :: Type) (ys :: [l]) . Dims ys -> Maybe (Dims ds)
    constrainDims' ys
      | listDims ys == listDims dimsBound'
                  = Just (unsafeCastTL ys)
      | otherwise = Nothing

#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
instance Dimensions ns => BoundedDims (ns :: [Nat]) where
    dimsBound = undefined
    constrainDims = undefined
#endif


instance BoundedDims ('[] :: [XNat]) where
    dimsBound = U
    constrainDims U        = Just U
    constrainDims (_ :* _) = Nothing

instance (BoundedDim n, BoundedDims ns) => BoundedDims ((n ': ns) :: [XNat]) where
    dimsBound = dimBound @n :* dimsBound @ns
    constrainDims U         = Nothing
    constrainDims (y :* ys) = (:*) <$> constrainDim y <*> constrainDims ys



-- | Construct a @Dims ds@ if there is an instance of @Typeable ds@ around.
typeableDims :: forall (ds :: [Nat]) . Typeable ds => Dims ds
typeableDims = case typeRep @ds of
    App (App _ (tx :: TypeRep (n :: k1))) (txs :: TypeRep (ns :: k2))
      | Dict <- unsafeEqTypes @k1 @Nat
      , Dict <- unsafeEqTypes @k2 @[Nat]
      , Dict <- unsafeEqTypes @ds @(n ': ns)
      -> withTypeable tx (typeableDim @n) :* withTypeable txs (typeableDims @ns)
    Con _
      -> unsafeCoerce U
    r -> error ("typeableDims -- impossible typeRep: " ++ show r)
{-# INLINE typeableDims #-}

-- | @Dims (ds :: [Nat])@ is always @Typeable@.
inferTypeableDims :: forall (ds :: [Nat]) . Dims ds -> Dict (Typeable ds)
inferTypeableDims U         = Dict
inferTypeableDims ((D :: Dim d) :* ds)
  | Dict <- mapDict cls (Dict @(KnownDim d))
  , Dict <- inferTypeableDims ds
    = Dict


-- | @O(1)@ Convert @Dims xs@ to a plain haskell list of dimension sizes.
--
--   Note, for @XNat@-indexed list it returns actual content dimensions,
--   not the constraint numbers (@XN m@)
listDims :: forall xs . Dims xs -> [Word]
listDims = unsafeCoerce
{-# INLINE listDims #-}

-- | Convert a plain haskell list of dimension sizes into an unknown
--   type-level dimensionality  @O(1)@.
someDimsVal :: [Word] -> SomeDims
someDimsVal = SomeDims . unsafeCoerce
{-# INLINE someDimsVal #-}

-- | Product of all dimension sizes @O(Length xs)@.
totalDim :: forall xs . Dims xs -> Word
totalDim = product . listDims
{-# INLINE totalDim #-}

-- | Product of all dimension sizes @O(Length xs)@.
totalDim' :: forall xs . Dimensions xs => Word
totalDim' = totalDim (dims @xs)
{-# INLINE totalDim' #-}

-- | Drop the given prefix from a Dims list.
--   It returns Nothing if the list did not start with the prefix given,
--    or Just the Dims after the prefix, if it does.
stripPrefixDims :: forall (xs :: [Nat]) (ys :: [Nat])
                 . Dims xs -> Dims ys
                -> Maybe (Dims (StripPrefix xs ys))
stripPrefixDims = unsafeCoerce (Data.List.stripPrefix :: [Word] -> [Word] -> Maybe [Word])
{-# INLINE stripPrefixDims #-}

-- | Drop the given suffix from a Dims list.
--   It returns Nothing if the list did not end with the suffix given,
--    or Just the Dims before the suffix, if it does.
stripSuffixDims :: forall (xs :: [Nat]) (ys :: [Nat])
                 . Dims xs -> Dims ys
                -> Maybe (Dims (StripSuffix xs ys))
stripSuffixDims = unsafeCoerce stripSuf
  where
    stripSuf :: [Word] -> [Word] -> Maybe [Word]
    stripSuf suf whole = go pref whole
      where
        pref = getPref suf whole
        getPref (_:as) (_:bs) = getPref as bs
        getPref [] bs         = zipWith const whole bs
        getPref _  []         = []
        go (_:as) (_:bs) = go as bs
        go  []     bs    = if suf == bs then Just pref else Nothing
        go  _      []    = Nothing
{-# INLINE stripSuffixDims #-}

-- | We either get evidence that this function was instantiated with the
--   same type-level Dimensions, or 'Nothing' @O(Length xs)@.
sameDims :: forall (as :: [Nat]) (bs :: [Nat])
          . Dims as -> Dims bs -> Maybe (Dict (as ~ bs))
sameDims as bs
  | listDims as == listDims bs
    = Just (unsafeEqTypes @as @bs)
  | otherwise = Nothing
{-# INLINE sameDims #-}

-- | We either get evidence that this function was instantiated with the
--   same type-level Dimensions, or 'Nothing' @O(Length xs)@.
sameDims' :: forall (as :: [Nat]) (bs :: [Nat]) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
           . (Dimensions as, Dimensions bs)
          => p as -> q bs -> Maybe (Dict (as ~ bs))
sameDims' = const . const $ sameDims (dims @as) (dims @bs)
{-# INLINE sameDims' #-}


-- | Restricted version of `const`, similar to `asProxyTypeOf`;
--   to be used on such implicit functions as `dims`, `dimsBound` etc.
inSpaceOf :: forall ds p q
           . p ds -> q ds -> p ds
inSpaceOf = const
{-# INLINE inSpaceOf #-}

-- | Constrain @Nat@ dimensions hidden behind @XNat@s.
--   This is a link connecting the two types of type-level dims;
--   you often need it to convert @Dims@, @Idxs@, and data.
type family FixedDims (xns::[XNat]) (ns :: [Nat]) :: Constraint where
    FixedDims '[] ns = (ns ~ '[])
    FixedDims (xn ': xns) ns
      = ( ns ~ (Head ns ': Tail ns)
        , FixedDim xn (Head ns)
        , FixedDims xns (Tail ns))

-- | Try to instantiate the `FixedDims` constraint given two @Dims@ lists.
--
--   The first @Dims@ is assumed to be the output of @minimalDims@,
--   i.e. @listDims xns == toList (listDims xns)@.
--
--   If you input a list that is not equal to its type-level @DimsBound@,
--   you will just have a lower chance to get @Just Dict@ result.
inferFixedDims :: forall (xns :: [XNat]) (ns :: [Nat])
                . All KnownDimType xns
               => Dims xns -> Dims ns -> Maybe (Dict (FixedDims xns ns))
inferFixedDims U U = Just Dict
inferFixedDims (Dx (a :: Dim n) :* xns) (b :* ns)
  | Dict <- unsafeEqTypes @n @(DimBound (Head xns))
  , Just Dict <- lessOrEqDim a b
  , Just Dict <- inferFixedDims xns ns
    = Just Dict
inferFixedDims (Dn a :* xns) (b :* ns)
  | Just Dict <- sameDim a b
  , Just Dict <- inferFixedDims xns ns
    = Just Dict
inferFixedDims _ _ = Nothing

-- | A very unsafe function that bypasses all type-level checks and constructs
--   the evidence from nothing.
unsafeInferFixedDims :: forall (xns :: [XNat]) (ns :: [Nat])
                      . Dims ns -> Dict (FixedDims xns ns)
unsafeInferFixedDims U
  | Dict <- unsafeEqTypes @xns @'[] = Dict
unsafeInferFixedDims ((D :: Dim n) :* ns)
    {-
    Very unsafe operation.
    I rely here on the fact that FixedDim xn n has the same
    runtime rep as a single type equality.
    If that changes, then the code is broken.
     -}
  | Dict <- unsafeEqTypes @xns @(N n ': Tail xns)
  , Dict <- unsafeInferFixedDims @(Tail xns) ns = Dict
{-# INLINE unsafeInferFixedDims #-}

-- | Infer `FixedDims` if you know that all of dims are exact (@d ~ N n@).
--   This function is totally safe and faithful.
inferExactFixedDims :: forall (ds :: [XNat]) . ExactDims ds
                    => Dims (DimsBound ds)
                    -> Dict (All KnownDimType ds, FixedDims ds (DimsBound ds))
inferExactFixedDims U = Dict
inferExactFixedDims (_ :* ns)
  | Dict <- inferExactFixedDims @(Tail ds) ns = Dict
{-# INLINE inferExactFixedDims #-}

instance Typeable d => Data (Dim (d :: Nat)) where
    gfoldl _ f = f
    gunfold _ z = const (z (typeableDim @d))
    toConstr = const $ dimNatConstr (dimVal (typeableDim @d))
    dataTypeOf = const $ dimDataType (dimVal (typeableDim @d))

dimDataType :: Word -> DataType
dimDataType = mkDataType "Numeric.Dim.Dim" . (:[]) . dimNatConstr

dimNatConstr :: Word -> Constr
dimNatConstr d = mkConstr (dimDataType d) ("D" ++ show d) [] Prefix

instance KnownDim d => G.Generic (Dim (d :: Nat)) where
    type Rep (Dim d) = G.D1
          ('G.MetaData "Dim" "Numeric.Dim" "dimensions" 'False)
          (G.C1 ('G.MetaCons (AppendSymbol "D" (ShowNat d)) 'G.PrefixI 'False) G.U1)
    from D = G.M1 (G.M1 G.U1)
    to = const (dim @d)

instance Eq (Dim (n :: Nat)) where
    (==) = const (const True)
    {-# INLINE (==) #-}
    (/=) = const (const False)
    {-# INLINE (/=) #-}

instance Eq (Dim (x :: XNat)) where
    (==) = coerce ((==) :: Word -> Word -> Bool)
    {-# INLINE (==) #-}
    (/=) = coerce ((/=) :: Word -> Word -> Bool)
    {-# INLINE (/=) #-}

instance Eq (Dims (ds :: [Nat])) where
    (==) = const (const True)
    {-# INLINE (==) #-}
    (/=) = const (const False)
    {-# INLINE (/=) #-}

instance Eq (Dims (ds :: [XNat])) where
    (==) = unsafeCoerce ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}
    (/=) = unsafeCoerce ((/=) :: [Word] -> [Word] -> Bool)
    {-# INLINE (/=) #-}

instance Eq SomeDims where
    SomeDims as == SomeDims bs = listDims as == listDims bs
    {-# INLINE (==) #-}
    SomeDims as /= SomeDims bs = listDims as /= listDims bs
    {-# INLINE (/=) #-}

instance Ord (Dim (n :: Nat)) where
    compare = const (const EQ)
    {-# INLINE compare #-}

instance Ord (Dim (x :: XNat)) where
    compare = coerce (compare :: Word -> Word -> Ordering)
    {-# INLINE compare #-}

instance Ord (Dims (ds :: [Nat])) where
    compare = const (const EQ)
    {-# INLINE compare #-}

instance Ord (Dims (ds :: [XNat])) where
    compare = unsafeCoerce (compare :: [Word] -> [Word] -> Ordering)
    {-# INLINE compare #-}

instance Ord SomeDims where
    compare (SomeDims as) (SomeDims bs) = compare (listDims as) (listDims bs)
    {-# INLINE compare #-}

instance Show (Dim (x :: k)) where
    showsPrec _ d = showChar 'D' . shows (dimVal d)
    {-# INLINE showsPrec #-}

instance Show (Dims (xs :: [k])) where
    showsPrec = typedListShowsPrec @Dim @xs showsPrec

instance Show SomeDims where
    showsPrec p (SomeDims ds)
      = showParen (p >= 10)
      $ showString "SomeDims " . showsPrec 10 ds

instance BoundedDim x => Read (Dim (x :: k)) where
    readPrec = Read.lexP >>= \case
      Read.Ident ('D':s)
        | Just d <- Read.readMaybe s
            >>= constrainDim @x @(XN 0) . DimSing
          -> return d
      _  -> Read.pfail
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance BoundedDims xs => Read (Dims (xs :: [k])) where
    readPrec = typedListReadPrec @BoundedDim ":*" Read.readPrec (tList @xs)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance Read SomeDims where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "SomeDims"
      withTypedListReadPrec @Dim @SomeDims
        (\g -> (\(Dx d) -> g d) <$> Read.readPrec @(Dim (XN 0)))
        SomeDims



-- | This function does GHC's magic to convert user-supplied `dim` function
--   to create an instance of `KnownDim` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall (k :: Type) (d :: k) (rep :: RuntimeRep) (r :: TYPE rep)
          . Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce (MagicDim k :: MagicDim d r) d
{-# INLINE reifyDim #-}
newtype MagicDim (d :: k) (r :: TYPE rep) = MagicDim (KnownDim d => r)

reifyNat :: forall (r :: Type) (d :: Nat) . Natural -> (KnownNat d => r) -> r
reifyNat d k = unsafeCoerce (MagicNat k :: MagicNat d r) d
{-# INLINE reifyNat #-}
newtype MagicNat (d :: Nat) (r :: Type) = MagicNat (KnownNat d => r)

reifyDims :: forall (k :: Type) (ds :: [k]) (rep :: RuntimeRep) (r :: TYPE rep)
           . Dims ds -> (Dimensions ds => r) -> r
reifyDims ds k = unsafeCoerce (MagicDims k :: MagicDims ds r) ds
{-# INLINE reifyDims #-}
newtype MagicDims (ds :: [k]) (r :: TYPE rep) = MagicDims (Dimensions ds => r)


data PatDim (d :: k) where
  PatNat   :: KnownDim n =>          PatDim (n :: Nat)
  PatXNatN ::               Dim n -> PatDim (N n)
  PatXNatX :: (m <= n)   => Dim n -> PatDim (XN m)

patDim :: forall (k :: Type) (d :: k) . DimType d -> Dim d -> PatDim d
patDim DimTNat   d = reifyDim d  PatNat
patDim DimTXNatN d = PatXNatN (coerce d)
patDim DimTXNatX d = f (coerce d) d
  where
    f :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim (XN m) -> PatDim (XN m)
    f n = case unsafeCoerceDict @(m <= m) @(m <= n) Dict of
            Dict -> const (PatXNatX n)
{-# INLINE patDim #-}

data PatDims (ds :: [k]) where
  PatNats  :: Dimensions ds   =>            PatDims (ds :: [Nat])
  PatXNats :: FixedDims ds ns => Dims ns -> PatDims (ds :: [XNat])

patDims :: forall (k :: Type) (ds :: [k]) . DimKind k -> Dims ds -> PatDims ds
patDims DimKNat  ds = reifyDims ds PatNats
patDims DimKXNat ds = withBareConstraint
    (dictToBare (unsafeInferFixedDims @ds ds')) (PatXNats ds')
  where
    ds' = unsafeCastTL ds -- convert to *some* [Nat]
{-# INLINE patDims #-}

{-
I know that Dimensions can be either Nats or N-known XNats.
Therefore, inside the worker function I can (un)safely pick up a BoundedDim
instance for (d ~ N n)
 -}
inferAllBoundedDims :: forall (k :: Type) (ds :: [k])
                     . (Dimensions ds, KnownDimKind k)
                    => Dims ds -> Dict (All BoundedDim ds, RepresentableList ds)
inferAllBoundedDims = go
  where
    reifyBoundedDim :: forall (d :: k) . Dim d -> Dict (BoundedDim d)
    reifyBoundedDim = case dimKind @k of
      DimKNat -> (`reifyDim` Dict)
      DimKXNat
        | Dict <- unsafeEqTypes @d @(N (DimBound d))
              -> \d -> reifyDim (coerce d :: Dim (DimBound d)) Dict
    go :: forall (xs :: [k]) . Dims xs
       -> Dict (All BoundedDim xs, RepresentableList xs)
    go U             = Dict
    go (d :* ds)
      | Dict <- reifyBoundedDim d
      , Dict <- go ds = Dict
{-# INLINE inferAllBoundedDims #-}

data PatKDims (ns :: [Nat])
  = ( All KnownDim ns, All BoundedDim ns
    , RepresentableList ns, Dimensions ns)
  => PatKDims

patKDims :: forall (ns :: [Nat]) . Dims ns -> PatKDims ns
patKDims U = PatKDims
patKDims (D :* ns) = case patKDims ns of
  PatKDims -> PatKDims
{-# INLINE patKDims #-}

unsafeCoerceDict :: forall (a :: Constraint) (b :: Constraint)
                  . Dict a -> Dict b
unsafeCoerceDict = unsafeCoerce

unsafeCastTL :: TypedList f (xs :: [k]) -> TypedList g (ys :: [l])
unsafeCastTL = unsafeCoerce
