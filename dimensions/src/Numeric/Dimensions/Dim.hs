{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
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
    XNat (..), XN, N, XNatType (..)
    -- ** Term level dimension
  , Dim ( D, Dn, Dx
        , D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13
        , D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25
        )
  , SomeDim
  , KnownDim (..), BoundedDim (..), minDim, KnownXNatType (..), FixedDim
  , dimVal, dimVal', typeableDim, someDimVal
  , sameDim, sameDim'
  , compareDim, compareDim'
  , constrainBy, relax
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
  , plusDim, minusDim, minusDimM, timesDim, powerDim, divDim, modDim, log2Dim
    -- ** Re-export part of `Data.Type.Lits` for convenience
  , Nat, CmpNat, SOrdering (..), type (+), type (-), type (*), type (^), type (<=)
    -- ** Inferring kind of type-level dimension
  , KnownDimKind (..), DimKind (..)
    -- * @Dims@: a list of dimensions
  , Dims, SomeDims (..), Dimensions (..), BoundedDims (..), DimsBound, minDims
  , TypedList ( Dims, XDims, AsXDims, KnownDims
              , U, (:*), Empty, TypeList, Cons, Snoc, Reverse)
  , typeableDims, inferTypeableDims
  , listDims, someDimsVal, totalDim, totalDim'
  , sameDims, sameDims'
  , inSpaceOf, asSpaceOf
  , xDims, xDims'
  , stripPrefixDims, stripSuffixDims
    -- ** Type-level programming
    --   Provide type families to work with lists of dimensions (`[Nat]` or `[XNat]`)
  , AsXDims, AsDims, FixedDims, KnownXNatTypes
    -- ** Re-export type list
  , RepresentableList (..), TypeList, types
  , order, order'
#if !(defined(__HADDOCK__) || defined(__HADDOCK_VERSION__))
    -- hide a plugin-related func
  , natInstBoundedDims
#endif
  ) where


import           Data.Bits                (countLeadingZeros, finiteBitSize)
import           Data.Coerce
import           Data.Constraint
import           Data.Constraint.Deriving
import           Data.Data                hiding (TypeRep, typeRep,
                                           typeRepTyCon)
import           Data.Kind                (Constraint, Type)
import qualified Data.List                (stripPrefix)
import           Data.Type.List
import           Data.Type.List.Internal
import           Data.Type.Lits
import           GHC.Exts                 (Proxy#, proxy#, unsafeCoerce#)
import qualified GHC.Generics             as G
import           Numeric.Natural          (Natural)
import           Numeric.TypedList
import qualified Text.Read                as Read
import qualified Text.Read.Lex            as Read
import           Type.Reflection

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
  deriving ( Typeable )

-- | Type-level dimensionality.
type Dims (xs :: [k]) = TypedList Dim xs

-- | Match against this pattern to bring `KnownDim` instance into scope.
pattern D :: forall (n :: Nat) . () => KnownDim n => Dim n
pattern D <- (dimEv -> Dict)
  where
    D = dim @n
{-# COMPLETE D #-}

#define PLEASE_STYLISH_HASKELL \
  forall (xn :: XNat) . KnownXNatType xn => \
  forall (n :: Nat) . (KnownDim n, xn ~ 'N n) => \
  Dim n -> Dim xn

-- | Statically known `XNat`
pattern Dn :: PLEASE_STYLISH_HASKELL
pattern Dn k <- (dimXNEv (xNatType @xn) -> PatN k)
  where
    Dn k = coerce k
#undef PLEASE_STYLISH_HASKELL

#define PLEASE_STYLISH_HASKELL \
  forall (xn :: XNat) . KnownXNatType xn => \
  forall (n :: Nat) (m :: Nat) . (KnownDim n, m <= n, xn ~ 'XN m) => \
  Dim n -> Dim xn

-- | `XNat` that is unknown at compile time.
--   Same as `SomeNat`, but for a dimension:
--   Hide dimension size inside, but allow specifying its minimum possible value.
pattern Dx :: PLEASE_STYLISH_HASKELL
pattern Dx k <- (dimXNEv (xNatType @xn) -> PatXN k)
  where
    Dx k = coerce k
{-# COMPLETE Dn, Dx #-}
#undef PLEASE_STYLISH_HASKELL

-- | This class provides the `Dim` associated with a type-level natural.
--
--   Note, kind of the @KnownDim@ argument is always @Nat@, because
--     it is impossible to create a unique @KnownDim (XN m)@ instance.
class KnownDim (n :: Nat) where
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
class (KnownDimKind k, KnownDim (DimBound n)) => BoundedDim (n :: k) where
    -- | Minimal or exact bound of a @Dim@.
    --   Useful for indexing: it is safe to index something by an index less than
    --   @DimBound n@ (for both @Nat@ and @Xnat@ indexed dims).
    type family DimBound n :: Nat
    -- | Get such a minimal @Dim (DimBound n)@, that @Dim n@ is guaranteed
    --   to be not less than @dimBound@ if @n ~ XN a@,
    --     otherwise, the return @Dim@ is the same as @n@.
    dimBound :: Dim (DimBound n)
    -- | If the runtime value of @Dim y@ satisfies @dimBound @k @x@,
    --   then coerce to @Dim x@. Otherwise, return @Nothing@.
    --
    --   To satisfy the @dimBound@ means to be equal to @N n@ or be not less than @XN m@.
    constrainDim :: forall (l :: Type) (y :: l) . Dim y -> Maybe (Dim n)


instance KnownDim n => BoundedDim (n :: Nat) where
    type DimBound n = n
    dimBound = dim @n
    {-# INLINE dimBound #-}
    constrainDim (DimSing y)
       | dimVal' @n == y = Just (DimSing y)
       | otherwise       = Nothing
    {-# INLINE constrainDim #-}

instance KnownDim n => BoundedDim ('N n) where
    type DimBound ('N n) = n
    dimBound = dim @n
    {-# INLINE dimBound #-}
    constrainDim (DimSing y)
       | dimVal' @n == y = Just (DimSing y)
       | otherwise       = Nothing
    {-# INLINE constrainDim #-}

instance KnownDim m => BoundedDim ('XN m) where
    type DimBound ('XN m) = m
    dimBound = dim @m
    {-# INLINE dimBound #-}
    constrainDim (DimSing y)
       | dimVal' @m <= y = Just (DimSing y)
       | otherwise       = Nothing
    {-# INLINE constrainDim #-}

-- | Returns the minimal @Dim@ that satisfies the @BoundedDim@ constraint
--   (this is the exact @dim@ for @Nat@s and the minimal bound for @XNat@s).
minDim :: forall (k :: Type) (d :: k) . BoundedDim d => Dim d
minDim = coerce (dimBound @k @d)

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

-- | Similar to `natVal` from `GHC.TypeNats`, but returns `Word`.
dimVal :: forall (k :: Type) (x :: k) . Dim (x :: k) -> Word
dimVal = coerce
{-# INLINE dimVal #-}

-- | Similar to `natVal` from `GHC.TypeNats`, but returns `Word`.
dimVal' :: forall (n :: Nat) . KnownDim n => Word
dimVal' = coerce (dim @n)
{-# INLINE dimVal' #-}

-- | Construct a @Dim n@ if there is an instance of @Typeable n@ around.
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

instance Class (KnownNat n) (KnownDim n) where
    cls = Sub $ reifyNat @_ @n (fromIntegral $ dimVal' @n) Dict

-- | Similar to `someNatVal` from `GHC.TypeNats`.
someDimVal :: Word -> SomeDim
someDimVal = coerce
{-# INLINE someDimVal #-}

-- | `constrainDim` with explicitly-passed constraining @Dim@
--   to avoid @AllowAmbiguousTypes@.
constrainBy :: forall (k :: Type) (x :: k) (p :: k -> Type) (l :: Type) (y :: l)
             . BoundedDim x => p x -> Dim y -> Maybe (Dim x)
constrainBy = const (constrainDim @k @x @l @y)
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
  | a == b    = Just (unsafeCoerceDict @(x ~ x) Dict)
  | otherwise = Nothing
{-# INLINE sameDim #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim' :: forall (x :: Nat) (y :: Nat) (p :: Nat -> Type) (q :: Nat -> Type)
          . (KnownDim x, KnownDim y)
         => p x -> q y -> Maybe (Dict (x ~ y))
sameDim' = const . const $ sameDim (dim @x) (dim @y)
{-# INLINE sameDim' #-}

-- | Ordering of dimension values.
--
--   Note: `CmpNat` forces type parameters to kind `Nat`;
--         if you want to compare unknown `XNat`s, use `Ord` instance of `Dim`.
compareDim :: forall (a :: Nat) (b :: Nat)
            . Dim a -> Dim b -> SOrdering (CmpNat a b)
compareDim a b
  = case coerce (compare :: Word -> Word -> Ordering) a b of
    LT -> unsafeCoerce# SLT
    EQ -> unsafeCoerce# SEQ
    GT -> unsafeCoerce# SGT
{-# INLINE compareDim #-}

-- | Ordering of dimension values.
--
--   Note: `CmpNat` forces type parameters to kind `Nat`;
--         if you want to compare unknown `XNat`s, use `Ord` instance of `Dim`.
compareDim' :: forall (a :: Nat) (b :: Nat) (p :: Nat -> Type) (q :: Nat -> Type)
             . (KnownDim a, KnownDim b) => p a -> q b -> SOrdering (CmpNat a b)
compareDim' = const . const $ compareDim (dim @a)  (dim @b)
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


-- | GADT to support `KnownDimKind` type class.
--   Match against its constructors to know if @k@ is @Nat@ or @XNat@
data DimKind :: Type -> Type where
    -- | Working on @Nat@.
    DimNat  :: DimKind Nat
    -- | Working on @XNat@.
    DimXNat :: DimKind XNat

-- | Figure out whether the type-level dimension is `Nat` or `XNat`.
--   Useful for generalized inference functions.
class KnownDimKind (k :: Type) where
    dimKind :: DimKind k

instance KnownDimKind Nat where
    dimKind = DimNat

instance KnownDimKind XNat where
    dimKind = DimXNat


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



-- | @O(1)@ Pattern-matching against this constructor brings a `Dimensions`
--   instance into the scope.
--   Thus, you can do arbitrary operations on your dims and use this pattern
--   at any time to reconstruct the class instance at runtime.
pattern Dims :: forall (ds :: [Nat]) . () => Dimensions ds => Dims ds
pattern Dims <- (dimsEv -> Dict)
  where
    Dims = dims @ds
{-# COMPLETE Dims #-}

-- | @O(Length ds)@ A heavy weapon against all sorts of type errors
pattern KnownDims :: forall (ds :: [Nat]) . ()
                  => ( All KnownDim ds, All BoundedDim ds
                     , RepresentableList ds, Dimensions ds)
                     => Dims ds
pattern KnownDims <- (patKDims -> PatKDims)
  where
    KnownDims = dims @ds
{-# COMPLETE KnownDims #-}


#define PLEASE_STYLISH_HASKELL \
  forall (xns :: [XNat]) . KnownXNatTypes xns => \
  forall (ns :: [Nat]) . (FixedDims xns ns, Dimensions ns) => \
  Dims ns -> Dims xns

-- | Pattern-matching against this constructor reveals Nat-kinded list of dims,
--   pretending the dimensionality is known at compile time within the scope
--   of the pattern match.
--   This is the main recommended way to get `Dims` at runtime;
--   for example, reading a list of dimensions from a file.
--
--   In order to use this pattern, one must know @XNat@ type constructors in
--   each dimension at compile time.
pattern XDims :: PLEASE_STYLISH_HASKELL
pattern XDims ns <- (patXDims -> PatXDims ns)
  where
    XDims ns = unsafeCoerce# ns
{-# COMPLETE XDims #-}
#undef PLEASE_STYLISH_HASKELL

-- | An easy way to convert Nat-indexed dims into XNat-indexed dims.
pattern AsXDims :: forall (ns :: [Nat]) . ()
                => (KnownXNatTypes (AsXDims ns), RepresentableList (AsXDims ns))
                => Dims (AsXDims ns) -> Dims ns
pattern AsXDims xns <- (patAsXDims -> PatAsXDims xns)
  where
    AsXDims xns = unsafeCoerce# xns
{-# COMPLETE AsXDims #-}

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDims = forall (ns :: [Nat]) . SomeDims (Dims ns)

-- | Put runtime evidence of `Dims` value inside function constraints.
--   Similar to `KnownDim` or `KnownNat`, but for lists of numbers.
--
--   Note, kind of the @Dimensions@ list is always @Nat@, restricted by
--   @KnownDim@ being also @Nat@-indexed
--     (it is impossible to create a unique @KnownDim (XN m)@ instance).
class Dimensions (ds :: [Nat]) where
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

instance Dimensions '[] where
    dims = U
    {-# INLINE dims #-}

instance (KnownDim d, Dimensions ds) => Dimensions (d ': ds) where
    dims = dim :* dims
    {-# INLINE dims #-}

-- | Minimal or exact bound of @Dims@.
--   This is a plural form of `DimBound`.
type family DimsBound (ds :: [k]) :: [Nat] where
    DimsBound (ns :: [Nat]) = ns
    DimsBound ('[] :: [XNat]) = '[]
    DimsBound (n ': ns) = DimBound n ': DimsBound ns

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
class ( KnownDimKind k, All BoundedDim ds, RepresentableList ds
      , Dimensions (DimsBound ds), BoundedDimsTail ds)
   => BoundedDims (ds :: [k]) where
    -- | Plural form for `dimBound`
    dimsBound :: Dims (DimsBound ds)
    -- | Plural form for `constrainDim`.
    --
    --   Given a @Dims ys@, test if its runtime value satisfies constraints imposed by
    --   @BoundedDims ds@, and returns it back coerced to @Dims ds@ on success.
    --
    --   This function allows to guess safely individual dimension values,
    --   as well as the length of the dimension list.
    --   It returns @Nothing@ if @ds@ and @xds@ have different length or if any
    --   of the values in @ys@ are less than the corresponding values of @ds@.
    constrainDims :: forall (l :: Type) (ys :: [l]) . Dims ys -> Maybe (Dims ds)

{-# ANN defineBoundedDims ClassDict #-}
defineBoundedDims :: forall (k :: Type) (ds :: [k])
                   . ( KnownDimKind k, All BoundedDim ds, RepresentableList ds
                     , Dimensions (DimsBound ds), BoundedDimsTail ds)
                  => Dims (DimsBound ds)
                  -> (forall (l :: Type) (ys :: [l]) . Dims ys -> Maybe (Dims ds))
                  -> Dict (BoundedDims ds)
defineBoundedDims = defineBoundedDims

-- instance Dimensions ns => BoundedDims (ns :: [Nat])
{-# ANN natInstBoundedDims (ToInstance NoOverlap) #-}
natInstBoundedDims :: forall (ns :: [Nat]) . Dimensions ns => Dict (BoundedDims ns)
natInstBoundedDims = natInstBoundedDims' @ns

natInstBoundedDims' :: forall (ns :: [Nat]) . Dimensions ns => Dict (BoundedDims ns)
natInstBoundedDims'
  | Dict <- inferTail
  , Dict <- inferAllBoundedDims
    = defineBoundedDims dimsBound' constrainDims'
  where
    dimsBound' :: Dims (DimsBound ns)
    dimsBound' = dims @ns
    constrainDims' :: forall (l :: Type) (ys :: [l]) . Dims ys -> Maybe (Dims ns)
    constrainDims' ys
      | listDims ys == listDims dimsBound'
                  = Just (unsafeCoerce# ys)
      | otherwise = Nothing
    inferTail :: Dict (BoundedDimsTail ns)
    inferTail = case dimsBound' of
        U         -> Dict
        _ :* Dims -> natInstBoundedDims'
    inferAllBoundedDims :: Dict (All BoundedDim ns, RepresentableList ns)
    inferAllBoundedDims = go dimsBound'
      where
        go :: forall (ds :: [Nat]) . Dims ds
           -> Dict (All BoundedDim ds, RepresentableList ds)
        go  U             = Dict
        go (D :* ds)
          | Dict <- go ds = Dict

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
    dimsBound = dimBound @XNat @n :* dimsBound @XNat @ns
    constrainDims U         = Nothing
    constrainDims (y :* ys) = (:*) <$> constrainDim y <*> constrainDims ys


-- | Minimal runtime @Dims ds@ value that satifies the constraints imposed by
--   the type signature of @Dims ds@
--   (this is the exact @dims@ for @Nat@s and the minimal bound for @XNat@s).
minDims :: forall (k :: Type) (ds :: [k])
         . BoundedDims ds => Dims ds
minDims = unsafeCoerce# (dimsBound @k @ds)




-- | Construct a @Dims ds@ if there is an instance of @Typeable ds@ around.
typeableDims :: forall (ds :: [Nat]) . Typeable ds => Dims ds
typeableDims = case typeRep @ds of
    App (App _ (tx :: TypeRep (n :: k1))) (txs :: TypeRep (ns :: k2))
      -> case unsafeCoerceDict @(Nat ~ Nat, [Nat] ~ [Nat])
                               @(Nat ~ k1 , [Nat] ~ k2) Dict of
          Dict -> case unsafeCoerceDict @(ds ~ ds)
                                        @(ds ~ (n ': ns)) Dict of
            Dict -> withTypeable tx (typeableDim @n)
                 :* withTypeable txs (typeableDims @ns)
    Con _
      -> unsafeCoerce# U
    r -> error ("typeableDims -- impossible typeRep: " ++ show r)
{-# INLINE typeableDims #-}

-- | @Dims (ds :: [Nat])@ is always @Typeable@.
inferTypeableDims :: forall (ds :: [Nat]) . Dims ds -> Dict (Typeable ds)
inferTypeableDims U         = Dict
inferTypeableDims ((D :: Dim d) :* ds)
  | Dict <- mapDict cls (Dict @(KnownDim d))
  , Dict <- inferTypeableDims ds
    = Dict


-- | Convert `Dims xs` to a plain haskell list of dimension sizes @O(1)@.
--
--   Note, for @XNat@-indexed list it returns actual content dimensions,
--   not the constraint numbers (@XN m@)
listDims :: forall (k :: Type) (xs :: [k]) . Dims xs -> [Word]
listDims = unsafeCoerce#
{-# INLINE listDims #-}

-- | Convert a plain haskell list of dimension sizes into an unknown
--   type-level dimensionality  @O(1)@.
someDimsVal :: [Word] -> SomeDims
someDimsVal = SomeDims . unsafeCoerce#
{-# INLINE someDimsVal #-}

-- | Product of all dimension sizes @O(Length xs)@.
totalDim :: forall (k :: Type) (xs :: [k]) . Dims xs -> Word
totalDim = product . listDims
{-# INLINE totalDim #-}

-- | Product of all dimension sizes @O(Length xs)@.
totalDim' :: forall (xs :: [Nat]) . Dimensions xs => Word
totalDim' = totalDim (dims @xs)
{-# INLINE totalDim' #-}

-- | Get XNat-indexed dims given their fixed counterpart.
xDims :: forall (xns :: [XNat]) (ns :: [Nat])
       . FixedDims xns ns => Dims ns -> Dims xns
xDims = unsafeCoerce#
{-# INLINE xDims #-}

-- | Get XNat-indexed dims given their fixed counterpart.
xDims' :: forall (xns :: [XNat]) (ns :: [Nat])
        . (FixedDims xns ns, Dimensions ns) => Dims xns
xDims' = xDims @xns (dims @ns)
{-# INLINE xDims' #-}

-- | Drop the given prefix from a Dims list.
--   It returns Nothing if the list did not start with the prefix given,
--    or Just the Dims after the prefix, if it does.
stripPrefixDims :: forall (xs :: [Nat]) (ys :: [Nat])
                 . Dims xs -> Dims ys
                -> Maybe (Dims (StripPrefix xs ys))
stripPrefixDims = unsafeCoerce# (Data.List.stripPrefix :: [Word] -> [Word] -> Maybe [Word])
{-# INLINE stripPrefixDims #-}

-- | Drop the given suffix from a Dims list.
--   It returns Nothing if the list did not end with the suffix given,
--    or Just the Dims before the suffix, if it does.
stripSuffixDims :: forall (xs :: [Nat]) (ys :: [Nat])
                 . Dims xs -> Dims ys
                -> Maybe (Dims (StripSuffix xs ys))
stripSuffixDims = unsafeCoerce# stripSuf
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
    = Just (unsafeCoerceDict @(as ~ as) Dict)
  | otherwise = Nothing
{-# INLINE sameDims #-}

-- | We either get evidence that this function was instantiated with the
--   same type-level Dimensions, or 'Nothing' @O(Length xs)@.
sameDims' :: forall (as :: [Nat]) (bs :: [Nat]) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
           . (Dimensions as, Dimensions bs)
          => p as -> q bs -> Maybe (Dict (as ~ bs))
sameDims' = const . const $ sameDims (dims @as) (dims @bs)
{-# INLINE sameDims' #-}


-- | Similar to `const` or `asProxyTypeOf`;
--   to be used on such implicit functions as `dim`, `dimMax`, etc.
inSpaceOf :: forall (k :: Type) (ds :: [k]) (p :: [k] -> Type) (q :: [k] -> Type)
           . p ds -> q ds -> p ds
inSpaceOf = const
{-# INLINE inSpaceOf #-}

-- | Similar to `asProxyTypeOf`,
--   Give a hint to type checker to fix the type of a function argument.
asSpaceOf :: forall (k :: Type) (ds :: [k])
                    (p :: [k] -> Type) (q :: [k] -> Type) (r :: Type)
           . p ds -> (q ds -> r) -> (q ds -> r)
asSpaceOf = const id
{-# INLINE asSpaceOf #-}

-- | Map Dims onto XDims (injective)
type family AsXDims (ns :: [Nat]) = (xns :: [XNat]) | xns -> ns where
    AsXDims '[] = '[]
    AsXDims (n ': ns) = N n ': AsXDims ns

-- | Map XDims onto Dims (injective)
type family AsDims (xns::[XNat]) = (ns :: [Nat]) | ns -> xns where
    AsDims '[] = '[]
    AsDims (N x ': xs) = x ': AsDims xs

-- | Constrain @Nat@ dimensions hidden behind @XNat@s.
type family FixedDims (xns::[XNat]) (ns :: [Nat]) :: Constraint where
    FixedDims '[] ns = (ns ~ '[])
    FixedDims (xn ': xns) ns
      = ( ns ~ (Head ns ': Tail ns)
        , FixedDim xn (Head ns)
        , FixedDims xns (Tail ns))

-- | Know the structure of each dimension
type KnownXNatTypes xns = All KnownXNatType xns




instance Typeable d => Data (Dim (d :: Nat)) where
    gfoldl _ = id
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
    (==) = unsafeCoerce# ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}
    (/=) = unsafeCoerce# ((/=) :: [Word] -> [Word] -> Bool)
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
    compare = unsafeCoerce# (compare :: [Word] -> [Word] -> Ordering)
    {-# INLINE compare #-}

instance Ord SomeDims where
    compare (SomeDims as) (SomeDims bs) = compare (listDims as) (listDims bs)
    {-# INLINE compare #-}

instance Show (Dim (x :: k)) where
    showsPrec _ d = showChar 'D' . shows (dimVal d)
    {-# INLINE showsPrec #-}

instance Show (Dims (xs :: [k])) where
    showsPrec = typedListShowsPrec @k @Dim @xs showsPrec

instance Show SomeDims where
    showsPrec p (SomeDims ds)
      = showParen (p >= 10)
      $ showString "SomeDims " . showsPrec 10 ds

instance BoundedDim x => Read (Dim (x :: k)) where
    readPrec = Read.lexP >>= \case
      Read.Ident ('D':s)
        | Just d <- Read.readMaybe s
            >>= constrainDim @k @x @XNat @(XN 0) . DimSing
          -> return d
      _  -> Read.pfail
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance BoundedDims xs => Read (Dims (xs :: [k])) where
    readPrec = typedListReadPrec @k @BoundedDim ":*" Read.readPrec (tList @k @xs)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance Read SomeDims where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "SomeDims"
      withTypedListReadPrec @Nat @Dim @SomeDims
        (\g -> (\(Dx d) -> g d) <$> Read.readPrec @(Dim (XN 0)))
        SomeDims



-- | This function does GHC's magic to convert user-supplied `dim` function
--   to create an instance of `KnownDim` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall (r :: Type) (d :: Nat) . Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce# (MagicDim k :: MagicDim d r) d
{-# INLINE reifyDim #-}
newtype MagicDim (d :: Nat) (r :: Type) = MagicDim (KnownDim d => r)

reifyNat :: forall (r :: Type) (d :: Nat) . Natural -> (KnownNat d => r) -> r
reifyNat d k = unsafeCoerce# (MagicNat k :: MagicNat d r) d
{-# INLINE reifyNat #-}
newtype MagicNat (d :: Nat) (r :: Type) = MagicNat (KnownNat d => r)

dimEv :: forall (d :: Nat) . Dim d -> Dict (KnownDim d)
dimEv d = reifyDim d Dict
{-# INLINE dimEv #-}

reifyDims :: forall (r :: Type) (ds :: [Nat]) . Dims ds -> (Dimensions ds => r) -> r
reifyDims ds k = unsafeCoerce# (MagicDims k :: MagicDims ds r) ds
{-# INLINE reifyDims #-}
newtype MagicDims (ds :: [Nat]) (r :: Type) = MagicDims (Dimensions ds => r)

dimsEv :: forall (ds :: [Nat]) . Dims ds -> Dict (Dimensions ds)
dimsEv ds = reifyDims ds Dict
{-# INLINE dimsEv #-}


data PatXDim (xn :: XNat) where
  PatN :: KnownDim n => Dim n -> PatXDim ('N n)
  PatXN :: (KnownDim n, m <= n) => Dim n -> PatXDim ('XN m)

dimXNEv :: forall (xn :: XNat) . XNatType xn -> Dim xn -> PatXDim xn
dimXNEv Nt (DimSing k) = reifyDim dd (PatN dd)
  where
    dd = DimSing @Nat @_ k
dimXNEv XNt xn@(DimSing k) = reifyDim dd (f dd xn)
  where
    dd = DimSing @Nat @_ k
    f :: forall (d :: Nat) (m :: Nat)
       . KnownDim d => Dim d -> Dim ('XN m) -> PatXDim ('XN m)
    f d = case unsafeCoerceDict @(m <= m) @(m <= d) Dict of
      Dict -> const (PatXN d)
{-# INLINE dimXNEv #-}

data PatXDims (xns :: [XNat])
  = forall (ns :: [Nat])
  . (FixedDims xns ns, Dimensions ns) => PatXDims (Dims ns)

patXDims :: forall (xns :: [XNat])
          . All KnownXNatType xns => Dims xns -> PatXDims xns
patXDims U = PatXDims U
patXDims (Dn n :* xns) = case patXDims xns of
  PatXDims ns -> PatXDims (n :* ns)
patXDims (Dx n :* xns) = case patXDims xns of
  PatXDims ns -> PatXDims (n :* ns)
{-# INLINE patXDims #-}

data PatAsXDims (ns :: [Nat])
  = (KnownXNatTypes (AsXDims ns), RepresentableList (AsXDims ns))
  => PatAsXDims (Dims (AsXDims ns))

patAsXDims :: forall (ns :: [Nat]) . Dims ns -> PatAsXDims ns
patAsXDims U = PatAsXDims U
patAsXDims (n@D :* ns) = case patAsXDims ns of
  PatAsXDims xns -> PatAsXDims (Dn n :* xns)
{-# INLINE patAsXDims #-}

data PatKDims (ns :: [Nat])
  = (All KnownDim ns, All BoundedDim ns, RepresentableList ns, Dimensions ns) => PatKDims

patKDims :: forall (ns :: [Nat]) . Dims ns -> PatKDims ns
patKDims U = PatKDims
patKDims (D :* ns) = case patKDims ns of
  PatKDims -> PatKDims
{-# INLINE patKDims #-}

unsafeCoerceDict :: forall (a :: Constraint) (b :: Constraint)
                  . Dict a -> Dict b
unsafeCoerceDict = unsafeCoerce#
