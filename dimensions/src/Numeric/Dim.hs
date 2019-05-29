{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dim
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- This module is based on `GHC.TypeNats` and re-exports its functionality.
-- It provides `KnownDim` class that is similar to `KnownNat`, but keeps
-- `Word`s instead of `Natural`s;
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
  , Dim ( D, Dn, Dx
        , D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14
        , D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25
        )
  , SomeDim
  , KnownDim (..), BoundedDim (..), KnownXNatType (..)
  , MinDim, FixedDim, Compared, SOrdering (..)
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
  , plusDim, minusDim, minusDimM, timesDim, powerDim
    -- * Re-export part of `GHC.TypeNats` for convenience
  , TN.Nat, TN.CmpNat, type (TN.+), type (TN.-), type (TN.*), type (TN.^)
    -- * Inferring kind of type-level dimension
  , KnownDimKind (..), DimKind (..)
  ) where


import           Data.Data       hiding (TypeRep, typeRep, typeRepTyCon)
import           GHC.Base        (Type)
import           GHC.Exts        (Constraint, Proxy#, proxy#, unsafeCoerce#)
import qualified GHC.Generics    as G
import           GHC.TypeLits    (AppendSymbol, ErrorMessage (..), Symbol,
                                  TypeError)
import           GHC.TypeNats    as TN
import           Numeric.Natural (Natural)
import           Type.Reflection

import Data.Constraint


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
{-# COMPLETE D #-}
{-# COMPLETE Dn, Dx #-}

instance Typeable d => Data (Dim (d :: Nat)) where
    gfoldl _ z = z
    gunfold _ z _ = z (typeableDim @d)
    toConstr _ = dimNatConstr $ dimVal (typeableDim @d)
    dataTypeOf _ = dimDataType $ dimVal (typeableDim @d)

dimDataType :: Word -> DataType
dimDataType = mkDataType "Numeric.Dim.Dim" . (:[]) . dimNatConstr

dimNatConstr :: Word -> Constr
dimNatConstr d = mkConstr (dimDataType d) ("D" ++ show d) [] Prefix

instance KnownDim d => G.Generic (Dim (d :: Nat)) where
    type Rep (Dim d) = G.D1
          ('G.MetaData "Dim" "Numeric.Dim" "dimensions" 'False)
          (G.C1 ('G.MetaCons (AppendSymbol "D" (ShowNat d)) 'G.PrefixI 'False) G.U1)
    from D = G.M1 (G.M1 G.U1)
    to _ = dim @d

-- | Convert type-level @Nat@ into a type-level @Symbol@.
type family ShowNat (n :: Nat) :: Symbol where
    -- LOL
    ShowNat 0 = "0"
    ShowNat 1 = "1"
    ShowNat 2 = "2"
    ShowNat 3 = "3"
    ShowNat 4 = "4"
    ShowNat 5 = "5"
    ShowNat 6 = "6"
    ShowNat 7 = "7"
    ShowNat 8 = "8"
    ShowNat 9 = "9"
    ShowNat d = AppendSymbol (ShowNat (Div d 10)) (ShowNat (Mod d 10))


-- | Match against this pattern to bring `KnownDim` instance into scope.
pattern D :: forall (n :: Nat) . () => KnownDim n => Dim n
pattern D <- (dimEv -> Dict)
  where
    D = dim @n

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
class KnownDimKind k => BoundedDim (n :: k) where
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
    constrain :: forall (l :: Type) (y :: l) . Dim y -> Maybe (Dim n)


instance KnownDim n => BoundedDim (n :: Nat) where
    type DimBound n = n
    dimBound = dim @n
    {-# INLINE dimBound #-}
    constrain (DimSing y)
       | dimVal' @n == y = Just (DimSing y)
       | otherwise            = Nothing
    {-# INLINE constrain #-}

instance KnownDim n => BoundedDim ('N n) where
    type DimBound ('N n) = n
    dimBound = dim @n
    {-# INLINE dimBound #-}
    constrain (DimSing y)
       | dimVal' @n == y = Just (DimSing y)
       | otherwise            = Nothing
    {-# INLINE constrain #-}

instance KnownDim m => BoundedDim ('XN m) where
    type DimBound ('XN m) = m
    dimBound = dim @m
    {-# INLINE dimBound #-}
    constrain (DimSing y)
       | dimVal' @m <= y = Just (DimSing y)
       | otherwise            = Nothing
    {-# INLINE constrain #-}

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
dimVal = unsafeCoerce#
{-# INLINE dimVal #-}

-- | Similar to `natVal` from `GHC.TypeNats`, but returns `Word`.
dimVal' :: forall (n :: Nat) . KnownDim n => Word
dimVal' = unsafeCoerce# (dim @n)
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

-- | Friendly error message if `m <= n` constraint is not satisfied.
type family MinDim (m :: Nat) (n :: Nat) :: Constraint where
  MinDim m n = Compared
    (CmpNat m n)
    (() :: Constraint)
    (() :: Constraint)
    (TypeError
      ('Text "Minimum Dim size constraint ("
          ':<>: 'ShowType m
          ':<>: 'Text " <= "
          ':<>: 'ShowType n
          ':<>: 'Text ") is not satisfied."
        ':$$: 'Text "Minimum Dim: " ':<>: 'ShowType m
        ':$$: 'Text " Actual Dim: " ':<>: 'ShowType n
      )
    )

-- | Act upon the result of some type-level comparison
type family Compared
      (c :: Ordering)
      (lt :: k)
      (eq :: k)
      (gt :: k) :: k where
    Compared 'LT lt _  _  = lt
    Compared 'EQ _  eq _  = eq
    Compared 'GT _  _  gt = gt

-- | Singleton-style version of `Ordering`.
--   Pattern-match againts its constructor to witness the result of
--   type-level comparison.
data SOrdering :: Ordering -> Type where
    SLT :: SOrdering 'LT
    SEQ :: SOrdering 'EQ
    SGT :: SOrdering 'GT

-- | Constraints given by an XNat type on possible values of a Nat hidden inside.
type family FixedDim (x :: XNat) (n :: Nat) :: Constraint where
    FixedDim ('N a)  b = a ~ b
    FixedDim ('XN m) b = MinDim m b

instance {-# OVERLAPPABLE #-} KnownNat n => KnownDim n where
    {-# INLINE dim #-}
    dim = DimSing (fromIntegral (TN.natVal' (proxy# :: Proxy# n)))

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

instance Class (KnownNat n) (KnownDim n) where
    cls = Sub $ reifyNat @_ @n (fromIntegral $ dimVal' @n) Dict

-- | Similar to `someNatVal` from `GHC.TypeNats`.
someDimVal :: Word -> SomeDim
someDimVal = unsafeCoerce#
{-# INLINE someDimVal #-}



-- | `constrain` with explicitly-passed constraining @Dim@
--   to avoid @AllowAmbiguousTypes@.
constrainBy :: forall (k :: Type) (x :: k) (l :: Type) (y :: l)
             . BoundedDim x => Dim x -> Dim y -> Maybe (Dim x)
constrainBy _ = constrain @k @x @l @y
{-# INLINE constrainBy #-}


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
         . Dim x -> Dim y -> Maybe (Dict (x ~ y))
sameDim (DimSing a) (DimSing b)
  | a == b    = Just (unsafeCoerce# (Dict @(x ~ x)))
  | otherwise = Nothing
{-# INLINE sameDim #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim' :: forall (x :: Nat) (y :: Nat) (p :: Nat -> Type) (q :: Nat -> Type)
          . (KnownDim x, KnownDim y)
         => p x -> q y -> Maybe (Dict (x ~ y))
sameDim' _ _ = sameDim (dim @x) (dim @y)
{-# INLINE sameDim' #-}

-- | Ordering of dimension values.
--
--   Note: `CmpNat` forces type parameters to kind `Nat`;
--         if you want to compare unknown `XNat`s, use `Ord` instance of `Dim`.
compareDim :: forall (a :: Nat) (b :: Nat)
            . Dim a -> Dim b -> SOrdering (CmpNat a b)
compareDim a b
  = case unsafeCoerce# (compare :: Word -> Word -> Ordering) a b of
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
compareDim' _ _ = compareDim (dim @a)  (dim @b)
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
    compare = unsafeCoerce# (compare :: Word -> Word -> Ordering)
    {-# INLINE compare #-}

instance Show (Dim x) where
    showsPrec p = showsPrec p . dimVal
    {-# INLINE showsPrec #-}

instance KnownDim m => Read (Dim ('XN m)) where
    readsPrec p xs = do (a,ys) <- readsPrec p xs
                        case constrain (someDimVal a) of
                          Nothing -> []
                          Just n  -> [(n,ys)]




plusDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim (n + m)
plusDim (DimSing a) (DimSing b) = unsafeCoerce# (a + b)
{-# INLINE plusDim #-}

minusDim :: forall (n :: Nat) (m :: Nat) . MinDim m n => Dim n -> Dim m -> Dim (n - m)
minusDim (DimSing a) (DimSing b) = unsafeCoerce# (a - b)
{-# INLINE minusDim #-}

minusDimM :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Maybe (Dim (n - m))
minusDimM (DimSing a) (DimSing b)
  | a >= b    = Just (unsafeCoerce# (a - b))
  | otherwise = Nothing
{-# INLINE minusDimM #-}

timesDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim ((TN.*) n m)
timesDim (DimSing a) (DimSing b) = unsafeCoerce# (a * b)
{-# INLINE timesDim #-}

powerDim :: forall (n :: Nat) (m :: Nat) . Dim n -> Dim m -> Dim ((TN.^) n m)
powerDim (DimSing a) (DimSing b) = unsafeCoerce# (a ^ b)
{-# INLINE powerDim #-}



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



--------------------------------------------------------------------------------

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
    f d _ = case ( unsafeCoerce# (Dict @(MinDim m m))
                :: Dict (MinDim m d)
               ) of
      Dict -> PatXN d
{-# INLINE dimXNEv #-}

-- MOAR PATTERNS
-- NB: I don't provide D0 and D1 to discourage their use.

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
