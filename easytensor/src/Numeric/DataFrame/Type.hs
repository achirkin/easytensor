{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
#ifdef UNSAFE_INDICES
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
{- |
  The core @easytensor@ types.
 -}
module Numeric.DataFrame.Type
  ( -- * Data types
    SomeDataFrame (..), DataFrame'
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
  , DataFrame (SingleFrame, MultiFrame, XFrame)
  , pattern Z, pattern (:*:)
  , pattern S, pattern DF2, pattern DF3, pattern DF4, pattern DF5
  , pattern DF6, pattern DF7, pattern DF8, pattern DF9
#else
  , DataFrame ( SingleFrame, MultiFrame, XFrame, (:*:), Z
              , S, DF2, DF3, DF4, DF5, DF6, DF7, DF8, DF9)
#endif
  , scalar, unScalar
  , IndexFrame (..), SubFrameIndexCtx
    -- * Flexible assembling and disassembling
  , PackDF, packDF, unpackDF
  , appendDF, consDF, snocDF
  , fromFlatList, fromListWithDefault, fromList
  , constrainDF, asDiag
    -- * Infer type class instances
  , KnownBackend (), DFBackend, KnownBackends
  , InferKnownBackend (..), inferPrimElem
    -- * Re-exports
  , Dim (..), Idx (), Nat, XNat (..), N, XN, Dims, Idxs, TypedList (..)
  , PrimBytes (), bSizeOf, bAlignOf, bFieldOffsetOf
  , PrimArray (), ixOff
  ) where


#if !(MIN_VERSION_base(4,13,0))
import Data.Proxy (Proxy)
#endif
import           Control.Arrow                   (second, (***))
import           Data.Data
import           Data.Semigroup                  hiding (All, Min)
import           Data.Type.Lits
import           Data.Void
import           Foreign.Storable                (Storable (..))
import           GHC.Base
import           GHC.Exts                        (TYPE)
import qualified GHC.Generics                    as G
import           GHC.Ptr                         (Ptr (..))
import qualified Text.ParserCombinators.ReadPrec as Read
import qualified Text.Read                       as Read
import qualified Text.Read.Lex                   as Read
import           Unsafe.Coerce

import           Numeric.Basics
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial
import           Numeric.TypedList                    (typeables)

import           Numeric.DataFrame.Internal.BackendI (DFBackend, KnownBackend)
import qualified Numeric.DataFrame.Internal.BackendI as Backend



-- | Keep data in a primitive data frame
--    and maintain information about Dimensions in the type system
data family DataFrame (t :: l) (xs :: [k])

-- | Single frame
newtype instance DataFrame (t :: Type) (ns :: [Nat])
  = SingleFrame { getSingleFrame :: DFBackend t ns }

-- | Multiple "columns" of data frames of the same shape
newtype instance DataFrame (ts :: [Type]) (ns :: [Nat])
  = MultiFrame ( TypedList (DataFrame' ns) ts )

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame.
data instance DataFrame (ts :: l) (xns :: [XNat])
  = forall (ns :: [Nat])
  . (All KnownDimType xns, FixedDims xns ns, Dimensions ns, KnownBackends ts ns)
  => XFrame (DataFrame ts ns)

-- | Data frame that has an unknown dimensionality at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame
data SomeDataFrame (t :: l)
  = forall (ns :: [Nat]) . (Dimensions ns, KnownBackends t ns)
  => SomeDataFrame (DataFrame t ns)
  deriving Typeable

-- | DataFrame with its type arguments swapped.
newtype DataFrame' (xs :: [k]) (t :: l) = DataFrame' (DataFrame t xs)
  deriving Typeable

{-# COMPLETE Z, (:*:) #-}

#define PLEASE_STYLISH_HASKELL \
  forall (xs :: [Type]) (ns :: [Nat])  . () => \
  forall (y :: Type)    (ys :: [Type]) . (xs ~ (y ': ys)) => \
  DataFrame y ns -> DataFrame ys ns -> DataFrame xs ns

-- | Constructing a @MultiFrame@ using DataFrame columns
pattern (:*:) :: PLEASE_STYLISH_HASKELL
pattern (:*:) x xs <- (MultiFrame (DataFrame' x :* (MultiFrame -> xs)))
  where
    (:*:) x (MultiFrame xs) = MultiFrame (DataFrame' x :* xs)
infixr 6 :*:
#undef PLEASE_STYLISH_HASKELL

-- | Empty MultiFrame
pattern Z :: forall (xs :: [Type]) (ns :: [Nat])
           . () => (xs ~ '[]) => DataFrame xs ns
pattern Z = MultiFrame U

-- | Convert a scalar back to an ordinary type
unScalar :: DataFrame t ('[] :: [Nat]) -> t
-- rely on that Scalar is just two times newtype alias to t
unScalar = unsafeCoerce
{-# INLINE unScalar #-}

-- | Convert any type to a scalar wrapper
scalar :: t -> DataFrame t ('[] :: [Nat])
-- rely on that Scalar is just two times newtype alias to t
scalar = unsafeCoerce
{-# INLINE scalar #-}

-- | I use this kind-polymorphic constraint to generalize @XFrame@ and @SomeDataFrame@
--   over @SingleFrame@ and @MultiFrame@.
type family KnownBackends (ts :: l) (ns :: [Nat]) :: Constraint where
    KnownBackends ( t      ::  Type ) ns = KnownBackend t ns
    KnownBackends ('[]     :: [Type]) _  = ()
    KnownBackends (t ': ts :: [Type]) ns =
      (KnownBackend t ns, KnownBackends ts ns)

-- | Allow inferring @KnownBackends@ if you know the dimensions and the element types.
class InferKnownBackend t (ds :: [Nat]) where
    -- | Infer @KnownBackends@ if you know the dimensions and the element types.
    inferKnownBackend :: Dict (KnownBackends t ds)

instance (PrimBytes t, Dimensions ds) => InferKnownBackend (t :: Type) ds where
    inferKnownBackend = Backend.inferKnownBackend @t @ds

instance (RepresentableList ts, All PrimBytes ts, Dimensions ds)
      => InferKnownBackend (ts :: [Type]) ds where
    inferKnownBackend = go (tList @ts)
      where
        go :: forall ss . All PrimBytes ss
           => TypeList ss -> Dict (KnownBackends ss ds)
        go U = Dict
        go ((_ :: Proxy t) :* ts)
             = case Backend.inferKnownBackend @t @ds of
                 Dict -> case go ts of
                   Dict -> Dict


-- | All component data frames must satisfy a given constraint.
type family AllFrames (f :: Type -> Constraint) (ts :: [Type]) (ds :: [Nat])
                                                             :: Constraint where
    AllFrames _ '[] _ = ()
    AllFrames f (t ': ts) ds = (f (DataFrame t ds), AllFrames f ts ds)


-- | Index one dimension deep into a @DataFrame@.
class IndexFrame t d ds where
    -- | Index one dimension deep into a @DataFrame@.
    --
    --   Note, this function does not provide indexing safety at the type level;
    --   it throws an `OutOfDimBounds` exception if an index is out of bounds
    --   (unless @unsafeindices@ package flag is enabled, which is even more dangerous).
    (!) :: DataFrame t (d ': ds) -> Word -> DataFrame t ds

instance (PrimArray t (DataFrame t '[d]), KnownDim d)
      => IndexFrame (t :: Type) (d :: Nat) '[] where
    {-# INLINE (!) #-}
    (!) df i
#ifndef UNSAFE_INDICES
      | i >= dimVal' @d = outOfDimBoundsNoCallStack "IndexFrame.(!)"
                            i (dimVal' @d) Nothing Nothing
      | otherwise
#endif
        = S (ixOff (fromIntegral i) df)

instance {-# INCOHERENT #-}
         ( PrimArray t (DataFrame t (d ': ds))
         , KnownDim d, KnownBackend t ds)
      => IndexFrame (t :: Type) (d :: Nat) ds where
    {-# INLINE (!) #-}
    (!) df i
#ifndef UNSAFE_INDICES
      | i >= dimVal' @d = outOfDimBoundsNoCallStack "IndexFrame.(!)"
                            i (dimVal' @d) Nothing Nothing
      | otherwise
#endif
        = withArrayContent broadcast
           (\(CumulDims ~(_:ss@(s:_))) off0 ->
               fromElems (CumulDims ss) (case s*i of W# off -> off0 +# word2Int# off)
           ) df


instance IndexFrame ('[] :: [Type]) (d :: Nat) ds where
    {-# INLINE (!) #-}
    (!) Z _ = Z

instance (IndexFrame t d ds, IndexFrame ts d ds)
      => IndexFrame ((t ': ts) :: [Type]) (d :: Nat) ds where
    {-# INLINE (!) #-}
    (!) (df :*: dfs) i = df ! i :*: dfs ! i

instance PrimBytes t
      => IndexFrame (t :: Type) (xd :: XNat) xds where
    {-# INLINE (!) #-}
    (!) (XFrame df) i
      | D :* (Dims :: Dims ns) <- dims `inSpaceOf` df
      , Dict <- inferKnownBackend @t @ns
        = XFrame (df ! i)
    (!) _ _ = error "IndexFrame: impossible pattern"

instance (RepresentableList ts, All PrimBytes ts)
      => IndexFrame (ts :: [Type]) (xd :: XNat) xds where
    {-# INLINE (!) #-}
    (!) (XFrame df) i
      | (D :: Dim n) :* (Dims :: Dims ns) <- dims `inSpaceOf` df
      , Dict <- getTsEvs @ts @n @ns tList
        = XFrame (df ! i)
      where
        getTsEvs :: forall (as :: [Type]) (d :: Nat) (ds :: [Nat])
                  . ( All PrimBytes as, KnownBackends as (d ': ds)
                    , Dimensions ds, KnownDim d)
                 => TypeList as -> Dict (IndexFrame as d ds, KnownBackends as ds)
        getTsEvs U = Dict
        getTsEvs ((_ :: Proxy a) :* as')
          | Dict <- getTsEvs @_ @d @ds as'
          , Dict <- inferKnownBackend @a @ds = Dict
    (!) _ _ = error "IndexFrame: impossible pattern"

{- |
This type family describes two strategies for dealing with dimensions when
slicing a @DataFrame@.

If the original space dimension (@d@) is fixed at compile time
(@d :: Nat@ or @d ~ N n@), then we enforce the safety of a slicing operation
with types:
    the index space is bounded by the size of the original space
    minus the subframe size.

If the original space dimension (@d@) is not fixed (@d ~ XN m@), then we give up.
Just let the user do the slicing as easy as possible, and throw an `OutOfDimBounds`
exception at runtime if the index plus the sliced size is greater than the
original size of the DataFrame.
 -}
type family SubFrameIndexCtx (d :: k) (idxN :: k) (subN :: k) :: Constraint where
    SubFrameIndexCtx (n :: Nat) idxN subN
      = (n + 1) ~ (idxN + subN)
    SubFrameIndexCtx (N n) idxN subN
      = ( (n + 1) ~ (DimBound idxN + DimBound subN)
        , idxN ~ N (DimBound idxN)
        , subN ~ N (DimBound subN)
        )
    SubFrameIndexCtx (XN m) idxN subN
      = ( idxN ~ XN m
        , subN ~ N (DimBound subN)
        )

deriving instance Typeable (DataFrame (t :: l) (xs :: [k]))
deriving instance ( Data (DataFrame t xs)
                  , Typeable t, Typeable xs, Typeable k, Typeable l)
               => Data (DataFrame' (xs :: [k]) (t :: l))
deriving instance Eq (DFBackend t ds)
               => Eq (DataFrame t ds)
deriving instance Ord (DFBackend t ds)
               => Ord (DataFrame t ds)
deriving instance ProductOrder (DFBackend t ds)
               => ProductOrder (DataFrame t ds)
deriving instance Bounded (DFBackend t ds)
               => Bounded (DataFrame t ds)
deriving instance Enum (DFBackend t ds)
               => Enum (DataFrame t ds)
deriving instance Integral (DFBackend t ds)
               => Integral (DataFrame t ds)
deriving instance Num (DFBackend t ds)
               => Num (DataFrame t ds)
deriving instance Fractional (DFBackend t ds)
               => Fractional (DataFrame t ds)
deriving instance Floating (DFBackend t ds)
               => Floating (DataFrame t ds)
deriving instance Epsilon (DFBackend t ds)
               => Epsilon (DataFrame t ds)
deriving instance Real (DFBackend t ds)
               => Real (DataFrame t ds)
deriving instance RealExtras (DFBackend t ds)
               => RealExtras (DataFrame t ds)
deriving instance RealFrac (DFBackend t ds)
               => RealFrac (DataFrame t ds)
deriving instance RealFloat (DFBackend t ds)
               => RealFloat (DataFrame t ds)
deriving instance RealFloatExtras (DFBackend t ds)
               => RealFloatExtras (DataFrame t ds)
deriving instance PrimBytes (DFBackend t ds)
               => PrimBytes (DataFrame t ds)
deriving instance (PrimArray t (DFBackend t ds), PrimBytes t)
               => PrimArray t (DataFrame t ds)


instance Ord (NonTransitive.ProductOrd (DFBackend t ds))
      => Ord (NonTransitive.ProductOrd (DataFrame t ds)) where
    (>) = coerce ((>) @(NonTransitive.ProductOrd (DFBackend t ds)))
    (<) = coerce ((<) @(NonTransitive.ProductOrd (DFBackend t ds)))
    (>=) = coerce ((>=) @(NonTransitive.ProductOrd (DFBackend t ds)))
    (<=) = coerce ((<=) @(NonTransitive.ProductOrd (DFBackend t ds)))
    compare = coerce (compare @(NonTransitive.ProductOrd (DFBackend t ds)))
    min = coerce (min @(NonTransitive.ProductOrd (DFBackend t ds)))
    max = coerce (max @(NonTransitive.ProductOrd (DFBackend t ds)))

instance (Ord (Partial.ProductOrd (DFBackend t ds)), Eq (DFBackend t ds))
      => Ord (Partial.ProductOrd (DataFrame t ds)) where
    (>) = coerce ((>) @(Partial.ProductOrd (DFBackend t ds)))
    (<) = coerce ((<) @(Partial.ProductOrd (DFBackend t ds)))
    (>=) = coerce ((>=) @(Partial.ProductOrd (DFBackend t ds)))
    (<=) = coerce ((<=) @(Partial.ProductOrd (DFBackend t ds)))
    compare = coerce (compare @(Partial.ProductOrd (DFBackend t ds)))
    min = coerce (min @(Partial.ProductOrd (DFBackend t ds)))
    max = coerce (max @(Partial.ProductOrd (DFBackend t ds)))



instance PrimBytes (DataFrame t ds) => Storable (DataFrame t ds) where
    sizeOf x = I# (byteSize x)
    alignment x = I# (byteAlign x)
    peek (Ptr addr) = IO (readAddr addr)
    poke (Ptr addr) a = IO (\s -> (# writeAddr a addr s, () #))


instance AllFrames Eq ts ds => Eq (DataFrame (ts :: [Type]) ds) where
    Z == Z = True
    (a :*: as) == (b :*: bs) = a == b && as == bs

instance Eq t => Eq (DataFrame (t :: Type) (ds :: [XNat])) where
    XFrame dfa == XFrame dfb
      | Just Dict <- sameDims' dfa dfb = dfa == dfb
      | otherwise                      = False

instance All Eq ts => Eq (DataFrame (ts :: [Type]) (ds :: [XNat])) where
    XFrame dfa == XFrame dfb
      | Just Dict <- sameDims' dfa dfb = eqFrames dfa dfb
      | otherwise                      = False

instance Eq t => Eq (SomeDataFrame (t :: Type)) where
    SomeDataFrame dfa == SomeDataFrame dfb
      | Just Dict <- sameDims' dfa dfb = dfa == dfb
      | otherwise                      = False

instance All Eq ts => Eq (SomeDataFrame (ts :: [Type])) where
    SomeDataFrame dfa == SomeDataFrame dfb
      | Just Dict <- sameDims' dfa dfb = eqFrames dfa dfb
      | otherwise                      = False


eqFrames :: forall (xs :: [Type]) (ns :: [Nat])
          . (KnownBackends xs ns, All Eq xs)
         => DataFrame xs ns -> DataFrame xs ns -> Bool
eqFrames Z Z                   = True
eqFrames (a :*: as) (b :*: bs) = a == b && eqFrames as bs



instance ( Show t
         , PrimBytes t
         , Dimensions ds
         ) => Show (DataFrame (t :: Type) (ds :: [Nat])) where
    showsPrec p x = case dims @ds of
      U       -> showParen (p >= 10)
        $ showString "S " . showsPrec 10 (unScalar x)
      D0 :* _ -> showString "DF0"
      d  :* _ -> showParen (p >= 10)
        $ unpackDF' ( \Dict f ->
            let g :: Endo (Int -> ShowS)
                g = Endo $ \k -> f (\o e -> k o . showChar ' ' . showsPrec 11 e)
                n = dimVal d
            in  appEndo (stimes n g) (const $ showString "DF" . shows n)
          ) x

instance ( All Show ts
         , All PrimBytes ts
         , Dimensions ds
         ) => Show (DataFrame (ts :: [Type]) (ds :: [Nat])) where
    showsPrec _ Z = showChar 'Z'
    showsPrec p (x :*: xs) = showParen (p >= 7) $
        showsPrec 7 x . showString " :*: " . showsPrec 6 xs

instance (Show t, PrimBytes t)
      => Show (DataFrame (t :: Type) (xns :: [XNat])) where
    showsPrec p (XFrame x)
      = showParen (p >= 10) $ showString "XFrame " . showsPrec 11 x

instance (All Show ts, All PrimBytes ts)
      => Show (DataFrame (ts :: [Type]) (xns :: [XNat])) where
    showsPrec p (XFrame x)
      = showParen (p >= 10) $ showString "XFrame " . showsPrec 11 x

instance (Show t, PrimBytes t)
      => Show (SomeDataFrame (t :: Type)) where
    showsPrec p (SomeDataFrame x)
      = showParen (p >= 10) $ showString "SomeDataFrame " . showsPrec 11 x

instance (All Show ts, All PrimBytes ts)
      => Show (SomeDataFrame (ts :: [Type])) where
    showsPrec p (SomeDataFrame x)
      = showParen (p >= 10) $ showString "SomeDataFrame " . showsPrec 11 x


instance (Read t, PrimBytes t, Dimensions ds)
      => Read (DataFrame (t :: Type) (ds :: [Nat])) where
    readPrec = readPrecFixedDF (dims @ds)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance (All Read ts, All PrimBytes ts, RepresentableList ts, Dimensions ds)
      => Read (DataFrame (ts :: [Type]) (ds :: [Nat])) where
    readPrec = readFixedMultiDF (tList @ts) (dims @ds)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance (Read t, PrimBytes t, BoundedDims ds, All KnownDimType ds)
      => Read (DataFrame (t :: Type) (ds :: [XNat])) where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "XFrame"
      Read.step $ readPrecBoundedDF (minimalDims @ds)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance ( All Read ts, All PrimBytes ts, RepresentableList ts
         , BoundedDims ds, All KnownDimType ds)
      => Read (DataFrame (ts :: [Type]) (ds :: [XNat])) where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "XFrame"
      Read.step $ readBoundedMultiDF (tList @ts) (minimalDims @ds)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance (Read t, PrimBytes t)
      => Read (SomeDataFrame (t :: Type)) where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "SomeDataFrame"
      Read.step readPrecSomeDF
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance ( All Read ts, All PrimBytes ts, RepresentableList ts )
      => Read (SomeDataFrame (ts :: [Type])) where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "SomeDataFrame"
      Read.step $ readSomeMultiDF (tList @ts)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

readPrecFixedDF :: forall (t :: Type) (ds :: [Nat])
                 . (Read t, PrimBytes t)
                => Dims ds -> Read.ReadPrec (DataFrame t ds)
readPrecFixedDF U
  = Read.parens . Read.prec 10 $ do
    Read.lift . Read.expect $ Read.Ident "S"
    S <$> Read.step Read.readPrec
readPrecFixedDF (D0 :* Dims)
  = Read.parens $ do
    Read.lift . Read.expect . Read.Ident $ "DF0"
    return (packDF @t @0)
readPrecFixedDF (d@D :* ds@Dims)
  = Read.parens . Read.prec 10 $ do
    Read.lift . Read.expect . Read.Ident $ "DF" ++ show (dimVal d)
    packDF' (<*> Read.step (readPrecFixedDF ds)) pure

{-
The first argument is, in fact, a @dimsBound@, but covered in the same @[XNat]@
  form. That is, XN values are lower bounds rather than actual runtime dimensions.

The interesting bit about this function is that it is very particular in specifying
what is enforced and what is unknown.
At the very least, the call site must know the order of the Dims (unless DF0 is present)
  -- and specify all dims as @XN 0@.
It allows a fine-grained control over the shape of a DataFrame.

For example,

>>> readPrecBoundedDF (Dx @5 DF5 :* Dn DF4 :* Dn DF4 :* U)

  reads an array of 4x4 matrices of length at least 5.

 -}
readPrecBoundedDF :: forall (t :: Type) (ds :: [XNat])
                   . (Read t, PrimBytes t, All KnownDimType ds)
                  => Dims ds -> Read.ReadPrec (DataFrame t ds)
-- reading scalar
readPrecBoundedDF U
  = Read.parens . Read.prec 10 $ do
    Read.lift . Read.expect $ Read.Ident "S"
    case inferKnownBackend @t @'[] of
      Dict -> XFrame . S <$> Read.step Read.readPrec
-- DF0 is funny because it will succesfully parse any dimension to the right of it.
readPrecBoundedDF (Dn D0 :* XDims (Dims :: Dims ns))
  = Read.parens $ do
    Read.lift . Read.expect . Read.Ident $ "DF0"
    return $ case inferKnownBackend @t @(0 ': ns) of
      Dict -> XFrame @Type @t @ds @(0 ': ns) (packDF @t @0 @ns)
-- Fixed dimension:
--  The number of component frames is exactly n
--  the first component frame fixes the shape of the rest n-1
readPrecBoundedDF (Dn d@(D :: Dim n) :* xns)
  = Read.parens . Read.prec 10 $ do
    Read.lift . Read.expect . Read.Ident $ "DF" ++ show (dimVal d)
    XFrame (x :: DataFrame t ns) <- Read.step $ readPrecBoundedDF @t xns
    case inferKnownBackend @t @(n ': ns) of
      Dict -> fmap XFrame . snd . runDelay $ packDF' @t @n @ns
        (readDelayed $ Read.prec 10 (readPrecFixedDF @t @ns dims))
        (followedBy x)
-- Bounded dimension:
--   The number of component frames is at least m
--   the first component frame fixes the shape of the rest n-1
readPrecBoundedDF ((Dx (m :: Dim m) :: Dim xm) :* xns)
  | Dict <- unsafeEqTypes @XNat @('XN m) @xm -- by user contract the argument is dimBound
  = Read.parens $ lookLex >>= \case
    Read.Ident ('D':'F':s)
      | Just (Dx (n :: Dim n)) <- (Read.readMaybe ('D':s) :: Maybe SomeDim)
      , Just Dict <- lessOrEqDim m n
        -> case n of
          D0 -> do -- need to repack it under different constraint dims
            XFrame x <- readPrecBoundedDF @t (Dn D0 :* xns)
            return (XFrame x)
          D  -> do
            Read.lift . Read.expect . Read.Ident $ "DF" ++ show (dimVal n)
            XFrame (x :: DataFrame t ns) <- Read.prec 10 $ readPrecBoundedDF @t xns
            case inferKnownBackend @t @(n ': ns) of
              Dict -> fmap XFrame . snd . runDelay $ packDF' @t @n @ns
                (readDelayed $ Read.prec 10 (readPrecFixedDF @t @ns dims))
                (followedBy x)

    _ -> Read.pfail

{-
In this case we know Nothing about the dimensionality of a DataFrame.
The logic is similar to readPrecBoundedDF, but a bit simpler:
the first dimension is flexible, but fixes the rest dimensions.
 -}
readPrecSomeDF :: forall (t :: Type) . (Read t, PrimBytes t)
               => Read.ReadPrec (SomeDataFrame t)
readPrecSomeDF = Read.parens $
    Read.prec 10 (do
      Read.lift . Read.expect $ Read.Ident "S"
      case inferKnownBackend @t @'[] of
        Dict -> SomeDataFrame . S <$> Read.readPrec
    )
    Read.+++
    (lookLex >>= \case
       Read.Ident ('D':'F':s)
         | Just (Dx (d@D :: Dim d)) <- (Read.readMaybe ('D':s) :: Maybe SomeDim)
           -> case d of
             D0 | Dict <- inferKnownBackend @t @'[0]
                 -> SomeDataFrame <$> readPrecFixedDF @t (D0 :* U)
             _ -> do
               Read.lift . Read.expect . Read.Ident $ "DF" ++ show (dimVal d)
               SomeDataFrame (x :: DataFrame t ds) <- Read.prec 10 $ readPrecSomeDF @t
               case inferKnownBackend @t @(d ': ds) of
                 Dict -> fmap SomeDataFrame . snd . runDelay $ packDF' @t @d @ds
                   (readDelayed $ Read.prec 10 (readPrecFixedDF @t @ds dims))
                   (followedBy x)
       _ -> Read.pfail
    )

readFixedMultiDF :: forall (ts :: [Type]) (ds :: [Nat])
                  . (All Read ts, All PrimBytes ts)
                 => TypeList ts
                 -> Dims ds
                 -> Read.ReadPrec (DataFrame ts ds)
readFixedMultiDF U _ = Read.parens $
    Z <$ Read.lift (Read.expect $ Read.Ident "Z")
readFixedMultiDF (_ :* ts) ds = Read.parens . Read.prec 6 $ do
    x <- Read.step $ readPrecFixedDF ds
    Read.lift . Read.expect $ Read.Symbol ":*:"
    xs <- readFixedMultiDF ts ds
    return (x :*: xs)

readBoundedMultiDF :: forall (ts :: [Type]) (ds :: [XNat])
                    . (All Read ts, All PrimBytes ts, All KnownDimType ds)
                   => TypeList ts
                   -> Dims ds
                   -> Read.ReadPrec (DataFrame ts ds)
readBoundedMultiDF U (XDims (Dims :: Dims ns))
  = Read.parens $
    XFrame @[Type] @'[] @ds @ns Z <$ Read.lift (Read.expect $ Read.Ident "Z")
readBoundedMultiDF ((_ :: Proxy t) :* ts@TypeList) ds
  = Read.parens . Read.prec 6 $ do
    XFrame (x :: DataFrame t ns) <- Read.step $ readPrecBoundedDF @t ds
    Read.lift . Read.expect $ Read.Symbol ":*:"
    xs <- readFixedMultiDF ts (dims @ns)
    case inferKnownBackend @ts @ns of
      Dict -> return $ XFrame (x :*: xs)

readSomeMultiDF :: forall (ts :: [Type])
                 . (All Read ts, All PrimBytes ts)
                => TypeList ts
                -> Read.ReadPrec (SomeDataFrame ts)
readSomeMultiDF U
  = Read.parens $
    SomeDataFrame @[Type] @ts @'[] Z <$ Read.lift (Read.expect $ Read.Ident "Z")
readSomeMultiDF ((_ :: Proxy t) :* ts@TypeList)
  = Read.parens . Read.prec 6 $ do
    SomeDataFrame (x :: DataFrame t ns) <- Read.step $ readPrecSomeDF @t
    Read.lift . Read.expect $ Read.Symbol ":*:"
    xs <- readFixedMultiDF ts (dims @ns)
    case inferKnownBackend @ts @ns of
      Dict -> return $ SomeDataFrame (x :*: xs)

-- First element is read separately, enforcing the structure of the rest.
newtype Delayed t ds c a = Delayed { runDelay :: (c (DataFrame t ds), c a) }

followedBy :: Applicative c => DataFrame t ds -> a -> Delayed t ds c a
followedBy x = Delayed . (,) (pure x) . pure

readDelayed :: forall (t :: Type) (ds :: [Nat]) (c :: Type -> Type) (r :: Type)
             . Applicative c
            => c (DataFrame t ds)
            -> Delayed t ds c (DataFrame t ds -> r) -> Delayed t ds c r
readDelayed readF (Delayed (cprev, cf)) = Delayed (readF, cf <*> cprev)


-- | Check the next lexeme without consuming it
lookLex :: Read.ReadPrec Read.Lexeme
lookLex = Read.look >>=
  Read.choice . map (pure . fst) . Read.readPrec_to_S Read.lexP 10




-- | Evidence that the elements of the DataFrame are PrimBytes.
inferPrimElem
  :: forall (t :: Type) (d :: Nat) (ds :: [Nat])
   . KnownBackend t (d ': ds)
  => DataFrame t (d ': ds) -> Dict (PrimBytes t)
inferPrimElem = Backend.inferPrimElem . getSingleFrame

-- | Construct a DataFrame from a flat list.
--
--   The values are filled according to the DataFrame layout: row-by-row and
--   further from the last dimension (least significant) to the first dimension
--   (most significant).
--
--   If the argument list is shorter than @totalDim@, then the rest of the frame
--   is padded with a default value (second argument).
--
--   If the argument list is longer than @totalDim@, then unused values are dropped.
--   If you want, you can pass an infinite list as an argument, i.e. the following
--   is a valid use:
--
--   >>> fromFlatList (dims :: Dims '[2,5]) 0 [6,8..]
--
fromFlatList :: forall (t :: Type) (ds :: [Nat])
              . PrimArray t (DataFrame t ds)
             => Dims ds -> t -> [t] -> DataFrame t ds
fromFlatList = unsafeFromFlatList



-- | A scalar DataFrame is just a newtype wrapper on a value.
pattern S :: forall (t :: Type) . t -> DataFrame t ('[] :: [Nat])
-- rely on that Scalar is just two times newtype alias to t
pattern S x <- (unScalar -> x)
  where
    S = scalar
{-# COMPLETE S #-}


pattern DF2 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (2 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t (2 ': ds)
pattern DF2 a1 a2
    <- (unpackDF @t @2 @ds (#,,#) -> (# a1,a2,Dict #))
  where DF2 = packDF @t @2 @ds
{-# COMPLETE DF2 #-}

pattern DF3 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (3 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t (3 ': ds)
pattern DF3 a1 a2 a3
    <- (unpackDF @t @3 @ds (#,,,#) -> (# a1,a2,a3,Dict #))
  where DF3 = packDF @t @3 @ds
{-# COMPLETE DF3 #-}

pattern DF4 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (4 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t (4 ': ds)
pattern DF4 a1 a2 a3 a4
    <- (unpackDF @t @4 @ds (#,,,,#) -> (# a1,a2,a3,a4,Dict #))
  where DF4 = packDF @t @4 @ds
{-# COMPLETE DF4 #-}

pattern DF5 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (5 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t ds
            -> DataFrame t (5 ': ds)
pattern DF5 a1 a2 a3 a4 a5
    <- (unpackDF @t @5 @ds (#,,,,,#) -> (# a1,a2,a3,a4,a5,Dict #))
  where DF5 = packDF @t @5 @ds
{-# COMPLETE DF5 #-}

pattern DF6 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (6 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t (6 ': ds)
pattern DF6 a1 a2 a3 a4 a5 a6
    <- (unpackDF @t @6 @ds (#,,,,,,#) -> (# a1,a2,a3,a4,a5,a6,Dict #))
  where DF6 = packDF @t @6 @ds
{-# COMPLETE DF6 #-}

pattern DF7 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (7 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t (7 ': ds)
pattern DF7 a1 a2 a3 a4 a5 a6 a7
    <- (unpackDF @t @7 @ds (#,,,,,,,#) -> (# a1,a2,a3,a4,a5,a6,a7,Dict #))
  where DF7 = packDF @t @7 @ds
{-# COMPLETE DF7 #-}

pattern DF8 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (8 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t (8 ': ds)
pattern DF8 a1 a2 a3 a4 a5 a6 a7 a8
    <- (unpackDF @t @8 @ds (#,,,,,,,,#) -> (# a1,a2,a3,a4,a5,a6,a7,a8,Dict #))
  where DF8 = packDF @t @8 @ds
{-# COMPLETE DF8 #-}

pattern DF9 :: forall (t :: Type) (ds :: [Nat])
             . (PrimBytes t, Dimensions (9 ': ds))
            => (Dimensions ds, KnownBackend t ds)
            => DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
            -> DataFrame t ds
            -> DataFrame t (9 ': ds)
pattern DF9 a1 a2 a3 a4 a5 a6 a7 a8 a9
    <- (unpackDF @t @9 @ds (#,,,,,,,,,#) -> (# a1,a2,a3,a4,a5,a6,a7,a8,a9,Dict #))
  where DF9 = packDF @t @9 @ds
{-# COMPLETE DF9 #-}


-- | Represent smart constructor functions `packDF` and `unpackDF`.
type family PackDF (t :: Type) (ds :: [Nat]) (d :: Nat) (r :: Type) :: Type where
    PackDF _ _  0 r = r
    PackDF t ds d r = DataFrame t ds -> PackDF t ds (d - 1) r

{- |
   Takes @d@ arguments of type @DataFrame t ds@ and produce a @DataFrame t (d ': ds)@.

   NB: always use @TypeApplications@ extension with this function to apply all
       type arguments!
       Otherwise, a very dumb type family @PackDF@ will not infer the types for you.

   The following example creates a @Matrix Double 12 3@ filled with twelve
   3D vectors (using @fromInteger@ of @Vector Double 3@):

   >>> packDF @Double @12 @'[3] 1 2 3 4 5 6 7 8 9 10 11 12

  `packDF` and `unpackDF`  together serve as a generic constructor for a DataFrame
   of an arbitrary (statically known) size.

 -}
packDF :: forall (t :: Type) (d :: Nat) (ds :: [Nat])
        . (PrimBytes t, Dimensions (d ': ds))
       => PackDF t ds d (DataFrame t (d ': ds))
packDF
  | d :* Dims <- dims @(d ': ds)
  , Dict <- inferKnownBackend @t @(d ': ds)
  , Dict <- inferKnownBackend @t @ds
    = go d
  | otherwise = error "Numeric.DataFrame.Type.packDF: impossible args"
  where
    go :: (Dimensions ds, KnownBackend t ds, KnownBackend t (d ': ds))
       => Dim d
       -> PackDF t ds d (DataFrame t (d ': ds))
    go d = recur d getResult
      where
        -- number of elements in the frame as Int#
        els = case dimVal d of W# w -> word2Int# w
        -- size of a single element in bytes
        asize = byteSize @(DataFrame t ds) undefined

        getResult :: forall rRep (r :: TYPE rRep)
                  . (forall s. Int# -> MutableByteArray# s -> State# s -> r)
                  -> r
        getResult f = runRW#
          ( \s0 -> case newByteArray# (asize *# els) s0 of
               (# s1, mba #) -> f 0# mba s1
          )

        recur :: forall n . Dim n
              -> (forall rRep (r :: TYPE rRep)
                    . (forall s. Int# -> MutableByteArray# s -> State# s -> r ) -> r)
              -> PackDF t ds n (DataFrame t (d ': ds))
        recur n f = case minusDimM n (D :: Dim 1) of
          Nothing -> case unsafeEqTypes @Nat @n @0 of
            Dict -> f (\_ mba s -> case unsafeFreezeByteArray# mba s of
                                     (# _, ba #) -> fromBytes 0# ba )
          Just n' -> case unsafeEqTypes @_
                           @(PackDF t ds n (DataFrame t (d ': ds)))
                           @(DataFrame t ds -> PackDF t ds (n - 1) (DataFrame t (d ': ds))) of
            Dict -> \x -> recur n'
              ( \c -> f (\off mba s -> c (off +# asize) mba (writeBytes mba off x s)) )



{- |
  Takes a function (e.g. a constructor) with @d+1@ argument (df1, df2, .. dfd, Dict)
   and a @DataFrame t (d ': ds)@.
   Feeds the dataframe elements into that function.
   For example, you can pass a tuple to this function, and get all dataframe elements
    (and some dictionaries -- useful evidence to work with element frames)

   NB: always use @TypeApplications@ extension with this function to apply all
       type arguments!
       Otherwise, a very dumb type family @PackDF@ will not infer the types for you.

   The following example unpacks a 3D vector
     (created using @fromInteger@ of @Vector Double 3@)
   into a 4-tuple with three scalars and one Dict:

   >>> unpackDF @Double @3 @'[] (,,,) 2

  `packDF` and `unpackDF`  together serve as a generic constructor for a DataFrame
   of an arbitrary (statically known) size.

 -}
unpackDF :: forall (t :: Type) (d :: Nat) (ds :: [Nat])
                   (rep :: RuntimeRep) (r :: TYPE rep)
          . (PrimBytes t, Dimensions (d ': ds))
         => PackDF t ds d (Dict (Dimensions ds, KnownBackend t ds) -> r)
         -> DataFrame t (d ': ds) -> r
unpackDF c
  | d :* Dims <- dims @(d ': ds)
    = unpackDF' (go d)
  | otherwise = error "Numeric.DataFrame.Type.unpackDF: impossible args"
  where
    go :: forall a . (a ~ Dict (Dimensions ds, KnownBackend t ds))
       => Dim d -> a
       -> (forall (zRep :: RuntimeRep) (z :: TYPE zRep)
                  . (Int -> DataFrame t ds -> z) -> Int -> z)
       -> Int -> r
    go d a k = recur d (const c)
      where
        recur :: forall n
               . Dim n
              -> (Int -> PackDF t ds n (a -> r))
              -> Int -> r
        recur n f = case minusDimM n (D :: Dim 1) of
          Nothing -> case unsafeEqTypes @Nat @n @0 of
            Dict -> (`f` a)
          Just n' -> case unsafeEqTypes @_
                           @(PackDF t ds n (a -> r))
                           @(DataFrame t ds -> PackDF t ds (n - 1) (a -> r)) of
            Dict -> recur n' (k f)


packDF' :: forall (t :: Type) (d :: Nat) (ds :: [Nat]) c
         . (PrimBytes t, Dimensions (d ': ds))
        => (forall r. c (DataFrame t ds -> r) -> c r)
        -> (forall r. r -> c r)
        -> c (DataFrame t (d ': ds))
packDF' k z
  | d :* _ <- dims @(d ': ds)
    = go d (z (packDF @t @d @ds))
  | otherwise = error "Numeric.DataFrame.Type.packDF': impossible args"
  where
    go :: forall n . Dim n
       -> c (PackDF t ds n (DataFrame t (d ': ds))) -> c (DataFrame t (d ': ds))
    go n = case minusDimM n (D :: Dim 1) of
      Nothing -> case unsafeEqTypes @Nat @n @0 of Dict -> id
      Just n' -> case unsafeEqTypes @_
                       @(PackDF t ds n (DataFrame t (d ': ds)))
                       @(DataFrame t ds -> PackDF t ds (n - 1) (DataFrame t (d ': ds))) of
        Dict -> go n' . k


-- Parameter Int# here is an element offset, it should not be used at the call site.
unpackDF' :: forall (rep :: RuntimeRep)
                    (t :: Type) (d :: Nat) (ds :: [Nat]) (r :: TYPE rep)
           . (PrimBytes t, Dimensions (d ': ds))
          => ( Dict (Dimensions ds, KnownBackend t ds)
               -> (forall (zRep :: RuntimeRep) (z :: TYPE zRep)
                          . (Int -> DataFrame t ds -> z) -> Int -> z)
               -> Int -> r)
          -> DataFrame t (d ': ds)
          -> r
unpackDF' k
  | d :* Dims <- dims @(d ': ds)
  , Dict <- inferKnownBackend @t @(d ': ds)
  , Dict <- inferKnownBackend @t @ds
    = withArrayContent
    ( \x ->
      let e = broadcast x
          f :: forall (zr :: RuntimeRep) (z :: TYPE zr)
             . (Int -> DataFrame t ds -> z) -> Int -> z
          f consume = (`consume` e)
      in k Dict f 0
    )
    ( \cdims off arr ->
      let cd = CumulDims . tail $ unCumulDims cdims
          td = cdTotalDim# cd
          n = case dimVal d of W# w -> word2Int# w
          f :: forall (zr :: RuntimeRep) (z :: TYPE zr)
             . (Int -> DataFrame t ds -> z) -> Int -> z
          f consume (I# o) = consume (I# (o -# td)) (fromElems cd o arr)
      in k Dict f (I# (off +# td *# (n -# 1#)))
    )
  | otherwise = error "Numeric.DataFrame.Type.unpackDF: impossible args"


-- | Append one DataFrame to another, sum up the first dimension.
--
--   If you want to deconstruct a DataFrame, use
--       `Numeric.DataFrame.SubSpace.index`
--    or `Numeric.DataFrame.SubSpace.slice` instead.
appendDF :: forall (n :: Nat) (m :: Nat) (ds :: [Nat]) (t :: Type)
       . ( PrimBytes t, Dimensions ds, KnownDim n, KnownDim m )
        => DataFrame t (n :+ ds)
        -> DataFrame t (m :+ ds)
        -> DataFrame t ((n + m) :+ ds)
appendDF
  | D <- D @n `plusDim` D @m
  , Dict <- inferKnownBackend @t @(n :+ ds)
  , Dict <- inferKnownBackend @t @(m :+ ds)
  , Dict <- inferKnownBackend @t @((n + m) :+ ds)
              = unsafeAppendPB
  | otherwise = error "Numeri.DataFrame.Type/appendDF: impossible arguments"

-- | Append a small DataFrame to a big DataFrame on the left.
--
--   If you want to deconstruct a DataFrame, use
--       `Numeric.DataFrame.SubSpace.index`
--    or `Numeric.DataFrame.SubSpace.slice` instead.
consDF :: forall (n :: Nat) (ds :: [Nat]) (t :: Type)
       . ( PrimBytes t, Dimensions ds, KnownDim n )
        => DataFrame t ds
        -> DataFrame t (n :+ ds)
        -> DataFrame t ((n + 1) :+ ds)
consDF
  | D <- D @n `plusDim` D1
  , Dict <- inferKnownBackend @t @(n :+ ds)
  , Dict <- inferKnownBackend @t @ds
  , Dict <- inferKnownBackend @t @((n + 1) :+ ds)
              = unsafeAppendPB
  | otherwise = error "Numeri.DataFrame.Type/consDF: impossible arguments"

-- | Append a small DataFrame to a big DataFrame on the right.
--
--   If you want to deconstruct a DataFrame, use
--       `Numeric.DataFrame.SubSpace.index`
--    or `Numeric.DataFrame.SubSpace.slice` instead.
snocDF :: forall (n :: Nat) (ds :: [Nat]) (t :: Type)
       . ( PrimBytes t, Dimensions ds, KnownDim n )
        => DataFrame t (n :+ ds)
        -> DataFrame t ds
        -> DataFrame t ((n + 1) :+ ds)
snocDF
  | D <- D @n `plusDim` D1
  , Dict <- inferKnownBackend @t @(n :+ ds)
  , Dict <- inferKnownBackend @t @ds
  , Dict <- inferKnownBackend @t @((n + 1) :+ ds)
              = unsafeAppendPB
  | otherwise = error "Numeri.DataFrame.Type/snocDF: impossible arguments"


-- | Unsafely copy two PrimBytes values into a third one.
unsafeAppendPB :: forall x y z
                . (PrimBytes x, PrimBytes y, PrimBytes z)
               => x -> y -> z
unsafeAppendPB x y = go (byteSize x) (byteSize y)
  where
    go :: Int# -> Int# -> z
    go 0# _  = fromBytes (byteOffset y) (getBytes y)
    go _  0# = fromBytes (byteOffset x) (getBytes x)
    go sx sy = case runRW#
      ( \s0 -> case newByteArray# (sx +# sy) s0 of
          (# s1, mba #) -> unsafeFreezeByteArray# mba
              ( writeBytes mba sx y
              ( writeBytes mba 0# x s1))
      ) of (# _, r #) -> fromBytes 0# r

-- | Construct a DataFrame from a list of smaller DataFrames.
--
--
--   If the argument list is shorter than @d@, then the rest of the frame
--   is padded with a default value (first argument).
--
--   If the argument list is longer than @d@, then unused values are dropped.
--   If you want, you can pass an infinite list as an argument.
fromListWithDefault :: forall (t :: Type) (d :: Nat) (ds :: [Nat])
                     . (PrimBytes t, Dimensions (d ': ds))
                    => DataFrame t ds -> [DataFrame t ds] -> DataFrame t (d ': ds)
fromListWithDefault d ds = snd $ packDF' f ((,) ds)
  where
    f :: forall r . ([DataFrame t ds], DataFrame t ds -> r) -> ([DataFrame t ds], r)
    f ([] ,  k) = ([], k d)
    f (x:xs, k) = (xs, k x)

-- | Construct a dynamic DataFrame from a list of smaller DataFrames.
--   Pattern-match against the resulting @XFrame@ to find out its dimensionality.
--
--   You must not provide an infinite list as an argument.
fromList :: forall (t :: Type) (ds :: [Nat]) (xds :: [XNat])
          . (PrimBytes t, Dimensions ds, xds ~ Map N ds, ds ~ UnMap N xds)
         => [DataFrame t ds] -> DataFrame t (XN 0 ': xds)
fromList xs
    | Dx (D :: Dim n) <- someDimVal . fromIntegral $ length xs
    , Dict <- inferKnownBackend @t @(n ': ds)
    , Dict <- unsafeEqTypes @_ @ds @(DimsBound xds)
    , Dict <- inferExactFixedDims (dims @ds)
      = XFrame (fromListWithDefault @t @n @ds undefined xs)
    | otherwise
      = case Dict @(ds ~ UnMap N xds) of
          Dict -> -- just make GHC not complain about unused constraints.
            error "Numeri.DataFrame.Type/fromList: impossible arguments"

-- | Try to convert between @XNat@-indexed DataFrames.
--
--   This is useful for imposing restrictions on unknown DataFrames,
--   e.g. increasing the minimum number of elements.
constrainDF :: forall (ds :: [XNat]) (ys :: [XNat]) t
             . (BoundedDims ds, All KnownDimType ds)
            => DataFrame t ys -> Maybe (DataFrame t ds)
constrainDF (XFrame (df :: DataFrame t ns))
  | ns <- dims @ns
  = case constrainDims @ds ns of
      Just (XDims (Dims :: Dims ms))
        | Dict <- unsafeEqTypes @[Nat] @ns @ms
          -> Just $ XFrame df
      _   -> Nothing

-- | Place elements of a vector on the main diagonal of a matrix;
--   fill the rest of the matrix with zeroes.
--
--   Note, this function is naturally generalized onto higher dimensions
--    (which can be seen from the type signature).
--
--   Note, the argument of this function does not fully determine its result type.
--   This may cause some obscure type errors.
--   Specify type parameters @n@ and @m@ explicitly or make sure the result type is fixed.
asDiag :: forall (n :: Nat) (m :: Nat) (ds :: [Nat]) (t :: Type)
        . ( Dimensions ds, KnownDim n, KnownDim m
          , KnownBackend t (Min n m :+ ds)
          , KnownBackend t (n :+ m :+ ds)
          , PrimBytes t)
       => DataFrame t (Min n m :+ ds)
       -> DataFrame t (n :+ m :+ ds)
asDiag x
  | elemSize <- byteSize @t undefined
  , dMinNM@D <- minDim (D @n) (D @m)
  , xba <- getBytes x
  , ds <- dims @ds
  , steps@(CumulDims (elemsNR:_:elemsNE:_)) <- cumulDims (D @n :* D @m :* ds)
  , m <- dimVal' @m
  , bsE <- case elemsNE of W# w -> word2Int# w *# elemSize
  , bsR <- case elemsNR of W# w -> word2Int# w *# elemSize
  , bsShift <- case m + 1 of W# w -> word2Int# w *# bsE
  , bsLim <- case m * dimVal dMinNM of W# w -> word2Int# w *# bsE
    = case runRW#
      ( \s0 -> case newByteArray# bsR s0 of
          (# s1, mba #) ->
            let go offSrc offDst s
                  | isTrue# (offDst >=# bsLim) = s
                  | otherwise = go (offSrc +# bsE) (offDst +# bsShift)
                      (copyByteArray# xba offSrc mba offDst bsE s)
            in  unsafeFreezeByteArray# mba
                  (go (byteOffset x) 0# (setByteArray# mba 0# bsR 0# s1))

      ) of (# _, r #) -> fromElems steps 0# r
  | otherwise
    = error "Numeri.DataFrame.Type/asDiag: impossible arguments"
{-# INLINE asDiag #-}

-- Need this for @packDF'@ to make @Int -> c z@ a proper second-order type
-- parameterized by the result type.
newtype Off c z = Off { runOff :: Int -> c z }

-- | Term-level structure of a @SingleFrame t ds@ is fully determined by its
--   type dimensionality @Typeable ds@.
--   Thus, @gunfold@ does not use its last argument (@Constr@) at all,
--   relying on the structure of the type parameter.
instance (Data t, PrimBytes t, Typeable ds)
      => Data (DataFrame (t :: Type) (ds :: [Nat])) where
    gfoldl k z v = case typeableDims @ds of
      U | S x <- v
        -> z S `k` x
      D :* (Dims :: Dims ns)
        -> case inferTypeableCons @ds of
          Dict ->
            -- PLZ don't ask me how does this work
            unpackDF' (\_ f -> runOff $ packDF'
                           (\g -> Off . f $ k . runOff g)
                           (Off . const . z)
                       ) v
    gunfold k z _ = case typeableDims @ds of
      U      -> k (z S)
      D :* (Dims :: Dims ns)
        -> case inferTypeableCons @ds of Dict -> packDF' k z
    toConstr _ = case typeableDims @ds of
      U      -> scalarFrameConstr
      d :* _ -> singleFrameConstr $ dimVal d
    dataTypeOf _ = case typeableDims @ds of
      U      -> dataFrameDataType [scalarFrameConstr]
      d :* _ -> dataFrameDataType . (:[]). singleFrameConstr $ dimVal d

-- | Term-level structure of a @MultiFrame ts@ is fully determined by its
--   type @Typeable ts@.
--   Thus, @gunfold@ does not use its last argument (@Constr@) at all,
--   relying on the structure of the type parameter.
instance (AllFrames Data ts ds, Typeable ts, Typeable ds)
      => Data (DataFrame (ts :: [Type]) (ds :: [Nat])) where
    gfoldl _ z Z = z Z
    gfoldl k z (x :*: xs) = case inferTypeableCons @ts of
      Dict -> z (:*:) `k` x `k` xs
    gunfold k z _ = case typeables @ts of
      U      -> z Z
      _ :* _ -> case inferTypeableCons @ts of Dict -> k (k (z (:*:)))
    toConstr Z         = multiFrameZConstr
    toConstr (_ :*: _) = multiFrameConsConstr
    dataTypeOf _ = dataFrameDataType [multiFrameZConstr, multiFrameConsConstr]


dataFrameDataType :: [Constr] -> DataType
dataFrameDataType = mkDataType "Numeric.DataFrame.Type.DataFrame"

scalarFrameConstr :: Constr
scalarFrameConstr
  = mkConstr (dataFrameDataType [scalarFrameConstr]) "S" [] Prefix

singleFrameConstr :: Word -> Constr
singleFrameConstr d
  = mkConstr (dataFrameDataType [singleFrameConstr d]) ("DF" ++ show d) [] Prefix

multiFrameZConstr :: Constr
multiFrameZConstr = mkConstr
     (dataFrameDataType [multiFrameZConstr, multiFrameConsConstr])
     "Z" [] Prefix

multiFrameConsConstr :: Constr
multiFrameConsConstr = mkConstr
     (dataFrameDataType [multiFrameZConstr, multiFrameConsConstr])
     ":*:" [] Infix


type DFMetaSel = 'G.MetaSel
  'Nothing 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy

type family DFTree (t :: Type) (ds :: [Nat]) (d :: Nat) where
  DFTree t ds 0 = G.U1
  DFTree t ds 1 = G.S1 DFMetaSel (G.Rec0 (DataFrame t ds))
  DFTree t ds n = DFTree t ds (Div n 2) G.:*: DFTree t ds (Div n 2 + Mod n 2)

type family SingleFrameRep (t :: Type) (ds :: [Nat]) :: (Type -> Type) where
  SingleFrameRep t '[]
    = G.C1 ('G.MetaCons "S" 'G.PrefixI 'False) (G.S1 DFMetaSel (G.Rec0 t))
  SingleFrameRep t (d ': ds)
    = G.C1 ('G.MetaCons (AppendSymbol "DF" (ShowNat d)) 'G.PrefixI 'False) (DFTree t ds d)

instance (PrimBytes t, Dimensions ds)
      => G.Generic (DataFrame (t :: Type) (ds :: [Nat])) where
    type Rep (DataFrame t ds) = G.D1
          ('G.MetaData "DataFrame" "Numeric.DataFrame.Type" "easytensor" 'False)
          ( SingleFrameRep t ds )
    from = G.M1 . fromSingleFrame (dims @ds)
    to (G.M1 rep) = toSingleFrame (dims @ds) rep

fromSingleFrame :: forall (t :: Type) (ds :: [Nat]) (x :: Type)
                 . PrimBytes t
                => Dims ds
                -> DataFrame t ds
                -> SingleFrameRep t ds x
fromSingleFrame U (S x) = G.M1 . G.M1 $ G.K1 x
fromSingleFrame (dd@D :* (Dims :: Dims ds')) x
  | Dict <- inferKnownBackend @t @ds
  , Dict <- inferKnownBackend @t @ds'
    = G.M1 $ withArrayContent
        (\e -> fillRep @_ @ds' (const $ broadcast e) 0 dd)
        (\cdims off arr ->
           let cd = CumulDims . tail $ unCumulDims cdims
               td = cdTotalDim# cd
           in  fillRep @_ @ds'
                 (\(W# i) -> fromElems cd (off +# td *# word2Int# i) arr) 0 dd
        ) x
  where
    fillRep :: forall (n :: Nat) (ns :: [Nat])
             . (Word -> DataFrame t ns)
            -> Word
            -> Dim n
            -> DFTree t ns n x
    fillRep _ _ D0 = G.U1
    fillRep f i D1 = G.M1 . G.K1 $ f i
    fillRep f i d
        | Dict <- unsafeEqTypes @(Type -> Type)
            @(DFTree t ns n)
            @(DFTree t ns (Div n 2) G.:*: DFTree t ns (Div n 2 + Mod n 2))
          = fillRep f i d2 G.:*: fillRep f (i + dimVal d2) d2'
      where
        d2  = divDim d D2
        d2' = d2 `plusDim` modDim d D2

toSingleFrame :: forall (t :: Type) (ds :: [Nat]) (x :: Type)
               . PrimBytes t
              => Dims ds
              -> SingleFrameRep t ds x
              -> DataFrame t ds
toSingleFrame U (G.M1 (G.M1 (G.K1 x))) = S x
toSingleFrame (dd@D :* (Dims :: Dims ds')) (G.M1 rep)
  | Dict  <- inferKnownBackend @t @ds
  , Dict  <- inferKnownBackend @t @ds'
  , els   <- case dimVal dd of W# w -> word2Int# w
  , asize <- byteSize @(DataFrame t ds') undefined
    = runRW#
    ( \s0 -> case newByteArray# (asize *# els) s0 of
       (# s1, mba #)
         | s2 <- fillDF @_ @ds'
                  (\(W# i) df -> writeBytes mba (asize *# word2Int# i) df)
                  0 dd rep s1
         , (# _, ba #) <- unsafeFreezeByteArray# mba s2
           -> fromBytes 0# ba
    )
  where
    fillDF :: forall (n :: Nat) (ns :: [Nat]) s
            . (Word -> DataFrame t ns -> State# s -> State# s)
           -> Word
           -> Dim n
           -> DFTree t ns n x
           -> State# s -> State# s
    fillDF _ _ D0 _ s               = s
    fillDF f i D1 (G.M1 (G.K1 e)) s = f i e s
    fillDF f i d    xy             s
      | Dict <- unsafeEqTypes @(Type -> Type)
          @(DFTree t ns n)
          @(DFTree t ns (Div n 2) G.:*: DFTree t ns (Div n 2 + Mod n 2))
      , x G.:*: y <- xy
        = fillDF f (i + dimVal d2) d2' y (fillDF f i d2 x s)
      where
        d2  = divDim d D2
        d2' = d2 `plusDim` modDim d D2
{-# INLINE toSingleFrame #-}

type family MultiFrameRepNil (ts :: [Type]) :: (Type -> Type) where
    MultiFrameRepNil '[]      = G.C1 ('G.MetaCons "Z" 'G.PrefixI 'False) G.U1
    MultiFrameRepNil (_ ': _) = G.Rec0 Void

type family MultiFrameRepCons (ts :: [Type]) (ds :: [Nat]) :: (Type -> Type) where
    MultiFrameRepCons '[]       _  = G.Rec0 Void
    MultiFrameRepCons (t ': ts) ds = G.C1
      ('G.MetaCons ":*:" ('G.InfixI 'G.RightAssociative 6) 'False)
      ( G.S1 DFMetaSel
           (G.Rec0 (DataFrame t ds))
       G.:*:
        G.S1 DFMetaSel
           (G.Rec0 (DataFrame ts ds))
      )

instance G.Generic (DataFrame (ts :: [Type]) (ds :: [Nat])) where
    type Rep (DataFrame ts ds) = G.D1
          ('G.MetaData "DataFrame" "Numeric.DataFrame.Type" "easytensor" 'False)
          ( MultiFrameRepNil ts G.:+: MultiFrameRepCons ts ds )
    from  Z         = G.M1 (G.L1 (G.M1 G.U1))
    from (x :*: xs) = G.M1 (G.R1 (G.M1 (G.M1 (G.K1 x) G.:*: G.M1 (G.K1 xs))))
    to (G.M1 (G.L1 _))
      | Dict <- unsafeEqTypes @[Type] @ts @'[] = Z
    to (G.M1 (G.R1 xxs))
      | Dict <- unsafeEqTypes @[Type] @ts @(Head ts ': Tail ts)
      , G.M1 (G.M1 (G.K1 x) G.:*: G.M1 (G.K1 xs)) <- xxs = x :*: xs



instance (AllFrames Eq ts ds, AllFrames Ord ts ds)
      => Ord (DataFrame (ts :: [Type]) ds) where
    compare Z Z                   = EQ
    compare (a :*: as) (b :*: bs) = compare a b <> compare as bs


withFixedDF1 :: forall (l :: Type) (ts :: l) (xns :: [XNat])
                       (rep :: RuntimeRep) (r :: TYPE rep)
              . xns ~ Map 'N (DimsBound xns)
             => ( forall (ns :: [Nat])
               . ( All KnownDimType xns, FixedDims xns ns
                 , Dimensions ns
                 , KnownBackends ts ns
                 , ns ~ DimsBound xns
                 , xns ~ Map 'N ns
                 ) => DataFrame ts ns -> r
              ) -> DataFrame ts xns -> r
withFixedDF1 f (XFrame (df :: DataFrame ts ds))
  | Dict <- unsafeEqTypes @_ @ds @(DimsBound xns)
    = f df
{-# INLINE withFixedDF1 #-}

withFixedDF2 :: forall (l :: Type) (ts :: l) (xns :: [XNat])
                       (rep :: RuntimeRep) (r :: TYPE rep)
              . xns ~ Map 'N (DimsBound xns)
             => ( forall (ns :: [Nat])
               . ( All KnownDimType xns, FixedDims xns ns
                 , Dimensions ns
                 , KnownBackends ts ns
                 , ns ~ DimsBound xns
                 , xns ~ Map 'N ns
                 ) => DataFrame ts ns -> DataFrame ts ns -> r
              ) -> DataFrame ts xns -> DataFrame ts xns -> r
withFixedDF2 f (XFrame (a :: DataFrame ts as)) (XFrame (b :: DataFrame ts bs))
  | Dict <- unsafeEqTypes @_ @as @(DimsBound xns)
  , Dict <- unsafeEqTypes @_ @bs @(DimsBound xns)
    = f a b
{-# INLINE withFixedDF2 #-}

instance ( xns ~ Map 'N (DimsBound xns)
         , Eq (DataFrame ts xns)
         , Ord (DataFrame ts (DimsBound xns))
         ) => Ord (DataFrame (ts :: l) (xns :: [XNat])) where
    compare = withFixedDF2 compare
    (>=) = withFixedDF2 (>=)
    (<=) = withFixedDF2 (<=)
    (>)  = withFixedDF2 (>)
    (<)  = withFixedDF2 (<)
    min  = withFixedDF2 ((.) XFrame . min)
    max  = withFixedDF2 ((.) XFrame . max)

instance ( Dimensions xns
         , KnownBackends ts (DimsBound xns)
         , Num (DataFrame ts (DimsBound xns))
         ) => Num (DataFrame (ts :: l) (xns :: [XNat])) where
    (+) = withKnownXDims @xns $ withFixedDF2 ((.) XFrame . (+))
    (-) = withKnownXDims @xns $ withFixedDF2 ((.) XFrame . (-))
    (*) = withKnownXDims @xns $ withFixedDF2 ((.) XFrame . (*))
    negate = withKnownXDims @xns $ withFixedDF1 (XFrame . negate)
    abs    = withKnownXDims @xns $ withFixedDF1 (XFrame . abs)
    signum = withKnownXDims @xns $ withFixedDF1 (XFrame . signum)
    fromInteger = withKnownXDims @xns
      (XFrame . fromInteger @(DataFrame ts (DimsBound xns)))

instance ( Dimensions xns
         , KnownBackends ts (DimsBound xns)
         , Fractional (DataFrame ts (DimsBound xns))
         ) => Fractional (DataFrame (ts :: l) (xns :: [XNat])) where
    (/) = withKnownXDims @xns $ withFixedDF2 ((.) XFrame . (/))
    recip = withKnownXDims @xns $ withFixedDF1 (XFrame . recip)
    fromRational = withKnownXDims @xns
      (XFrame . fromRational @(DataFrame ts (DimsBound xns)))

instance ( Dimensions xns
         , KnownBackends ts (DimsBound xns)
         , Floating (DataFrame ts (DimsBound xns))
         ) => Floating (DataFrame (ts :: l) (xns :: [XNat])) where
    pi    = withKnownXDims @xns $ XFrame (pi :: DataFrame ts (DimsBound xns))
    exp   = withKnownXDims @xns $ withFixedDF1 (XFrame . exp)
    log   = withKnownXDims @xns $ withFixedDF1 (XFrame . log)
    sqrt  = withKnownXDims @xns $ withFixedDF1 (XFrame . sqrt)
    sin   = withKnownXDims @xns $ withFixedDF1 (XFrame . sin)
    cos   = withKnownXDims @xns $ withFixedDF1 (XFrame . cos)
    tan   = withKnownXDims @xns $ withFixedDF1 (XFrame . tan)
    asin  = withKnownXDims @xns $ withFixedDF1 (XFrame . asin)
    acos  = withKnownXDims @xns $ withFixedDF1 (XFrame . acos)
    atan  = withKnownXDims @xns $ withFixedDF1 (XFrame . atan)
    sinh  = withKnownXDims @xns $ withFixedDF1 (XFrame . sinh)
    cosh  = withKnownXDims @xns $ withFixedDF1 (XFrame . cosh)
    tanh  = withKnownXDims @xns $ withFixedDF1 (XFrame . tanh)
    asinh = withKnownXDims @xns $ withFixedDF1 (XFrame . asinh)
    acosh = withKnownXDims @xns $ withFixedDF1 (XFrame . acosh)
    atanh = withKnownXDims @xns $ withFixedDF1 (XFrame . atanh)
    (**)    = withKnownXDims @xns $ withFixedDF2 ((.) XFrame . (**))
    logBase = withKnownXDims @xns $ withFixedDF2 ((.) XFrame . logBase)

instance ( xns ~ Map 'N (DimsBound xns)
         , ProductOrder (DataFrame ts (DimsBound xns))
         ) => ProductOrder (DataFrame (ts :: l) (xns :: [XNat])) where
    cmp = withFixedDF2 cmp

instance ( Dimensions xns
         , KnownBackends ts (DimsBound xns)
         , Bounded (DataFrame ts (DimsBound xns))
         ) => Bounded (DataFrame (ts :: l) (xns :: [XNat])) where
    minBound = withKnownXDims @xns (XFrame (minBound :: DataFrame ts (DimsBound xns)))
    maxBound = withKnownXDims @xns (XFrame (maxBound :: DataFrame ts (DimsBound xns)))

instance ( Dimensions xns
         , KnownBackends ts (DimsBound xns)
         , PrimBytes (DataFrame ts (DimsBound xns))
         ) => PrimBytes (DataFrame (ts :: l) (xns :: [XNat])) where
    type PrimFields (DataFrame ts xns)
      = PrimFields (DataFrame ts (DimsBound xns))
    getBytes = withKnownXDims @xns $ withFixedDF1 getBytes
    getBytesPinned = withKnownXDims @xns $ withFixedDF1 getBytesPinned
    fromBytes i ba
      = withKnownXDims @xns (XFrame (fromBytes i ba :: DataFrame ts (DimsBound xns)))
    readBytes mba i s0
      | (# s1, (a  :: DataFrame ts (DimsBound xns)) #) <- readBytes mba i s0
        = (# s1, withKnownXDims @xns (XFrame a) #)
    writeBytes mba i = withKnownXDims @xns $ withFixedDF1 (writeBytes mba i)
    readAddr addr s0
      | (# s1, (a  :: DataFrame ts (DimsBound xns)) #) <- readAddr addr s0
        = (# s1, withKnownXDims @xns (XFrame a) #)
    writeAddr = withKnownXDims @xns $ withFixedDF1 writeAddr
    byteSize  _ = withKnownXDims @xns (byteSize @(DataFrame ts (DimsBound xns)) undefined)
    byteAlign _ = withKnownXDims @xns (byteAlign @(DataFrame ts (DimsBound xns)) undefined)
    byteOffset  = withKnownXDims @xns (withFixedDF1 byteOffset)
    byteFieldOffset n _
        = withKnownXDims @xns (byteFieldOffset @(DataFrame ts (DimsBound xns)) n undefined)
    indexArray ba i
      = withKnownXDims @xns (XFrame (indexArray ba i :: DataFrame ts (DimsBound xns)))
    readArray mba i s0
      | (# s1, (a  :: DataFrame ts (DimsBound xns)) #) <- readArray mba i s0
        = (# s1, withKnownXDims @xns (XFrame a) #)
    writeArray mba i = withKnownXDims @xns (withFixedDF1 (writeArray mba i))

instance ( Dimensions xns
         , KnownBackend t (DimsBound xns)
         , PrimArray t (DataFrame t (DimsBound xns))
         , PrimBytes t
         ) => PrimArray t (DataFrame t (xns :: [XNat])) where
    broadcast#
      = withKnownXDims @xns (XFrame . broadcast# @t @(DataFrame t (DimsBound xns)))
    ix# i = withKnownXDims @xns (withFixedDF1 (ix# i))
    gen# cd f s0
      | (# s1, (a  :: DataFrame t (DimsBound xns)) #) <- gen# cd f s0
        = (# s1, withKnownXDims @xns (XFrame a) #)
    upd# cd i t = withKnownXDims @xns (withFixedDF1 (XFrame . upd# cd i t))
    withArrayContent# f g = withKnownXDims @xns (withFixedDF1 (withArrayContent f g))
    offsetElems = withKnownXDims @xns (withFixedDF1 offsetElems)
    uniqueOrCumulDims = withKnownXDims @xns (withFixedDF1 uniqueOrCumulDims)
    fromElems# cd i ba
      = withKnownXDims @xns (XFrame (fromElems cd i ba :: DataFrame t (DimsBound xns)))


-- The following instance only make sense on scalars.

instance ( Enum (DataFrame ts ('[] :: [Nat]))
         , KnownBackends ts ('[] :: [Nat])
         ) => Enum (DataFrame (ts :: l) ('[] :: [XNat])) where
    succ (XFrame x) = XFrame (succ x)
    pred (XFrame x) = XFrame (pred x)
    toEnum  = XFrame . toEnum
    fromEnum (XFrame x) = fromEnum x
    enumFrom (XFrame x) = map XFrame $ enumFrom x
    enumFromThen   (XFrame x) (XFrame y) = map XFrame $ enumFromThen x y
    enumFromTo     (XFrame x) (XFrame y) = map XFrame $ enumFromTo x y
    enumFromThenTo (XFrame x) (XFrame y) (XFrame z)
      = map XFrame $ enumFromThenTo x y z

instance ( Epsilon (DataFrame ts ('[] :: [Nat]))
         , KnownBackends ts ('[] :: [Nat])
         , Eq (DataFrame ts ('[] :: [XNat]))
         ) => Epsilon (DataFrame (ts :: l) ('[] :: [XNat])) where
    epsilon = XFrame epsilon

instance ( Real (DataFrame ts ('[] :: [Nat]))
         , KnownBackends ts ('[] :: [Nat])
         , Eq (DataFrame ts ('[] :: [XNat]))
         ) => Real (DataFrame (ts :: l) ('[] :: [XNat])) where
    toRational (XFrame x) = toRational x

instance ( Integral (DataFrame ts ('[] :: [Nat]))
         , KnownBackends ts ('[] :: [Nat])
         , Eq (DataFrame ts ('[] :: [XNat]))
         ) => Integral (DataFrame (ts :: l) ('[] :: [XNat])) where
    quot (XFrame x) (XFrame y) = XFrame (quot x y)
    rem (XFrame x) (XFrame y) = XFrame (rem x y)
    div (XFrame x) (XFrame y) = XFrame (div x y)
    mod (XFrame x) (XFrame y) = XFrame (mod x y)
    quotRem (XFrame x) (XFrame y) = (XFrame *** XFrame) (quotRem x y)
    divMod (XFrame x) (XFrame y) = (XFrame *** XFrame) (divMod x y)
    toInteger (XFrame x) = toInteger x

instance ( RealExtras (DataFrame t ('[] :: [Nat]))
         , KnownBackend t ('[] :: [Nat])
         , Eq t
         ) => RealExtras (DataFrame (t :: Type) ('[] :: [XNat])) where
    copysign (XFrame x) (XFrame y) = XFrame (copysign x y)

instance ( RealFrac (DataFrame t ('[] :: [Nat]))
         , KnownBackend t ('[] :: [Nat])
         , Eq t
         ) => RealFrac (DataFrame (t :: Type) ('[] :: [XNat])) where
    properFraction (XFrame x) = second XFrame (properFraction x)
    truncate (XFrame x) = truncate x
    round (XFrame x) = round x
    ceiling (XFrame x) = ceiling x
    floor (XFrame x) = floor x

instance ( RealFloat (DataFrame t ('[] :: [Nat]))
         , KnownBackend t ('[] :: [Nat])
         , Eq t
         ) => RealFloat (DataFrame (t :: Type) ('[] :: [XNat])) where
    floatRadix  = const (floatRadix  @(DataFrame t ('[] :: [Nat])) undefined)
    floatDigits = const (floatDigits @(DataFrame t ('[] :: [Nat])) undefined)
    floatRange  = const (floatRange  @(DataFrame t ('[] :: [Nat])) undefined)
    decodeFloat (XFrame x) = decodeFloat x
    encodeFloat i = XFrame . encodeFloat i
    exponent (XFrame x) = exponent x
    significand (XFrame x) = XFrame (significand x)
    scaleFloat i (XFrame x) = XFrame (scaleFloat i x)
    isNaN (XFrame x) = isNaN x
    isInfinite (XFrame x) = isInfinite x
    isDenormalized (XFrame x) = isDenormalized x
    isNegativeZero (XFrame x) = isNegativeZero x
    isIEEE (XFrame x) = isIEEE x
    atan2 (XFrame y) (XFrame x) = XFrame (atan2 y x)

instance ( RealFloatExtras (DataFrame t ('[] :: [Nat]))
         , KnownBackend t ('[] :: [Nat])
         , Eq t
         ) => RealFloatExtras (DataFrame (t :: Type) ('[] :: [XNat])) where
    hypot (XFrame x) (XFrame y) = XFrame (hypot x y)
    maxFinite = XFrame maxFinite



unsafeEqTypes :: forall k (a :: k) (b :: k) . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce (Dict :: Dict (a ~ a))
