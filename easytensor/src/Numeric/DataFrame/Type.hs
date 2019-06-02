{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
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
{- |
  The core @easytensor@ types.
 -}
module Numeric.DataFrame.Type
  ( -- * Data types
-- #if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
--     DataFrame (SingleFrame, MultiFrame, XFrame)
--   , pattern Z, pattern (:*:)
--   , pattern S, pattern DF2, pattern DF3, pattern DF4, pattern DF5
--   , pattern DF6, pattern DF7, pattern DF8, pattern DF9
-- #else
    DataFrame ( SingleFrame, MultiFrame, XFrame, (:*:), Z
              , S, DF2, DF3, DF4, DF5, DF6, DF7, DF8, DF9)
-- #endif
  , SomeDataFrame (..), DataFrame'
    -- * Flexible assembling and disassembling
  , PackDF, packDF, unpackDF
    -- * Infer type class instances
  , KnownBackend (), DFBackend, KnownBackends
  , InferKnownBackend (..), inferPrimElem
    -- * Re-exports
  , Dim (..), Idx (), XNat (..), N, XN, Dims, Idxs, TypedList (..)
  , PrimBytes (), bSizeOf, bAlignOf
  , PrimArray (), ixOff, fromFlatList
  ) where


-- import           GHC.Generics     hiding (Infix, Prefix)
import           Data.Data
import           Data.Proxy                      (Proxy)
import           Foreign.Storable                (Storable (..))
import           GHC.Base
import           GHC.Exts
import           GHC.Ptr                         (Ptr (..))
import qualified Text.ParserCombinators.ReadPrec as Read
import qualified Text.Read                       as Read
import qualified Text.Read.Lex                   as Read

import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial
import           Numeric.Semigroup                    hiding (All)
import           Numeric.TypedList                    (typeables)

import {-# SOURCE #-} Numeric.DataFrame.Internal.Backend (DFBackend,
                                                          KnownBackend)
import {-# SOURCE #-} qualified Numeric.DataFrame.Internal.Backend as Backend


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
  . (KnownXNatTypes xns, FixedDims xns ns, Dimensions ns, KnownBackends ts ns)
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


-- | Constructing a @MultiFrame@ using DataFrame columns
pattern (:*:) :: forall (xs :: [Type]) (ns :: [Nat])  . ()
              => forall (y :: Type)    (ys :: [Type]) . (xs ~ (y ': ys))
              => DataFrame y ns -> DataFrame ys ns -> DataFrame xs ns
pattern (:*:) x xs <- (MultiFrame (DataFrame' x :* (MultiFrame -> xs)))
  where
    (:*:) x (MultiFrame xs) = MultiFrame (DataFrame' x :* xs)
infixr 6 :*:

-- | Empty MultiFrame
pattern Z :: forall (xs :: [Type]) (ns :: [Nat])
           . () => (xs ~ '[]) => DataFrame xs ns
pattern Z = MultiFrame U

-- | I use this kind-polymorphic constraint to generalize @XFrame@ and @SomeDataFrame@
--   over @SingleFrame@ and @MultiFrame@.
type family KnownBackends (ts :: l) (ns :: [Nat]) :: Constraint where
    KnownBackends ( t      ::  Type ) ns = KnownBackend t ns
    KnownBackends ('[]     :: [Type]) _  = ()
    KnownBackends (t ': ts :: [Type]) ns =
      (KnownBackend t ns, KnownBackends ts ns)

-- | Allow inferring @KnownBackends@ if you know the dimensions and the element types.
class InferKnownBackend (t :: k) ds where
    -- Infer @KnownBackends@ if you know the dimensions and the element types.
    inferKnownBackend :: Dict (KnownBackends t ds)

instance (PrimBytes t, Dimensions ds) => InferKnownBackend (t :: Type) ds where
    inferKnownBackend = Backend.inferKnownBackend @t @ds

instance (RepresentableList ts, All PrimBytes ts, Dimensions ds)
      => InferKnownBackend (ts :: [Type]) ds where
    inferKnownBackend = go (tList @_ @ts)
      where
        go :: forall ss . All PrimBytes ss => TypeList ss -> Dict (KnownBackends ss ds)
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
deriving instance Real (DFBackend t ds)
               => Real (DataFrame t ds)
deriving instance RealFrac (DFBackend t ds)
               => RealFrac (DataFrame t ds)
deriving instance RealFloat (DFBackend t ds)
               => RealFloat (DataFrame t ds)
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
        $ showString "S " . showsPrec 10 (unsafeCoerce# x :: t)
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
    readPrec = readFixedMultiDF (tList @Type @ts) (dims @ds)
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance (Read t, PrimBytes t, BoundedDims ds, All KnownXNatType ds)
      => Read (DataFrame (t :: Type) (ds :: [XNat])) where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "XFrame"
      Just ds <- pure $ constrainDims (dimsBound @XNat @ds)
      Read.step $ readPrecBoundedDF ds
    readList = Read.readListDefault
    readListPrec = Read.readListPrecDefault

instance ( All Read ts, All PrimBytes ts, RepresentableList ts
         , BoundedDims ds, All KnownXNatType ds)
      => Read (DataFrame (ts :: [Type]) (ds :: [XNat])) where
    readPrec = Read.parens . Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "XFrame"
      Just ds <- pure $ constrainDims (dimsBound @XNat @ds)
      Read.step $ readBoundedMultiDF (tList @Type @ts) ds
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
      Read.step $ readSomeMultiDF (tList @Type @ts)
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
                   . (Read t, PrimBytes t, KnownXNatTypes ds)
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
      , Just Dict <- indeedLE m n -- check if the DF dim is not less than m
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
  where
    indeedLE :: forall (a ::Nat) (b :: Nat) . Dim a -> Dim b -> Maybe (Dict (a <= b))
    indeedLE a b = case compareDim a b of
      SLT -> Just Dict
      SEQ -> Just Dict
      SGT -> Nothing

{-
In this case we know Nothing about the dimensionality of a DataFrame.
The logic is similar to readPrecBoundedDF, but a bit simpler:
the first dimension is flexible, but fixes the rest dimensions.
 -}
readPrecSomeDF :: forall (t :: Type) . (Read t, PrimBytes t)
               => Read.ReadPrec (SomeDataFrame t)
readPrecSomeDF = Read.parens $
    (Read.prec 10 $ do
      Read.lift . Read.expect $ Read.Ident "S"
      case inferKnownBackend @t @'[] of
        Dict -> SomeDataFrame . S <$> Read.readPrec
    )
    Read.+++
    (do
     lookLex >>= \case
       Read.Ident ('D':'F':s)
         | Just (Dx (d :: Dim d)) <- (Read.readMaybe ('D':s) :: Maybe SomeDim)
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
                    . (All Read ts, All PrimBytes ts, KnownXNatTypes ds)
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
pattern S x <- (unsafeCoerce# -> x)
  where
    S = unsafeCoerce#
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
         => ( PackDF t ds d (Dict (Dimensions ds, KnownBackend t ds) -> r) )
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
    go d a k = recur d (\_ -> c)
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
unpackDF' k df
  | d :* Dims <- dims @(d ': ds)
  , Dict <- inferKnownBackend @t @(d ': ds)
  , Dict <- inferKnownBackend @t @ds
    = case arrayContent# df of
        (# x | #)
          | e <- broadcast x
            -> let f :: forall (zr :: RuntimeRep) (z :: TYPE zr)
                      . (Int -> DataFrame t ds -> z) -> Int -> z
                   f consume = (`consume` e)
               in k Dict f 0
        (# | (# cdims, off, arr #) #)
          | cd <- CumulDims . tail $ unCumulDims cdims
          , td <- cdTotalDim# cd
          , n <- case dimVal d of W# w -> word2Int# w
            -> let f :: forall (zr :: RuntimeRep) (z :: TYPE zr)
                      . (Int -> DataFrame t ds -> z) -> Int -> z
                   f consume (I# o) = consume (I# (o -# td)) (fromElems cd o arr)
               in k Dict f (I# (off +# td *# (n -# 1#)))
  | otherwise = error "Numeric.DataFrame.Type.unpackDF: impossible args"



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
        -> case inferTypeableCons @_ @ds of
          Dict ->
            -- PLZ don't ask me how does this work
            unpackDF' (\_ f -> runOff $ packDF'
                           (\g -> Off . f $ k . runOff g)
                           (Off . const . z)
                       ) v
    gunfold k z _ = case typeableDims @ds of
      U      -> k (z S)
      D :* (Dims :: Dims ns)
        -> case inferTypeableCons @_ @ds of Dict -> packDF' k z
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
    gfoldl k z (x :*: xs) = case inferTypeableCons @Type @ts of
      Dict -> z (:*:) `k` x `k` xs
    gunfold k z _ = case typeables @Type @ts of
      U      -> z Z
      _ :* _ -> case inferTypeableCons @_ @ts of Dict -> k (k (z (:*:)))
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


unsafeEqTypes :: forall k (a :: k) (b :: k) . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce# (Dict :: Dict (a ~ a))
