{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
{-# LANGUAGE ViewPatterns              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Dims
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Provides a data type @Dims ds@ to keep dimension sizes
-- for multiple-dimensional data.
-- Higher indices go first, i.e. assumed enumeration
--          is i = i1*n1*n2*...*n(k-1) + ... + i(k-2)*n1*n2 + i(k-1)*n1 + ik
-- This corresponds to row-first layout of matrices and multidimenional arrays.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Dims
  ( Dims, SomeDims (..), Dimensions (..), BoundedDims, DimsBound
  , TypedList ( Dims, XDims, AsXDims, KnownDims
              , U, (:*), Empty, TypeList, Cons, Snoc, Reverse)
  , constrainDims, dimsBound, typeableDims, inferTypeableDims
  , listDims, someDimsVal, totalDim, totalDim'
  , sameDims, sameDims'
  , inSpaceOf, asSpaceOf
  , xDims, xDims'
  , stripPrefixDims, stripSuffixDims
    -- * Type-level programming
    --   Provide type families to work with lists of dimensions (`[Nat]` or `[XNat]`)
  , AsXDims, AsDims, FixedDims, KnownXNatTypes
    -- * Re-export type list
  , RepresentableList (..), TypeList, types
  , order, order'
    -- * Re-export single dimension type and functions
  , module Numeric.Dim
  ) where


import           Data.Constraint
import           Data.List       (stripPrefix)
import           Data.Proxy      (Proxy)
import           GHC.Base        (Type)
import           GHC.Exts        (Constraint, unsafeCoerce#)
import qualified Text.Read       as Read
import           Type.Reflection

import Data.Type.List
import Numeric.Dim
import Numeric.TypedList (RepresentableList (..), TypeList, TypedList (..),
                          order, order', types)


-- | Type-level dimensionality.
type Dims (xs :: [k]) = TypedList Dim xs

{-# COMPLETE Dims #-}
{-# COMPLETE XDims #-}
{-# COMPLETE AsXDims #-}
{-# COMPLETE KnownDims #-}

-- | @O(1)@ Pattern-matching against this constructor brings a `Dimensions`
--   instance into the scope.
--   Thus, you can do arbitrary operations on your dims and use this pattern
--   at any time to reconstruct the class instance at runtime.
pattern Dims :: forall (ds :: [Nat]) . () => Dimensions ds => Dims ds
pattern Dims <- (dimsEv -> Dict)
  where
    Dims = dims @ds

-- | @O(Length ds)@ A heavy weapon against all sorts of type errors
pattern KnownDims :: forall (ds :: [Nat]) . ()
                  => ( All KnownDim ds, All BoundedDim ds
                     , RepresentableList ds, Dimensions ds)
                     => Dims ds
pattern KnownDims <- (patKDims -> PatKDims)
  where
    KnownDims = dims @ds


-- | Pattern-matching against this constructor reveals Nat-kinded list of dims,
--   pretending the dimensionality is known at compile time within the scope
--   of the pattern match.
--   This is the main recommended way to get `Dims` at runtime;
--   for example, reading a list of dimensions from a file.
--
--   In order to use this pattern, one must know @XNat@ type constructors in
--   each dimension at compile time.
pattern XDims :: forall (xns :: [XNat]) . KnownXNatTypes xns
              => forall (ns :: [Nat]) . (FixedDims xns ns, Dimensions ns)
              => Dims ns -> Dims xns
pattern XDims ns <- (patXDims -> PatXDims ns)
  where
    XDims ns = unsafeCoerce# ns

-- | An easy way to convert Nat-indexed dims into XNat-indexed dims.
pattern AsXDims :: forall (ns :: [Nat]) . ()
                => (KnownXNatTypes (AsXDims ns), RepresentableList (AsXDims ns))
                => Dims (AsXDims ns) -> Dims ns
pattern AsXDims xns <- (patAsXDims -> PatAsXDims xns)
  where
    AsXDims xns = unsafeCoerce# xns

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


-- | Constrain a list of @Dims@.
type BoundedDims (xds :: [k]) = (RepresentableList xds, All BoundedDim xds)
-- | Plural form of `DimBound`
type family DimsBound (xds :: [k]) :: [Nat] where
  DimsBound '[] = '[]
  DimsBound (n ': ns) = DimBound n ': DimsBound ns


-- | Given a @Dims ds@, test if its runtime value satisfies constraints imposed by
--   @All BoundedDim xds@, and returns it back coerced to @Dims xds@ on success.
--
--   This function allows to guess safely individual dimension values,
--   as well as the length of the dimension list.
--   It returns @Nothing@ if @ds@ and @xds@ have different length or if any
--   of the values in @ds@ is less than in @xds@.
constrainDims :: forall (k :: Type) (xds :: [k]) (ds :: [Nat])
               . BoundedDims xds
              => Dims ds -> Maybe (Dims xds)
constrainDims = go (tList @k @xds)
  where
    go :: forall (xns :: [k]) (ns :: [Nat])
        . All BoundedDim xns => TypeList xns -> Dims ns ->  Maybe (Dims xns)
    go U U = Just U
    go ((_ :: Proxy xn) :* xns) (d :* ns)
           = (:*) <$> constrain @k @xn d <*> go xns ns
    go _ _ = Nothing

-- | Plural form of `dimBound`
dimsBound :: forall (k :: Type) (xds :: [k])
           . BoundedDims xds => Dims (DimsBound xds)
dimsBound = go (tList @k @xds)
  where
    go :: forall (xns :: [k])
        . All BoundedDim xns => TypeList xns -> Dims (DimsBound xds)
    go U = unsafeCoerce# U
    go ((_ :: Proxy xn) :* xns)
         = unsafeCoerce# (dimBound @k @xn :* go xns)



-- | Construct a @Dims ds@ if there is an instance of @Typeable ds@ around.
typeableDims :: forall (ds :: [Nat]) . Typeable ds => Dims ds
typeableDims = case typeRep @ds of
    App (App _ (tx :: TypeRep (n :: k1))) (txs :: TypeRep (ns :: k2))
      -> case (unsafeCoerce# (Dict @(Nat ~ Nat, [Nat] ~ [Nat]))
                :: Dict (Nat ~ k1, [Nat] ~ k2)) of
          Dict -> case (unsafeCoerce# (Dict @(ds ~ ds))
                          :: Dict (ds ~ (n ': ns))) of
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
stripPrefixDims = unsafeCoerce# (stripPrefix :: [Word] -> [Word] -> Maybe [Word])
{-# INLINE stripPrefixDims #-}

-- | Drop the given suffix from a Dims list.
--   It returns Nothing if the list did not end with the suffix given,
--    or Just the Dims before the suffix, if it does.
stripSuffixDims :: forall (xs :: [Nat]) (ys :: [Nat])
                 . Dims xs -> Dims ys
                -> Maybe (Dims (StripSuffix xs ys))
stripSuffixDims = unsafeCoerce# (stripSuffix :: [Word] -> [Word] -> Maybe [Word])
{-# INLINE stripSuffixDims #-}

stripSuffix :: [Word] -> [Word] -> Maybe [Word]
stripSuffix suf whole = go pref whole
  where
    pref = getPref suf whole
    getPref (_:as) (_:bs) = getPref as bs
    getPref [] bs         = zipWith const whole bs
    getPref _  []         = []
    go (_:as) (_:bs) = go as bs
    go  []     bs    = if suf == bs then Just pref else Nothing
    go  _      []    = Nothing
{-# INLINE stripSuffix #-}

-- | We either get evidence that this function was instantiated with the
--   same type-level Dimensions, or 'Nothing' @O(Length xs)@.
sameDims :: forall (as :: [Nat]) (bs :: [Nat])
          . Dims as -> Dims bs -> Maybe (Dict (as ~ bs))
sameDims as bs
  | listDims as == listDims bs
    = Just (unsafeCoerce# (Dict @('[] ~ '[])))
  | otherwise = Nothing
{-# INLINE sameDims #-}

-- | We either get evidence that this function was instantiated with the
--   same type-level Dimensions, or 'Nothing' @O(Length xs)@.
sameDims' :: forall (as :: [Nat]) (bs :: [Nat]) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
           . (Dimensions as, Dimensions bs)
          => p as -> q bs -> Maybe (Dict (as ~ bs))
sameDims' _ _ = sameDims (dims @as) (dims @bs)
{-# INLINE sameDims' #-}


-- | Similar to `const` or `asProxyTypeOf`;
--   to be used on such implicit functions as `dim`, `dimMax`, etc.
inSpaceOf :: forall (ds :: [Nat]) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
           . p ds -> q ds -> p ds
inSpaceOf = const
{-# INLINE inSpaceOf #-}

-- | Similar to `asProxyTypeOf`,
--   Give a hint to type checker to fix the type of a function argument.
asSpaceOf :: forall (ds :: [Nat])
                    (p :: [Nat] -> Type) (q :: [Nat] -> Type) (r :: Type)
           . p ds -> (q ds -> r) -> (q ds -> r)
asSpaceOf = const id
{-# INLINE asSpaceOf #-}


instance Eq (Dims (ds :: [Nat])) where
    (==) _ _ = True

instance Eq (Dims (ds :: [XNat])) where
    (==) = unsafeCoerce# ((==) :: [Word] -> [Word] -> Bool)

instance Eq SomeDims where
    SomeDims as == SomeDims bs = listDims as == listDims bs

instance Ord (Dims (ds :: [Nat])) where
    compare _ _ = EQ

instance Ord (Dims (ds :: [XNat])) where
    compare = unsafeCoerce# (compare :: [Word] -> [Word] -> Ordering)

instance Ord SomeDims where
    compare = unsafeCoerce# (compare :: [Word] -> [Word] -> Ordering)

instance Show (Dims xs) where
    show ds = "Dims " ++ show (listDims ds)
    showsPrec p ds
      = showParen (p >= 10)
      $ showString "Dims " . showsPrec p (listDims ds)

instance Show SomeDims where
    show (SomeDims ds) = "SomeDims " ++ show (listDims ds)
    showsPrec p (SomeDims ds)
      = showParen (p >= 10)
      $ showString "SomeDims " . showsPrec p (listDims ds)

instance Read SomeDims where
    readPrec = Read.parens $ Read.prec 10 $ do
      s <- Read.lexP
      if s == Read.Ident "SomeDims"
      then someDimsVal <$> Read.readPrec
      else Read.pfail

instance Dimensions ds => Bounded (Dims ds) where
    maxBound = dims
    {-# INLINE maxBound #-}
    minBound = dims
    {-# INLINE minBound #-}



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

--------------------------------------------------------------------------------

-- | This function does GHC's magic to convert user-supplied `dims` function
--   to create an instance of `Dimensions` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDims :: forall (r :: Type) (ds :: [Nat]) . Dims ds -> (Dimensions ds => r) -> r
reifyDims ds k = unsafeCoerce# (MagicDims k :: MagicDims ds r) ds
{-# INLINE reifyDims #-}
newtype MagicDims (ds :: [Nat]) (r :: Type) = MagicDims (Dimensions ds => r)

dimsEv :: forall (ds :: [Nat]) . Dims ds -> Dict (Dimensions ds)
dimsEv ds = reifyDims ds Dict
{-# INLINE dimsEv #-}


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
