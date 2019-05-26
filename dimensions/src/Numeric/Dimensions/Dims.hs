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
{-# LANGUAGE Rank2Types                #-}
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
  ( Dims, SomeDims (..), Dimensions (..), XDimensions (..)
  , TypedList ( Dims, XDims, AsXDims, KnownDims
              , U, (:*), Empty, TypeList, Cons, Snoc, Reverse)
  , listDims, someDimsVal, totalDim, totalDim'
  , sameDims, sameDims'
  , compareDims, compareDims'
  , inSpaceOf, asSpaceOf
  , xDims, xDims'
  , stripPrefixDims, stripSuffixDims
    -- * Type-level programming
    --   Provide type families to work with lists of dimensions (`[Nat]` or `[XNat]`)
  , AsXDims, AsDims, FixedDims, KnownXNatTypes, CmpNats
    -- * Re-export type list
  , RepresentableList (..), TypeList, types
  , order, order'
    -- * Re-export single dimension type and functions
  , module Numeric.Dim
  ) where


import           Data.Constraint
import           Data.List       (stripPrefix)
import           GHC.Exts        (Constraint, unsafeCoerce#)
import qualified Text.Read       as Read

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
pattern Dims :: forall ds . () => Dimensions ds => Dims ds
pattern Dims <- (dimsEv -> Dict)
  where
    Dims = dims @_ @ds

-- | @O(Length ds)@ `Dimensions` and `KnownDim` for each individual dimension.
pattern KnownDims :: forall ds . ()
                  => (All KnownDim ds, Dimensions ds) => Dims ds
pattern KnownDims <- (patKDims -> PatKDims)
  where
    KnownDims = dims @_ @ds


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
--   Normally, the kind paramater is `Nat` (known dimenionality)
--   or `XNat` (either known or constrained dimensionality).
class Dimensions (ds :: [k]) where
    -- | Get dimensionality of a space at runtime,
    --   represented as a list of `Dim`.
    --
    --   Note, this function is supposed to be used with @TypeApplications@,
    --   and the @Dimensions@ class has varying kind of the parameter;
    --   thus, the function has two type paremeters (kind and type of @ds@).
    --   For example, you can type:
    --
    --   >>>:set -XTypeApplications
    --   >>>:set -XDataKinds
    --   >>>:t dims @_ @'[N 17, N 12]
    --   dims @_ @'[N 17, N 12] :: Dims '[N 17, N 12]
    --
    --   >>>:t dims @XNat @'[]
    --   dims @XNat @'[] :: Dims '[]
    --
    --
    --   >>>:t dims @_ @(Tail '[3,2,5,7])
    --   dims @_ @(Tail '[3,2,5,7]) :: Dims '[2, 5, 7]
    --
    dims :: Dims ds

instance Dimensions ('[] :: [k]) where
    dims = U
    {-# INLINE dims #-}

instance (KnownDim d, Dimensions ds) => Dimensions (d ': ds :: [k]) where
    dims = dim :* dims
    {-# INLINE dims #-}

-- | Analogous to `Dimensions`, but weaker and more specific (list of `XNat`).
--   This class is used to check if an existing fixed `Dims` satisfy
--   constraints imposed by some interface (e.g. all dimensions are greater than 2).
--   It is weaker than `Dimensions` in that it only requires knowledge of constraints
--   rather than exact dimension values.
class KnownXNatTypes xds => XDimensions (xds :: [XNat]) where
    -- | Given a `Dims`, test if its runtime value satisfies constraints imposed by
    --   @XDimensions@, and returns it back being `XNat`-indexed on success.
    --
    --   This function allows to guess safely individual dimension values,
    --   as well as the length of the dimension list.
    constrainDims :: Dims (ds :: [k]) -> Maybe (Dims xds)

instance XDimensions '[] where
    constrainDims U = Just U
    constrainDims _ = Nothing
    {-# INLINE constrainDims #-}

instance (XDimensions xs, KnownDim m) => XDimensions (XN m ': xs) where
    constrainDims (d :* ds) = case constrain d of
      Nothing -> Nothing
      Just xd -> (xd :*) <$> constrainDims ds
    constrainDims Empty = Nothing

instance (XDimensions xs, KnownDim n) => XDimensions (N n ': xs) where
    constrainDims (d :* ds)
      | unsafeCoerce# d == dimVal' @n = (Dn D :*) <$> constrainDims ds
      | otherwise                     = Nothing
    constrainDims Empty = Nothing


-- | Convert `Dims xs` to a plain haskell list of dimension sizes @O(1)@.
listDims :: Dims xs -> [Word]
listDims = unsafeCoerce#
{-# INLINE listDims #-}

-- | Convert a plain haskell list of dimension sizes into an unknown
--   type-level dimensionality  @O(1)@.
someDimsVal :: [Word] -> SomeDims
someDimsVal = SomeDims . unsafeCoerce#
{-# INLINE someDimsVal #-}

-- | Product of all dimension sizes @O(Length xs)@.
totalDim :: Dims xs -> Word
totalDim = product . listDims
{-# INLINE totalDim #-}

-- | Product of all dimension sizes @O(Length xs)@.
totalDim' :: forall xs . Dimensions xs => Word
totalDim' = totalDim (dims @_ @xs)
{-# INLINE totalDim' #-}

-- | Get XNat-indexed dims given their fixed counterpart.
xDims :: FixedDims xns ns => Dims ns -> Dims xns
xDims = unsafeCoerce#
{-# INLINE xDims #-}

-- | Get XNat-indexed dims given their fixed counterpart.
xDims' :: forall xns ns . (FixedDims xns ns, Dimensions ns) => Dims xns
xDims' = xDims @xns (dims @Nat @ns)
{-# INLINE xDims' #-}

-- | Drop the given prefix from a Dims list.
--   It returns Nothing if the list did not start with the prefix given,
--    or Just the Dims after the prefix, if it does.
stripPrefixDims :: Dims (xs :: [Nat]) -> Dims (ys :: [Nat])
                -> Maybe (Dims (StripPrefix xs ys))
stripPrefixDims = unsafeCoerce# (stripPrefix :: [Word] -> [Word] -> Maybe [Word])
{-# INLINE stripPrefixDims #-}

-- | Drop the given suffix from a Dims list.
--   It returns Nothing if the list did not end with the suffix given,
--    or Just the Dims before the suffix, if it does.
stripSuffixDims :: Dims (xs :: [Nat]) -> Dims (ys :: [Nat])
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
--
--   Note, this function works on @Nat@-indexed dimensions only,
--   because @Dims '[XN x]@ does not have runtime evidence to infer @x@
--   and `KnownDim x` does not imply `KnownDim (XN x)`.
sameDims :: Dims (as :: [Nat]) -> Dims (bs :: [Nat]) -> Maybe (Dict (as ~ bs))
sameDims as bs
  | listDims as == listDims bs
    = Just (unsafeCoerce# (Dict @('[] ~ '[])))
  | otherwise = Nothing
{-# INLINE sameDims #-}

-- | We either get evidence that this function was instantiated with the
--   same type-level Dimensions, or 'Nothing' @O(Length xs)@.
sameDims' :: forall (as :: [Nat]) (bs :: [Nat]) p q
           . (Dimensions as, Dimensions bs)
          => p as -> q bs -> Maybe (Dict (as ~ bs))
sameDims' _ _ = sameDims (dims @Nat @as) (dims @Nat @bs)
{-# INLINE sameDims' #-}

-- | Compare dimensions by their size in lexicorgaphic order
--   from the first dimension to the last dimension
--   (the first dimension is the most significant one).
--
--   This is the same @compare@ rule, as for `Idxs` and normal Haskell lists.
--
--   Note: `CmpNats` forces type parameters to kind `Nat`;
--         if you want to compare unknown `XNat`s, use `Ord` instance of `Dims`.
compareDims :: Dims as -> Dims bs -> SOrdering (CmpNats as bs)
compareDims a b
  = case unsafeCoerce# (compare :: [Word] -> [Word] -> Ordering) a b of
    LT -> unsafeCoerce# SLT
    EQ -> unsafeCoerce# SEQ
    GT -> unsafeCoerce# SGT
{-# INLINE compareDims #-}

-- | Compare dimensions by their size in lexicorgaphic order
--   from the first dimension to the last dimension
--   (the first dimension is the most significant one).
--
--   This is the same @compare@ rule, as for `Idxs` and normal Haskell lists.
compareDims' :: forall as bs p q
              . (Dimensions as, Dimensions bs)
             => p as -> q bs -> SOrdering (CmpNats as bs)
compareDims' _ _ = compareDims (dims @_ @as) (dims @_ @bs)
{-# INLINE compareDims' #-}

-- | Similar to `const` or `asProxyTypeOf`;
--   to be used on such implicit functions as `dim`, `dimMax`, etc.
inSpaceOf :: a ds -> b ds -> a ds
inSpaceOf x _ = x
{-# INLINE inSpaceOf #-}

-- | Similar to `asProxyTypeOf`,
--   Give a hint to type checker to fix the type of a function argument.
asSpaceOf :: a ds -> (b ds -> c) -> (b ds -> c)
asSpaceOf _ = id
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

type family CmpNats (xs :: [Nat]) (ys :: [Nat]) :: Ordering where
    CmpNats '[] '[] = 'EQ
    CmpNats '[]  _  = 'LT
    CmpNats  _  '[] = 'GT
    CmpNats (x:xs) (y:ys) = Compared
      (CmpNat x y) 'LT (CmpNats xs ys) 'GT

-- | Know the structure of each dimension
type KnownXNatTypes xns = All KnownXNatType xns

--------------------------------------------------------------------------------

-- | This function does GHC's magic to convert user-supplied `dims` function
--   to create an instance of `Dimensions` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDims :: forall r ds . Dims ds -> (Dimensions ds => r) -> r
reifyDims ds k = unsafeCoerce# (MagicDims k :: MagicDims ds r) ds
{-# INLINE reifyDims #-}
newtype MagicDims ds r = MagicDims (Dimensions ds => r)

dimsEv :: Dims ds -> Dict (Dimensions ds)
dimsEv ds = reifyDims ds Dict
{-# INLINE dimsEv #-}


data PatXDims (xns :: [XNat])
  = forall (ns :: [Nat])
  . (FixedDims xns ns, Dimensions ns) => PatXDims (Dims ns)


patXDims :: All KnownXNatType xns => Dims xns -> PatXDims xns
patXDims U = PatXDims U
patXDims (Dn n :* xns) = case patXDims xns of
  PatXDims ns -> PatXDims (n :* ns)
patXDims (Dx n :* xns) = case patXDims xns of
  PatXDims ns -> PatXDims (n :* ns)
{-# INLINE patXDims #-}


data PatAsXDims (ns :: [Nat])
  = (KnownXNatTypes (AsXDims ns), RepresentableList (AsXDims ns))
  => PatAsXDims (Dims (AsXDims ns))


patAsXDims :: Dims ns -> PatAsXDims ns
patAsXDims U = PatAsXDims U
patAsXDims (n@D :* ns) = case patAsXDims ns of
  PatAsXDims xns -> PatAsXDims (Dn n :* xns)
{-# INLINE patAsXDims #-}



data PatKDims (ns :: [k])
  = (All KnownDim ns, Dimensions ns) => PatKDims


patKDims :: Dims ns -> PatKDims ns
patKDims U = PatKDims
patKDims (Dim :* ns) = case patKDims ns of
  PatKDims -> PatKDims
{-# INLINE patKDims #-}
