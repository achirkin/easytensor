{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)
{-# LANGUAGE StandaloneDeriving         #-}
#else
{-# OPTIONS_GHC -fplugin Data.Constraint.Deriving #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Idx
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Provides a data type `Idx` to index `Dim` and `Idxs`
--   that enumerates through multiple dimensions.
--
-- Higher indices go first, i.e. assumed enumeration
--          is i = i1*n1*n2*...*n(k-1) + ... + i(k-2)*n1*n2 + i(k-1)*n1 + ik
-- This corresponds to row-first layout of matrices and multidimenional arrays.
--
-- == Type safety
--
-- Same as `Dim` and `Dims`, `Idx` and `Idxs` defined in this module incorporate
-- two different indexing mechanics.
-- Both of them can be specified with exact @Nat@ values
--   (when @d :: Nat@ or @d ~ N n@),
-- or with lower bound values (i.e. @d ~ XN m@).
-- In the former case, the @Idx@/@Idxs@ type itself guarantees that the value
-- inside is within the @Dim@/@Dims@ bounds.
-- In the latter case, @Idx@/@Idxs@ can contain any values of type @Word@.
-- In other words:
--
--   * @(d :: Nat) || (d ~ N n) =>@ using @Idx d@ to index data is always safe,
--     but creating an index using unsafe functions can yield an `OutOfDimBounds`
--     exception at runtime.
--   * @(d ~ XN m) =>@ using @Idx d@ to index data can result in an `OutOfDimBounds`
--     exception, but you can safely manipulate the index itself
--     using familiar interfaces, such as @Enum@, @Num@, etc; as if @Idx d@
--     was a plain synonym to @Word@.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Idx
  ( -- * Data types
    Idx (Idx), Idxs
  , idxFromWord, idxToWord
  , listIdxs, idxsFromWords
  , liftIdxs, unliftIdxs, unsafeUnliftIdxs
  , TypedList ( XIdxs, U, (:*), Empty, Cons, Snoc, Reverse)
    -- * Checking the index bounds
  , OutOfDimBounds (..), outOfDimBounds, outOfDimBoundsNoCallStack
#if !defined(__HADDOCK__) && !defined(__HADDOCK_VERSION__)
  , xnatNInstEnumIdx, xnatXInstEnumIdx, incohInstEnumIdx
  , xnatNInstNumIdx, xnatXInstNumIdx, incohInstNumIdx
  , instRealIdx, instIntegralIdx
#endif
  ) where


import           Data.Coerce
import           Data.Data        (Data)
import           Foreign.Storable (Storable)
import           GHC.Enum
import           GHC.Generics     (Generic)
import qualified Text.Read        as P
import           Unsafe.Coerce

import GHC.Exception
import GHC.Stack
#ifdef UNSAFE_INDICES
import GHC.Base (Int (..), Type, Word (..), int2Word#, word2Int#)
#else
import GHC.Base (Int (..), Type, Word (..), int2Word#, maxInt, plusWord2#,
                 timesWord2#, word2Int#)
#endif

#if !defined(__HADDOCK__) && !defined(__HADDOCK_VERSION__)
import Data.Constraint
import Data.Constraint.Bare
import Data.Constraint.Deriving
#endif

import Numeric.Dimensions.Dim
import Numeric.TypedList      (typedListReadPrec, typedListShowsPrec)


{- | This type is used to index a single dimension.

  * @(k ~ Nat)  =>@ the range of indices is from @0@ to @d-1@.
  * @(d ~ N n)  =>@ the range of indices is from @0@ to @n-1@.
  * @(d ~ XN m) =>@ the range of indices is from @0@ to @maxBound :: Word@.

That is, using @Idx (n :: Nat)@ or @Idx (N n)@ is guaranteed to be safe by the
type system.
But an index of type @Idx (XN m)@ can have any value, and using it may yield
an `OutOfDimBounds` exception -- just the same as a generic @index@ function that
takes a plain @Int@ or @Word@ as an argument.
Thus, if you have data indexed by @(XN m)@, I would suggest to use @lookup@-like
functions that return @Maybe@. You're warned.

 -}
newtype Idx (d :: k) = Idx' Word
  deriving ( Data, Generic, Storable, Eq, Ord )


{- | Convert between `Word` and `Idx`.

Converting from `Idx` to `Word` is always safe.

Converting from `Word` to `Idx` generally is unsafe:

  * @(k ~ Nat)  =>@ if @w >= d@, it fails with an `OutOfDimBounds` exception.
  * @(d ~ N n)  =>@ if @w >= n@, it fails with an `OutOfDimBounds` exception.
  * @(d ~ XN m) =>@ the constructor always succeeds, but using the result for
      indexing may fail with an `OutOfDimBounds` exception later.

If @unsafeindices@ flag it turned on, this function always succeeds.
 -}
pattern Idx :: forall d . BoundedDim d => Word -> Idx d
pattern Idx w <- Idx' w
  where
    Idx = unsafeIdxFromWord
{-# COMPLETE Idx #-}

-- | Type-level dimensional indexing with arbitrary Word values inside.
--   Most of the operations on it require `Dimensions` or `BoundedDims` constraint,
--   because the @Idxs@ itself does not store info about dimension bounds.
type Idxs = (TypedList Idx :: [k] -> Type)


unsafeIdxFromWord :: forall (k :: Type) (d :: k) . BoundedDim d => Word -> Idx d
#ifdef UNSAFE_INDICES
unsafeIdxFromWord = coerce
#else
unsafeIdxFromWord w
  | DimTXNatX <- dimType @d
              = coerce w
  | w < d     = coerce w
  | otherwise = outOfDimBoundsNoCallStack "unsafeIdxFromWord" w d Nothing Nothing
  where
    d = dimVal (dimBound @d)
#endif
{-# INLINE unsafeIdxFromWord #-}

-- | Convert an arbitrary Word to @Idx@.
--   This is a safe alternative to the pattern @Idx@.
--
--   Note, when @(d ~ XN m)@, it returns @Nothing@ if @w >= m@.
--   Thus, the resulting index is always safe to use
--    (but you cannot index stuff beyond @DimBound d@ this way).
idxFromWord :: forall d . BoundedDim d => Word -> Maybe (Idx d)
idxFromWord w
  | w < dimVal (dimBound @d) = Just (coerce w)
  | otherwise                = Nothing
{-# INLINE idxFromWord #-}

-- | Get the value of an @Idx@.
idxToWord :: forall d . Idx d -> Word
idxToWord = coerce
{-# INLINE idxToWord #-}

{-# RULES
"fromIntegral/idxToWord"
  fromIntegral = idxToWord
  #-}

-- | /O(1)/ Convert @Idxs xs@ to a plain list of words.
listIdxs :: forall ds . Idxs ds -> [Word]
listIdxs = unsafeCoerce
{-# INLINE listIdxs #-}

-- | /O(n)/ Convert a plain list of words into an @Idxs@, while checking
--   the index bounds.
--
--   Same as with `idxFromWord`, it is always safe to use the resulting index,
--     but you cannot index stuff outside of the @DimsBound ds@ this way.
idxsFromWords :: forall ds . BoundedDims ds => [Word] -> Maybe (Idxs ds)
idxsFromWords = unsafeCoerce . go (listDims (dimsBound @ds))
  where
    go :: [Word] -> [Word] -> Maybe [Word]
    go [] [] = Just []
    go (d : ds) (i : is)
      | i < d = (i:) <$> go ds is
    go _ _   = Nothing


-- | Transform between @Nat@-indexed and @XNat@-indexed @Idxs@.
--
--   Note, this pattern is not a @COMPLETE@ match, because converting from @XNat@
--   to @Nat@ indexed @Idxs@ may fail (see `unliftIdxs`).
pattern XIdxs :: forall (ds :: [XNat]) (ns :: [Nat])
               . (FixedDims ds ns, Dimensions ns) => Idxs ns -> Idxs ds
pattern XIdxs ns <- (unliftIdxs -> Just ns)
  where
    XIdxs = liftIdxs

-- | @O(1)@ Coerce a @Nat@-indexed list of indices into a @XNat@-indexed one.
--   This function does not need any runtime checks and thus runs in constant time.
liftIdxs :: forall (ds :: [XNat]) (ns :: [Nat])
         . FixedDims ds ns => Idxs ns -> Idxs ds
liftIdxs = unsafeCoerce
{-# INLINE liftIdxs #-}

-- | @O(n)@ Coerce a @XNat@-indexed list of indices into a @Nat@-indexed one.
--   This function checks if an index is within Dim bounds for every dimension.
unliftIdxs :: forall (ds :: [XNat]) (ns :: [Nat])
            . (FixedDims ds ns, Dimensions ns) => Idxs ds -> Maybe (Idxs ns)
unliftIdxs U = Just U
unliftIdxs (Idx' i :* is)
  | d :* Dims <- dims @ns
  , i < dimVal d = (Idx' i :*) <$> unliftIdxs is
  | otherwise    = Nothing
{-# INLINE unliftIdxs #-}

-- | Coerce a @XNat@-indexed list of indices into a @Nat@-indexed one.
--   Can throw an `OutOfDimBounds` exception unless @unsafeindices@ flag is active.
unsafeUnliftIdxs :: forall (ds :: [XNat]) (ns :: [Nat])
                  . (FixedDims ds ns, Dimensions ns) => Idxs ds -> Idxs ns
#ifdef UNSAFE_INDICES
unsafeUnliftIdxs = unsafeCoerce
#else
unsafeUnliftIdxs is' = unsafeCoerce (zipWith f is ds)
  where
    f i d | i < d     = i
          | otherwise = err i d
    is = listIdxs is'
    ds = listDims (dims @ns)
    err i d = outOfDimBoundsNoCallStack
      "unsafeUnliftIdxs" i d Nothing (Just (ds, is))
#endif
{-# INLINE unsafeUnliftIdxs #-}


instance BoundedDim d => Read (Idx d) where
    readPrec = do
      w <- P.readPrec
      case dimType @d of
        DimTXNatX     -> return (Idx' w)
        _ | w < dimVal (dimBound @d)
                      -> return (Idx' w)
          | otherwise -> P.pfail
    readList = P.readListDefault
    readListPrec = P.readListPrecDefault

instance Show (Idx d) where
    showsPrec = coerce (showsPrec :: Int -> Word -> ShowS)

instance BoundedDim d => Bounded (Idx d) where
    minBound = coerce (0 :: Word)
    {-# INLINE minBound #-}
    {- | Note, @maxBound == Idx (dimVal (dimBound @d) - 1)@
            -- is defined in terms of @BoundedDim@.
         Thus, when @(d ~ XN m)@, your actual index may be larger than @maxBound@.
     -}
    maxBound = coerce (dimVal (dimBound @d) - 1)
    {-# INLINE maxBound #-}


instance KnownDim n => Enum (Idx (n :: Nat)) where

#ifdef UNSAFE_INDICES
    succ = coerce ((+ 1) :: Word -> Word)
#else
    succ x@(Idx' i)
      | x < maxBound = coerce (i + 1)
      | otherwise = outOfDimBoundsNoCallStack
         "Enum.succ{Idx}" (i + 1) (dimVal' @n) Nothing Nothing
#endif
    {-# INLINE succ #-}

#ifdef UNSAFE_INDICES
    pred = coerce (subtract 1 :: Word -> Word)
#else
    pred x@(Idx' i)
      | x > minBound = coerce (i - 1)
      | otherwise = outOfDimBoundsNoCallStack
         "Enum.pred{Idx}" (-1 :: Int) (dimVal' @n) Nothing Nothing
#endif
    {-# INLINE pred #-}

#ifdef UNSAFE_INDICES
    toEnum (I# i#) = coerce (W# (int2Word# i#))
#else
    toEnum i
        | i >= 0 && i' < d = coerce i'
        | otherwise        = outOfDimBoundsNoCallStack
           "Enum.toEnum{Idx}" i d Nothing Nothing
      where
        d  = dimVal' @n
        i' = fromIntegral i
#endif
    {-# INLINE toEnum #-}

#ifdef UNSAFE_INDICES
    fromEnum (Idx' (W# w#)) = I# (word2Int# w#)
#else
    fromEnum (Idx' x@(W# w#))
        | x <= maxIntWord = I# (word2Int# w#)
        | otherwise       = fromEnumError "Idx" x
        where
          maxIntWord = W# (case maxInt of I# i -> int2Word# i)
#endif
    {-# INLINE fromEnum #-}
    enumFrom (Idx' n) = coerce (enumFromTo n (dimVal' @n - 1))
    {-# INLINE enumFrom #-}
    enumFromThen (Idx' n0) (Idx' n1)
      = coerce (enumFromThenTo n0 n1 lim)
      where
        lim = if n1 >= n0 then maxBound else minBound
    {-# INLINE enumFromThen #-}
    enumFromTo
      = coerce (enumFromTo :: Word -> Word -> [Word])
    {-# INLINE enumFromTo #-}
    enumFromThenTo
      = coerce (enumFromThenTo :: Word -> Word -> Word -> [Word])
    {-# INLINE enumFromThenTo #-}


instance KnownDim n => Num (Idx (n :: Nat)) where

#ifdef UNSAFE_INDICES
    (+) = coerce ((+) :: Word -> Word -> Word)
#else
    (Idx' a@(W# a#)) + (Idx' b@(W# b#))
        | ovf || r >= d
          = outOfDimBoundsNoCallStack
              ("Num.(" ++ show a ++ " + " ++ show b ++ "){Idx}")
              (toInteger a + toInteger b) d Nothing Nothing
        | otherwise = coerce r
      where
        (ovf, r) = case plusWord2# a# b# of
          (# r2#, r1# #) -> ( W# r2# > 0 , W# r1# )
        d = dimVal' @n
#endif
    {-# INLINE (+) #-}

#ifdef UNSAFE_INDICES
    (-) = coerce ((-) :: Word -> Word -> Word)
#else
    (Idx' a) - (Idx' b)
        | b > a
          = outOfDimBoundsNoCallStack
              ("Num.(" ++ show a ++ " - " ++ show b ++ "){Idx}")
              (toInteger a - toInteger b) (dimVal' @n) Nothing Nothing
        | otherwise = coerce (a - b)
#endif
    {-# INLINE (-) #-}

#ifdef UNSAFE_INDICES
    (*) = coerce ((*) :: Word -> Word -> Word)
#else
    (Idx' a@(W# a#)) * (Idx' b@(W# b#))
        | ovf || r >= d
          = outOfDimBoundsNoCallStack
              ("Num.(" ++ show a ++ " * " ++ show b ++ "){Idx}")
              (toInteger a * toInteger b) d Nothing Nothing
        | otherwise = coerce r
      where
        (ovf, r) = case timesWord2# a# b# of
          (# r2#, r1# #) -> ( W# r2# > 0 , W# r1# )
        d = dimVal' @n
#endif
    {-# INLINE (*) #-}

#ifdef UNSAFE_INDICES
    negate = id
#else
    negate (Idx' 0) = Idx' 0
    negate (Idx' i) = outOfDimBoundsNoCallStack
        "Num.negate{Idx}" (- toInteger i) (dimVal' @n) Nothing Nothing
#endif
    {-# INLINE negate #-}
    abs = id
    {-# INLINE abs #-}
    signum = const (Idx' 1)
    {-# INLINE signum #-}

#ifdef UNSAFE_INDICES
    fromInteger = coerce (fromInteger :: Integer -> Word)
#else
    fromInteger i
      | i >= 0 && i < toInteger d
                  = Idx' (fromInteger i)
      | otherwise = outOfDimBoundsNoCallStack
          "Num.fromInteger{Idx}" i d Nothing Nothing
      where
        d =  dimVal' @n
#endif
    {-# INLINE fromInteger #-}



#if defined(__HADDOCK__) || defined(__HADDOCK_VERSION__)

{- |
Although @Enum (Idx d)@ requires @BoundedDim d@, it does not use @maxBound@
when @(d ~ XN m)@.
You can use list comprehensions safely for known dims
(@(k ~ Nat)@ or @(d ~ N d)@),
but you may get an index larger than your struct to be indexed when @d ~ XN m@.
 -}
deriving instance BoundedDim d => Enum (Idx d)
deriving instance BoundedDim d => Integral (Idx d)
deriving instance BoundedDim d => Real (Idx d)
{- |
Although @Num (Idx d)@ requires @BoundedDim d@, it does not use @maxBound@
when @(d ~ XN m)@.
That is, if @(d ~ XN m)@ then @i = fromIntegral n@ always succeeds.
 -}
deriving instance BoundedDim d => Num (Idx d)

#else

{-# ANN xnatNInstEnumIdx (ToInstance NoOverlap) #-}
xnatNInstEnumIdx ::
       forall (n :: Nat)
     . KnownDim n => Dict (Enum (Idx (N n)))
xnatNInstEnumIdx = unsafeCoerce (Dict @(Enum (Idx n)))

{-# ANN xnatXInstEnumIdx (ToInstance NoOverlap) #-}
xnatXInstEnumIdx ::
       forall (m :: Nat)
     . Dict (Enum (Idx (XN m)))
xnatXInstEnumIdx = unsafeCoerce (Dict @(Enum Word))

{-# ANN incohInstEnumIdx (ToInstance Incoherent) #-}
incohInstEnumIdx ::
       forall (k :: Type) (d :: k)
     . BoundedDim d => Dict (Enum (Idx d))
incohInstEnumIdx = case dimType @d of
  DimTNat   -> Dict
  DimTXNatN -> xnatNInstEnumIdx
  DimTXNatX -> xnatXInstEnumIdx

{-# ANN xnatNInstNumIdx (ToInstance NoOverlap) #-}
xnatNInstNumIdx ::
       forall (n :: Nat)
     . KnownDim n => Dict (Num (Idx (N n)))
xnatNInstNumIdx = unsafeCoerce (Dict @(Num (Idx n)))

{-# ANN xnatXInstNumIdx (ToInstance NoOverlap) #-}
xnatXInstNumIdx ::
       forall (m :: Nat)
     . Dict (Num (Idx (XN m)))
xnatXInstNumIdx = unsafeCoerce (Dict @(Num Word))

{-# ANN incohInstNumIdx (ToInstance Incoherent) #-}
incohInstNumIdx ::
       forall (k :: Type) (d :: k)
     . BoundedDim d => Dict (Num (Idx d))
incohInstNumIdx = case dimType @d of
  DimTNat   -> Dict
  DimTXNatN -> xnatNInstNumIdx
  DimTXNatX -> xnatXInstNumIdx

{-# ANN defineReal ClassDict #-}
defineReal ::
       forall a
     . (Num a, Ord a)
    => (a -> Rational) -- toRational
    -> Dict (Real a)
defineReal = defineReal

{-# ANN instRealIdx (ToInstance NoOverlap) #-}
instRealIdx ::
       forall (k :: Type) (d :: k)
     . BoundedDim d => Dict (Real (Idx d))
instRealIdx
  = withBareConstraint (dictToBare (incohInstNumIdx @k @d))
  $ defineReal (coerce (toRational @Word))

{-# ANN defineIntegral ClassDict #-}
defineIntegral ::
       forall a
     . (Real a, Enum a)
    => (a -> a -> a) -- quot
    -> (a -> a -> a) -- rem
    -> (a -> a -> a) -- div
    -> (a -> a -> a) -- mod
    -> (a -> a -> (a,a)) -- quotRem
    -> (a -> a -> (a,a)) -- divMod
    -> (a -> Integer) -- toInteger
    -> Dict (Integral a)
defineIntegral = defineIntegral

{-# ANN instIntegralIdx (ToInstance NoOverlap) #-}
instIntegralIdx ::
       forall (k :: Type) (d :: k)
     . BoundedDim d => Dict (Integral (Idx d))
instIntegralIdx
  = withBareConstraint (dictToBare (instRealIdx @k @d))
  $ withBareConstraint (dictToBare (incohInstEnumIdx @k @d))
  $ defineIntegral
    (coerce (quot @Word)) (coerce (rem @Word))
    (coerce (div @Word))  (coerce (mod @Word))
    (coerce (quotRem @Word)) (coerce (divMod @Word))
    (coerce (toInteger @Word))


#endif

instance Eq (Idxs (xs :: [k])) where
    (==) = unsafeCoerce ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}

-- | Compare indices by their importance in lexicorgaphic order
--   from the first dimension to the last dimension
--   (the first dimension is the most significant one).
--
--   Literally,
--
--   > compare a b = compare (listIdxs a) (listIdxs b)
--
--   This is the same @compare@ rule, as for `Dims`.
--   This is also consistent with offsets:
--
--   > sort == sortOn fromEnum
--
instance Ord (Idxs (xs :: [k])) where
    compare = unsafeCoerce (compare :: [Word] -> [Word] -> Ordering)
    {-# INLINE compare #-}

instance Show (Idxs (xs :: [k])) where
    showsPrec = typedListShowsPrec @Idx @xs showsPrec

instance BoundedDims xs => Read (Idxs (xs :: [k])) where
    readPrec = typedListReadPrec @BoundedDim ":*" P.readPrec (tList @xs)
    readList = P.readListDefault
    readListPrec = P.readListPrecDefault

instance BoundedDims ds => Bounded (Idxs (ds :: [k])) where
    maxBound = f (minimalDims @ds)
      where
        f :: forall (ns :: [k]) . Dims ns -> Idxs ns
        f U         = U
        f (d :* ds) = coerce (dimVal d - 1) :* f ds
    {-# INLINE maxBound #-}
    minBound = f (minimalDims @ds)
      where
        f :: forall (ns :: [k]) . Dims ns -> Idxs ns
        f U         = U
        f (_ :* ds) = Idx' 0 :* f ds
    {-# INLINE minBound #-}

{- |
@ds@ must be fixed (either @[Nat]@ or all (N n)) to know exact bounds in
each dimension.
 -}
instance Dimensions ds => Enum (Idxs ds) where

    succ idx = case go dds idx of
        (True , _ ) -> succError $ showIdxsType dds
        (False, i') -> i'
      where
        dds = dims @ds
        go :: forall ns . Dims ns -> Idxs ns -> (Bool, Idxs ns)
        go U U = (True, U)
        go (d :* ds) (Idx' i :* is) = case go ds is of
          (True , is')
            | i + 1 == dimVal d -> (True , Idx'  0    :* is')
            | otherwise         -> (False, Idx' (i+1) :* is')
          (False, is')          -> (False, Idx'  i    :* is')
    {-# INLINE succ #-}

    pred idx = case go dds idx of
        (True , _ ) -> predError $ showIdxsType dds
        (False, i') -> i'
      where
        dds = dims @ds
        go :: forall ns . Dims ns -> Idxs ns -> (Bool, Idxs ns)
        go U U = (True, U)
        go (d :* ds) (Idx' i :* is) = case go ds is of
          (True , is')
            | i == 0    -> (True , Idx' (dimVal d - 1) :* is')
            | otherwise -> (False, Idx' (i-1)          :* is')
          (False, is')  -> (False, Idx'  i             :* is')
    {-# INLINE pred #-}

    toEnum off0 = case go dds of
        (0, i) -> i
        _      -> toEnumError (showIdxsType dds) off0 (0, totalDim dds - 1)
      where
        dds = dims @ds
        go :: forall ns . Dims ns -> (Word, Idxs ns)
        go  U = (fromIntegral off0, U)
        go (d :* ds)
          | (off , is) <- go ds
          , (off', i ) <- quotRem off (dimVal d)
              = (off', Idx' i :* is)
    {-# INLINE toEnum #-}

    fromEnum = fromIntegral . snd
             . foldr f (1, 0)
             . zip (listDims $ dims @ds) . listIdxs
      where
        f :: (Word, Word) -> (Word, Word) -> (Word, Word)
        f (d, i) (td, off) = (d * td, off + td * i)
    {-# INLINE fromEnum #-}

    enumFrom = unsafeCoerce go True (dims @ds)
      where
        go :: Bool -> [Word] -> [Word] -> [[Word]]
        go b (d:ds) (i:is) =
          [ i' : is' | (b', i') <- zip (b : repeat False)
                                     $ enumFromTo (if b then i else 0) (d - 1)
                     , is' <- go b' ds is ]
        go _ _ _  = [[]]
    {-# INLINE enumFrom #-}

    enumFromTo = unsafeCoerce go True True (dims @ds)
      where
        go :: Bool -> Bool -> [Word] -> [Word] -> [Word] -> [[Word]]
        go bl bu (d:ds) (x:xs) (y:ys) =
          [ i : is | (bl', bu', i) <- prepapp bl bu
                                    $ enumFromTo (if bl then x else 0)
                                                 (if bu then y else d - 1)
                   , is <- go bl' bu' ds xs ys ]
        go _ _ _ _ _ = [[]]
        prepapp _  _  []     = []
        prepapp bl bu [i]    = [(bl, bu, i)]
        prepapp bl bu (i:is) = (bl, False, i :: Word) : app bu is
        app _  []     = []
        app bu [i]    = [(False, bu, i :: Word)]
        app bu (i:is) = (False, False, i) : app bu is
    {-# INLINE enumFromTo #-}

    enumFromThen x0 x1 = case compare x1 x0 of
        EQ -> repeat x0
        GT -> enumFromThenTo x0 x1 $ maxB ds
        LT -> enumFromThenTo x0 x1 $ minB ds
      where
        ds = dims @ds
        maxB :: forall ns . Dims ns -> Idxs ns
        maxB U         = U
        maxB (x :* xs) = coerce (dimVal x - 1) :* maxB xs
        minB :: forall ns . Dims ns -> Idxs ns
        minB U         = U
        minB (_ :* xs) = Idx' 0 :* minB xs
    {-# INLINE enumFromThen #-}

    enumFromThenTo x0 x1 y = case dir of
        EQ -> if allYs >= allX0s then repeat x0 else []
        GT -> let (_, allDXs) = idxMinus allDs allX0s allX1s
                  repeatStep is
                    = if is <= allYs
                      then is : case idxPlus allDs is allDXs of
                        (0, is') -> repeatStep is'
                        _        -> []
                      else []
              in unsafeCoerce (repeatStep allX0s)
        LT -> let (_, allDXs) = idxMinus allDs allX1s allX0s
                  repeatStep is
                    = if is >= allYs
                      then is : case idxMinus allDs allDXs is of
                        (0, is') -> repeatStep is'
                        _        -> []
                      else []
              in unsafeCoerce (repeatStep allX0s)
      where
        allDs  = listDims $ dims @ds
        allX0s = listIdxs x0
        allX1s = listIdxs x1
        allYs  = listIdxs y
        dir    = compare allX1s allX0s -- succ or pred?
        -- second arg minus first arg
        idxMinus :: [Word] -> [Word] -> [Word] -> (Word, [Word])
        idxMinus (d:ds) (a:as) (b:bs)
          = let (one , xs ) = idxMinus ds as bs
                (one', x  ) = quotRem (d + b - a - one) d
            in  (1 - one', x : xs)
        idxMinus _ _ _ = (0, [])
        idxPlus :: [Word] -> [Word] -> [Word] -> (Word, [Word])
        idxPlus (d:ds) (a:as) (b:bs)
          = let (one , xs ) = idxPlus ds as bs
                (one', x  ) = quotRem (a + b + one) d
            in  (one', x : xs)
        idxPlus _ _ _ = (0, [])
    {-# INLINE enumFromThenTo #-}


-- | Show type of Idxs (for displaying nice errors).
showIdxsType :: Dims ns -> String
showIdxsType ds = "Idxs '" ++ show (listDims ds)

-- | Throw an `OutOfDimBounds` exception without the CallStack attached.
outOfDimBoundsNoCallStack ::
       Integral i
    => String  -- ^ Label (e.g. function name)
    -> i       -- ^ Bad index
    -> Word    -- ^ Target dim
    -> Maybe Word -- ^ SubSpace Dim, if applicable.
    -> Maybe ([Word], [Word]) -- ^ Larger picture: Dims and Idxs
    -> a
outOfDimBoundsNoCallStack s i d msubd dimsCtx
  = throw OutOfDimBounds
  { oodIdx       = toInteger i
  , oodDim       = d
  , oodSubDim    = msubd
  , oodDimsCtx   = dimsCtx
  , oodName      = s
  , oodCallStack = Nothing
  }

-- | Throw an `OutOfDimBounds` exception.
outOfDimBounds ::
       (HasCallStack, Integral i)
    => String  -- ^ Label (e.g. function name)
    -> i       -- ^ Bad index
    -> Word    -- ^ Target dim
    -> Maybe Word -- ^ SubSpace Dim, if applicable.
    -> Maybe ([Word], [Word]) -- ^ Larger picture: Dims and Idxs
    -> a
outOfDimBounds s i d msubd dimsCtx
  = throw OutOfDimBounds
  { oodIdx       = toInteger i
  , oodDim       = d
  , oodSubDim    = msubd
  , oodDimsCtx   = dimsCtx
  , oodName      = s
  , oodCallStack = Just callStack
  }

{- |
Typically, this exception can occur in the following cases:

  * Converting from integral values to @Idx d@ when @d ~ N n@ or @d :: Nat@.
  * Using @Enum@ and @Num@ when @d ~ N n@ or @d :: Nat@.
  * Converting from @Idx (XN m :: XNat)@ to @Idx (n :: Nat)@.
  * Indexing or slicing data using  @Idx (XN m :: XNat)@.

If you are mad and want to avoid any overhead related to bounds checking and the
related error handling, you can turn on the @unsafeindices@ flag to remove all of
this from the library at once.
 -}
data OutOfDimBounds
  = OutOfDimBounds
  { oodIdx       :: Integer
    -- ^ A value that should have been a valid `Idx`
  , oodDim       :: Word
    -- ^ A runtime value of a `Dim`
  , oodSubDim    :: Maybe Word
    -- ^ When used for slicing, this should have satisfied
    --   @oodIdx + oodSubDim <= oodDim@.
  , oodDimsCtx   :: Maybe ([Word], [Word])
    -- ^ If available, contains (Dims xns, Idxs xns).
  , oodName      :: String
    -- ^ Short location of the error description, typically a function name.
  , oodCallStack :: Maybe CallStack
    -- ^ Function call stack, if available.
    --   Note, this field is ignored in the `Eq` and `Ord` instances.
  }

-- | Note, this instance ignores `oodCallStack`
instance Eq OutOfDimBounds where
  (==) a b = and
    [ (==) (oodIdx a) (oodIdx b)
    , (==) (oodDim a) (oodDim b)
    , (==) (oodSubDim a) (oodSubDim b)
    , (==) (oodDimsCtx a) (oodDimsCtx b)
    , (==) (oodName a) (oodName b)
    ]

-- | Note, this instance ignores `oodCallStack`
instance Ord OutOfDimBounds where
  compare a b = mconcat
    [ compare (oodIdx a) (oodIdx b)
    , compare (oodDim a) (oodDim b)
    , compare (oodSubDim a) (oodSubDim b)
    , compare (oodDimsCtx a) (oodDimsCtx b)
    , compare (oodName a) (oodName b)
    ]

instance Show OutOfDimBounds where
  showsPrec p e = addLoc errStr
    where
      addLoc s
        = let someE = case oodCallStack e of
                Nothing -> errorCallException s
                Just st -> errorCallWithCallStackException s st
              errc :: ErrorCall
              errc = case fromException someE of
                Nothing -> ErrorCall s
                Just ec -> ec
          in  showsPrec p errc
      errStr = oodName e ++ ": " ++ errContent ++ errCtx
      errContent = case oodSubDim e of
        Nothing -> "index " ++ show (oodIdx e) ++
                   " is outside of Dim bounds (0 <= i < " ++ show (oodDim e) ++ ")"
        Just sd -> "index " ++ show (oodIdx e) ++
                   " and subspace dim " ++ show sd ++
                   " together exceed the original space dim " ++ show (oodDim e)
      errCtx = case oodDimsCtx e of
        Nothing -> "."
        Just (ds, is)
            -> ";\n  dims: " ++ (case someDimsVal ds of SomeDims x -> show x)
            ++  "\n  idxs: " ++ show (unsafeCoerce is :: Idxs ns)

instance Exception OutOfDimBounds
