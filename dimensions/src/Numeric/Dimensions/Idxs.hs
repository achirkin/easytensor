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
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeApplications           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Idxs
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Provides a data type Idx that enumerates through multiple dimensions.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Idxs
  ( -- * Data types
    Idx (..), Idxs
  , idxFromWord, unsafeIdxFromWord, idxToWord
  , listIdxs, idxsFromWords
    -- * Re-export dimensions types
  , module Numeric.Dimensions.Dims
  ) where


import           Control.Arrow           (first)
import           Data.Data               (Data)
import           Foreign.Storable        (Storable)
import           GHC.Base
import           GHC.Enum
import           GHC.Generics            (Generic, Generic1)

import           Numeric.Dimensions.Dims


-- | This type is used to index a single dimension;
--   the range of indices is from @0@ to @n-1@.
--
newtype Idx n = Idx { unIdx :: Word }
  deriving ( Data, Generic, Generic1, Integral, Real, Storable, Eq, Ord )

instance Read (Idx n) where
    readsPrec d = fmap (first Idx) . readsPrec d

instance Show (Idx n) where
    showsPrec d = showsPrec d . unIdx


instance KnownDim n => Bounded (Idx n) where
    minBound = 0
    {-# INLINE minBound #-}
    maxBound = Idx (unsafeCoerce# (dim @_ @n)  - 1)
    {-# INLINE maxBound #-}

instance KnownDim n => Enum (Idx n) where

#ifdef UNSAFE_INDICES
    succ = unsafeCoerce# ((+ 1) :: Word -> Word)
#else
    succ x@(Idx i)
      | x /= maxBound = Idx (i + 1)
      | otherwise = succError $ "Idx " ++ show (dim @_ @n)
#endif
    {-# INLINE succ #-}

#ifdef UNSAFE_INDICES
    pred = unsafeCoerce# ((+ (-1)) :: Word -> Word)
#else
    pred x@(Idx i)
      | x /= maxBound = Idx (i + 1)
      | otherwise = predError $ "Idx " ++ show (dim @_ @n)
#endif
    {-# INLINE pred #-}

#ifdef UNSAFE_INDICES
    toEnum (I# i#) = unsafeCoerce# (W# (int2Word# i#))
#else
    toEnum i@(I# i#)
        | i >= 0 && i < d = unsafeCoerce# (W# (int2Word# i#))
        | otherwise        = toEnumError ("Idx " ++ show d) i (0, d - 1)
      where
        d = fromIntegral (unsafeCoerce# (dim @_ @n) :: Word)
#endif
    {-# INLINE toEnum #-}

#ifdef UNSAFE_INDICES
    fromEnum (Idx (W# w#)) = I# (word2Int# w#)
#else
    fromEnum (Idx x@(W# w#))
        | x <= maxIntWord = I# (word2Int# w#)
        | otherwise       = fromEnumError ("Idx " ++ show (dim @_ @n)) x
        where
          maxIntWord = W# (case maxInt of I# i -> int2Word# i)
#endif
    {-# INLINE fromEnum #-}

    enumFrom (Idx n)
      = unsafeCoerce# (enumFromTo n (unsafeCoerce# (dim @_ @n) - 1))
    {-# INLINE enumFrom #-}
    enumFromThen (Idx n0) (Idx n1)
      = case compare n0 n1 of
          LT -> unsafeCoerce# (enumFromThenTo n0 n1 (unsafeCoerce# (dim @_ @n) - 1))
          EQ -> unsafeCoerce# (repeat n0)
          GT -> unsafeCoerce# (enumFromThenTo n0 n1 0)
    {-# INLINE enumFromThen #-}
    enumFromTo
      = unsafeCoerce# (enumFromTo :: Word -> Word -> [Word])
    {-# INLINE enumFromTo #-}
    enumFromThenTo
      = unsafeCoerce# (enumFromThenTo :: Word -> Word -> Word -> [Word])
    {-# INLINE enumFromThenTo #-}

instance KnownDim n => Num (Idx n) where

#ifdef UNSAFE_INDICES
    (+) = unsafeCoerce# ((+) :: Word -> Word -> Word)
#else
    (Idx a) + (Idx b)
        | r >= d || r < a || r < b
          = errorWithoutStackTrace
          $ "Num.(+){Idx " ++ show d ++ "}: sum of "
            ++ show a ++ " and " ++ show b
            ++ " is outside of index bounds."
        | otherwise = Idx r
      where
        r = a + b
        d = unsafeCoerce# (dim @_ @n)
#endif
    {-# INLINE (+) #-}

#ifdef UNSAFE_INDICES
    (-) = unsafeCoerce# ((-) :: Word -> Word -> Word)
#else
    (Idx a) - (Idx b)
        | b > a
          = errorWithoutStackTrace
          $ "Num.(-){Idx " ++ show (dim @_ @n) ++ "}: difference of "
            ++ show a ++ " and " ++ show b
            ++ " is negative."
        | otherwise = Idx (a - b)
#endif
    {-# INLINE (-) #-}

#ifdef UNSAFE_INDICES
    (*) = unsafeCoerce# ((*) :: Word -> Word -> Word)
#else
    (Idx 0) * (Idx _) = 0
    (Idx _) * (Idx 0) = 0
    (Idx a) * (Idx b)
        | r >= d || r < a || r < b
          = errorWithoutStackTrace
          $ "Num.(*){Idx " ++ show d ++ "}: product of "
            ++ show a ++ " and " ++ show b
            ++ " is outside of index bounds."
        | otherwise = Idx r
      where
        r = a * b
        d = unsafeCoerce# (dim @_ @n)
#endif
    {-# INLINE (*) #-}

    negate = errorWithoutStackTrace
           $ "Num.(*){Idx " ++ show (dim @_ @n) ++ "}: cannot negate index."
    {-# INLINE negate #-}
    abs = id
    {-# INLINE abs #-}
    signum _ = Idx 1
    {-# INLINE signum #-}

#ifdef UNSAFE_INDICES
    fromInteger = unsafeCoerce# (fromInteger :: Integer -> Word)
#else
    fromInteger i
      | i >= 0 && i < d = Idx $ fromInteger i
      | otherwise       = errorWithoutStackTrace
                        $ "Num.fromInteger{Idx "
                        ++ show d ++ "}: integer "
                        ++ show i ++ " is outside of index bounds."
      where
        d = toInteger (unsafeCoerce# (dim @_ @n) :: Word)
#endif
    {-# INLINE fromInteger #-}


unsafeIdxFromWord :: forall d . KnownDim d => Word -> Idx d
#ifdef UNSAFE_INDICES
unsafeIdxFromWord = unsafeCoerce#
#else
unsafeIdxFromWord w
  | w < d     = Idx w
  | otherwise = errorWithoutStackTrace
              $ "idxFromWord{Idx "
              ++ show d ++ "}: word "
              ++ show w ++ " is outside of index bounds."
  where
    d = unsafeCoerce# (dim @_ @d)
#endif
{-# INLINE unsafeIdxFromWord #-}

idxFromWord :: forall d . KnownDim d => Word -> Maybe (Idx d)
idxFromWord w
  | w < unsafeCoerce# (dim @_ @d) = Just (Idx w)
  | otherwise                     = Nothing
{-# INLINE idxFromWord #-}


idxToWord :: Idx d -> Word
idxToWord = unsafeCoerce#
{-# INLINE idxToWord #-}

{-# RULES
"fromIntegral/idxToWord"
  fromIntegral = idxToWord
  #-}


-- | Type-level dimensional indexing with arbitrary Word values inside.
--   Most of the operations on it require `Dimensions` constraint,
--   because the @Idxs@ itself does not store info about dimension bounds.
type Idxs (xs :: [k]) = TypedList Idx xs


listIdxs :: Idxs xs -> [Word]
listIdxs = unsafeCoerce#
{-# INLINE listIdxs #-}


idxsFromWords :: forall ds . Dimensions ds => [Word] -> Maybe (Idx ds)
idxsFromWords = unsafeCoerce# . go (listDims (dims @_ @ds))
  where
    go :: [Word] -> [Word] -> Maybe [Word]
    go [] [] = Just []
    go (d : ds) (i : is)
      | i < d = (i:) <$> go ds is
    go _ _   = Nothing



instance Eq (Idxs xs) where
    (==) = unsafeCoerce# ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}

-- | Compare indices by their importance in lexicorgaphic order
--   from the last dimension to the first dimension
--   (the last dimension is the most significant one) @O(Length xs)@.
--
--   Literally,
--
--   > compare a b = compare (reverse $ listIdxs a) (reverse $ listIdxs b)
--
--   This is the same @compare@ rule, as for `Dims`.
--   Another reason to reverse the list of indices is to have a consistent
--   behavior when calculating index offsets:
--
--   > sort == sortOn fromEnum
--
instance Ord (Idxs xs) where
    compare a b = compare (reverse $ listIdxs a) (reverse $ listIdxs b)
    {-# INLINE compare #-}


instance Show (Idxs xs) where
    show ds = "Idxs " ++ show (listIdxs ds)
    showsPrec p ds
      = showParen (p >= 10)
      $ showString "Idxs " . showsPrec p (listIdxs ds)

-- | With this instance we can slightly reduce indexing expressions, e.g.
--
--   > x ! (1 :* 2 :* 4) == x ! (1 :* 2 :* 4 :* U)
--
instance KnownDim n => Num (Idxs '[n]) where
    (a:*U) + (b:*U) = (a+b) :* U
    {-# INLINE (+) #-}
    (a:*U) - (b:*U) = (a-b) :* U
    {-# INLINE (-) #-}
    (a:*U) * (b:*U) = (a*b) :* U
    {-# INLINE (*) #-}
    signum (a:*U)   = signum a :* U
    {-# INLINE signum #-}
    abs (a:*U)      = abs a :* U
    {-# INLINE abs #-}
    fromInteger i   = fromInteger i :* U
    {-# INLINE fromInteger #-}

instance Dimensions ds => Bounded (Idxs ds) where
    maxBound = f (dims @_ @ds)
      where
        f :: forall ns . Dims ns -> Idxs ns
        f U         = U
        f (d :* ds) = Idx (dimVal d - 1) :* f ds
    {-# INLINE maxBound #-}
    minBound = f (dims @_ @ds)
      where
        f :: forall ns . Dims ns -> Idxs ns
        f U         = U
        f (_ :* ds) = Idx 0 :* f ds
    {-# INLINE minBound #-}


instance Dimensions ds => Enum (Idxs ds) where

    succ = go (dims @_ @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go U U = succError $ "Idxs " ++ show (listDims $ dims @_ @ds)
        go (d :* ds) (Idx i :* is)
          | i + 1 == dimVal d = Idx 0 :* go ds is
          | otherwise         = Idx (i+1) :* is
    {-# INLINE succ #-}

    pred = go (dims @_ @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go U U = predError $ "Idxs " ++ show (listDims $ dims @_ @ds)
        go (d :* ds) (Idx i :* is)
          | i == 0    = Idx (dimVal d - 1) :* go ds is
          | otherwise = Idx (i-1) :* is
    {-# INLINE pred #-}

    toEnum i = go dds $ fromIntegral i
      where
        dds = dims @_ @ds
        go :: forall ns . Dims ns -> Word -> Idxs ns
        go U 0 = U
        go U _ = toEnumError ("Idxs " ++ show (listDims dds))
                             i (0, totalDim dds - 1)
        go (d :* ds) off = case divMod off (dimVal d) of
          (off', j) -> Idx j :* go ds off'
    {-# INLINE toEnum #-}

    fromEnum = fromIntegral . go 0 (dims @_ @ds)
      where
        go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
        go _ U U                     = 0
        go m (d :* ds) (Idx i :* is) = m * i + go (m * dimVal d) ds is
    {-# INLINE fromEnum #-}

    enumFrom x = take (diffIdx (dims @_ @ds) maxBound x + 1) $ iterate succ x
    {-# INLINE enumFrom #-}

    enumFromTo x y | x >= y    = take (diffIdx ds x y + 1) $ iterate pred x
                   | otherwise = take (diffIdx ds y x + 1) $ iterate succ x
      where
        ds = dims @_ @ds
    {-# INLINE enumFromTo #-}

    enumFromThen x x' = take n $ iterate (stepIdx ds dn) x
      where
        ds = dims @_ @ds
        dn = diffIdx ds x' x
        n  = 1 + if dn == 0
                 then 0
                 else if dn > 0
                      then diffIdx ds maxBound x `div` dn
                      else diffIdx ds x minBound `div` negate dn
    {-# INLINE enumFromThen #-}

    enumFromThenTo x x' y = take n $ iterate (stepIdx ds dn) x
      where
        ds = dims @_ @ds
        dn = diffIdx ds x' x
        n  = 1 + if dn == 0 then 0
                            else diffIdx ds y x `div` dn
    {-# INLINE enumFromThenTo #-}



--------------------------------------------------------------------------------



-- | Offset difference of two indices @idx1 - idx2@
diffIdx :: Dims xs -> Idxs xs -> Idxs xs -> Int
diffIdx U U U = 0
diffIdx (d :* ds) (Idx i1 :* is1) (Idx i2 :* is2)
  = fromIntegral i1 - fromIntegral i2
  + fromIntegral (dimVal d) * diffIdx ds is1 is2
{-# INLINE diffIdx #-}

-- | Step dimension index by an Int offset
stepIdx :: Dims ds -> Int -> Idxs ds -> Idxs ds
stepIdx U _ U = U
stepIdx (d :* ds) di (Idx i :* is)
      = case divMod (di + fromIntegral i) (fromIntegral (dimVal d)) of
         (0  , i') -> Idx (fromIntegral i') :* is
         (di', i') -> Idx (fromIntegral i') :* stepIdx ds di' is
{-# INLINE stepIdx #-}
