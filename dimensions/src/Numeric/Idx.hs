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
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Idx
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-- Provides a data type `Idx` to index `Dim`.
--
-----------------------------------------------------------------------------

module Numeric.Idx
  ( -- * Data types
    Idx (..)
  , idxFromWord, unsafeIdxFromWord, idxToWord
    -- * Re-export @Dim@ type
  , module Numeric.Dim
  ) where


import           Data.Coerce
import           Data.Data        (Data)
import           Foreign.Storable (Storable)
import           GHC.Base         (Int (..), Type, Word (..), int2Word#,
                                   word2Int#)
import           GHC.Generics     (Generic)
import qualified Text.Read        as P

#ifndef UNSAFE_INDICES
import GHC.Base (maxInt, plusWord2#, timesWord2#)
import GHC.Enum
#endif

import Numeric.Dim


-- | This type is used to index a single dimension;
--   the range of indices is from @0@ to @n-1@.
--
newtype Idx (n :: k) = Idx { unIdx :: Word }
  deriving ( Data, Generic, Integral, Real, Storable, Eq, Ord )

instance BoundedDim x => Read (Idx (x :: k)) where
    readPrec = do
      w <- P.readPrec
      if w < dimVal (dimBound @k @x)
      then return (Idx w)
      else P.pfail
    readList = P.readListDefault
    readListPrec = P.readListPrecDefault

instance Show (Idx (x :: k)) where
    showsPrec d = showsPrec d . unIdx

instance BoundedDim n => Bounded (Idx (n :: k)) where
    minBound = 0
    {-# INLINE minBound #-}
    maxBound = coerce (dimVal(dimBound @k @n)  - 1)
    {-# INLINE maxBound #-}

instance BoundedDim n => Enum (Idx (n :: k)) where

#ifdef UNSAFE_INDICES
    succ = coerce ((+ 1) :: Word -> Word)
#else
    succ x@(Idx i)
      | x < maxBound = Idx (i + 1)
      | otherwise = succError $ showIdxType @k @n
#endif
    {-# INLINE succ #-}

#ifdef UNSAFE_INDICES
    pred = coerce (subtract 1 :: Word -> Word)
#else
    pred x@(Idx i)
      | x > minBound = Idx (i - 1)
      | otherwise = predError $ showIdxType @k @n
#endif
    {-# INLINE pred #-}

#ifdef UNSAFE_INDICES
    toEnum (I# i#) = coerce (W# (int2Word# i#))
#else
    toEnum i
        | i >= 0 && i' < d = coerce i'
        | otherwise        = toEnumError (showIdxType @k @n) i (0, d - 1)
      where
        d  = dimVal (dimBound @k @n)
        i' = fromIntegral i
#endif
    {-# INLINE toEnum #-}

#ifdef UNSAFE_INDICES
    fromEnum (Idx (W# w#)) = I# (word2Int# w#)
#else
    fromEnum (Idx x@(W# w#))
        | x <= maxIntWord = I# (word2Int# w#)
        | otherwise       = fromEnumError (showIdxType @k @n) x
        where
          maxIntWord = W# (case maxInt of I# i -> int2Word# i)
#endif
    {-# INLINE fromEnum #-}

    enumFrom (Idx n)
      = coerce (enumFromTo n (dimVal (dimBound @k @n) - 1))
    {-# INLINE enumFrom #-}
    enumFromThen (Idx n0) (Idx n1)
      = coerce (enumFromThenTo n0 n1 lim)
      where
        lim = if n1 >= n0 then dimVal (dimBound @k @n) - 1 else 0
    {-# INLINE enumFromThen #-}
    enumFromTo
      = coerce (enumFromTo :: Word -> Word -> [Word])
    {-# INLINE enumFromTo #-}
    enumFromThenTo
      = coerce (enumFromThenTo :: Word -> Word -> Word -> [Word])
    {-# INLINE enumFromThenTo #-}

instance BoundedDim n => Num (Idx (n :: k)) where

#ifdef UNSAFE_INDICES
    (+) = coerce ((+) :: Word -> Word -> Word)
#else
    (Idx a@(W# a#)) + b@(Idx (W# b#))
        | ovf || r >= d
          = errorWithoutStackTrace
          $ "Num.(+){" ++ showIdxType @k @n ++ "}: sum of "
            ++ show a ++ " and " ++ show b
            ++ " is outside of index bounds."
        | otherwise = Idx r
      where
        (ovf, r) = case plusWord2# a# b# of
          (# r2#, r1# #) -> ( W# r2# > 0 , W# r1# )
        d = dimVal (dimBound @k @n)
#endif
    {-# INLINE (+) #-}

#ifdef UNSAFE_INDICES
    (-) = coerce ((-) :: Word -> Word -> Word)
#else
    (Idx a) - (Idx b)
        | b > a
          = errorWithoutStackTrace
          $ "Num.(-){" ++ showIdxType @k @n ++ "}: difference of "
            ++ show a ++ " and " ++ show b
            ++ " is negative."
        | otherwise = Idx (a - b)
#endif
    {-# INLINE (-) #-}

#ifdef UNSAFE_INDICES
    (*) = coerce ((*) :: Word -> Word -> Word)
#else
    (Idx a@(W# a#)) * b@(Idx (W# b#))
        | ovf || r >= d
          = errorWithoutStackTrace
          $ "Num.(*){" ++ showIdxType @k @n ++ "}: product of "
            ++ show a ++ " and " ++ show b
            ++ " is outside of index bounds."
        | otherwise = Idx r
      where
        (ovf, r) = case timesWord2# a# b# of
          (# r2#, r1# #) -> ( W# r2# > 0 , W# r1# )
        d = dimVal (dimBound @k @n)
#endif
    {-# INLINE (*) #-}

    negate = errorWithoutStackTrace
           $ "Num.(*){" ++ showIdxType @k @n ++ "}: cannot negate index."
    {-# INLINE negate #-}
    abs = id
    {-# INLINE abs #-}
    signum ~_ = Idx 1
    {-# INLINE signum #-}

#ifdef UNSAFE_INDICES
    fromInteger = coerce (fromInteger :: Integer -> Word)
#else
    fromInteger i
      | i >= 0 && i < d = Idx $ fromInteger i
      | otherwise       = errorWithoutStackTrace
                        $ "Num.fromInteger{" ++ showIdxType @k @n ++ "}: integer "
                        ++ show i ++ " is outside of index bounds."
      where
        d = toInteger $ dimVal (dimBound @k @n)
#endif
    {-# INLINE fromInteger #-}


unsafeIdxFromWord :: forall (k :: Type) (d :: k) . BoundedDim d => Word -> Idx d
#ifdef UNSAFE_INDICES
unsafeIdxFromWord = coerce
#else
unsafeIdxFromWord w
  | w < d     = Idx w
  | otherwise = errorWithoutStackTrace
              $ "idxFromWord{" ++ showIdxType @k @d ++ "}: word "
              ++ show w ++ " is outside of index bounds."
  where
    d = dimVal (dimBound @k @d)
#endif
{-# INLINE unsafeIdxFromWord #-}

idxFromWord :: forall (k :: Type) (d :: k) . BoundedDim d => Word -> Maybe (Idx d)
idxFromWord w
  | w < dimVal (dimBound @k @d) = Just (Idx w)
  | otherwise                   = Nothing
{-# INLINE idxFromWord #-}


idxToWord :: forall (k :: Type) (d :: k) . Idx d -> Word
idxToWord = coerce
{-# INLINE idxToWord #-}

{-# RULES
"fromIntegral/idxToWord"
  fromIntegral = idxToWord
  #-}


-- | Show type of Idx (for displaying nice errors).
showIdxType :: forall (k :: Type) (x :: k) . BoundedDim x => String
showIdxType = "Idx " ++ show (dimVal (dimBound @k @x))
