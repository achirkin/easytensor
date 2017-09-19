{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Type
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..)
    -- * Bring type classes into scope
  , NumericFrame
    -- * Utility type families and constraints
  , FPFRame, IntegralFrame, NumericVariantFrame, CommonOpFrame
  ) where

#include "MachDeps.h"
import           Data.Int                  (Int16, Int32, Int64, Int8)
import           Data.Word                 (Word16, Word32, Word64, Word8)
import           Foreign.Storable          (Storable (..))
import           GHC.Exts                  (Int (..), Ptr (..), Float#, Double#, Int#, Word#)
import           GHC.Prim                  (copyAddrToByteArray#,
                                            copyByteArrayToAddr#, newByteArray#,
                                            plusAddr#, quotInt#,
                                            unsafeFreezeByteArray#, (*#))
import           GHC.Types                 (Constraint, IO (..), Type, RuntimeRep (..))


import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions

-- | Keep data in a primitive data frame
--    and maintain information about Dimensions in the type-system
data family DataFrame (t :: Type) (xs :: [k])

-- | Completely fixed at compile time
newtype instance Dimensions ns => DataFrame t (ns :: [Nat])
  = KnownDataFrame { _getDF :: Array t ns }

-- | Partially known at compile time
data instance DataFrame t (xns :: [XNat])
  = forall (ns :: [Nat])
  . ( FixedDim xns ns ~ ns
    , FixedXDim xns ns ~ xns
    , NumericFrame t ns
    )
  => SomeDataFrame (DataFrame t ns)

-- | Allow all numeric operations depending on element type
type NumericFrame t ds = (CommonOpFrame t ds, NumericVariantFrame t ds)

-- | Allow all common operations on data frames
type CommonOpFrame t ds
  = ( Show (DataFrame t ds)
    , Eq (DataFrame t ds)
    , Ord (DataFrame t ds)
    , Num (DataFrame t ds)
    , ElementWise (Idx ds) t (DataFrame t ds)
    , PrimBytes (DataFrame t ds)
    , ArrayInstanceInference t ds
    , KnownDims ds
    , FiniteList ds
    , Dimensions ds
    )

-- | Allow floating-point operations on data frames
type FPFRame t ds
  = ( Fractional (DataFrame t ds)
    , Floating (DataFrame t ds)
    )

-- | Allow some integer-like operations on data frames
type IntegralFrame t (ds :: [Nat])
  = Bounded (DataFrame t ds)


type family NumericVariantFrame t ds :: Constraint where
  NumericVariantFrame Float  ds  = FPFRame Float ds
  NumericVariantFrame Double ds  = FPFRame Double ds
  NumericVariantFrame Int    ds  = IntegralFrame Int    ds
  NumericVariantFrame Int8   ds  = IntegralFrame Int8   ds
  NumericVariantFrame Int16  ds  = IntegralFrame Int16  ds
  NumericVariantFrame Int32  ds  = IntegralFrame Int32  ds
  NumericVariantFrame Int64  ds  = IntegralFrame Int64  ds
  NumericVariantFrame Word   ds  = IntegralFrame Word   ds
  NumericVariantFrame Word8  ds  = IntegralFrame Word8  ds
  NumericVariantFrame Word16 ds  = IntegralFrame Word16 ds
  NumericVariantFrame Word32 ds  = IntegralFrame Word32 ds
  NumericVariantFrame Word64 ds  = IntegralFrame Word64 ds
  NumericVariantFrame _      _   = ()




instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame t ds) where
  show (KnownDataFrame arr) = unlines
                            [ "DF [" ++ drop 4 (show $ dim @ds) ++ "]:"
                            , show arr
                            ]
deriving instance Bounded (Array t ds) => Bounded (DataFrame t ds)
deriving instance Enum (Array t ds) => Enum (DataFrame t ds)
deriving instance Eq (Array t ds) => Eq (DataFrame t ds)
deriving instance Integral (Array t ds)
               => Integral (DataFrame t ds)
deriving instance Num (Array t ds)
               => Num (DataFrame t ds)
deriving instance Fractional (Array t ds)
               => Fractional (DataFrame t ds)
deriving instance Floating (Array t ds)
               => Floating (DataFrame t ds)
deriving instance Ord (Array t ds)
               => Ord (DataFrame t ds)
deriving instance ( Read (Array t ds), Dimensions ds )
               => Read (DataFrame t ds)
deriving instance Real (Array t ds)
               => Real (DataFrame t ds)
deriving instance RealFrac (Array t ds)
               => RealFrac (DataFrame t ds)
deriving instance RealFloat (Array t ds)
               => RealFloat (DataFrame t ds)
instance ( Dimensions ds
         , ElementWise (Idx ds) t (Array t ds)
         ) => ElementWise (Idx ds) t (DataFrame t ds) where
  indexOffset#  = indexOffset# . _getDF
  {-# INLINE indexOffset# #-}
  (!) = (!) . _getDF
  {-# INLINE (!) #-}
  ewmap f = KnownDataFrame . ewmap f . _getDF
  {-# INLINE ewmap #-}
  ewgen = KnownDataFrame . ewgen
  {-# INLINE ewgen #-}
  ewgenA = fmap KnownDataFrame . ewgenA
  {-# INLINE ewgenA #-}
  ewfoldl f x0 = ewfoldl f x0 . _getDF
  {-# INLINE ewfoldl #-}
  ewfoldr f x0 = ewfoldr f x0 . _getDF
  {-# INLINE ewfoldr #-}
  elementWise f = fmap KnownDataFrame . elementWise f . _getDF
  {-# INLINE elementWise #-}
  indexWise f = fmap KnownDataFrame . indexWise f . _getDF
  {-# INLINE indexWise #-}
  broadcast = KnownDataFrame . broadcast
  {-# INLINE broadcast #-}
  update i x = KnownDataFrame . update i x . _getDF
  {-# INLINE update #-}


instance PrimBytes (DataFrame t ds) => Storable (DataFrame t ds) where
  sizeOf x = I# (byteSize x)
  alignment x = I# (byteAlign x)
  peekElemOff ptr (I# offset) =
    peekByteOff ptr (I# (offset *# byteSize (undefined :: DataFrame t ds)))
  pokeElemOff ptr (I# offset) =
    pokeByteOff ptr (I# (offset *# byteSize (undefined :: DataFrame t ds)))
  peekByteOff (Ptr addr) (I# offset) = IO $ \s0 -> case newByteArray# bsize s0 of
    (# s1, marr #) -> case copyAddrToByteArray# (addr `plusAddr#` offset)
                                                 marr 0# bsize s1 of
      s2 -> case unsafeFreezeByteArray# marr s2 of
        (# s3, arr #) -> (# s3, fromBytes (# 0#, bsize `quotInt#` ebsize, arr #) #)
    where
      bsize = byteSize (undefined :: DataFrame t ds)
      ebsize = elementByteSize (undefined :: DataFrame t ds)
  pokeByteOff (Ptr addr) (I# offset) x = IO
          $ \s0 -> case copyByteArrayToAddr# xbytes xboff
                                             (addr `plusAddr#` offset)
                                              bsize s0 of
       s2 -> (# s2, () #)
    where
      !(# elOff, elNum, xbytes #) = toBytes x
      bsize = elementByteSize x *# elNum
      xboff  = elementByteSize x *# elOff
  peek ptr = peekByteOff ptr 0
  poke ptr = pokeByteOff ptr 0




type instance ElemRep (DataFrame t xs) = ElemRep (Array t xs)
type instance ElemPrim (DataFrame Float  ds) = Float#
type instance ElemPrim (DataFrame Double ds) = Double#
type instance ElemPrim (DataFrame Int    ds) = Int#
type instance ElemPrim (DataFrame Int8   ds) = Int#
type instance ElemPrim (DataFrame Int16  ds) = Int#
type instance ElemPrim (DataFrame Int32  ds) = Int#
#ifndef ghcjs_HOST_OS
#if WORD_SIZE_IN_BITS < 64
type instance ElemPrim (DataFrame Int64  ds) = Int64#
#else
type instance ElemPrim (DataFrame Int64  ds) = Int#
#endif
#endif
type instance ElemPrim (DataFrame Word   ds) = Word#
type instance ElemPrim (DataFrame Word8  ds) = Word#
type instance ElemPrim (DataFrame Word16 ds) = Word#
type instance ElemPrim (DataFrame Word32 ds) = Word#
#ifdef ghcjs_HOST_OS
type instance ElemPrim (DataFrame Word8Clamped ds) = Int#
#else
#if WORD_SIZE_IN_BITS < 64
type instance ElemPrim (DataFrame Word64 ds) = Word64#
#else
type instance ElemPrim (DataFrame Word64 ds) = Word#
#endif
#endif
deriving instance ( PrimBytes (Array Float ds)
                  , ElemPrim (Array Float ds) ~ Float#
                  , ElemRep (Array Float ds) ~ 'FloatRep) => PrimBytes (DataFrame Float ds)
deriving instance ( PrimBytes (Array Double ds)
                  , ElemPrim (Array Double ds) ~ Double#
                  , ElemRep (Array Double ds) ~ 'DoubleRep) => PrimBytes (DataFrame Double ds)
deriving instance ( PrimBytes (Array Int ds)
                  , ElemPrim (Array Int ds) ~ Int#
                  , ElemRep (Array Int ds) ~ 'IntRep) => PrimBytes (DataFrame Int ds)
deriving instance ( PrimBytes (Array Int8 ds)
                  , ElemPrim (Array Int8 ds) ~ Int#
                  , ElemRep (Array Int8 ds) ~ 'IntRep) => PrimBytes (DataFrame Int8 ds)
deriving instance ( PrimBytes (Array Int16 ds)
                  , ElemPrim (Array Int16 ds) ~ Int#
                  , ElemRep (Array Int16 ds) ~ 'IntRep) => PrimBytes (DataFrame Int16 ds)
deriving instance ( PrimBytes (Array Int32 ds)
                  , ElemPrim (Array Int32 ds) ~ Int#
                  , ElemRep (Array Int32 ds) ~ 'IntRep) => PrimBytes (DataFrame Int32 ds)
#ifndef ghcjs_HOST_OS
deriving instance ( PrimBytes (Array Int64 ds)
#if WORD_SIZE_IN_BITS < 64
                  , ElemPrim (Array Int64 ds) ~ Int64#
                  , ElemRep (Array Int64 ds) ~ 'Int64Rep
#else
                  , ElemPrim (Array Int64 ds) ~ Int#
                  , ElemRep (Array Int64 ds) ~ 'IntRep
#endif
                  ) => PrimBytes (DataFrame Int64 ds)
#endif
deriving instance ( PrimBytes (Array Word ds)
                  , ElemPrim (Array Word ds) ~ Word#
                  , ElemRep (Array Word ds) ~ 'WordRep) => PrimBytes (DataFrame Word ds)
deriving instance ( PrimBytes (Array Word8 ds)
                  , ElemPrim (Array Word8 ds) ~ Word#
                  , ElemRep (Array Word8 ds) ~ 'WordRep) => PrimBytes (DataFrame Word8 ds)
deriving instance ( PrimBytes (Array Word16 ds)
                  , ElemPrim (Array Word16 ds) ~ Word#
                  , ElemRep (Array Word16 ds) ~ 'WordRep) => PrimBytes (DataFrame Word16 ds)
deriving instance ( PrimBytes (Array Word32 ds)
                  , ElemPrim (Array Word32 ds) ~ Word#
                  , ElemRep (Array Word32 ds) ~ 'WordRep) => PrimBytes (DataFrame Word32 ds)
#ifdef ghcjs_HOST_OS
deriving instance ( PrimBytes (Array Word8Clamped ds)
                  , ElemPrim (Array Word8Clamped ds) ~ Int#
                  , ElemRep (Array Word8Clamped ds) ~ 'IntRep) => PrimBytes (DataFrame Word8Clamped ds)
#else
deriving instance ( PrimBytes (Array Word64 ds)
#if WORD_SIZE_IN_BITS < 64
                  , ElemPrim (Array Word64 ds) ~ Word64#
                  , ElemRep (Array Word64 ds) ~ 'Word64Rep
#else
                  , ElemPrim (Array Word64 ds) ~ Word#
                  , ElemRep (Array Word64 ds) ~ 'WordRep
#endif
                  ) => PrimBytes (DataFrame Word64 ds)
#endif







instance Eq (DataFrame t (ds :: [XNat])) where
  SomeDataFrame (a :: DataFrame t nsa) == SomeDataFrame (b :: DataFrame t nsb)
      = case sameDim (dim @nsa) (dim @nsb) of
          Just Evidence -> a == b
          Nothing   -> False

instance Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame arr) = show arr



_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
