{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

import           Data.Int
import           Data.Word
import           Data.Type.Equality
import           GHC.TypeLits         (Nat)
import           GHC.Types
import           Numeric.Array.Family
import qualified Numeric.Commons      as NCommons
import           Numeric.Dimensions
import qualified Numeric.Matrix as M

-- | Keep data in a primitive data frame
--    and maintain information about Dimensions in the type-system
data family DataFrame (t :: Type) (xs :: [k])

-- | Completely fixed at compile time
newtype instance Dimensions ns => DataFrame t (ns :: [Nat])
  = KnownDataFrame { _getDF :: Array t ns }

-- | Partially known at compile time
data instance DataFrame t (xns :: [XNat])
  = forall (ns :: [Nat])
  . ( Dimensions ns
    , FixedDim xns ns ~ ns
    , FixedXDim xns ns ~ xns
    , NumericFrame t ns
    , NCommons.PrimBytes (DataFrame t ns)
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
    , NCommons.ElementWise (Idx ds) t (DataFrame t ds)
    , ArrayInstanceInference t ds
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
type instance NCommons.ElemRep (DataFrame t xs) = NCommons.ElemRep (Array t xs)
deriving instance NCommons.PrimBytes (Array t ds)
               => NCommons.PrimBytes (DataFrame t ds)
instance ( Dimensions ds
         , NCommons.ElementWise (Idx ds) t (Array t ds)
         ) => NCommons.ElementWise (Idx ds) t (DataFrame t ds) where
  (!) = (NCommons.!) . _getDF
  {-# INLINE (!) #-}
  ewmap f = KnownDataFrame . NCommons.ewmap f . _getDF
  {-# INLINE ewmap #-}
  ewgen = KnownDataFrame . NCommons.ewgen
  {-# INLINE ewgen #-}
  ewfold f x0 = NCommons.ewfold f x0 . _getDF
  {-# INLINE ewfold #-}
  elementWise f = fmap KnownDataFrame . NCommons.elementWise f . _getDF
  {-# INLINE elementWise #-}
  indexWise f = fmap KnownDataFrame . NCommons.indexWise f . _getDF
  {-# INLINE indexWise #-}
  broadcast = KnownDataFrame . NCommons.broadcast
  {-# INLINE NCommons.broadcast #-}





instance Eq (DataFrame t (ds :: [XNat])) where
  SomeDataFrame (a :: DataFrame t nsa) == SomeDataFrame (b :: DataFrame t nsb)
      = case sameDim (dim @nsa) (dim @nsb) of
          Just Refl -> a == b
          Nothing   -> False

instance Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame arr) = show arr


instance ( ConcatList as bs asbs
         , Dimensions asm
         , Dimensions (m ': bs)
         , asm ~ (as +: m)
         , Dimensions asbs
         , M.MatrixProduct (Array t (as +: m)) (Array t (m :+ bs)) (Array t asbs)
         )
       => M.MatrixProduct (DataFrame t asm)
                          (DataFrame t (m ': bs))
                          (DataFrame t asbs) where
  prod x y = KnownDataFrame $ M.prod (_getDF x) (_getDF y)





_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
