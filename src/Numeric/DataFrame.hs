{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame
  ( DataFrame
  , withShape
  , unboundShape
  ) where

import           Data.Type.Equality
import           GHC.TypeLits       (Nat)
import           GHC.Types          (Type)
import           Numeric.Array
import           Numeric.Commons
import           Numeric.Dimensions
import           Unsafe.Coerce

-- | Keep data in a primitive data frame
--    and maintain information about dimensions in the type-system
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
    , Show (Array t ns)
    , Eq (Array t ns)
    )
  => SomeDataFrame (Dim xns) (Array t ns)

instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame t ds) where
  show (KnownDataFrame arr) = "DataFrame:"
                         ++ "\n\tShape: " ++ show (dim `inSpaceOf` arr)
                         ++ "\n\tContent:\n" ++ show arr

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
deriving instance PrimBytes (Array t ds)
               => PrimBytes (DataFrame t ds)
deriving instance FloatBytes (Array t ds)
               => FloatBytes (DataFrame t ds)
deriving instance DoubleBytes (Array t ds)
               => DoubleBytes (DataFrame t ds)
deriving instance IntBytes (Array t ds)
               => IntBytes (DataFrame t ds)
deriving instance WordBytes (Array t ds)
               => WordBytes (DataFrame t ds)
instance ( Dimensions ds
         , ElementWise (Idx ds) t (Array Float ds)
         ) => ElementWise (Idx ds) t (DataFrame Float ds) where
  (!) = (!) . _getDF
  {-# INLINE (!) #-}
  ewmap f = KnownDataFrame . ewmap f . _getDF
  {-# INLINE ewmap #-}
  ewgen = KnownDataFrame . ewgen
  {-# INLINE ewgen #-}
  ewfold f x0 = ewfold f x0 . _getDF
  {-# INLINE ewfold #-}
  elementWise f = fmap KnownDataFrame . elementWise f . _getDF
  {-# INLINE elementWise #-}
  indexWise f = fmap KnownDataFrame . indexWise f . _getDF
  {-# INLINE indexWise #-}


instance Show (Dim ds)
      => Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame d arr) = "DataFrame:"
                         ++ "\n\tShape: " ++ show d
                         ++ "\n\tContent:\n" ++ show arr

instance Eq (DataFrame t (ds :: [XNat])) where
  SomeDataFrame d1 a1 == SomeDataFrame d2 a2
      = case check d1 d2 a1 a2 of
          Just Refl -> a1 == a2
          Nothing   -> False
    where
      check :: Dim ds -> Dim ds
            -> p ns1 -> q ns2
            -> Maybe (ns1 :~: ns2)
      check a b _ _ | a == b  = Just (unsafeCoerce Refl)
                    | otherwise = Nothing

-- | Do something with
withShape :: DataFrame t xns
          -> (forall ns . ( Dimensions ns
                          , FixedDim xns ns ~ ns
                          , FixedXDim xns ns ~ xns
                          ) => DataFrame t ns -> b)
          -> b
withShape (SomeDataFrame _ a) f = f (KnownDataFrame a)

-- | Put some of dimensions into existential data type
unboundShape :: ( FixedXDim xns ns ~ xns
                , FixedDim xns ns ~ ns
                , XDimensions ns xns
                , Dimensions ns
                , Show (Array t ns)
                , Eq (Array t ns)
                ) => DataFrame t ns -> DataFrame t xns
unboundShape (KnownDataFrame a)
    = SomeDataFrame (xdim $ dim `inSpaceOf` a) a


_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
