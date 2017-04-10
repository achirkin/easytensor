{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE InstanceSigs           #-}

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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Type
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Type
  ( DataFrame (..), withShape, unboundShape
  , ElementDataType (..), EDTRefl (..)
  ) where

import           Data.Type.Equality
import           GHC.TypeLits       (Nat)
import           GHC.Types
import           Numeric.Array
-- import qualified Numeric.Array.Family as AFam (Scalar (..))
import qualified Numeric.Commons as NCommons
import           Numeric.Dimensions
import qualified Numeric.Matrix.Class as M
import           Unsafe.Coerce

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
    , Show (Array t ns)
    , Eq (Array t ns)
    )
  => SomeDataFrame (Dim xns) (Array t ns)


-- | This class is used to pattern match against available data types
--   represented by EDTRefl
class ElementDataType t where
  -- | Get corresponding singleton constructor for a given element data type
  edtRefl :: proxy t -> EDTRefl t

-- | Represent available element data types
data EDTRefl :: (Type -> Type) where
  EDTFloat :: EDTRefl Float

instance ElementDataType Float where
  edtRefl _ = EDTFloat




-- | Do something with
withShape :: DataFrame t xns
          -> (forall ns . ( Dimensions ns
                          , FixedDim xns ns ~ ns
                          , FixedXDim xns ns ~ xns
                          ) => DataFrame t ns -> b)
          -> b
withShape (SomeDataFrame _ a) f = f (KnownDataFrame a)

-- | Put some of Dimensions into existential data type
unboundShape :: ( FixedXDim xns ns ~ xns
                , FixedDim xns ns ~ ns
                , XDimensions ns xns
                , Dimensions ns
                , Show (Array t ns)
                , Eq (Array t ns)
                ) => DataFrame t ns -> DataFrame t xns
unboundShape (KnownDataFrame a)
    = SomeDataFrame (xdim $ dim `inSpaceOf` a) a






instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame t ds) where
  show (KnownDataFrame arr) = unlines
                            [ "DF [" ++ drop 4 (show $ dim `inSpaceOf` arr) ++ "]:"
                            , show arr
                            ]

instance Show (Dim ds)
      => Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame d arr) = unlines
                            [ "DF [" ++ drop 4 (show d) ++ "]:"
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
deriving instance NCommons.PrimBytes (Array t ds)
               => NCommons.PrimBytes (DataFrame t ds)
deriving instance NCommons.FloatBytes (Array t ds)
               => NCommons.FloatBytes (DataFrame t ds)
deriving instance NCommons.DoubleBytes (Array t ds)
               => NCommons.DoubleBytes (DataFrame t ds)
deriving instance NCommons.IntBytes (Array t ds)
               => NCommons.IntBytes (DataFrame t ds)
deriving instance NCommons.WordBytes (Array t ds)
               => NCommons.WordBytes (DataFrame t ds)
instance ( Dimensions ds
         , NCommons.ElementWise (Idx ds) t (Array Float ds)
         ) => NCommons.ElementWise (Idx ds) t (DataFrame Float ds) where
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
