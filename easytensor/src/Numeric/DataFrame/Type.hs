{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE BangPatterns               #-}
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
{-# LANGUAGE UndecidableSuperClasses    #-}
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
  ( DataFrame (..), withShape, unboundShape
--  , ElementDataType (..), EDTRefl (..)
  ) where

import           Data.Proxy
import           Data.Type.Equality
import           GHC.Base             (runRW#)
import           GHC.Exts             (IsList (..))
import           GHC.Prim
import           GHC.TypeLits         (Nat, SomeNat (..), someNatVal)
import           GHC.Types
-- import           Numeric.Array
import           Numeric.Array.Family
import qualified Numeric.Commons      as NCommons
import           Numeric.Dimensions
import qualified Numeric.Matrix as M
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
    , NCommons.PrimBytes (Array t ns)
    )
  => SomeDataFrame (Array t ns)



instance ( xnsm ~ (x ': xns')
         , xns ~ Init xnsm
         , Last xnsm ~ XN
         , ns ~ UnwrapDims xns
         , NCommons.PrimBytes (DataFrame t ns)
         , Dimensions ns
         , Show t
         , Eq t
         , NCommons.PrimBytes t
         , ArrayInstanceInference t ns
         )
      => IsList (DataFrame t ((x ': xns') :: [XNat])) where
  type Item (DataFrame t (x ': xns')) = DataFrame t (UnwrapDims (Init (x ': xns')))
  fromList xs = fromListN (length xs) xs
  fromListN _ []  = error "DataFrame fromList: the list must have at least two elements"
  fromListN _ [_] = error "DataFrame fromList: the list must have at least two elements"
  fromListN n@(I# n#) xs  | Just (SomeNat pm) <- someNatVal (fromIntegral n)
                          , pns <- Proxy :: Proxy (UnwrapDims (Init (x ': xns')))
                          , pnsm <- snocP pns pm
                          , I# len# <- totalDim (Proxy @ns)
                          , resultBytes# <- df pnsm len#
      = case inferArrayInstance @t @ns of
          AIFloatX2 -> unsafeCoerce (SomeDataFrame (unsafeCoerce (ArrayF# 0# (n# *# len#) resultBytes#) :: Array Float '[]))
          AIFloatX3 -> unsafeCoerce (SomeDataFrame (unsafeCoerce (ArrayF# 0# (n# *# len#) resultBytes#) :: Array Float '[]))
          AIFloatX4 -> unsafeCoerce (SomeDataFrame (unsafeCoerce (ArrayF# 0# (n# *# len#) resultBytes#) :: Array Float '[]))
          AIArrayF  -> unsafeCoerce (SomeDataFrame (unsafeCoerce (ArrayF# 0# (n# *# len#) resultBytes#) :: Array Float '[]))
          AIArrayD  -> unsafeCoerce (SomeDataFrame (unsafeCoerce (ArrayD# 0# (n# *# len#) resultBytes#) :: Array Double '[]))
          _ -> error "Sorry, fromListN/fromBytes is not implemented for this array type yet."
    where
      elSize# = NCommons.elementByteSize (head xs)
      df :: Proxy nsm -> Int# -> ByteArray#
      df _ len# = case runRW#
        ( \s0 -> let !(# s1, marr #) = newByteArray# (n# *# elSize# *# len#) s0
                     go s _ [] = s
                     go s pos (earr : as) = case NCommons.toBytes earr of
                       (# eoff#, _, ea #) -> go
                         (copyByteArray# ea (eoff# *# elSize#) marr (pos *# elSize#) (elSize# *# len#) s)
                         (pos +# len#)
                         as
                     s2 = go s1 0# xs
                 in unsafeFreezeByteArray# marr s2
        ) of (# _, r #) -> r
      snocP :: Proxy ns -> Proxy m -> Proxy (ns +: m)
      snocP _ _ = Proxy
  fromListN n _ = error $ "DataFrame fromList: not a proper list length: " ++ show n
  toList (SomeDataFrame df) = go offset
    where
      !(I# step) = totalDim (Proxy @ns)
      !(# offset, lenN, arr #) = NCommons.toBytes df
      lim = offset +# lenN
      go pos | isTrue# (pos >=# lim)  = []
             | otherwise = NCommons.fromBytes (# pos, step , arr #) : go (pos +# step)



-- | Do something with
withShape :: DataFrame t xns
          -> (forall ns . ( Dimensions ns
                          , FixedDim xns ns ~ ns
                          , FixedXDim xns ns ~ xns
                          ) => DataFrame t ns -> b)
          -> b
withShape (SomeDataFrame a) f = f (KnownDataFrame a)

-- | Put some of Dimensions into existential data type
unboundShape :: ( FixedXDim xns ns ~ xns
                , FixedDim xns ns ~ ns
                , XDimensions xns
                , Dimensions ns
                , NCommons.PrimBytes (Array t ns)
                , Show (Array t ns)
                , Eq (Array t ns)
                ) => DataFrame t ns -> DataFrame t xns
unboundShape (KnownDataFrame a)
    = SomeDataFrame a






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
  SomeDataFrame (a :: Array t nsa) == SomeDataFrame (b :: Array t nsb)
      = case sameDim (dim @nsa) (dim @nsb) of
          Just Refl -> a == b
          Nothing   -> False

instance Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame (arr :: Array t ns)) = unlines
                            [ "DF [" ++ drop 4 (show $ dim @ns) ++ "]:"
                            , show arr
                            ]




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
