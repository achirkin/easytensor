{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Shape
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Construct new DataFrames from pieces.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Shape
    ( (<:>), (<::>), (<+:>)
    , fromList, DataFrameToList (..), fromScalar
    ) where

import           Data.Type.Equality          ((:~:) (..))
import           GHC.Base                    (runRW#)
import qualified GHC.Exts                    as Exts (IsList (..))
import           GHC.Prim
import           GHC.Types                   (Int (..), Type, isTrue#)
import           Unsafe.Coerce               (unsafeCoerce)

import qualified Numeric.DataFrame.Internal.Array.ElementWise   as EW
import           Numeric.DataFrame.Internal.Array.Family        hiding (Scalar)
import           Numeric.Commons
import           Numeric.DataFrame.Inference
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.TypeLits
import           Numeric.Scalar              as Scalar
import           Numeric.Vector              (vec2)

-- | Append one DataFrame to another, adding up their last dimensionality
(<:>) :: forall (n :: Nat) (m :: Nat) (npm :: Nat) (ds :: [Nat])
                (t :: Type)
       . ( PrimBytes (DataFrame t (ds +: n))
         , PrimBytes (DataFrame t (ds +: m))
         , PrimBytes (DataFrame t (ds +: npm))
         , npm ~ (n + m)
         , n   ~ (npm - m)
         , m   ~ (npm - n)
         )
        => DataFrame t (ds +: n)
        -> DataFrame t (ds +: m)
        -> DataFrame t (ds +: npm)
a <:> b = case (# toBytes a, toBytes b
                , byteSize a
                , byteSize b
                , elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <:>

-- | Append one DataFrame to another, adding up their last dimensionality
(<::>) :: forall (ds :: [Nat]) (t :: Type)
       .  ( PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t (ds +: 2 :: [Nat]))
          )
        => DataFrame t ds
        -> DataFrame t ds
        -> DataFrame t (ds +: 2 :: [Nat])
a <::> b = case (# toBytes a, toBytes b
                , byteSize a
                , byteSize b
                , elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <::>
{-# NOINLINE [1] (<::>) #-}

{-# RULES
"<::>/vec2-Float"  forall (a :: Scalar Float)  (b :: Scalar Float)  . a <::> b = vec2 (unScalar a) (unScalar b)
"<::>/vec2-Double" forall (a :: Scalar Double) (b :: Scalar Double) . a <::> b = vec2 (unScalar a) (unScalar b)
"<::>/vec2-Int"    forall (a :: Scalar Int)    (b :: Scalar Int)    . a <::> b = vec2 (unScalar a) (unScalar b)
-- "<::>/vec2-Word"   forall (a :: Scalar Word)   (b :: Scalar Word)   . a <::> b = vec2 (unScalar a) (unScalar b)
  #-}

-- | Append one DataFrame to another, adding up their last dimensionality
(<+:>) :: forall (ds :: [Nat]) (n :: Nat) (m :: Nat) (t :: Type)
        . ( PrimBytes (DataFrame t (ds +: n))
          , PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t (ds +: m))
          , m ~ (n + 1)
          )
        => DataFrame t (ds +: n)
        -> DataFrame t ds
        -> DataFrame t (ds +: m)
a <+:> b = case (# toBytes a, toBytes b
                , byteSize a
                , byteSize b
                , elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <+:>


-- | Input must be parametrized by [Nat] to make sure every element
--   in the input list has the same dimensionality.
--   Output is in [XNat], because the last dimension is unknown at compile time
fromList :: forall ns t xns xnsm
          . ( ns ~ AsDims xns
            , xnsm ~ (xns +: XN 2)
            , PrimBytes (DataFrame t ns)
            , Dimensions ns
            , ArrayInstanceInference t ns
            )
         => [DataFrame t ns] -> DataFrame t (xns +: XN 2)
fromList xs = fromListN (length xs) xs

-- | Implement function `toList`.
--   We need to create a dedicated type class for this
--   to make it polymorphic over kind k (Nat <-> XNat).
class DataFrameToList t z (ds :: [k]) where
    -- | Unwrap the last dimension of a DataFrame into a list  of smaller frames
    toList :: DataFrame t (ds +: z) -> [DataFrame t ds]



instance ( Dimensions ns
         , Dimensions (ns +: z)
         , PrimBytes (DataFrame t ns)
         , PrimBytes (DataFrame t (ns +: z))
         )
         => DataFrameToList t z (ns :: [Nat]) where
  toList df@(KnownDataFrame _) = go offset
    where
      !(I# step) = totalDim (Proxy @ns)
      !(# offset, lenN, arr #) = toBytes df
      lim = offset +# lenN
      go pos | isTrue# (pos >=# lim)  = []
             | otherwise = fromBytes (# pos, step , arr #) : go (pos +# step)

instance DataFrameToList t xz (xns :: [XNat]) where
  toList (SomeDataFrame (df :: DataFrame t nsz))
      | (pns :: Proxy ns, _ :: Proxy z, Refl, Refl, Refl, Refl, Refl) <- getXSZ @nsz
      , Just Evidence <- inferInitDimensions @nsz
      , Evidence <- inferInitArrayInstance df
      , Evidence <- inferNumericFrame @t @ns
      , I# step <- totalDim pns
      , (# offset, lenN, arr #) <- toBytes df
      = go pns step arr (offset +# lenN) offset
    where
      getXSZ :: forall xs z . (Proxy xs, Proxy z
                              , (xs +: z) :~: nsz
                              , xs :~: Init nsz
                              , (Head nsz :+ Tail nsz) :~: nsz
                              , FixedDim xns (Init nsz) :~: Init nsz
                              , FixedXDim xns (Init nsz) :~: xns)
      getXSZ = ( Proxy, Proxy, unsafeCoerce Refl
               , unsafeCoerce Refl
               , unsafeCoerce Refl
               , unsafeCoerce Refl
               , unsafeCoerce Refl)
      go :: forall ns
          . ( FixedDim xns ns ~ ns
            , FixedXDim xns ns ~ xns
            , NumericFrame t ns
            , Dimensions ns
            )
         => Proxy ns -> Int# -> ByteArray# -> Int# -> Int# -> [DataFrame t xns]
      go p step arr lim pos | isTrue# (pos >=# lim)  = []
                            | otherwise = SomeDataFrame (
                                           fromBytes (# pos, step , arr #) :: DataFrame t ns
                                        )
                                      : go p step arr lim (pos +# step)
  toList _ = error "(DataFrameToList.ToList) Impossible happend: DataFrame has rank zero!"


fromListN :: forall ns t xns xnsm
           . ( ns ~ AsDims xns
             , xnsm ~ (xns +: XN 2)
             , PrimBytes (DataFrame t ns)
             , Dimensions ns
             , ArrayInstanceInference t ns
             )
          => Int -> [DataFrame t ns] -> DataFrame t (xns +: XN 2)
fromListN _ []  = error "DataFrame fromList: the list must have at least two elements"
fromListN _ [_] = error "DataFrame fromList: the list must have at least two elements"
fromListN n@(I# n#) xs  | Just (SomeNat (pm :: Proxy m)) <- someNatVal (fromIntegral n)
                        , (pnsm, Refl, Refl, Refl) <- snocP pm
                        , I# len# <- totalDim (Proxy @ns)
                        , resultBytes# <- df len#
                        , Evidence <- inferSnocDimensions @ns @m
                        , Evidence <- inferSnocArrayInstance (head xs) pm
                        , Evidence <- inferPrimBytes @t @(ns +: m)
                        , Evidence <- inferNumericFrame @t @(ns +: m)
    = SomeDataFrame . enforceDim @t pnsm $ fromBytes (# 0#, n# *# len#, resultBytes# #)
  where
    elSize# = elementByteSize (head xs)
    df :: Int# -> ByteArray#
    df len# = case runRW#
      ( \s0 -> let !(# s1, marr #) = newByteArray# (n# *# elSize# *# len#) s0
                   go s _ [] = s
                   go s pos (earr : as) = case toBytes earr of
                     (# eoff#, _, ea #) -> go
                       (copyByteArray# ea (eoff# *# elSize#) marr (pos *# elSize#) (elSize# *# len#) s)
                       (pos +# len#)
                       as
                   s2 = go s1 0# xs
               in unsafeFreezeByteArray# marr s2
      ) of (# _, r #) -> r
    snocP :: forall m . Proxy m ->
           ( Proxy (ns +: m)
           , FixedXDim xnsm (ns +: m) :~: xnsm
           , FixedDim  xnsm (ns +: m) :~: (ns +: m)
           , (2 <=? m) :~: 'True
           )
    snocP _ = (Proxy, unsafeCoerce Refl, unsafeCoerce Refl, unsafeCoerce Refl)
    enforceDim :: forall s nsm . Proxy nsm -> DataFrame s nsm -> DataFrame s nsm
    enforceDim _ = id
fromListN n _ = error $ "DataFrame fromList: not a proper list length: " ++ show n


instance ( xnsm ~ (x ': xns')
         , xns ~ Init xnsm
         , Last xnsm ~ XN 2
         , ns ~ AsDims xns
         , (x ': xns') ~ (xns +: XN 2)
         , PrimBytes (DataFrame t ns)
         , Dimensions ns
         , ArrayInstanceInference t ns
         )
      => Exts.IsList (DataFrame t ((x ': xns') :: [XNat])) where
  type Item (DataFrame t (x ': xns')) = DataFrame t (AsDims (Init (x ': xns')))
  fromList xs = fromListN (length xs) xs
  fromListN = fromListN
  toList (SomeDataFrame (df :: DataFrame t ds))
    | Refl <- unsafeCoerce Refl :: ds :~: (ns +: Last ds) = toList df

-- | Broadcast scalar value onto a whole data frame
fromScalar :: EW.ElementWise (Idx ds) t (DataFrame t ds)
           => Scalar.Scalar t -> DataFrame t ds
fromScalar = EW.broadcast . Scalar.unScalar
