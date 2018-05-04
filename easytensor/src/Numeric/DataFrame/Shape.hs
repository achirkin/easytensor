{-# LANGUAGE AllowAmbiguousTypes       #-}
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
    -- , fromList, DataFrameToList (..)
    -- ,
    , fromScalar
    , fromListN, fromList
    ) where

import           GHC.Base

import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family (inferASing, inferPrim)
import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.Scalar                          as Scalar
import           Numeric.TypedList                       (TypedList (..))
import qualified Numeric.TypedList                       as Dims
import           Numeric.Vector

-- | Append one DataFrame to another, sum up last dimension
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
(<:>) = appendDF
infixl 5 <:>
{-# INLINE [1] (<:>) #-}

-- | Append one DataFrame to another,
--   add another @Dim = 2@ to their dimension list.
(<::>) :: forall (ds :: [Nat]) (t :: Type)
       .  ( PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t (ds +: 2 :: [Nat]))
          )
        => DataFrame t ds
        -> DataFrame t ds
        -> DataFrame t (ds +: 2 :: [Nat])
(<::>) = appendDF
infixl 5 <::>
{-# INLINE [1] (<::>) #-}


vec2t :: forall t . SubSpace t '[] '[2] '[2] => Scalar t -> Scalar t -> Vector t 2
vec2t = unsafeCoerce# (vec2 @t)
{-# INLINE vec2t #-}

{-# RULES
"<::>/vec2-Float"  (<::>) = vec2t @Float
"<::>/vec2-Double" (<::>) = vec2t @Double
"<::>/vec2-Int"    (<::>) = vec2t @Int
"<::>/vec2-Word"   (<::>) = vec2t @Word
  #-}

-- | Grow the first DataFrame by adding the second one to it
--   incrementing the last Dim in the list.
(<+:>) :: forall (ds :: [Nat]) (n :: Nat) (m :: Nat) (t :: Type)
        . ( PrimBytes (DataFrame t (ds +: n))
          , PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t (ds +: m))
          , m ~ (n + 1)
          )
        => DataFrame t (ds +: n)
        -> DataFrame t ds
        -> DataFrame t (ds +: m)
(<+:>) = appendDF
infixl 5 <+:>
{-# INLINE [1] (<+:>) #-}


-- | Concatenate a list of @DataFrame@s.
--   Returns @Nothing@ if the list does not have enough elements.
--
--   Input must be parametrized by @[Nat]@ to make sure every element
--   in the input list has the same dimensionality.
--   Output is in @[XNat]@, because the last dimension is unknown at compile time.
fromList :: forall m ns t
          . ( Dimensions ns
            , PrimBytes t
            )
         => Dim m
            -- ^ Minimum number of elements in a list
         -> [DataFrame t ns]
            -- ^ List of frames to concatenate
         -> Maybe (DataFrame t (AsXDims ns +: XN m))
fromList d xs = fromListN d (length xs) xs



-- | Implement function `toList`.
--   We need to create a dedicated type class for this
--   to make it polymorphic over kind k (Nat <-> XNat).
class DataFrameToList t (ds :: [k]) (z :: k) where
    -- | Unwrap the last dimension of a DataFrame into a list  of smaller frames
    toList :: DataFrame t (ds +: z) -> [DataFrame t ds]



-- instance ( Dimensions ns
--          , Dimensions (ns +: z)
--          , PrimBytes (DataFrame t ns)
--          , PrimBytes (DataFrame t (ns +: z))
--          )
--          => DataFrameToList t z (ns :: [Nat]) where
--   toList df@(KnownDataFrame _) = go offset
--     where
--       !(I# step) = totalDim (Proxy @ns)
--       !(# offset, lenN, arr #) = toBytes df
--       lim = offset +# lenN
--       go pos | isTrue# (pos >=# lim)  = []
--              | otherwise = fromBytes (# pos, step , arr #) : go (pos +# step)
--
-- instance DataFrameToList t xz (xns :: [XNat]) where
--   toList (SomeDataFrame (df :: DataFrame t nsz))
--       | (pns :: Proxy ns, _ :: Proxy z, Refl, Refl, Refl, Refl, Refl) <- getXSZ @nsz
--       , Just Evidence <- inferInitDimensions @nsz
--       , Evidence <- inferInitArrayInstance df
--       , Evidence <- inferNumericFrame @t @ns
--       , I# step <- totalDim pns
--       , (# offset, lenN, arr #) <- toBytes df
--       = go pns step arr (offset +# lenN) offset
--     where
--       getXSZ :: forall xs z . (Proxy xs, Proxy z
--                               , (xs +: z) :~: nsz
--                               , xs :~: Init nsz
--                               , (Head nsz :+ Tail nsz) :~: nsz
--                               , FixedDim xns (Init nsz) :~: Init nsz
--                               , FixedXDim xns (Init nsz) :~: xns)
--       getXSZ = ( Proxy, Proxy, unsafeCoerce Refl
--                , unsafeCoerce Refl
--                , unsafeCoerce Refl
--                , unsafeCoerce Refl
--                , unsafeCoerce Refl)
--       go :: forall ns
--           . ( FixedDim xns ns ~ ns
--             , FixedXDim xns ns ~ xns
--             , NumericFrame t ns
--             , Dimensions ns
--             )
--          => Proxy ns -> Int# -> ByteArray# -> Int# -> Int# -> [DataFrame t xns]
--       go p step arr lim pos | isTrue# (pos >=# lim)  = []
--                             | otherwise = SomeDataFrame (
--                                            fromBytes (# pos, step , arr #) :: DataFrame t ns
--                                         )
--                                       : go p step arr lim (pos +# step)
--   toList _ = error "(DataFrameToList.ToList) Impossible happend: DataFrame has rank zero!"

-- | Concatenate a list of @DataFrame@s.
--   Returns @Nothing@ if the list does not have enough elements
--   or if provided length is invalid.
fromListN :: forall m ns t
           . ( Dimensions ns
             , PrimBytes t
             )
          => Dim m
             -- ^ Minimum number of elements in a list
          -> Int
             -- ^ How many elements of a list to take.
             --   Must be not smaller than @m@ and not greater than @length ns@.
          -> [DataFrame t ns]
             -- ^ List of frames to concatenate
          -> Maybe (DataFrame t (AsXDims ns +: XN m))
fromListN Dim n@(I# n#) xs'
  | n < 0 = Nothing
  | Just dxn@(Dx dn@(D :: Dim n)) <- constrain @m (someDimVal (fromIntegral n))
  , Just xs <- takeMaybe n xs'
  , ns@(AsXDims xns) <- dims @Nat @ns
  , nsn@Dims <- Dims.snoc ns dn
  , xnsn <- Dims.snoc xns dxn
  , EvList <- Dims.snoc (EvList @_ @KnownXNatType @(AsXDims ns))
                        (E' @_ @KnownXNatType @(XN m))
  , XDims nsn' <- xnsn
  , Just E <- sameDims nsn nsn'
  , E <- inferASing @t @ns
  , E <- inferPrim @t @ns
  , E <- inferPrim @t @(ns +: n)
  , I# partElN <- fromIntegral $ totalDim' @ns
  , totalElN <- partElN *# n#
  , elS <- byteSize @t undefined
  , partBS <- partElN *# elS
  = case runRW#
    ( \s0 -> case newByteArray# (totalElN *# elS) s0 of
        (# s1, mba #) ->
          let go _ [] s = s
              go off (p : ps) s = go (off +# partBS) ps (writeBytes mba off p s)
          in unsafeFreezeByteArray# mba (go 0# xs s1)
    ) of (# _, r #)
            -> Just (XFrame (fromElems 0# totalElN r :: DataFrame t (ns +: n)))
fromListN _ _ _ = Nothing

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _        = Just []
takeMaybe _ []       = Nothing
takeMaybe n (x : xs) = (x:) <$> takeMaybe (n-1) xs



appendDF :: (PrimBytes x, PrimBytes y, PrimBytes z)
         => x -> y -> z
appendDF x y
  | sx <- byteSize x
  = case runRW#
    ( \s0 -> case newByteArray# (sx +# byteSize y) s0 of
        (# s1, mba #) -> unsafeFreezeByteArray# mba
            ( writeBytes mba sx y
            ( writeBytes mba 0# x s1))
    ) of (# _, r #) -> fromBytes 0# r
{-# INLINE appendDF #-}
