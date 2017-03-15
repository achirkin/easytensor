{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
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
  ( DataFrame, withShape, unboundShape
  , (%*), (<::>), (<+:>), (<:>)
  , Vector, Matrix, Scalar, Scf
  , Vec2f, Vec3f, Vec4f
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  , scalar, unScalar
  , vec2, vec3, vec4
  , mat22, mat33, mat44
  , (.*.), dot, (·)
  , cross, (×), det2
  , normL1, normL2, normLPInf, normLNInf, normLP
  , inverse, det, trace, eye, diag, transpose
  , slice, runSlice, subSpace, index
  , M.MatrixCalculus (), M.SquareMatrixCalculus ()
  , M.MatrixInverse (), M.MatrixProduct (..)
  , NumSpace
  , module Numeric.DataFrame.SubSpace
  ) where

import Data.Functor.Const
import           Data.Proxy
import           Data.Type.Equality
import           GHC.Base (runRW#)
import           GHC.Prim
import           GHC.TypeLits       (Nat, natVal, type (+), type (-), KnownNat)
import           GHC.Types
import           Numeric.Array
import qualified Numeric.Array.Family as AFam (Scalar (..))
import qualified Numeric.Commons as NCommons
import qualified Numeric.Dimensions as Dims
import qualified Numeric.Matrix.Class as M
import           Unsafe.Coerce

import           Numeric.DataFrame.Type
import           Numeric.DataFrame.SubSpace




--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------



-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.
(%*) :: ( M.MatrixProduct (DataFrame t (as Dims.+: m))
                          (DataFrame t (m ': bs))
                          (DataFrame t (as Dims.++ bs))
        )
     => DataFrame t (as Dims.+: m) -> DataFrame t (m Dims.:+ bs) -> DataFrame t (as Dims.++ bs)
(%*) = M.prod
{-# INLINE (%*) #-}
infixl 7 %*

-- | Append one DataFrame to another, adding up their last dimensionality
(<:>) :: ( NCommons.PrimBytes (DataFrame t (ds Dims.+: n))
         , NCommons.PrimBytes (DataFrame t (ds Dims.+: m))
         , NCommons.PrimBytes (DataFrame t (ds Dims.+: npm))
         , npm ~ (n + m)
         , n   ~ (npm - m)
         , m   ~ (npm - n)
         )
        => DataFrame t (ds Dims.+: n)
        -> DataFrame t (ds Dims.+: m)
        -> DataFrame t (ds Dims.+: npm)
a <:> b = case (# NCommons.toBytes a, NCommons.toBytes b
                , NCommons.byteSize a
                , NCommons.byteSize b
                , NCommons.elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> NCommons.fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <:>

-- | Append one DataFrame to another, adding up their last dimensionality
(<::>) :: ( NCommons.PrimBytes (DataFrame t ds)
          , NCommons.PrimBytes (DataFrame t ds)
          , NCommons.PrimBytes (DataFrame t (ds Dims.+: 2))
          )
        => DataFrame t ds
        -> DataFrame t ds
        -> DataFrame t (ds Dims.+: 2)
a <::> b = case (# NCommons.toBytes a, NCommons.toBytes b
                , NCommons.byteSize a
                , NCommons.byteSize b
                , NCommons.elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> NCommons.fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <::>

-- | Append one DataFrame to another, adding up their last dimensionality
(<+:>) :: ( NCommons.PrimBytes (DataFrame t (ds Dims.+: n))
          , NCommons.PrimBytes (DataFrame t ds)
          , NCommons.PrimBytes (DataFrame t (ds Dims.+: (n + 1)))
          )
        => DataFrame t (ds Dims.+: n)
        -> DataFrame t ds
        -> DataFrame t (ds Dims.+: (n + 1))
a <+:> b = case (# NCommons.toBytes a, NCommons.toBytes b
                , NCommons.byteSize a
                , NCommons.byteSize b
                , NCommons.elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> NCommons.fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <+:>

runSlice :: forall (t :: Type) (f :: Type -> Type)
                     (ods :: [Nat]) (nds :: [Nat])
           . (Dims.Idx '[] -> DataFrame t ods -> f (DataFrame t nds))
          -> DataFrame t ods
          -> f (DataFrame t nds)
runSlice f = f Dims.Z

subSpace :: forall (t :: Type) (f :: Type -> Type) (n :: Nat)
                   (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
         . ( Applicative f
           , Dims.Dimensions ods
           , Dims.Dimensions nds
           , Dims.Dimensions (ods Dims.+: n)
           , Dims.Dimensions (nds Dims.+: n)
           , KnownNat n
           , NCommons.PrimBytes (DataFrame t ods)
           , NCommons.PrimBytes (DataFrame t nds)
           , NCommons.PrimBytes (DataFrame t (ods Dims.+: n))
           , NCommons.PrimBytes (DataFrame t (nds Dims.+: n))
           )
        -- slice
        => (Dims.Idx (n Dims.:+ remDs) -> DataFrame t ods -> f (DataFrame t nds))
        -- new transform
        -> Dims.Idx remDs
        -> DataFrame t (ods Dims.+: n)
        -> f (DataFrame t (nds Dims.+: n))
subSpace = case unsafeCoerce Refl :: (nds Dims.>: n) :~: (nds Dims.+: n) of
    Refl -> slice Dims.Every
{-# INLINE [2] subSpace #-}

slice :: forall (t :: Type) (s :: Type) (f :: Type -> Type)
                (o :: Nat) (n :: Nat)
                (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
           . ( Applicative f
             , Dims.Dimensions ods
             , Dims.Dimensions nds
             , Dims.Dimensions (ods Dims.+: o)
             , Dims.Dimensions (nds Dims.+: n)
             , Dims.Dimensions (nds Dims.>: n)
             , KnownNat o
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ods)
             , NCommons.PrimBytes (DataFrame s nds)
             , NCommons.PrimBytes (DataFrame t (ods Dims.+: o))
             , NCommons.PrimBytes (DataFrame s (nds Dims.>: n))
             )
          -- slice
          => Dims.Slice o n
          -- old transform
          -> (Dims.Idx (n Dims.:+ remDs) -> DataFrame t ods -> f (DataFrame s nds))
          -- new transform
          -> Dims.Idx remDs
          -> DataFrame t (ods Dims.+: o)
          -> f (DataFrame s (nds Dims.>: n))
slice s f remIds oldDF
    = merge <$> traverse (\(I# i#, i) -> f (i Dims.:!remIds)
                           (NCommons.fromBytes (# offOld +# (i# -# 1#) *# lengthOld'
                                       , lengthOld'
                                       , arrOld #))
                         ) (zip (slice2list s) [1..])
  where
    merge [x] = NCommons.fromBytes (NCommons.toBytes x)
    merge xs = case runRW#
       (\s0 -> case newByteArray# bsNew s0 of
         (# s1, marr #) -> case writeOne xs marr 0# s1 of
           s2 -> unsafeFreezeByteArray# marr s2
       ) of (# _, r #) -> NCommons.fromBytes (# 0#, lengthNew, r #)
    writeOne [] _ _ s' = s'
    writeOne (x : xs) mr pos s' = case NCommons.toBytes x of
      (# off, l, a #) ->  writeOne xs mr (pos +# l)
        (copyByteArray# a (off *# elSizeS) mr (pos *# elSizeS) (l *# elSizeS) s')
    (# offOld, _, arrOld #) = NCommons.toBytes oldDF
    elSizeS = NCommons.elementByteSize (undefined :: DataFrame s nds)
    lengthOld' = case Dims.totalDim (Proxy @ods) of I# lo -> lo
    lengthNew  = case Dims.totalDim (Proxy @(nds Dims.>: n)) of I# ln -> ln
    bsNew      = lengthNew *# elSizeS
{-# INLINE [2] slice #-}

sliceF :: forall (t :: Type) (acc :: Type)
                 (o :: Nat) (n :: Nat)
                    (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
           . ( Monoid acc
             , Dims.Dimensions ods
             , Dims.Dimensions nds
             , Dims.Dimensions (ods Dims.+: o)
             , Dims.Dimensions (nds Dims.+: n)
             , KnownNat o
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ods)
             , NCommons.PrimBytes (DataFrame t nds)
             , NCommons.PrimBytes (DataFrame t (ods Dims.+: o))
             , NCommons.PrimBytes (DataFrame t (nds Dims.>: n))
             )
          -- slice
          => Dims.Slice o n
          -- old transform
          -> (Dims.Idx (n Dims.:+ remDs) -> DataFrame t ods -> Const acc (DataFrame t nds))
          -- new transform
          -> Dims.Idx remDs
          -> DataFrame t (ods Dims.+: o)
          -> Const acc (DataFrame t (nds Dims.>: n))
sliceF s f remIds oldDF
    = Const $ foldMap (\(I# i#, i) -> getConst $ f (i Dims.:!remIds)
                           (NCommons.fromBytes (# offOld +# (i# -# 1#) *# lengthOld'
                                       , lengthOld'
                                       , arrOld #))
                         ) (zip (slice2list s) [1..])
  where
    (# offOld, _, arrOld #) = NCommons.toBytes oldDF
    lengthOld' = case Dims.totalDim (Proxy @ods) of I# lo -> lo

subSpaceF :: forall (t :: Type) (acc :: Type) (n :: Nat)
                   (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
         . ( Monoid acc
           , Dims.Dimensions ods
           , Dims.Dimensions nds
           , Dims.Dimensions (ods Dims.+: n)
           , Dims.Dimensions (nds Dims.+: n)
           , KnownNat n
           , NCommons.PrimBytes (DataFrame t ods)
           , NCommons.PrimBytes (DataFrame t nds)
           , NCommons.PrimBytes (DataFrame t (ods Dims.+: n))
           , NCommons.PrimBytes (DataFrame t (nds Dims.+: n))
           )
        -- slice
        => (Dims.Idx (n Dims.:+ remDs) -> DataFrame t ods -> Const acc (DataFrame t nds))
        -- new transform
        -> Dims.Idx remDs
        -> DataFrame t (ods Dims.+: n)
        -> Const acc (DataFrame t (nds Dims.+: n))
subSpaceF = case unsafeCoerce Refl :: (nds Dims.>: n) :~: (nds Dims.+: n) of
    Refl -> sliceF Dims.Every



slice2list :: KnownNat m => Dims.Slice n m -> [Int]
slice2list x@Dims.Every = [1..fromInteger (natVal x)]
slice2list (Dims.Get i) = [i]
slice2list xx@(x Dims.:& i) = slice2list (xx `f` unsafeCoerce x) ++ [i]
  where
    f :: a -> a -> a
    f _ = id

-- | Apply a functor over a single element in an outer dimension
index :: forall (t :: Type) (f :: Type -> Type) (n :: Nat) (ds :: [Nat])
           . ( Functor f
             , Dims.Dimensions ds
             , Dims.Dimensions (ds Dims.+: n)
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ds)
             , NCommons.PrimBytes (DataFrame t (ds Dims.+: n))
             )
          -- slice a single element in an outer dimension
          => Int
          -- old transform
          -> (DataFrame t ds -> f (DataFrame t ds))
          -- new transform
          -> DataFrame t (ds Dims.+: n)
          -> f (DataFrame t (ds Dims.+: n))
index (I# i) f d = write <$> f x
  where
    (# offD, lengthD, arrD #) = NCommons.toBytes d
    elSize = NCommons.elementByteSize d
    x = NCommons.fromBytes (# offD +# tds *# (i -# 1#), tds , arrD #)
    tds = case Dims.totalDim (Proxy @ds) of I# q -> q
    write y = case NCommons.toBytes y of
      (# offX, lengthX, arrX #) -> case runRW#
       (\s0 -> case newByteArray# (lengthD *# elSize) s0 of
         (# s1, marr #) -> case copyByteArray# arrD (offD *# elSize)
                                               marr 0#
                                               (lengthD *# elSize) s1 of
           s2 -> case copyByteArray# arrX (offX *# elSize)
                                     marr (lengthX *# elSize *# (i -# 1#))
                                     (lengthX *# elSize) s2 of
             s3 -> unsafeFreezeByteArray# marr s3
       ) of (# _, r #) -> NCommons.fromBytes (# 0#, lengthD, r #)
{-# INLINE [2] index #-}

{-# RULES
"index/Const"   forall i (f :: DataFrame t ds -> Const a (DataFrame t ds)
                         ) .  index i f = indexC i f
"slice/Fold"    forall i (f :: Monoid m
                             => a -> DataFrame t ods -> Const m (DataFrame t nds)
                         ) . slice i f = sliceF i f
"subSpace/Fold" forall   (f :: Monoid m
                             => a -> DataFrame t ods -> Const m (DataFrame t nds)
                         ) . subSpace f = subSpaceF f
  #-}

indexC :: forall (a :: Type) (t :: Type) (n :: Nat) (ds :: [Nat])
           . ( Dims.Dimensions ds
             , Dims.Dimensions (ds Dims.+: n)
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ds)
             , NCommons.PrimBytes (DataFrame t (ds Dims.+: n))
             )
          => Int
          -> (DataFrame t ds -> Const a (DataFrame t ds))
          -> DataFrame t (ds Dims.+: n)
          -> Const a (DataFrame t (ds Dims.+: n))
indexC (I# i) f d = Const . getConst $ f x
  where
    (# offD, _, arrD #) = NCommons.toBytes d
    x = NCommons.fromBytes (# offD +# tds *# (i -# 1#), tds , arrD #)
    tds = case Dims.totalDim (Proxy @ds) of I# q -> q


type NumSpace t ds = ( NCommons.PrimBytes (DataFrame t ds)
                     , Dims.Dimensions ds
                     , NCommons.ElementWise (Dims.Idx ds) t (DataFrame t ds)
                     , Eq (DataFrame t ds)
                     , Ord (DataFrame t ds)
                     , Num (DataFrame t ds)
                     , Fractional (DataFrame t ds)
                     , Floating (DataFrame t ds)
                     )

-- class ( asbs ~ (as Dims.++ bs)
--       , as ~ Dims.Take (Dims.Length asbs - Dims.Length bs) asbs
--       , bs ~ Dims.Drop (Dims.Length as) asbs
--       , Dims.Dimensions as
--       , Dims.Dimensions bs
--       , Dims.Dimensions asbs
--       , NCommons.PrimBytes (DataFrame t as)
--       , NCommons.PrimBytes (DataFrame t asbs)
--       ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
--                                  | asbs as -> bs
--                                  , asbs bs -> as
--                                  , as bs -> asbs where
--   mapDim :: Dims.Dim bs
--          -> (DataFrame t as -> DataFrame t as)
--          -> DataFrame t asbs
--          -> DataFrame t asbs
--   foldDim :: Monoid m
--           => Dims.Dim bs
--           -> (DataFrame t as -> m)
--           -> DataFrame t asbs
--           -> m
--   (!..) :: Dims.Idx bs -> DataFrame t (as Dims.++ bs) -> DataFrame t as
-- infixr 4 !..

-- mapDims :: forall t t' as as' bs
--          . ( SubSpace t as bs (as Dims.++ bs)
--            , SubSpace t' as' bs (as' Dims.++ bs)
--            )
--         => Dims.Dim bs
--         -> (DataFrame t as -> DataFrame t' as')
--         -> DataFrame t (as Dims.++ bs)
--         -> DataFrame t' (as' Dims.++ bs)
-- mapDims _ f df = case (# NCommons.toBytes df
--                        , Dims.totalDim (Proxy @as)
--                        , Dims.totalDim (Proxy @(as' Dims.++ bs)) #) of
--     (# (# off, len1, arr #), I# l1#, I# len2 #) -> case runRW#
--        ( \s0 -> case newByteArray# (len2 *# elS2) s0 of
--            (# s1, marr #) -> case go off (off +# len1) l1# 0# arr marr s1 of
--                s2 -> unsafeFreezeByteArray# marr s2
--        ) of (# _, r #) -> NCommons.fromBytes (# 0#, len2 , r #)
--   where
--     elS2 = NCommons.elementByteSize (undefined :: DataFrame t' (as' Dims.++ bs))
--     go pos1 lim1 step1 pos2 arr marr s
--       | isTrue# (pos1 >=# lim1) = s
--       | otherwise = case NCommons.toBytes (f (NCommons.fromBytes (# pos1, step1, arr #))) of
--          (# offX, step2, arrX #) -> go (pos1 +# step1) lim1 step1 (pos2 +# step2) arr marr
--            (copyByteArray# arrX (offX *# elS2) marr (pos2 *# elS2) (step2 *# elS2) s)
--
-- instance ( asbs ~ (as Dims.++ bs)
--          , as ~ Dims.Take (Dims.Length asbs - Dims.Length bs) asbs
--          , bs ~ Dims.Drop (Dims.Length as) asbs
--          , Dims.Dimensions as
--          , Dims.Dimensions bs
--          , Dims.Dimensions asbs
--          , NCommons.PrimBytes (DataFrame t as)
--          , NCommons.PrimBytes (DataFrame t asbs)
--          ) => SubSpace t as bs asbs where
--   mapDim _ f df = case (# NCommons.toBytes df, Dims.totalDim (Proxy @as) #) of
--       (# (# off, len, arr #), I# l# #) -> case runRW#
--          ( \s0 -> case newByteArray# (len *# elS) s0 of
--              (# s1, marr #) -> case go off (off +# len) l# arr marr s1 of
--                  s2 -> unsafeFreezeByteArray# marr s2
--          ) of (# _, r #) -> NCommons.fromBytes (# 0#, len, r #)
--     where
--       elS = NCommons.elementByteSize df
--       go pos lim step arr marr s
--         | isTrue# (pos >=# lim) = s
--         | otherwise = case NCommons.toBytes (f (NCommons.fromBytes (# pos, step, arr #))) of
--            (# offX, _, arrX #) -> go (pos +# step) lim step arr marr
--              (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (step *# elS) s)
--
--   foldDim _ f df = case (# NCommons.toBytes df, Dims.totalDim ( Proxy @as) #) of
--       (# (# off, len, arr #), I# l# #) -> go off (off +# len) l# arr mempty
--     where
--       go pos lim step arr acc
--         | isTrue# (pos >=# lim) = acc
--         | otherwise = go (pos +# step) lim step arr
--             (acc `mappend` f (NCommons.fromBytes (# pos, step, arr #)) )
--
--   i !.. d = r
--     where
--       r = case (# NCommons.toBytes d, Dims.fromIdx i, Dims.totalDim r #) of
--             (# (# off, _, arr #), I# i#, I# l# #)
--               -> NCommons.fromBytes (# off +# i# *# l#, l#, arr #)

-- ff = runSlice . slice Every
--                 . slice (Get 4 :& 7 :& 4)
--                 $ slice (Get 1 :& 2) (const Just)

-- fancyFunc :: forall (t :: Type) (f :: Type -> Type)
--                     (n :: Nat) (m :: Nat)
--                     (oldRems :: [Nat]) (newRems :: [Nat])
--                     (oldIds :: [Nat])  (newIds :: [Nat])
--            . (Applicative f)
--       => Slice n m
--       -> (Dims.Idx (m Dims.:+ newIds) -> DataFrame t oldRems -> f (DataFrame t newRems))
--       -> Idx newIds
--       -> DataFrame t ((oldRems Dims.+: n) Dims.++ oldIds)
--       -> f (DataFrame t ((newRems Dims.+: m) Dims.++ newIds))
-- fancyFunc _slice _map _ids _oldDF = undefined

--------------------------------------------------------------------------------
-- * Scalar type
--------------------------------------------------------------------------------

type Scalar t = DataFrame t ('[] :: [Nat])
type Scf = Scalar Float

-- | Convert scalar back to ordinary type
unScalar :: Scalar t -> t
unScalar = AFam._unScalar . _unArray . _getDF

-- | Convert any type to scalar wrapper
scalar :: t -> Scalar t
scalar = KnownDataFrame . Array . AFam.Scalar



--------------------------------------------------------------------------------
-- * Vector type
--------------------------------------------------------------------------------

type Vector t (n :: Nat) = DataFrame t '[n]

type Vec2f = Vector Float 2
type Vec3f = Vector Float 3
type Vec4f = Vector Float 4


-- | Scalar product -- sum of Vecs' components products,
--                     propagated into whole Vec
(.*.) :: ( Num t
         , Num (Vector t n)
         , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
         )
      => Vector t n -> Vector t n -> Vector t n
(.*.) a b = NCommons.broadcast . NCommons.ewfold (const (+)) 0 $ a * b
infixl 7 .*.

-- | Scalar product -- sum of Vecs' components products -- a scalar
dot :: ( Num t
       , Num (Vector t n)
       , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
       , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
       )
    => Vector t n -> Vector t n -> Scalar t
dot a b = NCommons.broadcast . NCommons.ewfold (const (+)) 0 $ a * b

-- | Dot product of two vectors
infixl 7 ·
(·) :: ( Num t
       , Num (Vector t n)
       , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
       , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
       )
    => Vector t n -> Vector t n -> Scalar t
(·) = dot
{-# INLINE (·) #-}


-- | Sum of absolute values
normL1 :: ( Num t
          , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
          , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
          )
       => Vector t n -> Scalar t
normL1 = NCommons.broadcast . NCommons.ewfold (const (\a -> (abs a +))) 0

-- | hypot function (square root of squares)
normL2 :: ( Floating t
          , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
          , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
          )
       => Vector t n -> Scalar t
normL2 = NCommons.broadcast . sqrt . NCommons.ewfold (const (\a -> (a*a +))) 0

-- | Maximum of absolute values
normLPInf :: ( Ord t, Num t
             , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
             , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
             )
          => Vector t n -> Scalar t
normLPInf = NCommons.broadcast . NCommons.ewfold (const (max . abs)) 0

-- | Minimum of absolute values
normLNInf :: ( Ord t, Num t
             , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
             , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
             )
          => Vector t n -> Scalar t
normLNInf x = NCommons.broadcast $ NCommons.ewfold (const (min . abs))
                                 (abs $ x NCommons.! (1 Dims.:! Dims.Z)) x

-- | Norm in Lp space
normLP :: ( Floating t
          , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
          , NCommons.ElementWise (Dims.Idx '[n]) t (Vector t n)
          )
       => Int -> Vector t n -> Scalar t
normLP i' = NCommons.broadcast . (**ri) . NCommons.ewfold (const (\a -> (a**i +))) 0
  where
    i  = fromIntegral i'
    ri = recip i
{-# INLINE [2] normLP #-}
{-# RULES
"normLP/L1" normLP 1 = normL1
"normLP/L2" normLP 2 = normL2
  #-}

-- | Compose a 2D vector
vec2 :: NCommons.ElementWise (Dims.Idx '[2]) t (Vector t 2) => t -> t -> Vector t 2
vec2 a b = NCommons.ewgen f
  where
    f (1 Dims.:! Dims.Z) = a
    f _ = b

-- | Dims.Take a determinant of a matrix composed from two 2D vectors.
--   Like a cross product in 2D.
det2 :: ( NCommons.ElementWise (Dims.Idx '[2]) t (Vector t 2)
        , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
        , Num t
        ) => Vector t 2 -> Vector t 2 -> Scalar t
det2 a b = NCommons.broadcast $ a NCommons.! (1 Dims.:! Dims.Z) * b NCommons.! (2 Dims.:! Dims.Z)
                     - a NCommons.! (2 Dims.:! Dims.Z) * b NCommons.! (1 Dims.:! Dims.Z)

-- | Compose a 3D vector
vec3 :: NCommons.ElementWise (Dims.Idx '[3]) t (Vector t 3) => t -> t -> t -> Vector t 3
vec3 a b c = NCommons.ewgen f
  where
    f (1 Dims.:! Dims.Z) = a
    f (2 Dims.:! Dims.Z) = b
    f _ = c

-- | Cross product
cross :: ( NCommons.ElementWise (Dims.Idx '[3]) t (Vector t 3)
         , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
         , Num t
         ) => Vector t 3 -> Vector t 3 -> Vector t 3
cross a b = vec3 ( a NCommons.! (2 Dims.:! Dims.Z) * b NCommons.! (3 Dims.:! Dims.Z)
                 - a NCommons.! (3 Dims.:! Dims.Z) * b NCommons.! (2 Dims.:! Dims.Z) )
                 ( a NCommons.! (3 Dims.:! Dims.Z) * b NCommons.! (1 Dims.:! Dims.Z)
                 - a NCommons.! (1 Dims.:! Dims.Z) * b NCommons.! (3 Dims.:! Dims.Z) )
                 ( a NCommons.! (1 Dims.:! Dims.Z) * b NCommons.! (2 Dims.:! Dims.Z)
                 - a NCommons.! (2 Dims.:! Dims.Z) * b NCommons.! (1 Dims.:! Dims.Z) )


-- | Cross product for two vectors in 3D
infixl 7 ×
(×) :: ( NCommons.ElementWise (Dims.Idx '[3]) t (Vector t 3)
       , NCommons.ElementWise (Dims.Idx '[]) t (Scalar t)
       , Num t
        ) => Vector t 3 -> Vector t 3 -> Vector t 3
(×) = cross
{-# INLINE (×) #-}


-- | Compose a 3D vector
vec4 :: NCommons.ElementWise (Dims.Idx '[4]) t (Vector t 4)
     => t -> t -> t -> t -> Vector t 4
vec4 a b c d = NCommons.ewgen f
  where
    f (1 Dims.:! Dims.Z) = a
    f (2 Dims.:! Dims.Z) = b
    f (3 Dims.:! Dims.Z) = c
    f _ = d


--------------------------------------------------------------------------------
-- * Matrix type
--------------------------------------------------------------------------------

type Matrix t (n :: Nat) (m :: Nat) = DataFrame t '[n,m]

-- Type abbreviations

type Mat22f = Matrix Float 2 2
type Mat32f = Matrix Float 3 2
type Mat42f = Matrix Float 4 2
type Mat23f = Matrix Float 2 3
type Mat33f = Matrix Float 3 3
type Mat43f = Matrix Float 4 3
type Mat24f = Matrix Float 2 4
type Mat34f = Matrix Float 3 4
type Mat44f = Matrix Float 4 4


-- | Compose a 2x2D matrix
mat22 :: ( NCommons.PrimBytes (Vector t 2)
         , NCommons.PrimBytes (Matrix t 2 2)
         )
      => Vector t 2 -> Vector t 2 -> Matrix t 2 2
mat22 = (<::>)

-- | Compose a 3x3D matrix
mat33 :: ( NCommons.PrimBytes (Vector t 3)
         , NCommons.PrimBytes (Matrix t 3 2)
         , NCommons.PrimBytes (Matrix t 3 3)
         )
      => Vector t 3 -> Vector t 3 -> Vector t 3 -> Matrix t 3 3
mat33 a b c = a <::> b <+:> c

-- | Compose a 4x4D matrix
mat44 :: ( NCommons.PrimBytes (Vector t 4)
         , NCommons.PrimBytes (Matrix t 4 2)
         , NCommons.PrimBytes (Matrix t 4 4)
         )
      => Vector t 4 -> Vector t 4 -> Vector t 4 -> Vector t 4 -> Matrix t 4 4
mat44 a b c d = (a <::>) b <:> (c <::> d)


-- | Matrix transpose
transpose :: ( KnownNat n
             , KnownNat m
             , M.MatrixCalculus t n m (Array t '[n,m])
             , M.MatrixCalculus t m n (Array t '[m,n])
             , NCommons.PrimBytes (Array t '[m,n])
             )
          => Matrix t n m -> Matrix t m n
transpose = KnownDataFrame . M.transpose . _getDF

-- | One on a diagonal, zero everywhere else
eye :: ( KnownNat n
       , M.SquareMatrixCalculus t n (Array t '[n,n])
       ) => Matrix t n n
eye = KnownDataFrame M.eye

-- | Single element on a diagonal, zero everywhere else
diag :: ( KnownNat n
        , M.SquareMatrixCalculus t n (Array t '[n,n])
        ) => Scalar t -> Matrix t n n
diag = KnownDataFrame . M.diag . unScalar

-- | Determinant of  Mat
det :: ( KnownNat n
       , M.SquareMatrixCalculus t n (Array t '[n,n])
       ) => Matrix t n n -> Scalar t
det = scalar . M.det . _getDF

-- | Sum of diagonal elements
trace :: ( KnownNat n
         , M.SquareMatrixCalculus t n (Array t '[n,n])
         ) => Matrix t n n -> Scalar t
trace = scalar . M.trace . _getDF


-- | Sum of diagonal elements
inverse :: ( KnownNat n
           , M.MatrixInverse (Array t '[n,n])
           ) => Matrix t n n -> Matrix t n n
inverse = KnownDataFrame . M.inverse . _getDF
