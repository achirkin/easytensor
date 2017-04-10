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
  , (<::>), (<+:>), (<:>)
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
  , M.MatrixInverse ()
  , NumericFrame, FPDataFrame, ElementDataType (..), EDTRefl (..)
  , inferNumeric, inferFloating -- , inferSubSpace
   -- * Modules for DataFrame operations
  , module Numeric.DataFrame.SubSpace
  , module Numeric.DataFrame.Contraction
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
import           Numeric.Dimensions
import qualified Numeric.Matrix.Class as M
import           Unsafe.Coerce

import           Numeric.DataFrame.Type
import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Contraction




--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------




-- | Append one DataFrame to another, adding up their last dimensionality
(<:>) :: forall (n :: Nat) (m :: Nat) (npm :: Nat) (ds :: [Nat])
                (t :: Type)
       . ( NCommons.PrimBytes (DataFrame t (ds +: n))
         , NCommons.PrimBytes (DataFrame t (ds +: m))
         , NCommons.PrimBytes (DataFrame t (ds +: npm))
         , npm ~ (n + m)
         , n   ~ (npm - m)
         , m   ~ (npm - n)
         )
        => DataFrame t (ds +: n)
        -> DataFrame t (ds +: m)
        -> DataFrame t (ds +: npm)
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
(<::>) :: forall (ds :: [Nat]) (t :: Type) (n :: Nat)
       .  ( NCommons.PrimBytes (DataFrame t ds)
          , NCommons.PrimBytes (DataFrame t ds)
          , NCommons.PrimBytes (DataFrame t (ds +: n :: [Nat]))
          , n ~ 2
          )
        => DataFrame t ds
        -> DataFrame t ds
        -> DataFrame t (ds +: n :: [Nat])
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
(<+:>) :: forall (ds :: [Nat]) (n :: Nat) (m :: Nat) (t :: Type)
        . ( NCommons.PrimBytes (DataFrame t (ds +: n))
          , NCommons.PrimBytes (DataFrame t ds)
          , NCommons.PrimBytes (DataFrame t (ds +: m))
          , m ~ (n + 1)
          )
        => DataFrame t (ds +: n)
        -> DataFrame t ds
        -> DataFrame t (ds +: m)
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
           . (Idx '[] -> DataFrame t ods -> f (DataFrame t nds))
          -> DataFrame t ods
          -> f (DataFrame t nds)
runSlice f = f Z

subSpace :: forall (t :: Type) (f :: Type -> Type) (n :: Nat)
                   (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
         . ( Applicative f
           , Dimensions ods
           , Dimensions nds
           , Dimensions (ods +: n)
           , Dimensions (nds +: n)
           , KnownNat n
           , NCommons.PrimBytes (DataFrame t ods)
           , NCommons.PrimBytes (DataFrame t nds)
           , NCommons.PrimBytes (DataFrame t (ods +: n))
           , NCommons.PrimBytes (DataFrame t (nds +: n))
           )
        -- slice
        => (Idx (n :+ remDs) -> DataFrame t ods -> f (DataFrame t nds))
        -- new transform
        -> Idx remDs
        -> DataFrame t (ods +: n)
        -> f (DataFrame t (nds +: n))
subSpace = case unsafeCoerce Refl :: (nds >: n) :~: (nds +: n) of
    Refl -> slice Every
{-# INLINE [2] subSpace #-}

slice :: forall (t :: Type) (s :: Type) (f :: Type -> Type)
                (o :: Nat) (n :: Nat)
                (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
           . ( Applicative f
             , Dimensions ods
             , Dimensions nds
             , Dimensions (ods +: o)
             , Dimensions (nds +: n)
             , Dimensions (nds >: n)
             , KnownNat o
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ods)
             , NCommons.PrimBytes (DataFrame s nds)
             , NCommons.PrimBytes (DataFrame t (ods +: o))
             , NCommons.PrimBytes (DataFrame s (nds >: n))
             )
          -- slice
          => Slice o n
          -- old transform
          -> (Idx (n :+ remDs) -> DataFrame t ods -> f (DataFrame s nds))
          -- new transform
          -> Idx remDs
          -> DataFrame t (ods +: o)
          -> f (DataFrame s (nds >: n))
slice s f remIds oldDF
    = merge <$> traverse (\(I# i#, i) -> f (i :!remIds)
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
    lengthOld' = case totalDim (Proxy @ods) of I# lo -> lo
    lengthNew  = case totalDim (Proxy @(nds >: n)) of I# ln -> ln
    bsNew      = lengthNew *# elSizeS
{-# INLINE [2] slice #-}

sliceF :: forall (t :: Type) (acc :: Type)
                 (o :: Nat) (n :: Nat)
                    (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
           . ( Monoid acc
             , Dimensions ods
             , Dimensions nds
             , Dimensions (ods +: o)
             , Dimensions (nds +: n)
             , KnownNat o
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ods)
             , NCommons.PrimBytes (DataFrame t nds)
             , NCommons.PrimBytes (DataFrame t (ods +: o))
             , NCommons.PrimBytes (DataFrame t (nds >: n))
             )
          -- slice
          => Slice o n
          -- old transform
          -> (Idx (n :+ remDs) -> DataFrame t ods -> Const acc (DataFrame t nds))
          -- new transform
          -> Idx remDs
          -> DataFrame t (ods +: o)
          -> Const acc (DataFrame t (nds >: n))
sliceF s f remIds oldDF
    = Const $ foldMap (\(I# i#, i) -> getConst $ f (i :!remIds)
                           (NCommons.fromBytes (# offOld +# (i# -# 1#) *# lengthOld'
                                       , lengthOld'
                                       , arrOld #))
                         ) (zip (slice2list s) [1..])
  where
    (# offOld, _, arrOld #) = NCommons.toBytes oldDF
    lengthOld' = case totalDim (Proxy @ods) of I# lo -> lo

subSpaceF :: forall (t :: Type) (acc :: Type) (n :: Nat)
                   (ods :: [Nat]) (nds :: [Nat]) (remDs :: [Nat])
         . ( Monoid acc
           , Dimensions ods
           , Dimensions nds
           , Dimensions (ods +: n)
           , Dimensions (nds +: n)
           , KnownNat n
           , NCommons.PrimBytes (DataFrame t ods)
           , NCommons.PrimBytes (DataFrame t nds)
           , NCommons.PrimBytes (DataFrame t (ods +: n))
           , NCommons.PrimBytes (DataFrame t (nds +: n))
           )
        -- slice
        => (Idx (n :+ remDs) -> DataFrame t ods -> Const acc (DataFrame t nds))
        -- new transform
        -> Idx remDs
        -> DataFrame t (ods +: n)
        -> Const acc (DataFrame t (nds +: n))
subSpaceF = case unsafeCoerce Refl :: (nds >: n) :~: (nds +: n) of
    Refl -> sliceF Every



slice2list :: KnownNat m => Slice n m -> [Int]
slice2list x@Every = [1..fromInteger (natVal x)]
slice2list (Get i) = [i]
slice2list xx@(x :& i) = slice2list (xx `f` unsafeCoerce x) ++ [i]
  where
    f :: a -> a -> a
    f _ = id

-- | Apply a functor over a single element in an outer dimension
index :: forall (t :: Type) (f :: Type -> Type) (n :: Nat) (ds :: [Nat])
           . ( Functor f
             , Dimensions ds
             , Dimensions (ds +: n)
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ds)
             , NCommons.PrimBytes (DataFrame t (ds +: n))
             )
          -- slice a single element in an outer dimension
          => Int
          -- old transform
          -> (DataFrame t ds -> f (DataFrame t ds))
          -- new transform
          -> DataFrame t (ds +: n)
          -> f (DataFrame t (ds +: n))
index (I# i) f d = write <$> f x
  where
    (# offD, lengthD, arrD #) = NCommons.toBytes d
    elSize = NCommons.elementByteSize d
    x = NCommons.fromBytes (# offD +# tds *# (i -# 1#), tds , arrD #)
    tds = case totalDim (Proxy @ds) of I# q -> q
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
           . ( Dimensions ds
             , Dimensions (ds +: n)
             , KnownNat n
             , NCommons.PrimBytes (DataFrame t ds)
             , NCommons.PrimBytes (DataFrame t (ds +: n))
             )
          => Int
          -> (DataFrame t ds -> Const a (DataFrame t ds))
          -> DataFrame t (ds +: n)
          -> Const a (DataFrame t (ds +: n))
indexC (I# i) f d = Const . getConst $ f x
  where
    (# offD, _, arrD #) = NCommons.toBytes d
    x = NCommons.fromBytes (# offD +# tds *# (i -# 1#), tds , arrD #)
    tds = case totalDim (Proxy @ds) of I# q -> q


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
         , NCommons.ElementWise (Idx '[n]) t (Vector t n)
         )
      => Vector t n -> Vector t n -> Vector t n
(.*.) a b = NCommons.broadcast . NCommons.ewfold (const (+)) 0 $ a * b
infixl 7 .*.

-- | Scalar product -- sum of Vecs' components products -- a scalar
dot :: ( Num t
       , Num (Vector t n)
       , NCommons.ElementWise (Idx '[]) t (Scalar t)
       , NCommons.ElementWise (Idx '[n]) t (Vector t n)
       )
    => Vector t n -> Vector t n -> Scalar t
dot a b = NCommons.broadcast . NCommons.ewfold (const (+)) 0 $ a * b

-- | Dot product of two vectors
infixl 7 ·
(·) :: ( Num t
       , Num (Vector t n)
       , NCommons.ElementWise (Idx '[]) t (Scalar t)
       , NCommons.ElementWise (Idx '[n]) t (Vector t n)
       )
    => Vector t n -> Vector t n -> Scalar t
(·) = dot
{-# INLINE (·) #-}


-- | Sum of absolute values
normL1 :: ( Num t
          , NCommons.ElementWise (Idx '[]) t (Scalar t)
          , NCommons.ElementWise (Idx '[n]) t (Vector t n)
          )
       => Vector t n -> Scalar t
normL1 = NCommons.broadcast . NCommons.ewfold (const (\a -> (abs a +))) 0

-- | hypot function (square root of squares)
normL2 :: ( Floating t
          , NCommons.ElementWise (Idx '[]) t (Scalar t)
          , NCommons.ElementWise (Idx '[n]) t (Vector t n)
          )
       => Vector t n -> Scalar t
normL2 = NCommons.broadcast . sqrt . NCommons.ewfold (const (\a -> (a*a +))) 0

-- | Maximum of absolute values
normLPInf :: ( Ord t, Num t
             , NCommons.ElementWise (Idx '[]) t (Scalar t)
             , NCommons.ElementWise (Idx '[n]) t (Vector t n)
             )
          => Vector t n -> Scalar t
normLPInf = NCommons.broadcast . NCommons.ewfold (const (max . abs)) 0

-- | Minimum of absolute values
normLNInf :: ( Ord t, Num t
             , NCommons.ElementWise (Idx '[]) t (Scalar t)
             , NCommons.ElementWise (Idx '[n]) t (Vector t n)
             )
          => Vector t n -> Scalar t
normLNInf x = NCommons.broadcast $ NCommons.ewfold (const (min . abs))
                                 (abs $ x NCommons.! (1 :! Z)) x

-- | Norm in Lp space
normLP :: ( Floating t
          , NCommons.ElementWise (Idx '[]) t (Scalar t)
          , NCommons.ElementWise (Idx '[n]) t (Vector t n)
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
vec2 :: NCommons.ElementWise (Idx '[2]) t (Vector t 2) => t -> t -> Vector t 2
vec2 a b = NCommons.ewgen f
  where
    f (1 :! Z) = a
    f _ = b

-- | Take a determinant of a matrix composed from two 2D vectors.
--   Like a cross product in 2D.
det2 :: ( NCommons.ElementWise (Idx '[2]) t (Vector t 2)
        , NCommons.ElementWise (Idx '[]) t (Scalar t)
        , Num t
        ) => Vector t 2 -> Vector t 2 -> Scalar t
det2 a b = NCommons.broadcast $ a NCommons.! (1 :! Z) * b NCommons.! (2 :! Z)
                     - a NCommons.! (2 :! Z) * b NCommons.! (1 :! Z)

-- | Compose a 3D vector
vec3 :: NCommons.ElementWise (Idx '[3]) t (Vector t 3) => t -> t -> t -> Vector t 3
vec3 a b c = NCommons.ewgen f
  where
    f (1 :! Z) = a
    f (2 :! Z) = b
    f _ = c

-- | Cross product
cross :: ( NCommons.ElementWise (Idx '[3]) t (Vector t 3)
         , NCommons.ElementWise (Idx '[]) t (Scalar t)
         , Num t
         ) => Vector t 3 -> Vector t 3 -> Vector t 3
cross a b = vec3 ( a NCommons.! (2 :! Z) * b NCommons.! (3 :! Z)
                 - a NCommons.! (3 :! Z) * b NCommons.! (2 :! Z) )
                 ( a NCommons.! (3 :! Z) * b NCommons.! (1 :! Z)
                 - a NCommons.! (1 :! Z) * b NCommons.! (3 :! Z) )
                 ( a NCommons.! (1 :! Z) * b NCommons.! (2 :! Z)
                 - a NCommons.! (2 :! Z) * b NCommons.! (1 :! Z) )


-- | Cross product for two vectors in 3D
infixl 7 ×
(×) :: ( NCommons.ElementWise (Idx '[3]) t (Vector t 3)
       , NCommons.ElementWise (Idx '[]) t (Scalar t)
       , Num t
        ) => Vector t 3 -> Vector t 3 -> Vector t 3
(×) = cross
{-# INLINE (×) #-}


-- | Compose a 3D vector
vec4 :: NCommons.ElementWise (Idx '[4]) t (Vector t 4)
     => t -> t -> t -> t -> Vector t 4
vec4 a b c d = NCommons.ewgen f
  where
    f (1 :! Z) = a
    f (2 :! Z) = b
    f (3 :! Z) = c
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
mat44 :: forall (t :: Type)
       . ( NCommons.PrimBytes (Vector t (4 :: Nat))
         , NCommons.PrimBytes (Matrix t (4 :: Nat) (2 :: Nat))
         , NCommons.PrimBytes (Matrix t (4 :: Nat) (4 :: Nat))
         )
      => Vector t (4 :: Nat) -> Vector t (4 :: Nat) -> Vector t (4 :: Nat) -> Vector t (4 :: Nat)
      -> Matrix t (4 :: Nat) (4 :: Nat)
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



-- | If instance of Dimensions ds is available, I must provide a way
--   to bring instances of DataFrame into scope.
--   For example, Ord, Num, Eq, and SubSpace t '[] ds ds must be there for any ds.
type NumericFrame (t :: Type) (ds :: [Nat]) =
  ( Show (DataFrame t ds)
  , Eq (DataFrame t ds)
  , Ord (DataFrame t ds)
  , Num (DataFrame t ds)
  , SubSpace t '[] ds ds
  , ElementDataType t
  )

-- | NumericFrame t ds plus Fractional and Floating instances
type FPDataFrame (t :: Type) (ds :: [Nat]) =
  ( NumericFrame t ds
  , Fractional (DataFrame t ds)
  , Floating (DataFrame t ds)
  )


-- | Lookup a proper instance for our typeclasses at runtime
inferNumeric :: forall (x :: Type) (t :: Type) (as :: [Nat])
              . ( Dimensions as
                , NumericFrame t '[]
                )
             => DataFrame t as
             -> ( forall (bs :: [Nat]) . (NumericFrame t bs, as ~ bs) => DataFrame t bs -> x )
             -> x
inferNumeric x f = case (dim @as, edtRefl (Proxy @t)) of
    (D, _) -> f x
    (_ :* D, EDTFloat) -> f x
    (_ :* _ :* _, EDTFloat) -> f x


-- | Lookup a proper instance for our typeclasses at runtime
inferFloating :: forall (x :: Type) (t :: Type) (as :: [Nat])
              . ( Dimensions as
                , FPDataFrame t '[]
                )
              => DataFrame t as
              -> ( forall (bs :: [Nat]) . (FPDataFrame t bs, as ~ bs) => DataFrame t bs -> x )
              -> x
inferFloating x f = case (dim @as, edtRefl (Proxy @t)) of
    (D, _) -> f x
    (_ :* D, EDTFloat) -> f x
    (_ :* _ :* _, EDTFloat) -> f x




--inferSubSpace :: forall (x :: Type) (t :: Type) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
--                        (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
--              . ( ConcatList as bs asbs
--                , Dimensions asbs
--                , FiniteDims as
--                , KnownList as
--                , SubSpace t '[] asbs asbs
--                , ElementDataType t
--                )
--             => p as
--             -> q bs
--             -> DataFrame t asbs
--             -> ( ( Dimensions as
--                  , Dimensions bs
--                  , SubSpace t as bs asbs
--                  ) => Dim as -> Dim bs -> DataFrame t asbs -> x
--                )
--             -> x
---- inferSubSpace D D x f = f D D x
---- inferSubSpace D bs x f = case ( unsafeCoerce Refl :: bs :~: asbs) of Refl -> f D bs x
---- inferSubSpace as D x f = case ( unsafeCoerce Refl :: as :~: asbs
----                               ) of Refl -> f as D x
--inferSubSpace as0 bs0 x f = inferSubDimensions as0 bs0 $ \as1 bs1 ->  case (edtRefl (Proxy @t), as1, bs1) of
--    (_, D, D) ->  case ( unsafeEqProof :: asbs :~: '[] ) of Refl -> f D D x
--    (_, D, bs) -> case ( unsafeEqProof :: bs :~: asbs) of Refl -> f D bs x
--    (_, as, D) -> case ( unsafeEqProof :: as :~: asbs) of Refl -> f as D x
--    (EDTFloat, as@(_:*_), bs@(_:*_)) -> f as bs x
