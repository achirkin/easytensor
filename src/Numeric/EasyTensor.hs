{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.EasyTensor
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This module generalizes matrices and vectors.
-- Yet it is limited to rank 2, allowing for a simple and nicely type-checked interface.
--
--
--
-----------------------------------------------------------------------------

module Numeric.EasyTensor
  ( Tensor ()
  -- * Common operations
  , fill
  , prod, (%*)
  , inverse, transpose
  , (<:>), (//), (\\)
  , index, indexCol, indexRow, dimN, dimM
  , (V..*.), dot, (·), normL1,  normL2, normLPInf, normLNInf, normLP
  , eye, diag, det, trace
  , toDiag, toDiag', fromDiag, fromDiag'
  -- * Type abbreviations
  , Mat, Vec, Vec'
  , Vec2f, Vec3f, Vec4f
  , Vec2f', Vec3f', Vec4f'
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  -- * Simplified type constructors
  , scalar, vec2, vec3, vec4, vec2', vec3', vec4'
  , mat22, mat33, mat44
  -- * Some low-dimensional operations
  , det2, det2', cross, (×)
  ) where

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Numeric.Vector as V
import qualified Numeric.Matrix as M
import qualified Numeric.Matrix.Class as M
import Numeric.Commons



newtype Tensor t n m = Tensor { _unT :: TT t n m }
instance Show (TT t n m) => Show (Tensor t n m) where
  show (Tensor t) = show t
deriving instance Eq (TT t n m) => Eq (Tensor t n m)
deriving instance Ord (TT t n m) => Ord (Tensor t n m)
deriving instance Num (TT t n m) => Num (Tensor t n m)
deriving instance Fractional (TT t n m) => Fractional (Tensor t n m)
deriving instance Floating (TT t n m) => Floating (Tensor t n m)
deriving instance V.VectorCalculus t n (TT t n 1) => V.VectorCalculus t n (Tensor t n 1)
deriving instance V.VectorCalculus t m (TT t 1 m) => V.VectorCalculus t m (Tensor t 1 m)
deriving instance M.MatrixCalculus t n m (TT t n m) => M.MatrixCalculus t n m (Tensor t n m)
deriving instance M.SquareMatrixCalculus t n (TT t n n) => M.SquareMatrixCalculus t n (Tensor t n n)
deriving instance M.MatrixInverse (TT t n n) => M.MatrixInverse (Tensor t n n)
deriving instance PrimBytes (TT t n m) => PrimBytes (Tensor t n m)
deriving instance FloatBytes (TT t n m) => FloatBytes (Tensor t n m)
deriving instance DoubleBytes (TT t n m) => DoubleBytes (Tensor t n m)
deriving instance IntBytes (TT t n m) => IntBytes (Tensor t n m)
deriving instance WordBytes (TT t n m) => WordBytes (Tensor t n m)




newtype Scalar t = Scalar { _unScalar :: t }
  deriving ( Bounded, Enum, Eq, Integral, Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes, FloatBytes, DoubleBytes, IntBytes, WordBytes)
instance Show t => Show (Scalar t) where
  show (Scalar t) = "{ " ++ show t ++ " }"

newtype CoVector t n = CoVector {_unCoVec :: V.Vector t n}
instance Show (V.Vector t n) => Show (CoVector t n) where
  show (CoVector t) = show t
deriving instance Eq (V.Vector t n) => Eq (CoVector t n)
deriving instance Ord (V.Vector t n) => Ord (CoVector t n)
deriving instance Num (V.Vector t n) => Num (CoVector t n)
deriving instance Fractional (V.Vector t n) => Fractional (CoVector t n)
deriving instance Floating (V.Vector t n) => Floating (CoVector t n)
deriving instance PrimBytes (V.Vector t n) => PrimBytes (CoVector t n)
deriving instance V.VectorCalculus t n (V.Vector t n) => V.VectorCalculus t n (CoVector t n)
deriving instance FloatBytes (V.Vector t n) => FloatBytes (CoVector t n)
deriving instance DoubleBytes (V.Vector t n) => DoubleBytes (CoVector t n)
deriving instance IntBytes (V.Vector t n) => IntBytes (CoVector t n)
deriving instance WordBytes (V.Vector t n) => WordBytes (CoVector t n)

newtype ContraVector t n = ContraVector {_unContraVec :: V.Vector t n}
instance Show (V.Vector t n) => Show (ContraVector t n) where
  show (ContraVector t) = show t ++ "'"
deriving instance Eq (V.Vector t n) => Eq (ContraVector t n)
deriving instance Ord (V.Vector t n) => Ord (ContraVector t n)
deriving instance Num (V.Vector t n) => Num (ContraVector t n)
deriving instance Fractional (V.Vector t n) => Fractional (ContraVector t n)
deriving instance Floating (V.Vector t n) => Floating (ContraVector t n)
deriving instance PrimBytes (V.Vector t n) => PrimBytes (ContraVector t n)
deriving instance V.VectorCalculus t n (V.Vector t n) => V.VectorCalculus t n (ContraVector t n)
deriving instance FloatBytes (V.Vector t n) => FloatBytes (ContraVector t n)
deriving instance DoubleBytes (V.Vector t n) => DoubleBytes (ContraVector t n)
deriving instance IntBytes (V.Vector t n) => IntBytes (ContraVector t n)
deriving instance WordBytes (V.Vector t n) => WordBytes (ContraVector t n)

newtype Matrix t n m = Matrix (M.Matrix t n m)
instance Show (M.Matrix t n m) => Show (Matrix t n m) where
  show (Matrix t) = show t
deriving instance Eq (M.Matrix t n m) => Eq (Matrix t n m)
deriving instance Ord (M.Matrix t n m) => Ord (Matrix t n m)
deriving instance Num (M.Matrix t n m) => Num (Matrix t n m)
deriving instance Fractional (M.Matrix t n m) => Fractional (Matrix t n m)
deriving instance Floating (M.Matrix t n m) => Floating (Matrix t n m)
deriving instance PrimBytes (M.Matrix t n m) => PrimBytes (Matrix t n m)
deriving instance M.MatrixCalculus t n m (M.Matrix t n m) => M.MatrixCalculus t n m (Matrix t n m)
deriving instance M.SquareMatrixCalculus t n (M.Matrix t n n) => M.SquareMatrixCalculus t n (Matrix t n n)
deriving instance M.MatrixInverse (M.Matrix t n n) => M.MatrixInverse (Matrix t n n)
deriving instance FloatBytes (M.Matrix t n m) => FloatBytes (Matrix t n m)
deriving instance DoubleBytes (M.Matrix t n m) => DoubleBytes (Matrix t n m)
deriving instance IntBytes (M.Matrix t n m) => IntBytes (Matrix t n m)
deriving instance WordBytes (M.Matrix t n m) => WordBytes (Matrix t n m)




type family TT t (n :: Nat) (m :: Nat) = v | v -> t n m where
  TT t 1 1 = Scalar t
  TT t n 1 = ContraVector t n
  TT t 1 m = CoVector t m
  TT t n m = Matrix t n m

-- | Fill whole tensor with a single value
fill :: M.MatrixCalculus t n m (Tensor t n m) => Tensor t 1 1 -> Tensor t n m
fill = M.broadcastMat . _unScalar . _unT
{-# INLINE fill #-}

-- | Get an element of a tensor
index :: M.MatrixCalculus t n m (Tensor t n m) => Int -> Int -> Tensor t n m -> Tensor t 1 1
index i j = Tensor . Scalar . M.indexMat i j
{-# INLINE index #-}

-- | Get a column vector of a matrix
indexCol :: ( M.MatrixCalculus t n m (Tensor t n m)
            , V.VectorCalculus t n (Tensor t n 1)
            , PrimBytes (Tensor t n 1)
            )
         => Int -> Tensor t n m -> Tensor t n 1
indexCol = M.indexCol
{-# INLINE indexCol #-}

-- | Get a row vector of a matrix
indexRow :: ( M.MatrixCalculus t n m (Tensor t n m)
            , V.VectorCalculus t m (Tensor t 1 m)
            , PrimBytes (Tensor t 1 m)
            )
         => Int -> Tensor t n m -> Tensor t 1 m
indexRow = M.indexRow
{-# INLINE indexRow #-}

transpose :: ( M.MatrixCalculus t n m (Tensor t n m)
             , M.MatrixCalculus t m n (Tensor t m n)
             , PrimBytes (Tensor t m n)
             ) => Tensor t n m -> Tensor t m n
transpose = M.transpose
{-# INLINE transpose #-}


dimN :: ( M.MatrixCalculus t n m (Tensor t n m)) => Tensor t n m -> Int
dimN = M.dimN
{-# INLINE dimN #-}

dimM :: ( M.MatrixCalculus t n m (Tensor t n m)) => Tensor t n m -> Int
dimM = M.dimM
{-# INLINE dimM #-}

-- | Matrix product for tensors rank 2, as well matrix-vector or vector-matrix products
infixl 7 %*
(%*) :: (M.MatrixProduct (Tensor t n m) (Tensor t m k) (Tensor t n k))
     => Tensor t n m -> Tensor t m k -> Tensor t n k
(%*) = prod
{-# INLINE (%*) #-}

-- | Matrix product for tensors rank 2, as well matrix-vector or vector-matrix products
prod :: (M.MatrixProduct (Tensor t n m) (Tensor t m k) (Tensor t n k))
     => Tensor t n m -> Tensor t m k -> Tensor t n k
prod = M.prod
{-# INLINE prod #-}

-- | Divide on the right: R = A * B^(-1)
(//) :: ( M.MatrixProduct (Tensor t n m) (Tensor t m m) (Tensor t n m)
        , M.MatrixInverse (TT t m m))
     => Tensor t n m -> Tensor t m m -> Tensor t n m
(//) a b = prod a (inverse b)
{-# INLINE (//) #-}

-- | Divide on the left: R = A^(-1) * b
(\\) :: ( M.MatrixProduct (Tensor t n n) (Tensor t n m) (Tensor t n m)
        , M.MatrixInverse (TT t n n))
     => Tensor t n n -> Tensor t n m -> Tensor t n m
(\\) a b = prod (inverse a) b
{-# INLINE (\\) #-}


-- | Matrix inverse
inverse :: M.MatrixInverse (Tensor t n n) => Tensor t n n -> Tensor t n n
inverse = M.inverse
{-# INLINE inverse #-}


instance ( FloatBytes (Tensor Float n m)
         , FloatBytes (Tensor Float m k)
         , PrimBytes (Tensor Float n k)
         , M.MatrixCalculus Float n m (Tensor Float n m)
         , M.MatrixCalculus Float m k (Tensor Float m k)
         )
      => M.MatrixProduct (Tensor Float n m) (Tensor Float m k) (Tensor Float n k) where
  prod x y = case (dimN x, dimM x, dimM y) of
    ( I# n, I# m, I# k) -> M.prodF n m k x y
  {-# INLINE prod #-}

instance ( DoubleBytes (Tensor Double n m)
         , DoubleBytes (Tensor Double m k)
         , PrimBytes (Tensor Double n k)
         , M.MatrixCalculus Double n m (Tensor Double n m)
         , M.MatrixCalculus Double m k (Tensor Double m k)
         )
      => M.MatrixProduct (Tensor Double n m) (Tensor Double m k) (Tensor Double n k) where
  prod x y = case (dimN x, dimM x, dimM y) of
    ( I# n, I# m, I# k) -> M.prodD n m k x y
  {-# INLINE prod #-}



-- | Append one vector to another, adding up their dimensionality
(<:>) :: ( PrimBytes (Tensor t k n)
         , PrimBytes (Tensor t k m)
         , PrimBytes (Tensor t k (n + m))
         )
        => Tensor t k n -> Tensor t k m -> Tensor t k (n + m)
a <:> b = case (# toBytes a, toBytes b, byteSize a, byteSize b #) of
  (# arr1, arr2, n, m #) -> case runRW#
     ( \s0 -> case newByteArray# (n +# m) s0 of
         (# s1, marr #) -> case copyByteArray# arr1 0# marr 0# n s1 of
           s2 -> case copyByteArray# arr2 0# marr n m s2 of
             s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> fromBytes r
infixl 5 <:>

-- Simple types

type Mat t n m = Tensor t n m
type Vec t n = Tensor t n 1
type Vec' t m = Tensor t 1 m


-- Even Sympler types

type Vec2f = Tensor Float 2 1
type Vec3f = Tensor Float 3 1
type Vec4f = Tensor Float 4 1
type Vec2f' = Tensor Float 1 2
type Vec3f' = Tensor Float 1 3
type Vec4f' = Tensor Float 1 4


type Mat22f = Tensor Float 2 2
type Mat32f = Tensor Float 3 2
type Mat42f = Tensor Float 4 2
type Mat23f = Tensor Float 2 3
type Mat33f = Tensor Float 3 3
type Mat43f = Tensor Float 4 3
type Mat24f = Tensor Float 2 4
type Mat34f = Tensor Float 3 4
type Mat44f = Tensor Float 4 4


-- construct tensors

scalar :: t -> Tensor t 1 1
scalar = Tensor . Scalar

vec2 :: V.Vector2D t => t -> t -> Tensor t 2 1
vec2 a b = Tensor . ContraVector $ V.vec2 a b

vec2' :: V.Vector2D t => t -> t -> Tensor t 1 2
vec2' a b = Tensor . CoVector $ V.vec2 a b

vec3 :: V.Vector3D t => t -> t -> t -> Tensor t 3 1
vec3 a b c = Tensor . ContraVector $ V.vec3 a b c

vec3' :: V.Vector3D t => t -> t -> t -> Tensor t 1 3
vec3' a b c = Tensor . CoVector $ V.vec3 a b c

vec4 :: V.Vector4D t => t -> t -> t -> t -> Tensor t 4 1
vec4 a b c d = Tensor . ContraVector $ V.vec4 a b c d

vec4' :: V.Vector4D t => t -> t -> t -> t -> Tensor t 1 4
vec4' a b c d = Tensor . CoVector $ V.vec4 a b c d



-- | Compose a 2x2D matrix
mat22 :: M.Matrix2x2 t => Tensor t 2 1 -> Tensor t 2 1 -> Tensor t 2 2
mat22 (Tensor (ContraVector a)) (Tensor (ContraVector b)) = Tensor . Matrix $ M.mat22 a b

-- | Compose a 3x3D matrix
mat33 :: ( PrimBytes (Tensor t 3 3)
         , PrimBytes (Tensor t 3 2)
         , PrimBytes (Tensor t 3 1)
         )
      => Tensor t 3 1 -> Tensor t 3 1 -> Tensor t 3 1 -> Tensor t 3 3
mat33 a b c = a <:> b <:> c

-- | Compose a 4x4D matrix
mat44 :: ( PrimBytes (Tensor t 4 4)
         , PrimBytes (Tensor t 4 3)
         , PrimBytes (Tensor t 4 2)
         , PrimBytes (Tensor t 4 1)
         )
      => Tensor t 4 1 -> Tensor t 4 1 -> Tensor t 4 1 -> Tensor t 4 1 -> Tensor t 4 4
mat44 a b c d = a <:> b <:> c <:> d

-- useful low-dimensional functions

det2 :: V.Vector2D t => Tensor t 2 1 -> Tensor t 2 1 -> Tensor t 1 1
det2 (Tensor (ContraVector a)) (Tensor (ContraVector b)) = Tensor . Scalar $ V.det2 a b

det2' :: V.Vector2D t => Tensor t 1 2 -> Tensor t 1 2 -> Tensor t 1 1
det2' (Tensor (CoVector a)) (Tensor (CoVector b)) = Tensor . Scalar $ V.det2 a b

-- | Cross product for two vectors in 3D
infixl 7 ×
(×) :: V.Vector3D t => Tensor t 3 1 -> Tensor t 3 1 -> Tensor t 3 1
(×) = cross
{-# INLINE (×) #-}

-- | Cross product for two vectors in 3D
cross :: V.Vector3D t => Tensor t 3 1 -> Tensor t 3 1 -> Tensor t 3 1
cross (Tensor (ContraVector a)) (Tensor (ContraVector b)) = Tensor . ContraVector $ V.cross a b
{-# INLINE cross #-}


-- re-use functions provided by Vector and Matrix Calculus

-- | Dot product of two vectors
infixl 7 ·
(·) :: V.VectorCalculus t n v => v -> v -> Tensor t 1 1
(·) = dot
{-# INLINE (·) #-}


-- | Dot product of two vectors
dot :: V.VectorCalculus t n v => v -> v -> Tensor t 1 1
dot a b = Tensor . Scalar $ V.dot a b
{-# INLINE dot #-}

-- | Sum of absolute values
normL1 :: V.VectorCalculus t n v => v -> Tensor t 1 1
normL1 = Tensor . Scalar . V.normL1
{-# INLINE normL1 #-}

-- | hypot function (square root of squares)
normL2 :: V.VectorCalculus t n v => v -> Tensor t 1 1
normL2 = Tensor . Scalar . V.normL2
{-# INLINE normL2 #-}

-- | Maximum of absolute values
normLPInf :: V.VectorCalculus t n v => v -> Tensor t 1 1
normLPInf = Tensor . Scalar . V.normLPInf
{-# INLINE normLPInf #-}

-- | Minimum of absolute values
normLNInf :: V.VectorCalculus t n v => v -> Tensor t 1 1
normLNInf = Tensor . Scalar . V.normLNInf
{-# INLINE normLNInf #-}

-- | Norm in Lp space
normLP :: V.VectorCalculus t n v => Int -> v -> Tensor t 1 1
normLP p = Tensor . Scalar . V.normLP p
{-# INLINE normLP #-}

-- | Identity matrix. Mat with 1 on diagonal and 0 elsewhere
eye :: M.SquareMatrixCalculus t n (Tensor t n n) => Tensor t n n
eye = M.eye
{-# INLINE eye #-}

-- | Put the same value on the Mat diagonal, 0 otherwise
diag :: M.SquareMatrixCalculus t n (Tensor t n n) => Tensor t 1 1 -> Tensor t n n
diag = M.diag . _unScalar . _unT
{-# INLINE diag #-}

-- | Determinant of  Mat
det :: M.SquareMatrixCalculus t n (Tensor t n n) => Tensor t n n -> Tensor t 1 1
det = Tensor . Scalar . M.det
{-# INLINE det #-}

-- | Sum of diagonal elements
trace :: M.SquareMatrixCalculus t n (Tensor t n n) => Tensor t n n -> Tensor t 1 1
trace = Tensor . Scalar . M.trace
{-# INLINE trace #-}

-- | Get the diagonal elements from Mat into Vec
fromDiag :: ( M.SquareMatrixCalculus t n (Tensor t n n)
            , V.VectorCalculus t n (Tensor t n 1)
            , PrimBytes (Tensor t n 1)
            )
         => Tensor t n n -> Tensor t n 1
fromDiag = M.fromDiag
{-# INLINE fromDiag #-}

-- | Get the diagonal elements from Mat into Vec
fromDiag' :: ( M.SquareMatrixCalculus t n (Tensor t n n)
            , V.VectorCalculus t n (Tensor t 1 n)
            , PrimBytes (Tensor t 1 n)
            )
         => Tensor t n n -> Tensor t 1 n
fromDiag' = M.fromDiag
{-# INLINE fromDiag' #-}

-- | Set Vec values into the diagonal elements of Mat
toDiag :: ( M.SquareMatrixCalculus t n (Tensor t n n)
          , V.VectorCalculus t n (Tensor t n 1)
          , PrimBytes (Tensor t n 1)
          )
       => Tensor t n 1 -> Tensor t n n
toDiag = M.toDiag
{-# INLINE toDiag #-}

-- | Set Vec values into the diagonal elements of Mat
toDiag' :: ( M.SquareMatrixCalculus t n (Tensor t n n)
           , V.VectorCalculus t n (Tensor t 1 n)
           , PrimBytes (Tensor t 1 n)
           )
        => Tensor t 1 n -> Tensor t n n
toDiag' = M.toDiag
{-# INLINE toDiag' #-}


-- missing instances


instance Num t => V.VectorCalculus t 1 (Scalar t) where
  broadcastVec = Scalar
  {-# INLINE broadcastVec #-}
  (.*.) = (*)
  {-# INLINE (.*.) #-}
  dot a = _unScalar . (a *)
  {-# INLINE dot #-}
  indexVec 1 = _unScalar
  indexVec i = const . error $ "Bad index " ++ show i ++ " for a scalar"
  {-# INLINE indexVec #-}
  normL1 = _unScalar . abs
  {-# INLINE normL1 #-}
  normL2 = _unScalar . abs
  {-# INLINE normL2 #-}
  normLPInf = _unScalar
  {-# INLINE normLPInf #-}
  normLNInf = _unScalar
  {-# INLINE normLNInf #-}
  normLP _ = _unScalar . abs
  {-# INLINE normLP #-}
  dim _ = 1
  {-# INLINE dim #-}

instance Num t => M.MatrixCalculus t 1 1 (Scalar t) where
  broadcastMat = Scalar
  {-# INLINE broadcastMat #-}
  indexMat 1 1 = _unScalar
  indexMat i j = const . error $ "Bad index (" ++ show i ++ ", " ++ show j ++ ") for a scalar"
  {-# INLINE indexMat #-}
  transpose = unsafeCoerce
  {-# INLINE transpose #-}
  dimN _ = 1
  {-# INLINE dimN #-}
  dimM _ = 1
  {-# INLINE dimM #-}
  indexCol 1 = unsafeCoerce
  indexCol j = const . error $ "Bad column index " ++ show j ++ " for a scalar"
  {-# INLINE indexCol #-}
  indexRow 1 = unsafeCoerce
  indexRow i = const . error $ "Bad row index " ++ show i ++ " for a scalar"
  {-# INLINE indexRow #-}



instance Num t => M.SquareMatrixCalculus t 1 (Scalar t) where
  eye = Scalar 1
  {-# INLINE eye #-}
  diag = Scalar
  {-# INLINE diag #-}
  det = _unScalar
  {-# INLINE det #-}
  trace = _unScalar
  {-# INLINE trace #-}
  fromDiag = unsafeCoerce
  {-# INLINE fromDiag #-}
  toDiag = unsafeCoerce
  {-# INLINE toDiag #-}

instance Fractional t => M.MatrixInverse (Scalar t) where
  inverse = Scalar . recip . _unScalar


instance (KnownNat n, V.VectorCalculus t n (V.Vector t n)) => M.MatrixCalculus t n 1 (ContraVector t n) where
  broadcastMat = ContraVector . V.broadcastVec
  {-# INLINE broadcastMat #-}
  indexMat i 1 (ContraVector v) = V.indexVec i v
  indexMat i j (ContraVector v) = error $ "Bad index (" ++ show i ++ ", " ++ show j ++ ") for a " ++ show (V.dim v) ++ "x1D matrix"
  {-# INLINE indexMat #-}
  transpose (ContraVector v) = unsafeCoerce $ CoVector v
  {-# INLINE transpose #-}
  dimN = V.dim . _unContraVec
  {-# INLINE dimN #-}
  dimM _ = 1
  {-# INLINE dimM #-}
  indexCol 1 (ContraVector v) = unsafeCoerce $ ContraVector v
  indexCol j (ContraVector v) = error $ "Bad column index " ++ show j ++ " for a " ++ show (V.dim v) ++ "x1D matrix"
  {-# INLINE indexCol #-}
  indexRow i = unsafeCoerce . Scalar . V.indexVec i . _unContraVec
  {-# INLINE indexRow #-}


instance (KnownNat m, V.VectorCalculus t m (V.Vector t m)) => M.MatrixCalculus t 1 m (CoVector t m) where
  broadcastMat = CoVector . V.broadcastVec
  {-# INLINE broadcastMat #-}
  indexMat 1 i (CoVector v) = V.indexVec i v
  indexMat j i (CoVector v) = error $ "Bad index (" ++ show j ++ ", " ++ show i ++ ") for a 1x" ++ show (V.dim v) ++ "D matrix"
  {-# INLINE indexMat #-}
  transpose (CoVector v) = unsafeCoerce $ ContraVector v
  {-# INLINE transpose #-}
  dimN _ = 1
  {-# INLINE dimN #-}
  dimM = V.dim . _unCoVec
  {-# INLINE dimM #-}
  indexCol i = unsafeCoerce . Scalar . V.indexVec i . _unCoVec
  {-# INLINE indexCol #-}
  indexRow 1 x = unsafeCoerce x
  indexRow j (CoVector v) = error $ "Bad column index " ++ show j ++ " for a 1x" ++ show (V.dim v) ++ "D matrix"
  {-# INLINE indexRow #-}


