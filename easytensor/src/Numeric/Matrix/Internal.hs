{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.Matrix.Internal
  ( MatrixTranspose (..)
  , SquareMatrix (..)
  , MatrixDeterminant (..)
  , MatrixInverse (..)
  , MatrixLU (..), LUFact (..)
  , Matrix
  , HomTransform4 (..)
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  , Mat22d, Mat23d, Mat24d
  , Mat32d, Mat33d, Mat34d
  , Mat42d, Mat43d, Mat44d
  ) where

import           Numeric.DataFrame.Family
import           Numeric.Dimensions       (Nat)
import           Numeric.Scalar
import           Numeric.Vector

-- | Alias for DataFrames of rank 2
type Matrix (t :: l) (n :: k) (m :: k) = DataFrame t '[n,m]

class MatrixTranspose t (n :: k) (m :: k) where
    -- | Transpose Mat
    transpose :: Matrix t n m -> Matrix t m n

class SquareMatrix t (n :: Nat) where
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: Matrix t n n
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: Scalar t -> Matrix t n n
    -- | Sum of diagonal elements
    trace :: Matrix t n n -> Scalar t

class MatrixDeterminant t (n :: Nat) where
    -- | Determinant of  Mat
    det :: Matrix t n n -> Scalar t

class MatrixInverse t (n :: Nat) where
    -- | Matrix inverse
    inverse :: Matrix t n n -> Matrix t n n


-- | Result of LU factorization with Partial Pivoting
--   @ PA = LU @.
data LUFact t n
  = LUFact
  { luLower    :: Matrix t n n
    -- ^ Lower triangular matrix @L@.
    --   All elements on the diagonal of @L@ equal @1@.
  , luUpper    :: Matrix t n n
    -- ^ Upper triangular matrix @U@
  , luPerm     :: Matrix t n n
    -- ^ Row permutation matrix @P@
  , luPermSign :: Scalar t
    -- ^ Sign of permutation @luPermSign == det . luPerm@
  }

deriving instance (Show (Matrix t n n), Show t) => Show (LUFact t n)
deriving instance (Eq (Matrix t n n), Eq t) => Eq (LUFact t n)

class MatrixLU t (n :: Nat) where
    -- | Compute LU factorization with Partial Pivoting
    lu :: Matrix t n n -> LUFact t n


-- | Operations on 4x4 transformation matrices and vectors in homogeneous coordinates.
--   All angles are specified in radians.
--
--   Note: since version 2 of @easytensor@, DataFrames and matrices are row-major.
--         A good SIMD implementation may drastically improve performance
--         of 4D vector-matrix products of the form @v %* m@, but not so much
--         for products of the form @m %* v@ (due to memory layout).
--         Thus, all operations here assume the former form to benefit more from
--         SIMD in future.
class HomTransform4 t where
    -- | Create a translation matrix from a vector.  The 4th coordinate is ignored.
    --
    --   If @p ! 3 == 1@ and @v ! 3 == 0@, then
    --
    --   > p %* translate4 v == p + v
    --
    translate4  :: Vector t 4 -> Matrix t 4 4
    -- | Create a translation matrix from a vector.
    --
    --   If @p ! 3 == 1@, then
    --
    --   > p %* translate3 v == p + toHomVector v
    --
    translate3  :: Vector t 3 -> Matrix t 4 4
    -- | Rotation matrix for a rotation around the X axis, angle is given in radians.
    --   e.g. @p %* rotateX (pi/2)@ rotates point @p@ around @Ox@ by 90 degrees.
    rotateX     :: t -> Matrix t 4 4
    -- | Rotation matrix for a rotation around the Y axis, angle is given in radians.
    --   e.g. @p %* rotateY (pi/2)@ rotates point @p@ around @Oy@ by 90 degrees.
    rotateY     :: t -> Matrix t 4 4
    -- | Rotation matrix for a rotation around the Z axis, angle is given in radians.
    --   e.g. @p %* rotateZ (pi/2)@ rotates point @p@ around @Oz@ by 90 degrees.
    rotateZ     :: t -> Matrix t 4 4
    -- | Rotation matrix for a rotation around an arbitrary normalized vector
    --   e.g. @p %* rotate (pi/2) v@ rotates point @p@ around @v@ by 90 degrees.
    rotate      :: Vector t 3 -> t -> Matrix t 4 4
    -- | Rotation matrix from the Euler angles roll (axis @Z@), yaw (axis @Y'@), and pitch (axis @X''@).
    --   This order is known as Tait-Bryan angles (@Z-Y'-X''@ intrinsic rotations), or nautical angles, or Cardan angles.
    --
    --   > rotateEuler pitch yaw roll == rotateZ roll %* rotateY yaw %* rotateX pitch
    --
    --   https://en.wikipedia.org/wiki/Euler_angles#Conventions_2
    rotateEuler :: t -- ^ pitch (axis @X''@)
                -> t -- ^ yaw (axis @Y'@)
                -> t -- ^ roll (axis @Z@)
                -> Matrix t 4 4
    -- | Create a transform matrix using up direction, camera position and a point to look at.
    --   Just the same as GluLookAt.
    lookAt      :: Vector t 3 -- ^ The up direction, not necessary unit length or perpendicular to the view vector
                -> Vector t 3 -- ^ The viewers position
                -> Vector t 3 -- ^ The point to look at
                -> Matrix t 4 4
    -- | A perspective symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
    --   http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
    perspective :: t -- ^ Near plane clipping distance (always positive)
                -> t -- ^ Far plane clipping distance (always positive)
                -> t -- ^ Field of view of the y axis, in radians
                -> t -- ^ Aspect ratio, i.e. screen's width\/height
                -> Matrix t 4 4
    -- | An orthogonal symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
    --   http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
    orthogonal  :: t -- ^ Near plane clipping distance
                -> t -- ^ Far plane clipping distance
                -> t -- ^ width
                -> t -- ^ height
                -> Matrix t 4 4
    -- | Add one more dimension and set it to 1.
    toHomPoint  :: Vector t 3 -> Vector t 4
    -- | Add one more dimension and set it to 0.
    toHomVector :: Vector t 3 -> Vector t 4
    -- | Transform a homogenous vector or point into a normal 3D vector.
    --   If the last coordinate is not zero, divide the rest by it.
    fromHom     :: Vector t 4 -> Vector t 3

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

type Mat22d = Matrix Double 2 2
type Mat32d = Matrix Double 3 2
type Mat42d = Matrix Double 4 2
type Mat23d = Matrix Double 2 3
type Mat33d = Matrix Double 3 3
type Mat43d = Matrix Double 4 3
type Mat24d = Matrix Double 2 4
type Mat34d = Matrix Double 3 4
type Mat44d = Matrix Double 4 4
