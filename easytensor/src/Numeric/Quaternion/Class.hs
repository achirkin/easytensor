{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Quaternion.Class
    ( Quaternion (..)
    ) where

import Numeric.Matrix (Matrix)
import Numeric.Vector (Vector)

-- | Quaternion operations
class Quaternion t where
    -- | Quaternion data type. The ordering of coordinates is @x y z w@,
    --   where @w@ is an argument, and @x y z@ are components of a 3D vector
    data Quater t
    -- | Set the quaternion in format (x,y,z,w)
    setQ :: t -> t -> t -> t -> Quater t
    -- | Set the quaternion from 3D axis vector and argument
    fromVecNum :: Vector t 3 -> t -> Quater t
    -- | Set the quaternion from 4D vector in format (x,y,z,w)
    fromVec4 :: Vector t 4 -> Quater t
    -- | Transform the quaternion to 4D vector in format (x,y,z,w)
    toVec4 :: Quater t -> Vector t 4
    -- | Get the values of the quaternion in format (x,y,z,w)
    unpackQ :: Quater t -> (t,t,t,t)
    -- | Get scalar square of the quaternion
    square :: Quater t -> t
    -- | Imagine part of quaternion (orientation vector)
    im :: Quater t -> Quater t
    -- | Real part of the quaternion
    re :: Quater t -> Quater t
    -- | Get imagenery 3D vector of the quaternion
    imVec :: Quater t -> Vector t 3
    -- | Real part of the quaternion into number
    taker :: Quater t -> t
    -- | i-th component
    takei :: Quater t -> t
    -- | j-th component
    takej :: Quater t -> t
    -- | k-th component
    takek :: Quater t -> t
    -- | Conjugate quaternion (negate imaginary part)
    conjugate :: Quater t -> Quater t
    -- | Rotates and scales vector in 3D using quaternion.
    --   Let @q = (cos (a/2), sin (a/2) * v)@; then rotation angle is @a@, and axis of rotation is @v@.
    --   Scaling is proportional to @|V|^2@
    --   this is equivalent to q * x * (conjugate q)
    rotScale :: Quater t -> Vector t 3 -> Vector t 3
    -- | Creates a quaternion @q@ from two vectors @a@ and @b@.
    --   @rotScale q a == b@
    getRotScale :: Vector t 3 -> Vector t 3 -> Quater t
    -- | Creates a rotation versor from an axis vector and an angle in radians.
    axisRotation :: Vector t 3 -> t -> Quater t
    -- | quaternion rotation angle
    qArg :: Quater t -> t
    -- | Create a quaternion from a rotation matrix
    fromMatrix33 :: Matrix t 3 3 -> Quater t
    -- | Create a quaternion from a homogenious coordinates trasform matrix.
    --   Ignores matrix translation transform.
    fromMatrix44 :: Matrix t 4 4 -> Quater t
    -- | Create a rotation matrix from a quaternion
    toMatrix33 :: Quater t -> Matrix t 3 3
    -- | Create a homogenious coordinates trasform matrix from a quaternion.
    --   Translation of the output matrix is zero.
    toMatrix44 :: Quater t -> Matrix t 4 4
