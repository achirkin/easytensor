{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Numeric.Vector.Internal
    ( -- * Type aliases
      Vector
    , Vec2f, Vec3f, Vec4f, Vec2d, Vec3d, Vec4d
    , Vec2i, Vec3i, Vec4i, Vec2w, Vec3w, Vec4w
      -- * Vector constructors
    , Vector2 (..), Vector3 (..), Vector4 (..)
    , DataFrame(Vec2, Vec3, Vec4)
    ) where

import           Numeric.DataFrame.Family
import           Numeric.DataFrame.SubSpace
import           Numeric.Dimensions
import           Numeric.Scalar



type Vector (t :: l) (n :: k) = DataFrame t '[n]

type Vec2f = Vector Float 2
type Vec3f = Vector Float 3
type Vec4f = Vector Float 4
type Vec2d = Vector Double 2
type Vec3d = Vector Double 3
type Vec4d = Vector Double 4
type Vec2i = Vector Int 2
type Vec3i = Vector Int 3
type Vec4i = Vector Int 4
type Vec2w = Vector Word 2
type Vec3w = Vector Word 3
type Vec4w = Vector Word 4


pattern Vec4 :: Vector4 t => t -> t -> t -> t -> Vector t 4
pattern Vec4 a b c d <- (unpackV4# -> (# a, b, c, d #))
  where
    Vec4 = vec4
{-# COMPLETE Vec4 #-}

pattern Vec3 :: Vector3 t => t -> t -> t -> Vector t 3
pattern Vec3 a b c <- (unpackV3# -> (# a, b, c #))
  where
    Vec3 = vec3
{-# COMPLETE Vec3 #-}

pattern Vec2 :: Vector2 t => t -> t -> Vector t 2
pattern Vec2 a b <- (unpackV2# -> (# a, b #))
  where
    Vec2 = vec2
{-# COMPLETE Vec2 #-}

-- | Packing and unpacking 2D vectors
class Vector2 t where
  -- | Compose a 2D vector
  vec2 :: t -> t -> Vector t 2
  -- | Unpack 2D vector elements
  unpackV2# :: Vector t 2 -> (# t, t #)

-- | Packing and unpacking 3D vectors
class Vector3 t where
  -- | Compose a 3D vector
  vec3 :: t -> t -> t -> Vector t 3
  -- | Unpack 3D vector elements
  unpackV3# :: Vector t 3 -> (# t, t, t #)

-- | Packing and unpacking 4D vectors
class Vector4 t where
  -- | Compose a 4D vector
  vec4 :: t -> t -> t -> t -> Vector t 4
  -- | Unpack 4D vector elements
  unpackV4# :: Vector t 4 -> (# t, t, t, t #)

instance {-# OVERLAPPABLE #-} SubSpace t '[2] '[] '[2] => Vector2 t where
  vec2 a b = iwgen f
    where
      f (0 :* U) = scalar a
      f _        = scalar b
  {-# INLINE vec2 #-}
  unpackV2# v =
    (# unScalar (indexOffset# 0# v)
     , unScalar (indexOffset# 1# v) #)
  {-# INLINE unpackV2# #-}

instance {-# OVERLAPPABLE #-} SubSpace t '[3] '[] '[3] => Vector3 t where
  vec3 a b c = iwgen f
    where
      f (0 :* U) = scalar a
      f (1 :* U) = scalar b
      f _        = scalar c
  {-# INLINE vec3 #-}
  unpackV3# v =
    (# unScalar (indexOffset# 0# v)
     , unScalar (indexOffset# 1# v)
     , unScalar (indexOffset# 2# v) #)
  {-# INLINE unpackV3# #-}

instance {-# OVERLAPPABLE #-} SubSpace t '[4] '[] '[4] => Vector4 t where
  vec4 a b c d = iwgen f
    where
      f (0 :* U) = scalar a
      f (1 :* U) = scalar b
      f (2 :* U) = scalar c
      f _        = scalar d
  {-# INLINE vec4 #-}
  unpackV4# v =
    (# unScalar (indexOffset# 0# v)
     , unScalar (indexOffset# 1# v)
     , unScalar (indexOffset# 2# v)
     , unScalar (indexOffset# 3# v) #)
  {-# INLINE unpackV4# #-}
