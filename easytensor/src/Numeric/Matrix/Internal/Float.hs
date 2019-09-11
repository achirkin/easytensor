{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Matrix.Internal.Float () where

import qualified Control.Monad.ST        as ST
import qualified Numeric.DataFrame.ST    as ST
import           Numeric.DataFrame.Type  (inferKnownBackend)
import           Numeric.Dimensions      (Dict (..), KnownDim)
import           Numeric.Matrix.Internal
import           Numeric.Matrix.LU
import           Numeric.Scalar.Internal
import           Numeric.Vector.Internal

{-# INLINE mkMat #-}
mkMat ::
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  Mat44f
mkMat
  _11 _12 _13 _14
  _21 _22 _23 _24
  _31 _32 _33 _34
  _41 _42 _43 _44
  = ST.runST $ do
    df <- ST.newDataFrame
    ST.writeDataFrameOff df 0  $ scalar _11
    ST.writeDataFrameOff df 1  $ scalar _12
    ST.writeDataFrameOff df 2  $ scalar _13
    ST.writeDataFrameOff df 3  $ scalar _14
    ST.writeDataFrameOff df 4  $ scalar _21
    ST.writeDataFrameOff df 5  $ scalar _22
    ST.writeDataFrameOff df 6  $ scalar _23
    ST.writeDataFrameOff df 7  $ scalar _24
    ST.writeDataFrameOff df 8  $ scalar _31
    ST.writeDataFrameOff df 9  $ scalar _32
    ST.writeDataFrameOff df 10 $ scalar _33
    ST.writeDataFrameOff df 11 $ scalar _34
    ST.writeDataFrameOff df 12 $ scalar _41
    ST.writeDataFrameOff df 13 $ scalar _42
    ST.writeDataFrameOff df 14 $ scalar _43
    ST.writeDataFrameOff df 15 $ scalar _44
    ST.unsafeFreezeDataFrame df

instance HomTransform4 Float where
  {-# INLINE translate4 #-}
  translate4 (unpackV4# -> (# x, y, z, _ #)) = mkMat
    1 0 0 0
    0 1 0 0
    0 0 1 0
    x y z 1

  {-# INLINE translate3 #-}
  translate3 (unpackV3# -> (# x, y, z #)) = mkMat
    1 0 0 0
    0 1 0 0
    0 0 1 0
    x y z 1

  {-# INLINE rotateX #-}
  rotateX a = mkMat
    1 0 0 0
    0 c s 0
    0 n c 0
    0 0 0 1
    where
      c = cos a
      s = sin a
      n = -s

  {-# INLINE rotateY #-}
  rotateY a = mkMat
    c 0 n 0
    0 1 0 0
    s 0 c 0
    0 0 0 1
    where
      c = cos a
      s = sin a
      n = -s

  {-# INLINE rotateZ #-}
  rotateZ a = mkMat
    c s 0 0
    n c 0 0
    0 0 1 0
    0 0 0 1
    where
      c = cos a
      s = sin a
      n = -s

  {-# INLINE rotate #-}
  rotate (unpackV3# -> (# x, y, z #)) a = mkMat
    (c+xxv)  (yxv+zs) (zxv-ys) 0
    (xyv-zs) (c+yyv)  (zyv+xs) 0
    (xzv+ys) (yzv-xs) (c+zzv)  0
     0        0        0       1
    where
      c = cos a
      v = 1 - c -- v for versine
      s = sin a
      xxv = x * x * v
      xyv = x * y * v
      xzv = x * z * v
      yxv = xyv
      yyv = y * y * v
      yzv = y * z * v
      zxv = xzv
      zyv = yzv
      zzv = z * z * v
      xs = x * s
      ys = y * s
      zs = z * s

  {-# INLINE rotateEuler #-}
  rotateEuler x y z = mkMat
    (cy*cz)  (cx*sz+sx*sy*cz) (sx*sz-cx*sy*cz) 0
    (-cy*sz) (cx*cz-sx*sy*sz) (sx*cz+cx*sy*sz) 0
     sy      (-sx*cy)         (cx*cy)          0
     0        0                0               1
    where
      cx = cos x
      sx = sin x
      cy = cos y
      sy = sin y
      cz = cos z
      sz = sin z

  {-# INLINE lookAt #-}
  lookAt up cam foc = mkMat
    xb1 yb1 zb1 0
    xb2 yb2 zb2 0
    xb3 yb3 zb3 0
    tx  ty  tz  1
    where
      (# xb1, xb2, xb3 #) = unpackV3# xb
      (# yb1, yb2, yb3 #) = unpackV3# yb
      (# zb1, zb2, zb3 #) = unpackV3# zb
      zb = normalized $ cam - foc -- Basis vector for "backward", since +Z is behind the camera
      xb = normalized $ up `cross` zb -- Basis vector for "right"
      yb = zb `cross` xb -- Basis vector for "up"
      ncam = -cam
      tx = unScalar $ xb `dot` ncam
      ty = unScalar $ yb `dot` ncam
      tz = unScalar $ zb `dot` ncam

  {-# INLINE perspective #-}
  perspective n f fovy aspect = mkMat
    dpw 0   0   0
    0   dph 0   0
    0   0   a (-1)
    0   0   b   0
    where
      hpd = tan (fovy * 0.5) -- height/distance
      wpd = aspect * hpd; -- width/distance
      dph = recip hpd -- distance/height
      dpw = recip wpd -- distance/width
      nmf = n - f
      a = (n + f) / nmf
      b = 2 * n * f / nmf

  {-# INLINE orthogonal #-}
  orthogonal n f w h = mkMat
    iw 0  0 0
    0  ih 0 0
    0  0  a 0
    0  0  b 1
    where
      ih = 2 / h
      iw = 2 / w
      nmf = n - f
      a = 2 / nmf
      b = (n + f) / nmf

  {-# INLINE toHomPoint #-}
  toHomPoint (unpackV3# -> (# x, y, z #)) = vec4 x y z 1

  {-# INLINE toHomVector #-}
  toHomVector (unpackV3# -> (# x, y, z #)) = vec4 x y z 0

  {-# INLINE fromHom #-}
  fromHom (unpackV4# -> (# x, y, z, w #))
    | w == 0    = vec3 x y z
    | otherwise = vec3 (x/w) (y/w) (z/w)

instance KnownDim n => MatrixInverse Float n where
    inverse
      | Dict <- inferKnownBackend @_ @Float @'[n]
      = inverseViaLU

instance KnownDim n => MatrixDeterminant Float n where
    det = detViaLU
