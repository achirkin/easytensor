{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Matrix.Mat44d () where

import qualified Control.Monad.ST as ST
import GHC.Exts
import Numeric.DataFrame.Internal.Array.Family.DoubleX3
import Numeric.DataFrame.Internal.Array.Family.DoubleX4
import qualified Numeric.DataFrame.ST as ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Matrix.Class
import Numeric.Scalar
import Numeric.Vector

{-# INLINE mat44d #-}
mat44d ::
  Scd -> Scd -> Scd -> Scd ->
  Scd -> Scd -> Scd -> Scd ->
  Scd -> Scd -> Scd -> Scd ->
  Scd -> Scd -> Scd -> Scd ->
  Mat44d
mat44d
  _11 _12 _13 _14
  _21 _22 _23 _24
  _31 _32 _33 _34
  _41 _42 _43 _44
  = ST.runST $ do
    df <- ST.newDataFrame
    ST.writeDataFrameOff df 0 _11
    ST.writeDataFrameOff df 1 _21
    ST.writeDataFrameOff df 2 _31
    ST.writeDataFrameOff df 3 _41
    ST.writeDataFrameOff df 4 _12
    ST.writeDataFrameOff df 5 _22
    ST.writeDataFrameOff df 6 _32
    ST.writeDataFrameOff df 7 _42
    ST.writeDataFrameOff df 8 _13
    ST.writeDataFrameOff df 9 _23
    ST.writeDataFrameOff df 10 _33
    ST.writeDataFrameOff df 11 _43
    ST.writeDataFrameOff df 12 _14
    ST.writeDataFrameOff df 13 _24
    ST.writeDataFrameOff df 14 _34
    ST.writeDataFrameOff df 15 _44
    ST.unsafeFreezeDataFrame df
 
instance HomTransform4 Double where
  {-# INLINE translate4 #-}
  translate4 (SingleFrame (DoubleX4# x# y# z# _)) = mat44d
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1
    where
      x = scalar $ D# x#
      y = scalar $ D# y#
      z = scalar $ D# z#

  {-# INLINE translate3 #-}
  translate3 (SingleFrame (DoubleX3# x# y# z#)) = mat44d
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1
    where
      x = scalar $ D# x#
      y = scalar $ D# y#
      z = scalar $ D# z#

  {-# INLINE rotateX #-}
  rotateX a = mat44d
    1 0 0 0
    0 c n 0
    0 s c 0
    0 0 0 1
    where
      c = scalar $ cos a
      s = scalar $ sin a
      n = -s

  {-# INLINE rotateY #-}
  rotateY a = mat44d
    c 0 s 0
    0 1 0 0
    n 0 c 0
    0 0 0 1
    where
      c = scalar $ cos a
      s = scalar $ sin a
      n = -s

  {-# INLINE rotateZ #-}
  rotateZ a = mat44d
    c n 0 0
    s c 0 0
    0 0 1 0
    0 0 0 1
    where
      c = scalar $ cos a
      s = scalar $ sin a
      n = -s

  {-# INLINE rotate #-}
  rotate (SingleFrame (DoubleX3# x# y# z#)) a = mat44d
    (c+xxv)  (xyv-zs) (xzv+ys) 0
    (yxv+zs) (c+yyv)  (yzv-xs) 0
    (zxv-ys) (zyv+xs) (c+zzv)  0
    0        0        0        1
    where
      c = scalar $ cos a
      v = 1 - c -- v for versine
      s = scalar $ sin a
      x = scalar $ D# x#
      y = scalar $ D# y#
      z = scalar $ D# z#
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
  rotateEuler x y z = mat44d
    (cy*cz)          (-cy*sz)         sy       0
    (cx*sz+sx*sy*cz) (cx*cz-sx*sy*sz) (-sx*cy) 0
    (sx*sz-cx*sy*cz) (sx*cz+cx*sy*sz) (cx*cy)  0
    0                0                0        1
    where
      cx = scalar $ cos x
      sx = scalar $ sin x
      cy = scalar $ cos y
      sy = scalar $ sin y
      cz = scalar $ cos z
      sz = scalar $ sin z

  {-# INLINE lookAt #-}
  lookAt up cam foc = mat44d
    (xb!1) (xb!2) (xb!3) tx
    (yb!1) (yb!2) (yb!3) ty
    (zb!1) (zb!2) (zb!3) tz
    0      0      0      1
    where
      zb = normalized $ cam - foc -- Basis vector for "backward", since +Z is behind the camera
      xb = normalized $ up `cross` zb -- Basis vector for "right"
      yb = zb `cross` xb -- Basis vector for "up"
      ncam = -cam
      tx = xb `dot` ncam
      ty = yb `dot` ncam
      tz = zb `dot` ncam

  {-# INLINE perspective #-}
  perspective n f fovy aspect = mat44d
    dpw 0   0    0
    0   dph 0    0
    0   0   a    b
    0   0   (-1) 0
    where
      hpd = tan (fovy * 0.5) -- height/distance
      wpd = aspect * hpd; -- width/distance
      dph = scalar $ 1 / hpd -- distance/height
      dpw = scalar $ 1 / wpd -- distance/width
      nmf = n - f
      a = scalar $ (n + f) / nmf
      b = scalar $ 2 * n * f / nmf

  {-# INLINE orthogonal #-}
  orthogonal n f w h = mat44d
    iw 0  0 0
    0  ih 0 0
    0  0  a b
    0  0  0 1
    where
      ih = scalar $ 2 / h
      iw = scalar $ 2 / w
      nmf = n - f
      a = scalar $ 2 / nmf
      b = scalar $ (n + f) / nmf

  {-# INLINE toHomPoint #-}
  toHomPoint (SingleFrame (DoubleX3# x# y# z#)) = SingleFrame (DoubleX4# x# y# z# 1.0##)

  {-# INLINE toHomVector #-}
  toHomVector (SingleFrame (DoubleX3# x# y# z#)) = SingleFrame (DoubleX4# x# y# z# 0.0##)

  {-# INLINE fromHom #-}
  fromHom (SingleFrame (DoubleX4# x# y# z# 0.0##)) = SingleFrame (DoubleX3# x# y# z#)
  fromHom (SingleFrame (DoubleX4# x# y# z# w#)) = SingleFrame (DoubleX3# (x# /## w#) (y# /## w#) (z# /## w#))
