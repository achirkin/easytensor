{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.Matrix.Mat44f () where

import qualified Control.Monad.ST as ST
import GHC.Exts
import Numeric.DataFrame.Internal.Array.Family.FloatX3
import Numeric.DataFrame.Internal.Array.Family.FloatX4
import qualified Numeric.DataFrame.ST as ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Matrix.Class
import Numeric.Scalar
import Numeric.Vector

{-# INLINE mat44f #-}
mat44f ::
  Scf -> Scf -> Scf -> Scf ->
  Scf -> Scf -> Scf -> Scf ->
  Scf -> Scf -> Scf -> Scf ->
  Scf -> Scf -> Scf -> Scf ->
  Mat44f
mat44f
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
 
instance HomTransform4 Float where
  {-# INLINE translate4 #-}
  translate4 (SingleFrame (FloatX4# x# y# z# _)) = mat44f
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1
    where
      x = scalar $ F# x#
      y = scalar $ F# y#
      z = scalar $ F# z#

  {-# INLINE translate3 #-}
  translate3 (SingleFrame (FloatX3# x# y# z#)) = mat44f
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1
    where
      x = scalar $ F# x#
      y = scalar $ F# y#
      z = scalar $ F# z#

  {-# INLINE rotateX #-}
  rotateX a = mat44f
    1 0 0 0
    0 c n 0
    0 s c 0
    0 0 0 1
    where
      c = scalar $ cos a
      s = scalar $ sin a
      n = -s

  {-# INLINE rotateY #-}
  rotateY a = mat44f
    c 0 s 0
    0 1 0 0
    n 0 c 0
    0 0 0 1
    where
      c = scalar $ cos a
      s = scalar $ sin a
      n = -s

  {-# INLINE rotateZ #-}
  rotateZ a = mat44f
    c n 0 0
    s c 0 0
    0 0 1 0
    0 0 0 1
    where
      c = scalar $ cos a
      s = scalar $ sin a
      n = -s

  {-# INLINE rotate #-}
  rotate (SingleFrame (FloatX3# x# y# z#)) a = mat44f
    (c+xxv)  (xyv-zs) (xzv+ys) 0
    (yxv+zs) (c+yyv)  (yzv-xs) 0
    (zxv-ys) (zyv+xs) (c+zzv)  0
    0        0        0        1
    where
      c = scalar $ cos a
      v = 1 - c -- v for versine
      s = scalar $ sin a
      x = scalar $ F# x#
      y = scalar $ F# y#
      z = scalar $ F# z#
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
  rotateEuler x y z = mat44f
    (cy*cz)  (cx*sz+sx*sy*cz) (sx*sz-cx*sy*cz) 0
    (-cy*sz) (cx*cz-sx*sy*sz) (sx*cz+cx*sy*sz) 0
    sy       (-sx*cy)         (cx*cy)          0
    0        0                0                1
    where
      cx = scalar $ cos x
      sx = scalar $ sin x
      cy = scalar $ cos y
      sy = scalar $ sin y
      cz = scalar $ cos z
      sz = scalar $ sin z

  {-# INLINE lookAt #-}
  lookAt up cam foc = mat44f
    (xb!1) (xb!2) (xb!3) tx
    (yb!1) (yb!2) (yb!3) ty
    (zb!1) (zb!2) (zb!3) tz
    0      0      0      1
    where
      xb = normalized $ up `cross` zb
      yb = zb `cross` xb
      zb = normalized $ foc - cam
      ncam = -cam
      tx = xb `dot` ncam
      ty = yb `dot` ncam
      tz = zb `dot` ncam

  {-# INLINE perspective #-}
  perspective n f fovy aspect = mat44f
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
  orthogonal n f w h = mat44f
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
  toHomPoint (SingleFrame (FloatX3# x# y# z#)) = SingleFrame (FloatX4# x# y# z# 1.0#)

  {-# INLINE toHomVector #-}
  toHomVector (SingleFrame (FloatX3# x# y# z#)) = SingleFrame (FloatX4# x# y# z# 0.0#)

  {-# INLINE fromHom #-}
  fromHom (SingleFrame (FloatX4# x# y# z# 0.0#)) = SingleFrame (FloatX3# x# y# z#)
  fromHom (SingleFrame (FloatX4# x# y# z# w#)) = SingleFrame (FloatX3# (x# `divideFloat#` w#) (y# `divideFloat#` w#) (z# `divideFloat#` w#))
