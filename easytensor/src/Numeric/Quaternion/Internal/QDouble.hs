{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.Internal.QDouble
    ( QDouble, Quater (..)
    ) where

import qualified Control.Monad.ST                     as ST
import           Data.Coerce                          (coerce)
import           Numeric.Basics
import           Numeric.DataFrame.Internal.PrimArray
import qualified Numeric.DataFrame.ST                 as ST
import           Numeric.DataFrame.Type
import           Numeric.PrimBytes                    (PrimBytes)
import           Numeric.Quaternion.Internal
import           Numeric.Scalar.Internal
import           Numeric.Vector.Internal

type QDouble = Quater Double

deriving instance PrimBytes (Quater Double)
deriving instance PrimArray Double (Quater Double)

instance Quaternion Double where
    newtype Quater Double = QDouble (Vector Double 4)
    {-# INLINE packQ #-}
    packQ = coerce (vec4 :: Double -> Double -> Double -> Double -> Vector Double 4)
    {-# INLINE unpackQ# #-}
    unpackQ# = coerce (unpackV4# :: Vector Double 4 -> (# Double, Double, Double, Double #))
    {-# INLINE fromVecNum #-}
    fromVecNum (unpackV3# -> (# x, y, z #))  = packQ x y z
    {-# INLINE fromVec4 #-}
    fromVec4 = coerce
    {-# INLINE toVec4 #-}
    toVec4 = coerce
    {-# INLINE square #-}
    square (unpackQ# -> (# x, y, z, w #)) = (x * x) + (y * y) + (z * z) + (w * w)
    {-# INLINE im #-}
    im (unpackQ# -> (# x, y, z, _ #)) = packQ x y z 0.0
    {-# INLINE re #-}
    re (unpackQ# -> (# _, _, _, w #)) = packQ 0 0 0 w
    {-# INLINE imVec #-}
    imVec (unpackQ# -> (# x, y, z, _ #)) = vec3 x y z
    {-# INLINE taker #-}
    taker (unpackQ# -> (# _, _, _, w #)) = w
    {-# INLINE takei #-}
    takei (unpackQ# -> (# x, _, _, _ #)) = x
    {-# INLINE takej #-}
    takej (unpackQ# -> (# _, y, _, _ #)) = y
    {-# INLINE takek #-}
    takek (unpackQ# -> (# _, _, z, _ #)) = z
    {-# INLINE conjugate #-}
    conjugate (unpackQ# -> (# x, y, z, w #))
      = packQ (negate x) (negate y) (negate z) w
    {-# INLINE rotScale #-}
    rotScale (unpackQ# -> (# i, j, k, t #))
             (unpackV3# -> (# x, y, z #))
      = let l = t*t - i*i - j*j - k*k
            d = 2.0 * ( i*x + j*y + k*z)
            t2 = t * 2.0
        in vec3 (l*x + d*i + t2 * (z*j - y*k))
                (l*y + d*j + t2 * (x*k - z*i))
                (l*z + d*k + t2 * (y*i - x*j))
    {-# INLINE getRotScale #-}
    getRotScale a b = case (# unpackV3# a, unpackV3# b #) of
      (# _, (# 0, 0, 0 #) #) -> packQ 0 0 0 0
      (# (# 0, 0, 0 #), _ #) -> let x = (1 / 0 :: Double) in packQ x x x x
      (# (# a1, a2, a3 #), (# b1, b2, b3 #) #) ->
        let ma = sqrt (a1*a1 + a2*a2 + a3*a3)
            mb = sqrt (b1*b1 + b2*b2 + b3*b3)
            d  = a1*b1 + a2*b2 + a3*b3
            c  = sqrt (ma*mb + d)
            ma2 = ma * 1.4142135623730951 -- sqrt 2.0
            r  = recip (ma2 * c)
            c' = sqrt (mb / ma) -- ratio of a and b for corner cases
            r' = recip (sqrt ( negate (a1*b1 + a2*b2) ))
        in case unpackV3# (cross a b) of
          (# 0, 0, 0 #)
              -- if a and b face the same direction, q is fully real
            | d >= 0       -> packQ 0 0 0 c'
              -- if a and b face opposite directions, find an orthogonal vector
              -- prerequisites: w == 0  and  a·(x,y,z) == 0
              -- corner cases: only one vector component is non-zero
            | b1 == 0      -> packQ c' 0 0 0
              -- otherwise set the last component to zero,
              -- and get an orthogonal vector in 2D.
            | otherwise    -> packQ (-b2*r') (b1*r') 0 0
              -- NB: here we have some precision troubles
              --     when a and b are close to parallel and opposite.
          (# t1, t2, t3 #) -> packQ (t1 * r) (t2 * r) (t3 * r) (c / ma2)
    {-# INLINE axisRotation #-}
    axisRotation v a = case unpackV3# v of
      (# 0, 0, 0 #) -> packQ 0 0 0 (negateUnless (abs a < M_PI) 1)
      (# x, y, z #) ->
        let c = cos (a * 0.5)
            s = sin (a * 0.5)
                / sqrt (x*x + y*y + z*z)
        in packQ (x * s) (y * s) (z * s) c
    {-# INLINE qArg #-}
    qArg (unpackQ# -> (# x, y, z, w #)) = 2 * atan2 (sqrt (x*x + y*y + z*z)) w
    {-# INLINE fromMatrix33 #-}
    fromMatrix33 m = fromM 1
      (ix# 0# m) (ix# 1# m) (ix# 2# m)
      (ix# 3# m) (ix# 4# m) (ix# 5# m)
      (ix# 6# m) (ix# 7# m) (ix# 8# m)

    {-# INLINE fromMatrix44 #-}
    fromMatrix44 m = fromM (ix# 15# m)
      (ix# 0# m) (ix# 1# m) (ix# 2# m)
      (ix# 4# m) (ix# 5# m) (ix# 6# m)
      (ix# 8# m) (ix# 9# m) (ix# 10# m)

    {-# INLINE toMatrix33 #-}
    toMatrix33 (unpackQ# -> (# 0.0, 0.0, 0.0, w #))
      = let x = w * w
            f 0 = (# 3 :: Int , x #)
            f k = (# k-1, 0 #)
        in case gen# (CumulDims [9,3,1]) f 0 of
            (# _, m #) -> m -- diag (scalar (w * w))
    toMatrix33 (unpackQ# -> (# x', y', z', w' #)) =
      let x = scalar x'
          y = scalar y'
          z = scalar z'
          w = scalar w'
          x2 = x * x
          y2 = y * y
          z2 = z * z
          w2 = w * w
          l2 = x2 + y2 + z2 + w2
      in ST.runST $ do
        df <- ST.newDataFrame
        ST.writeDataFrameOff df 0 $ l2 - 2*(z2 + y2)
        ST.writeDataFrameOff df 1 $ 2*(x*y + z*w)
        ST.writeDataFrameOff df 2 $ 2*(x*z - y*w)
        ST.writeDataFrameOff df 3 $ 2*(x*y - z*w)
        ST.writeDataFrameOff df 4 $ l2 - 2*(z2 + x2)
        ST.writeDataFrameOff df 5 $ 2*(y*z + x*w)
        ST.writeDataFrameOff df 6 $ 2*(x*z + y*w)
        ST.writeDataFrameOff df 7 $ 2*(y*z - x*w)
        ST.writeDataFrameOff df 8 $ l2 - 2*(y2 + x2)
        ST.unsafeFreezeDataFrame df
    {-# INLINE toMatrix44 #-}
    toMatrix44 (unpackQ# -> (# 0.0, 0.0, 0.0, w #)) = ST.runST $ do
      df <- ST.newDataFrame
      mapM_ (flip (ST.writeDataFrameOff df) 0) [0..15]
      let w2 = scalar (w * w)
      ST.writeDataFrameOff df 0 w2
      ST.writeDataFrameOff df 5 w2
      ST.writeDataFrameOff df 10 w2
      ST.writeDataFrameOff df 15 1
      ST.unsafeFreezeDataFrame df
    toMatrix44 (unpackQ# -> (# x', y', z', w' #)) =
      let x = scalar x'
          y = scalar y'
          z = scalar z'
          w = scalar w'
          x2 = x * x
          y2 = y * y
          z2 = z * z
          w2 = w * w
          l2 = x2 + y2 + z2 + w2
      in ST.runST $ do
        df <- ST.newDataFrame
        ST.writeDataFrameOff df 0 $ l2 - 2*(z2 + y2)
        ST.writeDataFrameOff df 1 $ 2*(x*y + z*w)
        ST.writeDataFrameOff df 2 $ 2*(x*z - y*w)
        ST.writeDataFrameOff df 3 0
        ST.writeDataFrameOff df 4 $ 2*(x*y - z*w)
        ST.writeDataFrameOff df 5 $ l2 - 2*(z2 + x2)
        ST.writeDataFrameOff df 6 $ 2*(y*z + x*w)
        ST.writeDataFrameOff df 7 0
        ST.writeDataFrameOff df 8 $ 2*(x*z + y*w)
        ST.writeDataFrameOff df 9 $ 2*(y*z - x*w)
        ST.writeDataFrameOff df 10 $ l2 - 2*(y2 + x2)
        ST.writeDataFrameOff df 11 0
        ST.writeDataFrameOff df 12 0
        ST.writeDataFrameOff df 13 0
        ST.writeDataFrameOff df 14 0
        ST.writeDataFrameOff df 15 1
        ST.unsafeFreezeDataFrame df


{- Calculate quaternion from a 3x3 matrix.

   First argument is a constant; it is either 1 for a 3x3 matrix,
   or m44 for a 4x4 matrix. I just need to multiply all components by
   this number.

   Further NB for the formulae:

   d == square q == det m ** (1/3)
   t == trace m  == 4 w w - d
   m01 - m10 == 4 z w
   m20 - m02 == 4 y w
   m12 - m21 == 4 x w
   m01 + m10 == 4 x y
   m20 + m02 == 4 x z
   m12 + m21 == 4 y z
   m00 == + x x - y y - z z + w w
   m11 == - x x + y y - z z + w w
   m22 == - x x - y y + z z + w w
   4 x x == d + m00 - m11 - m22
   4 y y == d - m00 + m11 - m22
   4 z z == d - m00 - m11 + m22
   4 w w == d + m00 + m11 + m22
 -}
fromM :: Double
      -> Double -> Double -> Double
      -> Double -> Double -> Double
      -> Double -> Double -> Double
      -> QDouble
fromM c'
  m00 m01 m02
  m10 m11 m12
  m20 m21 m22
    | t > 0
      = let dd = sqrt ( d + t )
            is = c / dd
        in packQ ((m12 - m21)*is) ((m20 - m02)*is) ((m01 - m10)*is) (c*dd)
    | m00 > m11 && m00 > m22
      = let dd = sqrt ( d + m00 - m11 - m22 )
            is = c / dd
        in packQ (c*dd) ((m01 + m10)*is) ((m02 + m20)*is) ((m12 - m21)*is)
    | m11 > m22
      = let dd = sqrt ( d - m00 + m11 - m22 )
            is = c / dd
        in packQ ((m01 + m10)*is) (c*dd) ((m12 + m21)*is) ((m20 - m02)*is)
    | otherwise
      = let dd = sqrt ( d - m00 - m11 + m22 )
            is = c / dd
        in packQ ((m02 + m20)*is) ((m12 + m21)*is) (c*dd) ((m01 - m10)*is)

  where
    -- normalizing constant
    c = recip $ 2 * sqrt c'
    -- trace
    t = m00 + m11 + m22
    -- cubic root of determinant
    d = ( m00 * ( m11 * m22 - m12 * m21 )
        - m01 * ( m10 * m22 - m12 * m20 )
        + m02 * ( m10 * m21 - m11 * m20 )
        ) ** 0.33333333333333333333333333333333




instance Num QDouble where
    QDouble a + QDouble b
      = QDouble (a + b)
    {-# INLINE (+) #-}
    QDouble a - QDouble b
      = QDouble (a - b)
    {-# INLINE (-) #-}
    (unpackQ# -> (# a1, a2, a3, a4 #)) * (unpackQ# -> (# b1, b2, b3, b4 #))
      = packQ ((a4 * b1) + (a1 * b4) + (a2 * b3) - (a3 * b2))
              ((a4 * b2) - (a1 * b3) + (a2 * b4) + (a3 * b1))
              ((a4 * b3) + (a1 * b2) - (a2 * b1) + (a3 * b4))
              ((a4 * b4) - (a1 * b1) - (a2 * b2) - (a3 * b3))
    {-# INLINE (*) #-}
    negate (QDouble a) = QDouble (negate a)
    {-# INLINE negate #-}
    abs = packQ 0 0 0 . sqrt . square
    {-# INLINE abs #-}
    signum q@(unpackQ# -> (# x, y, z, w #))
      | qd == 0   = q
      | otherwise = case ix + iy + iz + iw + nn of
        0 -> packQ (x * l) (y * l) (z * l) (w * l)
        1 -> packQ (copysign 1 x) 0 0 0
        2 -> packQ 0 (copysign 1 y) 0 0
        4 -> packQ 0 0 (copysign 1 z) 0
        8 -> packQ 0 0 0 (copysign 1 w)
        _ -> packQ n n n n
      where
        n  = 0 / 0 :: Double
        qd = x*x + y*y + z*z + w*w
        ix = if isInfinite x then 1 else 0 :: Int
        iy = if isInfinite y then 2 else 0 :: Int
        iz = if isInfinite z then 4 else 0 :: Int
        iw = if isInfinite w then 8 else 0 :: Int
        nn = if isNaN x || isNaN y || isNaN z || isNaN w then 16 else 0 :: Int
        l  = recip (sqrt qd)
    {-# INLINE signum #-}
    fromInteger = packQ 0 0 0 . fromInteger
    {-# INLINE fromInteger #-}


instance Fractional QDouble where
    {-# INLINE recip #-}
    recip q@(unpackQ# -> (# x, y, z, w #)) = case negate (recip (square q)) of
      c -> packQ (x * c) (y * c) (z * c) (negate (w * c))
    {-# INLINE (/) #-}
    a / b = a * recip b
    {-# INLINE fromRational #-}
    fromRational = packQ 0 0 0 . fromRational


instance Floating QDouble where
    {-# INLINE pi #-}
    pi = packQ 0 0 0 M_PI
    {-# INLINE exp #-}
    exp (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0  = packQ x y z ew
      | otherwise = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv  = sqrt mv2
        ew  = exp w
        l   = ew * sin mv / mv
        arg = ew * cos mv
    {-# INLINE log #-}
    log = log' (Vec3 1 0 0)
    {-# INLINE sqrt #-}
    sqrt = sqrt' (Vec3 1 0 0)
    {-# INLINE sin #-}
    sin (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0  = packQ x y z (sin w)
      | otherwise = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv  = sqrt mv2
        l   = cos w * sinh mv / mv
        arg = sin w * cosh mv
    {-# INLINE cos #-}
    cos (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0  = packQ x y z (cos w)
      | otherwise = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv  = sqrt mv2
        l   = sin w * sinh mv / negate mv
        arg = cos w * cosh mv
    {-# INLINE tan #-}
    tan (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0       = packQ x y z (tan w)
      | isInfinite mv2 = signum (packQ x y z 0)
      | otherwise      = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv = sqrt mv2
        b = 2*mv
        a = 2*w
        sina = sin a
        eb = exp (-b)
        eb2 = eb*eb
        d = 1 + eb2 + 2 * eb * cos a
        rd = recip d
        pa = M_PI - abs a
        rd' = 2 / (b*b + pa*pa)
        (l, arg) =
          if d >= M_EPS
          then ((1 - eb2) * rd / mv, 2 * sina * eb * rd)
          else (2 * rd' , negate sina * rd')
    {-# INLINE sinh #-}
    sinh (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0  = packQ x y z (sinh w)
      | otherwise = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv  = sqrt mv2
        l   = cosh w * sin mv / mv
        arg = sinh w * cos mv
    {-# INLINE cosh #-}
    cosh (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0  = packQ x y z (cosh w)
      | otherwise = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv  = sqrt mv2
        l   = sinh w * sin mv / mv
        arg = cosh w * cos mv
    {-# INLINE tanh #-}
    tanh (unpackQ# -> (# x, y, z, w #))
      | mv2 == 0       = packQ x y z (tanh w)
      | isInfinite mv2 = packQ 0 0 0 (signum w)
      | otherwise      = packQ (x * l) (y * l) (z * l) arg
      where
        mv2 = (x * x) + (y * y) + (z * z)
        mv = sqrt mv2
        b = 2*w
        a = 2*mv
        eb = exp (- abs b)
        eb2 = eb*eb
        d = 1 + eb2 + 2 * eb * cos a
        rd = recip d
        pa = M_PI - a
        rd' = 2 / (b*b + pa*pa)
        (l, arg) =
          if d >= M_EPS
          then (2 * sin a * eb * rd / mv, copysign (1 - eb2) b * rd)
          else (2 * rd' , b * rd')
    {-# INLINE asin #-}
    -- The original formula:
    -- asin q = -i * log (i*q + sqrt (1 - q*q))
    -- below is a more numerically stable version.
    asin (unpackQ# -> (# x, y, z, w #))
      | v2 == 0   = if w2 <= 1
                    then packQ x y z (asin w)
                    else packQ l 0 0 arg
      | otherwise  = packQ (x*c) (y*c) (z*c) arg
      where
        v2 = (x * x) + (y * y) + (z * z)
        v = sqrt v2
        w2 = w*w
        w1qq = 0.5 *(1 - w2 + v2)       -- real part of (1 - q*q)/2
        l1qq = sqrt (w1qq*w1qq + w2*v2) -- length of (1 - q*q)/2
        sp2 = l1qq + w1qq
        sn2 = l1qq - w1qq
        sp = sqrt sp2
        sn = copysign (sqrt sn2) w
        dp = v2 / ((sp + v)*(sn2 + v2))
        dn = w2 / ((sn + w)*(sp2 + w2))
        (wD, vD) =  -- choose a more stable (symbolically equiv) version
          case compare w1qq 0 of
            GT -> (dp, w * dp / sp)
            LT -> (v * dn / sn, dn)
            EQ -> (-v, w)
        l = -0.5 * log (wD*wD + vD*vD)
        c = l / v
        arg = atan2 vD wD
    {-# INLINE acos #-}
    acos q = M_PI_2 - asin q
    {-# INLINE atan #-}
    -- atan q = i / 2 * log ( (i + q) / (i - q) )
    atan (unpackQ# -> (# x, y, z, w #))
      | v2 == 0   = packQ x y z (atan w)
      | otherwise = packQ (x*c) (y*c) (z*c) arg
      where
        v2 = (x * x) + (y * y) + (z * z)
        v = sqrt v2
        w2 = w*w
        q2 = w2 + v2
        v' = v - 1
        mzero = w2 + v'*v'
        (c, arg) =
          if mzero == 0
          then ( sqrt maxFinite / v, 0)
          else ( 0.25 * (log (1 + q2 + 2*v) - log mzero) / v
               , 0.5 * atan2 (2*w) (1 - q2) )
    {-# INLINE asinh #-}
    -- The original formula:
    -- asinh q = log (q + sqrt (q*q + 1))
    -- below is a more numerically stable version.
    asinh (unpackQ# -> (# x, y, z, w #))
      | v2 == 0   = packQ x y z (asinh w)
      | otherwise = packQ (x*c) (y*c) (z*c) arg
      where
        v2 = (x * x) + (y * y) + (z * z)
        v = sqrt v2
        w2 = w*w
        w1qq = 0.5 *(1 + w2 - v2)       -- real part of (1 + q*q)/2
        l1qq = sqrt (w1qq*w1qq + w2*v2) -- length of (1 + q*q)/2
        sp2 = l1qq + w1qq
        sn2 = l1qq - w1qq
        sp = sqrt sp2
        sn = copysign (sqrt sn2) w
        dp = w2 / ((sp - w)*(w2 + sn2))
        dn = v2 / ((v - sn)*(v2 + sp2))
        (wD, vD) =  -- choose a more stable (symbolically equiv) version
          case compare w1qq 0 of
            GT -> if w >= 0 then (w + sp, v * (1 + w / sp)) else (dp, v * dp / sp)
            LT -> if w >= 0 then (w * (1 + v / sn), v + sn) else (w * dn / sn, dn)
            EQ -> (w, v)
        c = atan2 vD wD / v
        arg = 0.5 * log (wD*wD + vD*vD)
    {-# INLINE acosh #-}
    -- The original formula:
    -- asinh q = log (q + sqrt (q + 1) * sqrt (q - 1))
    -- below is a more numerically stable version.
    -- note, log (q + sqrt (q*q - 1)) would not work, because that would not
    -- be the principal value.
    acosh (unpackQ# -> (# x, y, z, w #))
      | v2 == 0   = packQ x y z (acosh w)
      | otherwise = packQ (x*c) (y*c) (z*c) arg
      where
        v2 = (x * x) + (y * y) + (z * z)
        v = sqrt v2
        w2 = w*w
        w1qq = 0.5 *(w2 - v2 - 1)       -- real part of (q*q - 1)/2
        l1qq = sqrt (w1qq*w1qq + w2*v2) -- length of (q*q - 1)/2
        sp2 = l1qq + w1qq
        sn2 = l1qq - w1qq
        sp = sqrt sp2
        sn = copysign (sqrt sn2) w
        dp = w2 / ((w - sp)*(w2 + sn2))
        dn = v2 / ((sn - v)*(v2 + sp2))
        (wD, vD) =  -- choose a more stable (symbolically equiv) version
          case compare w1qq 0 of
            GT -> if w >= 0 then (w + sp, v * (1 + w / sp)) else (dp, v * dp / sp)
            LT -> if w >= 0 then (w * (1 + v / sn), v + sn) else (w * dn / sn, dn)
            EQ -> (w, v)
        c = atan2 vD wD / v
        arg = 0.5 * log (wD*wD + vD*vD)
    {-# INLINE atanh #-}
    -- atanh q = 0.5 * log ( (1 + q) / (1 - q) )
    atanh (unpackQ# -> (# x, y, z, w #))
      | v2 ==  0  = packQ x y z (atanh w)
      | otherwise = packQ (x*c) (y*c) (z*c) (copysign arg w)
      where
        v2 = (x * x) + (y * y) + (z * z)
        v = sqrt v2
        w2 = w*w
        q2 = w2 + v2
        w' = abs w - 1
        c  = 0.5 * atan2 (2*v) (1 - q2) / v
        arg = if w' == 0
              then (1/0)
              else 0.25 * (log (1 + q2 + 2 * abs w) - log (v2 + w'*w'))


-- If q is negative real, provide a fallback axis to align log.
log' :: Vector Double 3 -> QDouble -> QDouble
log' r (unpackQ# -> (# x, y, z, w #))
  = case (x * x) + (y * y) + (z * z) of
    0.0 | w >= 0
           -> packQ 0 0 0 (log w)
        | Vec3 rx ry rz <- r
           ->  packQ (M_PI*rx) (M_PI*ry) (M_PI*rz) (log (negate w))
    mv2 -> case (# mv2 + w * w, sqrt mv2 #) of
      (# q2, mv #) -> case atan2 mv w / mv of
        l -> packQ (x * l) (y * l) (z * l) (0.5 * log q2)


-- If q is negative real, provide a fallback axis to align sqrt.
sqrt' :: Vector Double 3 -> QDouble -> QDouble
sqrt' r (unpackQ# -> (# x, y, z, w #))
  | v2 == 0 && w >= 0
    = packQ x y z (sqrt w)
  | v2 == 0
  , Vec3 rx ry rz <- r
  , sw <- sqrt (negate w)
    = packQ (sw*rx) (sw*ry) (sw*rz) 0
  | otherwise
    = packQ (x * c) (y * c) (z * c) arg
  where
    v2 = (x * x) + (y * y) + (z * z)
    mq = sqrt (v2 + w * w)
    arg = sqrt $ 0.5 * if w >= 0 then mq + w else v2 / (mq - w)
    c = 0.5 / arg

instance Eq QDouble where
    {-# INLINE (==) #-}
    QDouble a == QDouble b = a == b
    {-# INLINE (/=) #-}
    QDouble a /= QDouble b = a /= b
