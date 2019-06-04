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
module Numeric.Quaternion.Internal.QFloat
    ( QFloat, Quater (..)
    ) where

import qualified Control.Monad.ST                     as ST
import           Data.Coerce                          (coerce)
import           Numeric.DataFrame.Internal.PrimArray
import qualified Numeric.DataFrame.ST                 as ST
import           Numeric.DataFrame.Type
import           Numeric.PrimBytes                    (PrimBytes)
import           Numeric.Quaternion.Internal
import           Numeric.Scalar.Internal
import           Numeric.Vector.Internal

type QFloat = Quater Float

deriving instance PrimBytes (Quater Float)
deriving instance PrimArray Float (Quater Float)

instance Quaternion Float where
    newtype Quater Float = QFloat Vec4f
    {-# INLINE packQ #-}
    packQ = coerce (vec4 :: Float -> Float -> Float -> Float -> Vec4f)
    {-# INLINE unpackQ# #-}
    unpackQ# = coerce (unpackV4# :: Vec4f -> (# Float, Float, Float, Float #))
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
    re (unpackQ# -> (# _, _, _, w #)) = packQ 0.0 0.0 0.0 w
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
      (# _, (# 0, 0, 0 #) #) -> packQ 0.0 0.0 0.0 0.0
      (# (# 0, 0, 0 #), _ #) -> let x = (1 / 0 :: Float) in packQ x x x x
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
      (# 0, 0, 0 #) -> packQ 0.0 0.0 0.0 (if abs a < pi then 1 else -1)
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
fromM :: Float
      -> Float -> Float -> Float
      -> Float -> Float -> Float
      -> Float -> Float -> Float
      -> QFloat
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




instance Num QFloat where
    QFloat a + QFloat b
      = QFloat (a + b)
    {-# INLINE (+) #-}
    QFloat a - QFloat b
      = QFloat (a - b)
    {-# INLINE (-) #-}
    (unpackQ# -> (# a1, a2, a3, a4 #)) * (unpackQ# -> (# b1, b2, b3, b4 #))
      = packQ
           ((a4 * b1) +
            (a1 * b4) +
            (a2 * b3) -
            (a3 * b2)
            )
           ((a4 * b2) -
            (a1 * b3) +
            (a2 * b4) +
            (a3 * b1)
            )
           ((a4 * b3) +
            (a1 * b2) -
            (a2 * b1) +
            (a3 * b4)
            )
           ((a4 * b4) -
            (a1 * b1) -
            (a2 * b2) -
            (a3 * b3)
            )
    {-# INLINE (*) #-}
    negate (QFloat a) = QFloat (negate a)
    {-# INLINE negate #-}
    abs = packQ 0.0 0.0 0.0 . sqrt . square
    {-# INLINE abs #-}
    signum q@(unpackQ# -> (# x, y, z, w #))
      = case square q of
          0.0 -> packQ 0.0 0.0 0.0 0.0
          qd -> case recip (sqrt qd) of
             s -> packQ (x * s) (y * s) (z * s) (w * s)
    {-# INLINE signum #-}
    fromInteger = packQ 0.0 0.0 0.0 . fromInteger
    {-# INLINE fromInteger #-}


instance Fractional QFloat where
    {-# INLINE recip #-}
    recip q@(unpackQ# -> (# x, y, z, w #)) = case negate (recip (square q)) of
      c -> packQ (x * c) (y * c) (z * c) (negate (w * c))
    {-# INLINE (/) #-}
    a / b = a * recip b
    {-# INLINE fromRational #-}
    fromRational = packQ 0.0 0.0 0.0 . fromRational


instance Floating QFloat where
    {-# INLINE pi #-}
    pi = packQ 0.0 0.0 0.0 3.141592653589793
    {-# INLINE exp #-}
    exp (unpackQ# -> (# x, y, z, w #))
      = case (# (x * x) +
                (y * y) +
                (z * z)
             , exp w
             #) of
        (# 0.0, et #) -> packQ 0.0 0.0 0.0 et
        (# mv2, et #) -> case sqrt mv2 of
          mv -> case et * sin mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (et * cos mv)
    {-# INLINE log #-}
    log = log' (Vec3 1 0 0)
    {-# INLINE sqrt #-}
    sqrt = sqrt' (Vec3 1 0 0)
    {-# INLINE sin #-}
    sin (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (sin w)
        mv2 -> case sqrt mv2 of
          mv -> case cos w * sinh mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (sin w * cosh mv)
    {-# INLINE cos #-}
    cos (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (cos w)
        mv2 -> case sqrt mv2 of
          mv -> case sin w * sinh mv / negate mv of
            l -> packQ (x * l) (y * l) (z * l) (cos w * cosh mv)
    {-# INLINE tan #-}
    tan (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (tan w)
        mv2 ->
          let mv = sqrt mv2
              chv = cosh mv
              shv = sinh mv
              ct = cos w
              st = sin w
              cq = recip ( (ct * ct * chv * chv) + (st * st * shv * shv) )
              l = chv * shv * cq / mv
          in packQ (x * l) (y * l) (z * l) (ct * st * cq)
    {-# INLINE sinh #-}
    sinh (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (sinh w)
        mv2 -> case sqrt mv2 of
          mv -> case cosh w * sin mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (sinh w * cos mv)
    {-# INLINE cosh #-}
    cosh (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (cosh w)
        mv2 -> case sqrt mv2 of
          mv -> case sinh w * sin mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (cosh w * cos mv)
    {-# INLINE tanh #-}
    tanh (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (tanh w)
        mv2 ->
          let mv = sqrt mv2
              cv = cos mv
              sv = sin mv
              cht = cosh w
              sht = sinh w
              cq = recip ( (cht * cht * cv * cv) + (sht * sht * sv * sv) )
              l = cv * sv * cq / mv
          in packQ (x * l) (y * l) (z * l) (cht * sht * cq)
    {-# INLINE asin #-}
    asin q = -i * log' axis (i*q + sqrt' axis (1 - q*q))
        where
          axis = sigVec q
          i = fromVecNum axis 0
    {-# INLINE acos #-}
    acos q = pi/2 - asin q
    {-# INLINE atan #-}
    -- atan q = i / 2 * log ( (i + q) / (i - q) )
    atan (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0 -> packQ 0 0 0 (atan w)
        v2 ->
          let v = sqrt v2
              w2 = w*w
              vw2 = w2 + v2
              t  = 1 + vw2
              c  = 0.25 * log ((t + 2*v) / (t - 2*v)) / v
          in packQ (c*x) (c*y) (c*z) $ 0.5 * atan2 (2*w) (1 - vw2)
    {-# INLINE asinh #-}
    -- The original formula:
    -- asinh q = log (q + sqrt (q*q + 1))
    -- below is a more numerically stable version.
    asinh (unpackQ# -> (# x, y, z, w #))
      | v2 == 0
        = packQ 0 0 0 (asinh w)
      | w == 0
        = if v2 <= 1
          then let c = asin v / v
               in  packQ (c*x) (c*y) (c*z) 0
          else let c  = 0.5 * pi / v
                   w' = 0.5 * log (2*v2 - 1 + 2 * v * sqrt ( v2 - 1 ))
               in  packQ (c*x) (c*y) (c*z) w'
      | otherwise
        = let c = atan2 v t / v
              w' = 0.5 * log ( t2  + v2) - log t
                     + if w >= 0
                       then log ( t + w )
                       else log 2 + log w2 - log ( t - w ) - log ( r + w2 + v2 - 1 )
          in packQ (c*x) (c*y) (c*z) w'
      where
        v2 = (x * x) + (y * y) + (z * z)
        v  = sqrt v2
        w2 = w*w
        wvp1 = w2 - v2 + 1
        r = sqrt $ wvp1*wvp1 + 4*w2*v2
        t = sqrt $ ( r + wvp1 ) * 0.5
        t2 = t*t
    {-# INLINE acosh #-}
    -- note, log (q + sqrt (q*q - 1)) would not work, because that would not
    -- be the principal value.
    acosh q = log' axis (q + sqrt' axis (q + 1) * sqrt' axis (q - 1))
      where
        axis = sigVec q
    {-# INLINE atanh #-}
    atanh q = 0.5 * log ((1+q)/(1-q))

-- If q is negative real, provide a fallback axis to align log.
log' :: Vec3f -> QFloat -> QFloat
log' r (unpackQ# -> (# x, y, z, w #))
  = case (x * x) + (y * y) + (z * z) of
    0.0 | w >= 0
           -> packQ 0.0 0.0 0.0 (log w)
        | Vec3 rx ry rz <- r
           ->  packQ (pi*rx) (pi*ry) (pi*rz) (log (negate w))
    mv2 -> case (# sqrt (mv2 + (w * w)), sqrt mv2 #) of
      (# mq, mv #) -> case atan2 mv w / mv of
        l -> packQ (x * l) (y * l) (z * l) (log mq)


-- If q is negative real, provide a fallback axis to align sqrt.
sqrt' :: Vec3f -> QFloat -> QFloat
sqrt' r (unpackQ# -> (# x, y, z, w #))
  = case (x * x) + (y * y) + (z * z) of
    0.0 | w >= 0
          -> packQ 0.0 0.0 0.0 (sqrt w)
        | Vec3 rx ry rz <- r
        , sw <- sqrt (negate w)
          -> packQ (sw*rx) (sw*ry) (sw*rz) 0
    mv2 ->
      let mq = sqrt (mv2 + w * w)
          l2 = sqrt mq
          tq = w / (mq * 2.0)
          sina = sqrt (0.5 - tq) * l2 / sqrt mv2
      in packQ (x * sina) (y * sina) (z * sina) (sqrt (0.5 + tq) * l2)

sigVec :: QFloat -> Vec3f
sigVec (unpackQ# -> (# x, y, z, _ #))
  = case sqrt ((x * x) + (y * y) + (z * z)) of
      0 -> vec3 1 0 0
      l -> vec3 (x/l) (y/l) (z/l)

instance Eq QFloat where
    {-# INLINE (==) #-}
    QFloat a == QFloat b = a == b
    {-# INLINE (/=) #-}
    QFloat a /= QFloat b = a /= b
