{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QFloat
    ( QFloat, Quater (..)
    ) where

import           Data.Coerce                                     (coerce)
import           GHC.Exts

import qualified Control.Monad.ST                                as ST
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family.FloatX3
import           Numeric.DataFrame.Internal.Array.Family.FloatX4
import qualified Numeric.DataFrame.ST                            as ST
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import qualified Numeric.Dimensions.Fold                         as ST
import           Numeric.PrimBytes                               (PrimBytes)
import           Numeric.Quaternion.Class
import           Numeric.Scalar
import           Numeric.Vector


type QFloat = Quater Float

deriving instance PrimBytes (Quater Float)
deriving instance PrimArray Float (Quater Float)

instance Quaternion Float where
    newtype Quater Float = QFloat FloatX4
    {-# INLINE packQ #-}
    packQ (F# x) (F# y) (F# z) (F# w) = QFloat (FloatX4# x y z w)
    {-# INLINE unpackQ #-}
    unpackQ (QFloat (FloatX4# x y z w)) = (F# x, F# y, F# z, F# w)
    {-# INLINE fromVecNum #-}
    fromVecNum (SingleFrame (FloatX3# x y z)) (F# w) = QFloat (FloatX4# x y z w)
    {-# INLINE fromVec4 #-}
    fromVec4 = coerce
    {-# INLINE toVec4 #-}
    toVec4 = coerce
    {-# INLINE square #-}
    square q = F# (qdot q)
    {-# INLINE im #-}
    im (QFloat (FloatX4# x y z _)) = QFloat (FloatX4# x y z 0.0#)
    {-# INLINE re #-}
    re (QFloat (FloatX4# _ _ _ w)) = QFloat (FloatX4# 0.0# 0.0# 0.0# w)
    {-# INLINE imVec #-}
    imVec (QFloat (FloatX4# x y z _)) = SingleFrame (FloatX3# x y z)
    {-# INLINE taker #-}
    taker (QFloat (FloatX4# _ _ _ w)) = F# w
    {-# INLINE takei #-}
    takei (QFloat (FloatX4# x _ _ _)) = F# x
    {-# INLINE takej #-}
    takej (QFloat (FloatX4# _ y _ _)) = F# y
    {-# INLINE takek #-}
    takek (QFloat (FloatX4# _ _ z _)) = F# z
    {-# INLINE conjugate #-}
    conjugate (QFloat (FloatX4# x y z w)) = QFloat (FloatX4#
                                                (negateFloat# x)
                                                (negateFloat# y)
                                                (negateFloat# z) w)
    {-# INLINE rotScale #-}
    rotScale (QFloat (FloatX4# i j k t))
             (SingleFrame (FloatX3# x y z))
      = let l = t*%t -% i*%i -% j*%j -% k*%k
            d = 2.0# *% ( i*%x +% j*%y +% k*%z)
            t2 = t *% 2.0#
        in SingleFrame
            ( FloatX3#
                (l*%x +% d*%i +% t2 *% (z*%j -% y*%k))
                (l*%y +% d*%j +% t2 *% (x*%k -% z*%i))
                (l*%z +% d*%k +% t2 *% (y*%i -% x*%j))
            )
    {-# INLINE getRotScale #-}
    getRotScale _ (SingleFrame (FloatX3# 0.0# 0.0# 0.0#))
      = QFloat (FloatX4# 0.0# 0.0# 0.0# 0.0#)
    getRotScale (SingleFrame (FloatX3# 0.0# 0.0# 0.0#)) _
      = case infty of F# x -> QFloat (FloatX4# x x x x)
    getRotScale a@(SingleFrame (FloatX3# a1 a2 a3))
                b@(SingleFrame (FloatX3# b1 b2 b3))
      = let ma = sqrtFloat# (a1*%a1 +% a2*%a2 +% a3*%a3)
            mb = sqrtFloat# (b1*%b1 +% b2*%b2 +% b3*%b3)
            d  = a1*%b1 +% a2*%b2 +% a3*%b3
            c  = sqrtFloat# (ma*%mb +% d)
            ma2 = ma *% sqrtFloat# 2.0#
            r  = 1.0# /% (ma2 *% c)
        in case cross a b of
          SingleFrame (FloatX3# 0.0# 0.0# 0.0#) ->
            if isTrue# (gtFloat# d 0.0#)
            then QFloat (FloatX4#  0.0# 0.0# 0.0# (sqrtFloat# (mb /% ma)))
                 -- Shall we move result from k to i component?
            else QFloat (FloatX4#  0.0# 0.0# (sqrtFloat# (mb /% ma)) 0.0#)
          SingleFrame (FloatX3# t1 t2 t3) -> QFloat
                ( FloatX4#
                    (t1 *% r)
                    (t2 *% r)
                    (t3 *% r)
                    (c /% ma2)
                )
    {-# INLINE axisRotation #-}
    axisRotation (SingleFrame (FloatX3# 0.0# 0.0# 0.0#)) _
      = QFloat (FloatX4# 0.0# 0.0# 0.0# 1.0#)
    axisRotation (SingleFrame (FloatX3# x y z)) (F# a)
      = let c = cosFloat# (a *% 0.5#)
            s = sinFloat# (a *% 0.5#)
                /% sqrtFloat# (x*%x +% y*%y +% z*%z)
        in QFloat
              ( FloatX4#
                  (x *% s)
                  (y *% s)
                  (z *% s)
                  c
              )
    {-# INLINE qArg #-}
    qArg (QFloat (FloatX4# x y z w))
       = case atan2 (F# (sqrtFloat# (x*%x +% y*%y +% z*%z)))
                    (F# w) of
           F# a -> F# (a *% 2.0#)
    {-# INLINE fromMatrix33 #-}
    fromMatrix33 m
      = let d = powerFloat#
              (  ix 0# m *% ( ix 4# m *% ix 8# m -% ix 5# m *% ix 7# m )
              -% ix 1# m *% ( ix 3# m *% ix 8# m -% ix 5# m *% ix 6# m )
              +% ix 2# m *% ( ix 3# m *% ix 7# m -% ix 4# m *% ix 6# m )
              ) 0.33333333333333333333333333333333#
        in QFloat
           ( FloatX4#
            (sqrtFloat# (max# 0.0# (d +% ix 0# m -% ix 4# m -% ix 8# m )) *% sign# (ix 5# m -% ix 7# m) *% 0.5#)
            (sqrtFloat# (max# 0.0# (d -% ix 0# m +% ix 4# m -% ix 8# m )) *% sign# (ix 6# m -% ix 2# m) *% 0.5#)
            (sqrtFloat# (max# 0.0# (d -% ix 0# m -% ix 4# m +% ix 8# m )) *% sign# (ix 1# m -% ix 3# m) *% 0.5#)
            (sqrtFloat# (max# 0.0# (d +% ix 0# m +% ix 4# m +% ix 8# m )) *% 0.5#)
           )
    {-# INLINE fromMatrix44 #-}
    fromMatrix44 m
      = let d = powerFloat#
              (  ix 0# m *% ( ix 5# m *% ix 10# m -% ix 6# m *% ix 9# m )
              -% ix 1# m *% ( ix 4# m *% ix 10# m -% ix 6# m *% ix 8# m )
              +% ix 2# m *% ( ix 4# m *% ix  9# m -% ix 5# m *% ix 8# m )
              ) 0.33333333333333333333333333333333#
            c = 0.5# /% ix 15# m
        in QFloat
           ( FloatX4#
            (sqrtFloat# (max# 0.0# (d +% ix 0# m -% ix 5# m -% ix 10# m )) *% sign# (ix 6# m -% ix 9# m) *% c)
            (sqrtFloat# (max# 0.0# (d -% ix 0# m +% ix 5# m -% ix 10# m )) *% sign# (ix 8# m -% ix 2# m) *% c)
            (sqrtFloat# (max# 0.0# (d -% ix 0# m -% ix 5# m +% ix 10# m )) *% sign# (ix 1# m -% ix 4# m) *% c)
            (sqrtFloat# (max# 0.0# (d +% ix 0# m +% ix 5# m +% ix 10# m )) *% c)
           )
    {-# INLINE toMatrix33 #-}
    toMatrix33 (QFloat (FloatX4# 0.0# 0.0# 0.0# w))
      = let x = F# (w *% w)
            f 0 = (# 3 :: Int , x #)
            f k = (# k-1, 0 #)
        in case gen# 9# f 0 of
            (# _, m #) -> m -- diag (scalar (F# (w *% w)))
    toMatrix33 (QFloat (FloatX4# x' y' z' w')) =
      let x = scalar (F# x')
          y = scalar (F# y')
          z = scalar (F# z')
          w = scalar (F# w')
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
    toMatrix44 (QFloat (FloatX4# 0.0# 0.0# 0.0# w)) = ST.runST $ do
      df <- ST.newDataFrame
      ST.overDimOff_ (dims :: Dims '[4,4]) (\i -> ST.writeDataFrameOff df i 0) 0 1
      let w2 = scalar (F# (w *% w))
      ST.writeDataFrameOff df 0 w2
      ST.writeDataFrameOff df 5 w2
      ST.writeDataFrameOff df 10 w2
      ST.writeDataFrameOff df 15 1
      ST.unsafeFreezeDataFrame df
    toMatrix44 (QFloat (FloatX4# x' y' z' w')) =
      let x = scalar (F# x')
          y = scalar (F# y')
          z = scalar (F# z')
          w = scalar (F# w')
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
        ST.writeDataFrameOff df 15 0
        ST.unsafeFreezeDataFrame df

qdot :: QFloat -> Float#
qdot (QFloat (FloatX4# x y z w)) = (x *% x) +%
                                   (y *% y) +%
                                   (z *% z) +%
                                   (w *% w)
{-# INLINE qdot #-}

(*%) :: Float# -> Float# -> Float#
(*%) = timesFloat#
{-# INLINE (*%) #-}
infixl 7 *%

(-%) :: Float# -> Float# -> Float#
(-%) = minusFloat#
{-# INLINE (-%) #-}
infixl 6 -%

(+%) :: Float# -> Float# -> Float#
(+%) = plusFloat#
{-# INLINE (+%) #-}
infixl 6 +%

(/%) :: Float# -> Float# -> Float#
(/%) = divideFloat#
{-# INLINE (/%) #-}
infixl 7 /%

infty :: Float
infty = read "Infinity"

max# :: Float# -> Float# -> Float#
max# a b | isTrue# (gtFloat# a b) = a
         | otherwise = b
{-# INLINE max# #-}

sign# :: Float# -> Float#
sign# a | isTrue# (gtFloat# a 0.0#) = 1.0#
        | isTrue# (ltFloat# a 0.0#) = negateFloat# 1.0#
        | otherwise = 0.0#
{-# INLINE sign# #-}

ix :: PrimArray Float a => Int# -> a -> Float#
ix i a = case ix# i a of F# r -> r
{-# INLINE ix #-}

--------------------------------------------------------------------------
-- Num
--------------------------------------------------------------------------

instance Num QFloat where
    QFloat a + QFloat b
      = QFloat (a + b)
    {-# INLINE (+) #-}
    QFloat a - QFloat b
      = QFloat (a - b)
    {-# INLINE (-) #-}
    QFloat (FloatX4# a1 a2 a3 a4) * QFloat (FloatX4# b1 b2 b3 b4)
      = QFloat
         ( FloatX4#
           ((a4 *% b1) +%
            (a1 *% b4) +%
            (a2 *% b3) -%
            (a3 *% b2)
            )
           ((a4 *% b2) -%
            (a1 *% b3) +%
            (a2 *% b4) +%
            (a3 *% b1)
            )
           ((a4 *% b3) +%
            (a1 *% b2) -%
            (a2 *% b1) +%
            (a3 *% b4)
            )
           ((a4 *% b4) -%
            (a1 *% b1) -%
            (a2 *% b2) -%
            (a3 *% b3)
            )
         )
    {-# INLINE (*) #-}
    negate (QFloat a) = QFloat (negate a)
    {-# INLINE negate #-}
    abs q = QFloat (FloatX4# 0.0# 0.0# 0.0# (sqrtFloat# (qdot q)))
    {-# INLINE abs #-}
    signum q@(QFloat (FloatX4# x y z w))
      = case qdot q of
          0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# 0.0#)
          qd -> case 1.0# /% sqrtFloat# qd of
             s -> QFloat
               ( FloatX4#
                (x *% s)
                (y *% s)
                (z *% s)
                (w *% s)
               )
    {-# INLINE signum #-}
    fromInteger n = case fromInteger n of
      F# x -> QFloat (FloatX4# 0.0# 0.0# 0.0# x)
    {-# INLINE fromInteger #-}



--------------------------------------------------------------------------
-- Fractional
--------------------------------------------------------------------------

instance Fractional QFloat where
    {-# INLINE recip #-}
    recip q@(QFloat (FloatX4# x y z w)) = case -1.0# /% qdot q of
      c -> QFloat
        ( FloatX4#
         (x *% c)
         (y *% c)
         (z *% c)
         (negateFloat# (w *% c))
        )
    {-# INLINE (/) #-}
    a / b = a * recip b
    {-# INLINE fromRational #-}
    fromRational q = case fromRational q of
      F# x -> QFloat (FloatX4# 0.0# 0.0# 0.0# x)

--------------------------------------------------------------------------
-- Floating
--------------------------------------------------------------------------

instance  Floating QFloat where
    {-# INLINE pi #-}
    pi = QFloat (FloatX4# 0.0# 0.0# 0.0#
                          3.141592653589793#)
    {-# INLINE exp #-}
    exp (QFloat (FloatX4# x y z w))
      = case (# (x *% x) +%
                (y *% y) +%
                (z *% z)
             , expFloat# w
             #) of
        (# 0.0#, et #) -> QFloat (FloatX4# 0.0# 0.0# 0.0# et)
        (# mv2, et #) -> case sqrtFloat# mv2 of
          mv -> case et *% sinFloat# mv
                        /% mv of
            l -> QFloat
              ( FloatX4#
               (x *% l)
               (y *% l)
               (z *% l)
               (et *% cosFloat# mv)
              )
    {-# INLINE log #-}
    log (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> if isTrue# (w `geFloat#` 0.0#)
                then QFloat (FloatX4# 0.0# 0.0# 0.0# (logFloat# w))
                else QFloat (FloatX4# 3.141592653589793# 0.0# 0.0#
                                     (logFloat# (negateFloat# w)))
        mv2 -> case (# sqrtFloat# (mv2 +% (w *% w))
                     , sqrtFloat# mv2
                    #) of
          (# mq, mv #) -> case atan2 (F# mv) (F# w) / F# mv of
            F# l -> QFloat
              ( FloatX4#
               (x *% l)
               (y *% l)
               (z *% l)
               (logFloat# mq)
              )
    {-# INLINE sqrt #-}
    sqrt (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> if isTrue# (w `geFloat#` 0.0#)
                then QFloat (FloatX4# 0.0# 0.0# 0.0# (sqrtFloat# w))
                else QFloat (FloatX4# (sqrtFloat# (negateFloat# w)) 0.0# 0.0# 0.0#)
        mv2 ->
          let mq = sqrtFloat# (mv2 +% w *% w)
              l2 = sqrtFloat# mq
              tq = w /% (mq *% 2.0#)
              sina = sqrtFloat# (0.5# -% tq) *% l2 /% sqrtFloat# mv2
          in QFloat
                ( FloatX4#
                 (x *% sina)
                 (y *% sina)
                 (z *% sina)
                 (sqrtFloat# (0.5# +% tq) *% l2)
                )
    {-# INLINE sin #-}
    sin (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (sinFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case cosFloat# w *% sinhFloat# mv
                                 /% mv of
            l -> QFloat
              ( FloatX4#
               (x *% l)
               (y *% l)
               (z *% l)
               (sinFloat# w *% coshFloat# mv)
              )
    {-# INLINE cos #-}
    cos (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (cosFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case sinFloat# w *% sinhFloat# mv
                                 /% negateFloat# mv of
            l -> QFloat
              ( FloatX4#
               (x *% l)
               (y *% l)
               (z *% l)
               (cosFloat# w *% coshFloat# mv)
              )
    {-# INLINE tan #-}
    tan (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (tanFloat# w))
        mv2 ->
          let mv = sqrtFloat# mv2
              chv = coshFloat# mv
              shv = sinhFloat# mv
              ct = cosFloat# w
              st = sinFloat# w
              cq = 1.0# /%
                  ( (ct *% ct *% chv *% chv)
                    +%
                    (st *% st *% shv *% shv)
                  )
              l = chv *% shv *% cq
                      /% mv
          in QFloat
            ( FloatX4#
             (x *% l)
             (y *% l)
             (z *% l)
             (ct *% st *% cq)
            )
    {-# INLINE sinh #-}
    sinh (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (sinhFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case coshFloat# w *% sinFloat# mv
                                  /% mv of
            l -> QFloat
              ( FloatX4#
               (x *% l)
               (y *% l)
               (z *% l)
               (sinhFloat# w *% cosFloat# mv)
              )
    {-# INLINE cosh #-}
    cosh (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (coshFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case sinhFloat# w *% sinFloat# mv
                                  /% mv of
            l -> QFloat
              ( FloatX4#
               (x *% l)
               (y *% l)
               (z *% l)
               (coshFloat# w *% cosFloat# mv)
              )
    {-# INLINE tanh #-}
    tanh (QFloat (FloatX4# x y z w))
      = case (x *% x) +%
             (y *% y) +%
             (z *% z) of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (tanhFloat# w))
        mv2 ->
          let mv = sqrtFloat# mv2
              cv = cosFloat# mv
              sv = sinFloat# mv
              cht = coshFloat# w
              sht = sinhFloat# w
              cq = 1.0# /%
                  ( (cht *% cht *% cv *% cv)
                    +%
                    (sht *% sht *% sv *% sv)
                  )
              l = cv *% sv *% cq
                      /% mv
          in QFloat
            ( FloatX4#
             (x *% l)
             (y *% l)
             (z *% l)
             (cht *% sht *% cq)
            )
    {-# INLINE asin #-}
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where
          i = case signum . im $ q of
                0  -> QFloat (FloatX4# 1.0# 0.0# 0.0# 0.0#)
                i' -> i'
    {-# INLINE acos #-}
    acos q = pi/2 - asin q
    {-# INLINE atan #-}
    atan q@(QFloat (FloatX4# _ _ _ w))
      = if square imq == 0
        then QFloat (FloatX4# 0.0# 0.0# 0.0# (atanFloat# w))
        else i / 2 * log ( (i + q) / (i - q) )
      where
        i = signum imq
        imq = im q
    {-# INLINE asinh #-}
    asinh q = log (q + sqrt (q*q + 1))
    {-# INLINE acosh #-}
    acosh q = log (q + sqrt (q*q - 1))
    {-# INLINE atanh #-}
    atanh q = 0.5 * log ((1+q)/(1-q))

--------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------

instance Eq QFloat where
    {-# INLINE (==) #-}
    QFloat a == QFloat b = a == b
    {-# INLINE (/=) #-}
    QFloat a /= QFloat b = a /= b



--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QFloat where
    show (QFloat (FloatX4# x y z w)) =
        show (F# w) ++ ss x ++ "i"
                    ++ ss y ++ "j"
                    ++ ss z ++ "k"
      where
        ss a# = case F# a# of
          a -> if a >= 0 then " + " ++ show a
                         else " - " ++ show (negate a)
