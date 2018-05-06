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
module Numeric.Quaternion.QDouble
    ( QDouble, Quater (..)
    ) where

import           Data.Coerce                                      (coerce)
import           GHC.Exts

import qualified Control.Monad.ST                                 as ST
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family.DoubleX3
import           Numeric.DataFrame.Internal.Array.Family.DoubleX4
import qualified Numeric.DataFrame.ST                             as ST
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import qualified Numeric.Dimensions.Fold                          as ST
import           Numeric.PrimBytes                                (PrimBytes)
import           Numeric.Quaternion.Class
import           Numeric.Scalar
import           Numeric.Vector


type QDouble = Quater Double

deriving instance PrimBytes (Quater Double)
deriving instance PrimArray Double (Quater Double)

instance Quaternion Double where
    newtype Quater Double = QDouble DoubleX4
    {-# INLINE packQ #-}
    packQ (D# x) (D# y) (D# z) (D# w) = QDouble (DoubleX4# x y z w)
    {-# INLINE unpackQ #-}
    unpackQ (QDouble (DoubleX4# x y z w)) = (D# x, D# y, D# z, D# w)
    {-# INLINE fromVecNum #-}
    fromVecNum (SingleFrame (DoubleX3# x y z)) (D# w) = QDouble (DoubleX4# x y z w)
    {-# INLINE fromVec4 #-}
    fromVec4 = coerce
    {-# INLINE toVec4 #-}
    toVec4 = coerce
    {-# INLINE square #-}
    square q = D# (qdot q)
    {-# INLINE im #-}
    im (QDouble (DoubleX4# x y z _)) = QDouble (DoubleX4# x y z 0.0##)
    {-# INLINE re #-}
    re (QDouble (DoubleX4# _ _ _ w)) = QDouble (DoubleX4# 0.0## 0.0## 0.0## w)
    {-# INLINE imVec #-}
    imVec (QDouble (DoubleX4# x y z _)) = SingleFrame (DoubleX3# x y z)
    {-# INLINE taker #-}
    taker (QDouble (DoubleX4# _ _ _ w)) = D# w
    {-# INLINE takei #-}
    takei (QDouble (DoubleX4# x _ _ _)) = D# x
    {-# INLINE takej #-}
    takej (QDouble (DoubleX4# _ y _ _)) = D# y
    {-# INLINE takek #-}
    takek (QDouble (DoubleX4# _ _ z _)) = D# z
    {-# INLINE conjugate #-}
    conjugate (QDouble (DoubleX4# x y z w)) = QDouble (DoubleX4#
                                                (negateDouble# x)
                                                (negateDouble# y)
                                                (negateDouble# z) w)
    {-# INLINE rotScale #-}
    rotScale (QDouble (DoubleX4# i j k t))
             (SingleFrame (DoubleX3# x y z))
      = let l = t*##t -## i*##i -## j*##j -## k*##k
            d = 2.0## *## ( i*##x +## j*##y +## k*##z)
            t2 = t *## 2.0##
        in SingleFrame
            ( DoubleX3#
                (l*##x +## d*##i +## t2 *## (z*##j -## y*##k))
                (l*##y +## d*##j +## t2 *## (x*##k -## z*##i))
                (l*##z +## d*##k +## t2 *## (y*##i -## x*##j))
            )
    {-# INLINE getRotScale #-}
    getRotScale _ (SingleFrame (DoubleX3# 0.0## 0.0## 0.0##))
      = QDouble (DoubleX4# 0.0## 0.0## 0.0## 0.0##)
    getRotScale (SingleFrame (DoubleX3# 0.0## 0.0## 0.0##)) _
      = case infty of D# x -> QDouble (DoubleX4# x x x x)
    getRotScale a@(SingleFrame (DoubleX3# a1 a2 a3))
                b@(SingleFrame (DoubleX3# b1 b2 b3))
      = let ma = sqrtDouble# (a1*##a1 +## a2*##a2 +## a3*##a3)
            mb = sqrtDouble# (b1*##b1 +## b2*##b2 +## b3*##b3)
            d  = a1*##b1 +## a2*##b2 +## a3*##b3
            c  = sqrtDouble# (ma*##mb +## d)
            ma2 = ma *## sqrtDouble# 2.0##
            r  = 1.0## /## (ma2 *## c)
        in case cross a b of
          SingleFrame (DoubleX3# 0.0## 0.0## 0.0##) ->
            if isTrue# (d >## 0.0##)
            then QDouble (DoubleX4#  0.0## 0.0## 0.0## (sqrtDouble# (mb /## ma)))
                 -- Shall we move result from k to i component?
            else QDouble (DoubleX4#  0.0## 0.0## (sqrtDouble# (mb /## ma)) 0.0##)
          SingleFrame (DoubleX3# t1 t2 t3) -> QDouble
                ( DoubleX4#
                    (t1 *## r)
                    (t2 *## r)
                    (t3 *## r)
                    (c /## ma2)
                )
    {-# INLINE axisRotation #-}
    axisRotation (SingleFrame (DoubleX3# 0.0## 0.0## 0.0##)) _
      = QDouble (DoubleX4# 0.0## 0.0## 0.0## 1.0##)
    axisRotation (SingleFrame (DoubleX3# x y z)) (D# a)
      = let c = cosDouble# (a *## 0.5##)
            s = sinDouble# (a *## 0.5##)
                /## sqrtDouble# (x*##x +## y*##y +## z*##z)
        in QDouble
              ( DoubleX4#
                  (x *## s)
                  (y *## s)
                  (z *## s)
                  c
              )
    {-# INLINE qArg #-}
    qArg (QDouble (DoubleX4# x y z w))
       = case atan2 (D# (sqrtDouble# (x*##x +## y*##y +## z*##z)))
                    (D# w) of
           D# a -> D# (a *## 2.0##)
    {-# INLINE fromMatrix33 #-}
    fromMatrix33 m
      = let d =
              (  ix 0# m *## ( ix 4# m *## ix 8# m -## ix 5# m *## ix 7# m )
              -## ix 1# m *## ( ix 3# m *## ix 8# m -## ix 5# m *## ix 6# m )
              +## ix 2# m *## ( ix 3# m *## ix 7# m -## ix 4# m *## ix 6# m )
              ) **## 0.33333333333333333333333333333333##
        in QDouble
           ( DoubleX4#
            (sqrtDouble# (max# 0.0## (d +## ix 0# m -## ix 4# m -## ix 8# m )) *## sign# (ix 5# m -## ix 7# m) *## 0.5##)
            (sqrtDouble# (max# 0.0## (d -## ix 0# m +## ix 4# m -## ix 8# m )) *## sign# (ix 6# m -## ix 2# m) *## 0.5##)
            (sqrtDouble# (max# 0.0## (d -## ix 0# m -## ix 4# m +## ix 8# m )) *## sign# (ix 1# m -## ix 3# m) *## 0.5##)
            (sqrtDouble# (max# 0.0## (d +## ix 0# m +## ix 4# m +## ix 8# m )) *## 0.5##)
           )
    {-# INLINE fromMatrix44 #-}
    fromMatrix44 m
      = let d =
              (  ix 0# m *## ( ix 5# m *## ix 10# m -## ix 6# m *## ix 9# m )
              -## ix 1# m *## ( ix 4# m *## ix 10# m -## ix 6# m *## ix 8# m )
              +## ix 2# m *## ( ix 4# m *## ix  9# m -## ix 5# m *## ix 8# m )
              ) **## 0.33333333333333333333333333333333##
            c = 0.5## /## ix 15# m
        in QDouble
           ( DoubleX4#
            (sqrtDouble# (max# 0.0## (d +## ix 0# m -## ix 5# m -## ix 10# m )) *## sign# (ix 6# m -## ix 9# m) *## c)
            (sqrtDouble# (max# 0.0## (d -## ix 0# m +## ix 5# m -## ix 10# m )) *## sign# (ix 8# m -## ix 2# m) *## c)
            (sqrtDouble# (max# 0.0## (d -## ix 0# m -## ix 5# m +## ix 10# m )) *## sign# (ix 1# m -## ix 4# m) *## c)
            (sqrtDouble# (max# 0.0## (d +## ix 0# m +## ix 5# m +## ix 10# m )) *## c)
           )
    {-# INLINE toMatrix33 #-}
    toMatrix33 (QDouble (DoubleX4# 0.0## 0.0## 0.0## w))
      = let x = D# (w *## w)
            f 0 = (# 3 :: Int , x #)
            f k = (# k-1, 0 #)
        in case gen# 9# f 0 of
            (# _, m #) -> m -- diag (scalar (D# (w *## w)))
    toMatrix33 (QDouble (DoubleX4# x' y' z' w')) =
      let x = scalar (D# x')
          y = scalar (D# y')
          z = scalar (D# z')
          w = scalar (D# w')
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
    toMatrix44 (QDouble (DoubleX4# 0.0## 0.0## 0.0## w)) = ST.runST $ do
      df <- ST.newDataFrame
      ST.overDimOff_ (dims :: Dims '[4,4]) (\i -> ST.writeDataFrameOff df i 0) 0 1
      let w2 = scalar (D# (w *## w))
      ST.writeDataFrameOff df 0 w2
      ST.writeDataFrameOff df 5 w2
      ST.writeDataFrameOff df 10 w2
      ST.writeDataFrameOff df 15 1
      ST.unsafeFreezeDataFrame df
    toMatrix44 (QDouble (DoubleX4# x' y' z' w')) =
      let x = scalar (D# x')
          y = scalar (D# y')
          z = scalar (D# z')
          w = scalar (D# w')
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

qdot :: QDouble -> Double#
qdot (QDouble (DoubleX4# x y z w)) = (x *## x) +##
                                   (y *## y) +##
                                   (z *## z) +##
                                   (w *## w)
{-# INLINE qdot #-}

infty :: Double
infty = read "Infinity"

max# :: Double# -> Double# -> Double#
max# a b | isTrue# (a >## b) = a
         | otherwise = b
{-# INLINE max# #-}

sign# :: Double# -> Double#
sign# a | isTrue# (a >## 0.0##) = 1.0##
        | isTrue# (a <## 0.0##) = negateDouble# 1.0##
        | otherwise = 0.0##
{-# INLINE sign# #-}

ix :: PrimArray Double a => Int# -> a -> Double#
ix i a = case ix# i a of D# r -> r
{-# INLINE ix #-}

--------------------------------------------------------------------------
-- Num
--------------------------------------------------------------------------

instance Num QDouble where
    QDouble a + QDouble b
      = QDouble (a + b)
    {-# INLINE (+) #-}
    QDouble a - QDouble b
      = QDouble (a - b)
    {-# INLINE (-) #-}
    QDouble (DoubleX4# a1 a2 a3 a4) * QDouble (DoubleX4# b1 b2 b3 b4)
      = QDouble
         ( DoubleX4#
           ((a4 *## b1) +##
            (a1 *## b4) +##
            (a2 *## b3) -##
            (a3 *## b2)
            )
           ((a4 *## b2) -##
            (a1 *## b3) +##
            (a2 *## b4) +##
            (a3 *## b1)
            )
           ((a4 *## b3) +##
            (a1 *## b2) -##
            (a2 *## b1) +##
            (a3 *## b4)
            )
           ((a4 *## b4) -##
            (a1 *## b1) -##
            (a2 *## b2) -##
            (a3 *## b3)
            )
         )
    {-# INLINE (*) #-}
    negate (QDouble a) = QDouble (negate a)
    {-# INLINE negate #-}
    abs q = QDouble (DoubleX4# 0.0## 0.0## 0.0## (sqrtDouble# (qdot q)))
    {-# INLINE abs #-}
    signum q@(QDouble (DoubleX4# x y z w))
      = case qdot q of
          0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## 0.0##)
          qd -> case 1.0## /## sqrtDouble# qd of
             s -> QDouble
               ( DoubleX4#
                (x *## s)
                (y *## s)
                (z *## s)
                (w *## s)
               )
    {-# INLINE signum #-}
    fromInteger n = case fromInteger n of
      D# x -> QDouble (DoubleX4# 0.0## 0.0## 0.0## x)
    {-# INLINE fromInteger #-}



--------------------------------------------------------------------------
-- Fractional
--------------------------------------------------------------------------

instance Fractional QDouble where
    {-# INLINE recip #-}
    recip q@(QDouble (DoubleX4# x y z w)) = case -1.0## /## qdot q of
      c -> QDouble
        ( DoubleX4#
         (x *## c)
         (y *## c)
         (z *## c)
         (negateDouble# (w *## c))
        )
    {-# INLINE (/) #-}
    a / b = a * recip b
    {-# INLINE fromRational #-}
    fromRational q = case fromRational q of
      D# x -> QDouble (DoubleX4# 0.0## 0.0## 0.0## x)

--------------------------------------------------------------------------
-- Doubleing
--------------------------------------------------------------------------

instance  Floating QDouble where
    {-# INLINE pi #-}
    pi = QDouble (DoubleX4# 0.0## 0.0## 0.0##
                          3.141592653589793##)
    {-# INLINE exp #-}
    exp (QDouble (DoubleX4# x y z w))
      = case (# (x *## x) +##
                (y *## y) +##
                (z *## z)
             , expDouble# w
             #) of
        (# 0.0##, et #) -> QDouble (DoubleX4# 0.0## 0.0## 0.0## et)
        (# mv2, et #) -> case sqrtDouble# mv2 of
          mv -> case et *## sinDouble# mv
                        /## mv of
            l -> QDouble
              ( DoubleX4#
               (x *## l)
               (y *## l)
               (z *## l)
               (et *## cosDouble# mv)
              )
    {-# INLINE log #-}
    log (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> if isTrue# (w >=## 0.0##)
                then QDouble (DoubleX4# 0.0## 0.0## 0.0## (logDouble# w))
                else QDouble (DoubleX4# 3.141592653589793## 0.0## 0.0##
                                     (logDouble# (negateDouble# w)))
        mv2 -> case (# sqrtDouble# (mv2 +## (w *## w))
                     , sqrtDouble# mv2
                    #) of
          (# mq, mv #) -> case atan2 (D# mv) (D# w) / D# mv of
            D# l -> QDouble
              ( DoubleX4#
               (x *## l)
               (y *## l)
               (z *## l)
               (logDouble# mq)
              )
    {-# INLINE sqrt #-}
    sqrt (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> if isTrue# (w >=## 0.0##)
                then QDouble (DoubleX4# 0.0## 0.0## 0.0## (sqrtDouble# w))
                else QDouble (DoubleX4# (sqrtDouble# (negateDouble# w)) 0.0## 0.0## 0.0##)
        mv2 ->
          let mq = sqrtDouble# (mv2 +## w *## w)
              l2 = sqrtDouble# mq
              tq = w /## (mq *## 2.0##)
              sina = sqrtDouble# (0.5## -## tq) *## l2 /## sqrtDouble# mv2
          in QDouble
                ( DoubleX4#
                 (x *## sina)
                 (y *## sina)
                 (z *## sina)
                 (sqrtDouble# (0.5## +## tq) *## l2)
                )
    {-# INLINE sin #-}
    sin (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## (sinDouble# w))
        mv2 -> case sqrtDouble# mv2 of
          mv -> case cosDouble# w *## sinhDouble# mv
                                 /## mv of
            l -> QDouble
              ( DoubleX4#
               (x *## l)
               (y *## l)
               (z *## l)
               (sinDouble# w *## coshDouble# mv)
              )
    {-# INLINE cos #-}
    cos (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## (cosDouble# w))
        mv2 -> case sqrtDouble# mv2 of
          mv -> case sinDouble# w *## sinhDouble# mv
                                 /## negateDouble# mv of
            l -> QDouble
              ( DoubleX4#
               (x *## l)
               (y *## l)
               (z *## l)
               (cosDouble# w *## coshDouble# mv)
              )
    {-# INLINE tan #-}
    tan (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## (tanDouble# w))
        mv2 ->
          let mv = sqrtDouble# mv2
              chv = coshDouble# mv
              shv = sinhDouble# mv
              ct = cosDouble# w
              st = sinDouble# w
              cq = 1.0## /##
                  ( (ct *## ct *## chv *## chv)
                    +##
                    (st *## st *## shv *## shv)
                  )
              l = chv *## shv *## cq
                      /## mv
          in QDouble
            ( DoubleX4#
             (x *## l)
             (y *## l)
             (z *## l)
             (ct *## st *## cq)
            )
    {-# INLINE sinh #-}
    sinh (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## (sinhDouble# w))
        mv2 -> case sqrtDouble# mv2 of
          mv -> case coshDouble# w *## sinDouble# mv
                                  /## mv of
            l -> QDouble
              ( DoubleX4#
               (x *## l)
               (y *## l)
               (z *## l)
               (sinhDouble# w *## cosDouble# mv)
              )
    {-# INLINE cosh #-}
    cosh (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## (coshDouble# w))
        mv2 -> case sqrtDouble# mv2 of
          mv -> case sinhDouble# w *## sinDouble# mv
                                  /## mv of
            l -> QDouble
              ( DoubleX4#
               (x *## l)
               (y *## l)
               (z *## l)
               (coshDouble# w *## cosDouble# mv)
              )
    {-# INLINE tanh #-}
    tanh (QDouble (DoubleX4# x y z w))
      = case (x *## x) +##
             (y *## y) +##
             (z *## z) of
        0.0## -> QDouble (DoubleX4# 0.0## 0.0## 0.0## (tanhDouble# w))
        mv2 ->
          let mv = sqrtDouble# mv2
              cv = cosDouble# mv
              sv = sinDouble# mv
              cht = coshDouble# w
              sht = sinhDouble# w
              cq = 1.0## /##
                  ( (cht *## cht *## cv *## cv)
                    +##
                    (sht *## sht *## sv *## sv)
                  )
              l = cv *## sv *## cq
                      /## mv
          in QDouble
            ( DoubleX4#
             (x *## l)
             (y *## l)
             (z *## l)
             (cht *## sht *## cq)
            )
    {-# INLINE asin #-}
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where
          i = case signum . im $ q of
                0  -> QDouble (DoubleX4# 1.0## 0.0## 0.0## 0.0##)
                i' -> i'
    {-# INLINE acos #-}
    acos q = pi/2 - asin q
    {-# INLINE atan #-}
    atan q@(QDouble (DoubleX4# _ _ _ w))
      = if square imq == 0
        then QDouble (DoubleX4# 0.0## 0.0## 0.0## (atanDouble# w))
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

instance Eq QDouble where
    {-# INLINE (==) #-}
    QDouble a == QDouble b = a == b
    {-# INLINE (/=) #-}
    QDouble a /= QDouble b = a /= b



--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QDouble where
    show (QDouble (DoubleX4# x y z w)) =
        show (D# w) ++ ss x ++ "i"
                    ++ ss y ++ "j"
                    ++ ss z ++ "k"
      where
        ss a# = case D# a# of
          a -> if a >= 0 then " + " ++ show a
                         else " - " ++ show (negate a)
