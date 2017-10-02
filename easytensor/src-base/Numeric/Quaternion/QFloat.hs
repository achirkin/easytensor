{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QFloat
    ( QFloat, Quater (..)
    ) where

import GHC.Exts
import Data.Coerce (coerce)

import Numeric.Array
import Numeric.DataFrame.Type
--import Numeric.Vector
--import Numeric.Matrix

import Numeric.Quaternion.Class


type QFloat = Quater Float

notYet :: a
notYet = error "Sorry, this function is not implemented for current platform yet."

instance Quaternion Float where
    newtype Quater Float = QFloat FloatX4
    {-# INLINE packQ #-}
    packQ (F# x) (F# y) (F# z) (F# w) = QFloat (FloatX4# x y z w)
    {-# INLINE unpackQ #-}
    unpackQ (QFloat (FloatX4# x y z w)) = (F# x, F# y, F# z, F# w)
    {-# INLINE fromVecNum #-}
    fromVecNum (KnownDataFrame (FloatX3# x y z)) (F# w) = QFloat (FloatX4# x y z w)
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
    imVec (QFloat (FloatX4# x y z _)) = KnownDataFrame (FloatX3# x y z)
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
    rotScale = notYet
    {-# INLINE getRotScale #-}
    getRotScale = notYet
    {-# INLINE axisRotation #-}
    axisRotation = notYet
    {-# INLINE qArg #-}
    qArg = notYet
    {-# INLINE fromMatrix33 #-}
    fromMatrix33 = notYet
    {-# INLINE fromMatrix44 #-}
    fromMatrix44 = notYet
    {-# INLINE toMatrix33 #-}
    toMatrix33 = notYet
    {-# INLINE toMatrix44 #-}
    toMatrix44 = notYet

qdot :: QFloat -> Float#
qdot (QFloat (FloatX4# x y z w)) = timesFloat# x x `plusFloat#`
                                   timesFloat# y y `plusFloat#`
                                   timesFloat# z z `plusFloat#`
                                   timesFloat# w w
{-# INLINE qdot #-}

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
           (timesFloat# a4 b1 `plusFloat#`
            timesFloat# a1 b4 `plusFloat#`
            timesFloat# a2 b3 `minusFloat#`
            timesFloat# a3 b2
            )
           (timesFloat# a4 b2 `minusFloat#`
            timesFloat# a1 b3 `plusFloat#`
            timesFloat# a2 b4 `plusFloat#`
            timesFloat# a3 b1
            )
           (timesFloat# a4 b3 `plusFloat#`
            timesFloat# a1 b2 `minusFloat#`
            timesFloat# a2 b1 `plusFloat#`
            timesFloat# a3 b4
            )
           (timesFloat# a4 b4 `minusFloat#`
            timesFloat# a1 b1 `minusFloat#`
            timesFloat# a2 b2 `minusFloat#`
            timesFloat# a3 b3
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
          qd -> case 1.0# `divideFloat#` sqrtFloat# qd of
             s -> QFloat
               ( FloatX4#
                (timesFloat# x s)
                (timesFloat# y s)
                (timesFloat# z s)
                (timesFloat# w s)
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
    recip q@(QFloat (FloatX4# x y z w)) = case -1.0# `divideFloat#` qdot q of
      c -> QFloat
        ( FloatX4#
         (timesFloat# x c)
         (timesFloat# y c)
         (timesFloat# z c)
         (negateFloat# (timesFloat# w c))
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
      = case (# timesFloat# x x `plusFloat#`
                timesFloat# y y `plusFloat#`
                timesFloat# z z
             , expFloat# w
             #) of
        (# 0.0#, et #) -> QFloat (FloatX4# 0.0# 0.0# 0.0# et)
        (# mv2, et #) -> case sqrtFloat# mv2 of
          mv -> case et `timesFloat#` sinFloat# mv
                        `divideFloat#` mv of
            l -> QFloat
              ( FloatX4#
               (timesFloat# x l)
               (timesFloat# y l)
               (timesFloat# z l)
               (et `timesFloat#` cosFloat# mv)
              )
    {-# INLINE log #-}
    log (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> if isTrue# (w `geFloat#` 0.0#)
                then QFloat (FloatX4# 0.0# 0.0# 0.0# (logFloat# w))
                else QFloat (FloatX4# 3.141592653589793# 0.0# 0.0#
                                     (logFloat# (negateFloat# w)))
        mv2 -> case (# sqrtFloat# (mv2 `plusFloat#` timesFloat# w w)
                     , sqrtFloat# mv2
                    #) of
          (# mq, mv #) -> case atan2 (F# mv) (F# w) / F# mv of
            F# l -> QFloat
              ( FloatX4#
               (timesFloat# x l)
               (timesFloat# y l)
               (timesFloat# z l)
               (logFloat# mq)
              )
    {-# INLINE sqrt #-}
    sqrt (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> if isTrue# (w `geFloat#` 0.0#)
                then QFloat (FloatX4# 0.0# 0.0# 0.0# (sqrtFloat# w))
                else QFloat (FloatX4# (sqrtFloat# (negateFloat# w)) 0.0# 0.0# 0.0#)
        mv2 -> case sqrtFloat# (mv2 `plusFloat#` timesFloat# w w) of
          mq -> case (# sqrtFloat# mq
                     ,  w `divideFloat#` timesFloat# mq 2.0#
                     #) of
            (# l2, tq #) -> case sqrtFloat# (minusFloat# 0.5# tq)
                                `timesFloat#` l2
                                `divideFloat#` sqrtFloat# mv2 of
              sina -> QFloat
                ( FloatX4#
                 (timesFloat# x sina)
                 (timesFloat# y sina)
                 (timesFloat# z sina)
                 (sqrtFloat# (plusFloat# 0.5# tq) `timesFloat#` l2)
                )
    {-# INLINE sin #-}
    sin (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (sinFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case cosFloat# w `timesFloat#` sinhFloat# mv
                                 `divideFloat#` mv of
            l -> QFloat
              ( FloatX4#
               (timesFloat# x l)
               (timesFloat# y l)
               (timesFloat# z l)
               (sinFloat# w `timesFloat#` coshFloat# mv)
              )
    {-# INLINE cos #-}
    cos (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (cosFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case sinFloat# w `timesFloat#` sinhFloat# mv
                                 `divideFloat#` negateFloat# mv of
            l -> QFloat
              ( FloatX4#
               (timesFloat# x l)
               (timesFloat# y l)
               (timesFloat# z l)
               (cosFloat# w `timesFloat#` coshFloat# mv)
              )
    {-# INLINE tan #-}
    tan (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (tanFloat# w))
        mv2 ->
          let mv = sqrtFloat# mv2
              chv = coshFloat# mv
              shv = sinhFloat# mv
              ct = cosFloat# w
              st = sinFloat# w
              cq = 1.0# `divideFloat#`
                  ( (ct `timesFloat#` ct `timesFloat#` chv `timesFloat#` chv)
                    `plusFloat#`
                    (st `timesFloat#` st `timesFloat#` shv `timesFloat#` shv)
                  )
              l = chv `timesFloat#` shv `timesFloat#` cq
                      `divideFloat#` mv
          in QFloat
            ( FloatX4#
             (timesFloat# x l)
             (timesFloat# y l)
             (timesFloat# z l)
             (ct `timesFloat#` st `timesFloat#` cq)
            )
    {-# INLINE sinh #-}
    sinh (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (sinhFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case coshFloat# w `timesFloat#` sinFloat# mv
                                  `divideFloat#` mv of
            l -> QFloat
              ( FloatX4#
               (timesFloat# x l)
               (timesFloat# y l)
               (timesFloat# z l)
               (sinhFloat# w `timesFloat#` cosFloat# mv)
              )
    {-# INLINE cosh #-}
    cosh (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (coshFloat# w))
        mv2 -> case sqrtFloat# mv2 of
          mv -> case sinhFloat# w `timesFloat#` sinFloat# mv
                                  `divideFloat#` mv of
            l -> QFloat
              ( FloatX4#
               (timesFloat# x l)
               (timesFloat# y l)
               (timesFloat# z l)
               (coshFloat# w `timesFloat#` cosFloat# mv)
              )
    {-# INLINE tanh #-}
    tanh (QFloat (FloatX4# x y z w))
      = case timesFloat# x x `plusFloat#`
             timesFloat# y y `plusFloat#`
             timesFloat# z z of
        0.0# -> QFloat (FloatX4# 0.0# 0.0# 0.0# (tanhFloat# w))
        mv2 ->
          let mv = sqrtFloat# mv2
              cv = cosFloat# mv
              sv = sinFloat# mv
              cht = coshFloat# w
              sht = sinhFloat# w
              cq = 1.0# `divideFloat#`
                  ( (cht `timesFloat#` cht `timesFloat#` cv `timesFloat#` cv)
                    `plusFloat#`
                    (sht `timesFloat#` sht `timesFloat#` sv `timesFloat#` sv)
                  )
              l = cv `timesFloat#` sv `timesFloat#` cq
                      `divideFloat#` mv
          in QFloat
            ( FloatX4#
             (timesFloat# x l)
             (timesFloat# y l)
             (timesFloat# z l)
             (cht `timesFloat#` sht `timesFloat#` cq)
            )
    {-# INLINE asin #-}
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where
          i = case signum . im $ q of
                0 -> QFloat (FloatX4# 1.0# 0.0# 0.0# 0.0#)
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
