{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QDouble
    ( QDouble, Quater (..)
    ) where

import Data.JSString (JSString, unpack')
import Data.Coerce (coerce)

import Numeric.Array
import Numeric.DataFrame.Type
import Numeric.Vector
import Numeric.Matrix

import Numeric.Quaternion.Class


type QDouble = Quater Double

instance Quaternion Double where
    newtype Quater Double = QDouble (ArrayT Double '[4])
    {-# INLINE packQ #-}
    packQ = js_packQ
    {-# INLINE fromVecNum #-}
    fromVecNum = js_fromVecNum
    {-# INLINE fromVec4 #-}
    fromVec4 = coerce
    {-# INLINE toVec4 #-}
    toVec4 = coerce
    {-# INLINE unpackQ #-}
    unpackQ = js_unpackQ
    {-# INLINE square #-}
    square = js_square
    {-# INLINE im #-}
    im = js_im
    {-# INLINE re #-}
    re = js_re
    {-# INLINE imVec #-}
    imVec = js_imVec
    {-# INLINE taker #-}
    taker = js_taker
    {-# INLINE takei #-}
    takei = js_takei
    {-# INLINE takej #-}
    takej = js_takej
    {-# INLINE takek #-}
    takek = js_takek
    {-# INLINE conjugate #-}
    conjugate = js_conjugate
    {-# INLINE rotScale #-}
    rotScale = js_rotScale
    {-# INLINE getRotScale #-}
    getRotScale = js_getRotScale
    {-# INLINE axisRotation #-}
    axisRotation = js_axisRotation
    {-# INLINE qArg #-}
    qArg = js_qArg
    {-# INLINE fromMatrix33 #-}
    fromMatrix33 = js_fromMatrix33
    {-# INLINE fromMatrix44 #-}
    fromMatrix44 = js_fromMatrix44
    {-# INLINE toMatrix33 #-}
    toMatrix33 = js_toMatrix33
    {-# INLINE toMatrix44 #-}
    toMatrix44 = js_toMatrix44


foreign import javascript unsafe "new Float64Array([$1,$2,$3,$4])"
    js_packQ :: Double -> Double -> Double -> Double -> QDouble

foreign import javascript unsafe "new Float64Array([$1[0],$1[1],$1[2],$2])"
    js_fromVecNum :: Vector Double 3 -> Double -> QDouble

foreign import javascript unsafe "$r1 = $1[0];$r2 = $1[1];$r3 = $1[2];$r4 = $1[3];"
    js_unpackQ :: QDouble -> (Double,Double,Double,Double)


foreign import javascript unsafe "$1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]"
    js_square :: QDouble -> Double

foreign import javascript unsafe "new Float64Array([$1[0],$1[1],$1[2],0])"
    js_im :: QDouble -> QDouble

foreign import javascript unsafe "$1.slice(0,3)"
    js_imVec :: QDouble -> Vector Double 3

foreign import javascript unsafe "$r = new Float64Array(4); $r[3] = $1[3];"
    js_re :: QDouble -> QDouble

foreign import javascript unsafe "$1[3]"
    js_taker :: QDouble -> Double

foreign import javascript unsafe "$1[0]"
    js_takei :: QDouble -> Double

foreign import javascript unsafe "$1[1]"
    js_takej :: QDouble -> Double

foreign import javascript unsafe "$1[2]"
    js_takek :: QDouble -> Double

foreign import javascript unsafe "new Float64Array([-$1[0],-$1[1],-$1[2],$1[3]])"
    js_conjugate :: QDouble -> QDouble


foreign import javascript unsafe "new Float64Array(h$easytensor_rotScale($1,$2))"
    js_rotScale :: QDouble -> Vector Double 3 -> Vector Double 3

foreign import javascript unsafe "new Float64Array(h$easytensor_getRotScale($1,$2))"
    js_getRotScale :: Vector Double 3 -> Vector Double 3 -> QDouble

foreign import javascript unsafe "new Float64Array(h$easytensor_axisRotation($1,$2))"
    js_axisRotation :: Vector Double 3 -> Double -> QDouble

foreign import javascript unsafe "h$easytensor_qArg($1)"
    js_qArg :: QDouble -> Double

foreign import javascript unsafe "new Float64Array(h$easytensor_qfromMatrix33($1))"
    js_fromMatrix33 :: Matrix Double 3 3 -> QDouble

foreign import javascript unsafe "new Float64Array(h$easytensor_qfromMatrix44($1))"
    js_fromMatrix44 :: Matrix Double 4 4 -> QDouble

foreign import javascript unsafe "new Float64Array(h$easytensor_qtoMatrix33($1))"
    js_toMatrix33 :: QDouble -> Matrix Double 3 3

foreign import javascript unsafe "new Float64Array(h$easytensor_qtoMatrix44($1))"
    js_toMatrix44 :: QDouble -> Matrix Double 4 4


--------------------------------------------------------------------------
-- Num
--------------------------------------------------------------------------

instance Num QDouble where
    {-# INLINE (+) #-}
    (+) = js_plus
    {-# INLINE (-) #-}
    (-) = js_minus
    {-# INLINE (*) #-}
    (*) = js_times
    {-# INLINE abs #-}
    abs = js_abs
    {-# INLINE signum #-}
    signum = js_signum
    {-# INLINE negate #-}
    negate = fromVec4 . negate . toVec4
    {-# INLINE fromInteger #-}
    fromInteger = js_toQuaternion . fromInteger

foreign import javascript unsafe "$1.map(function (e, i) {return e + $2[i];})"
    js_plus :: QDouble -> QDouble -> QDouble


foreign import javascript unsafe "$1.map(function (e, i) {return e - $2[i];})"
    js_minus :: QDouble -> QDouble -> QDouble

foreign import javascript unsafe "new Float64Array(\
                                 \[ $1[3]*$2[0] + $1[0]*$2[3] + $1[1]*$2[2] - $1[2]*$2[1]\
                                 \, $1[3]*$2[1] - $1[0]*$2[2] + $1[1]*$2[3] + $1[2]*$2[0]\
                                 \, $1[3]*$2[2] + $1[0]*$2[1] - $1[1]*$2[0] + $1[2]*$2[3]\
                                 \, $1[3]*$2[3] - $1[0]*$2[0] - $1[1]*$2[1] - $1[2]*$2[2] ])"
    js_times :: QDouble -> QDouble -> QDouble

foreign import javascript unsafe "new Float64Array([0,0,0,Math.hypot($1[0],$1[1],$1[2],$1[3])])"
    js_abs :: QDouble -> QDouble

foreign import javascript unsafe "var l = Math.hypot($1[0],$1[1],$1[2],$1[3]); $r = (l == 0) ? (new Float64Array(4)) : $1.map(function (e) {return e/l;})"
    js_signum :: QDouble -> QDouble

foreign import javascript unsafe "$r = new Float64Array(4); $r[3] = $1;"
    js_toQuaternion :: Double -> QDouble

{-# RULES
"realToFrac/DoubleQDouble"  realToFrac = js_toQuaternion
"realToFrac/aQDouble"       realToFrac = js_toQuaternion . realToFrac
"fromIntegral/aQDouble"   fromIntegral = js_toQuaternion . fromIntegral
  #-}


--------------------------------------------------------------------------
-- Fractional
--------------------------------------------------------------------------

instance Fractional QDouble where
    {-# INLINE recip #-}
    recip = js_recip
    {-# INLINE (/) #-}
    (/) p = js_times p . js_recip
    {-# INLINE fromRational #-}
    fromRational = js_toQuaternion . fromRational

foreign import javascript unsafe "new Float64Array(h$easytensor_qrecip($1))"
    js_recip :: QDouble -> QDouble


--------------------------------------------------------------------------
-- Floating
--------------------------------------------------------------------------

instance  Floating QDouble where
    {-# INLINE pi #-}
    pi = js_toQuaternion pi
    {-# INLINE exp #-}
    exp = js_exp
    {-# INLINE log #-}
    log = js_log
    {-# INLINE sqrt #-}
    sqrt = js_sqrt
    {-# INLINE sin #-}
    sin = js_sin
    {-# INLINE cos #-}
    cos = js_cos
    {-# INLINE tan #-}
    tan = js_tan
    {-# INLINE sinh #-}
    sinh = js_sinh
    {-# INLINE cosh #-}
    cosh = js_cosh
    {-# INLINE tanh #-}
    tanh =  js_tanh
    {-# INLINE asin #-}
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where i = signum . im $ q
    {-# INLINE acos #-}
    acos q = pi/2 - asin q
    {-# INLINE atan #-}
    atan q = i/2 * (log (1 - iq) - log (1 + iq))
        where i = signum . im $ q
              iq = i*q
    {-# INLINE asinh #-}
    asinh q = log (q + sqrt (q*q + 1))
    {-# INLINE acosh #-}
    acosh q = log (q + sqrt (q*q - 1))
    {-# INLINE atanh #-}
    atanh q = 0.5 * log ((1+q)/(1-q))

foreign import javascript unsafe "new Float64Array(h$easytensor_qexp($1))"
    js_exp :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qlog($1))"
    js_log :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qsqrt($1))"
    js_sqrt :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qsin($1))"
    js_sin :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qcos($1))"
    js_cos :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qtan($1))"
    js_tan :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qsinh($1))"
    js_sinh :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qcosh($1))"
    js_cosh :: QDouble -> QDouble
foreign import javascript unsafe "new Float64Array(h$easytensor_qtanh($1))"
    js_tanh :: QDouble -> QDouble


--------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------

instance Eq QDouble where
    {-# INLINE (==) #-}
    (==) = js_eq
    {-# INLINE (/=) #-}
    (/=) = js_neq



foreign import javascript unsafe "$1[0] === $2[0] && $1[1] === $2[1] && $1[2] === $2[2] && $1[3] === $2[3]"
    js_eq :: QDouble -> QDouble -> Bool
foreign import javascript unsafe "$1[0] !== $2[0] || $1[1] !== $2[1] || $1[2] !== $2[2] || $1[3] !== $2[3]"
    js_neq :: QDouble -> QDouble -> Bool



--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QDouble where
    show = unpack' . js_show

foreign import javascript unsafe "$1[3].toPrecision(8)\
                                 \ + ($1[0] >= 0 ? ' + ' :  ' - ') + Math.abs($1[0]).toPrecision(8) + 'i'\
                                 \ + ($1[1] >= 0 ? ' + ' :  ' - ') + Math.abs($1[1]).toPrecision(8) + 'j'\
                                 \ + ($1[2] >= 0 ? ' + ' :  ' - ') + Math.abs($1[2]).toPrecision(8) + 'k'"
    js_show:: QDouble -> JSString


