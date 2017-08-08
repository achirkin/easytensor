-- Note,
--
-- The whole module is made by copying Numeric.Quaternion.QDouble and replacing:
--  Double -> Float
--  Float64 -> Float32
--
-- If we are doing any refactoring of one of these modules, just do the same operation.

{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QFloat
    ( QFloat, Quater (..)
    ) where

import Data.JSString (JSString, unpack')
import Data.Coerce (coerce)

import Numeric.Array
import Numeric.DataFrame.Type
import Numeric.Vector
import Numeric.Matrix

import Numeric.Quaternion.Class


type QFloat = Quater Float

instance Quaternion Float where
    newtype Quater Float = QFloat (ArrayT Float '[4])
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


foreign import javascript unsafe "new Float32Array([$1,$2,$3,$4])"
    js_packQ :: Float -> Float -> Float -> Float -> QFloat

foreign import javascript unsafe "new Float32Array([$1[0],$1[1],$1[2],$2])"
    js_fromVecNum :: Vector Float 3 -> Float -> QFloat

foreign import javascript unsafe "$r1 = $1[0];$r2 = $1[1];$r3 = $1[2];$r4 = $1[3];"
    js_unpackQ :: QFloat -> (Float,Float,Float,Float)


foreign import javascript unsafe "$1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]"
    js_square :: QFloat -> Float

foreign import javascript unsafe "new Float32Array([$1[0],$1[1],$1[2],0])"
    js_im :: QFloat -> QFloat

foreign import javascript unsafe "$1.slice(0,3)"
    js_imVec :: QFloat -> Vector Float 3

foreign import javascript unsafe "$r = new Float32Array(4); $r[3] = $1[3];"
    js_re :: QFloat -> QFloat

foreign import javascript unsafe "$1[3]"
    js_taker :: QFloat -> Float

foreign import javascript unsafe "$1[0]"
    js_takei :: QFloat -> Float

foreign import javascript unsafe "$1[1]"
    js_takej :: QFloat -> Float

foreign import javascript unsafe "$1[2]"
    js_takek :: QFloat -> Float

foreign import javascript unsafe "new Float32Array([-$1[0],-$1[1],-$1[2],$1[3]])"
    js_conjugate :: QFloat -> QFloat


foreign import javascript unsafe "new Float32Array(h$easytensor_rotScale($1,$2))"
    js_rotScale :: QFloat -> Vector Float 3 -> Vector Float 3

foreign import javascript unsafe "new Float32Array(h$easytensor_getRotScale($1,$2))"
    js_getRotScale :: Vector Float 3 -> Vector Float 3 -> QFloat

foreign import javascript unsafe "new Float32Array(h$easytensor_axisRotation($1,$2))"
    js_axisRotation :: Vector Float 3 -> Float -> QFloat

foreign import javascript unsafe "h$easytensor_qArg($1)"
    js_qArg :: QFloat -> Float

foreign import javascript unsafe "new Float32Array(h$easytensor_qfromMatrix33($1))"
    js_fromMatrix33 :: Matrix Float 3 3 -> QFloat

foreign import javascript unsafe "new Float32Array(h$easytensor_qfromMatrix44($1))"
    js_fromMatrix44 :: Matrix Float 4 4 -> QFloat

foreign import javascript unsafe "new Float32Array(h$easytensor_qtoMatrix33($1))"
    js_toMatrix33 :: QFloat -> Matrix Float 3 3

foreign import javascript unsafe "new Float32Array(h$easytensor_qtoMatrix44($1))"
    js_toMatrix44 :: QFloat -> Matrix Float 4 4


--------------------------------------------------------------------------
-- Num
--------------------------------------------------------------------------

instance Num QFloat where
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
    js_plus :: QFloat -> QFloat -> QFloat


foreign import javascript unsafe "$1.map(function (e, i) {return e - $2[i];})"
    js_minus :: QFloat -> QFloat -> QFloat

foreign import javascript unsafe "new Float32Array(\
                                 \[ $1[3]*$2[0] + $1[0]*$2[3] + $1[1]*$2[2] - $1[2]*$2[1]\
                                 \, $1[3]*$2[1] - $1[0]*$2[2] + $1[1]*$2[3] + $1[2]*$2[0]\
                                 \, $1[3]*$2[2] + $1[0]*$2[1] - $1[1]*$2[0] + $1[2]*$2[3]\
                                 \, $1[3]*$2[3] - $1[0]*$2[0] - $1[1]*$2[1] - $1[2]*$2[2] ])"
    js_times :: QFloat -> QFloat -> QFloat

foreign import javascript unsafe "new Float32Array([0,0,0,Math.hypot($1[0],$1[1],$1[2],$1[3])])"
    js_abs :: QFloat -> QFloat

foreign import javascript unsafe "var l = Math.hypot($1[0],$1[1],$1[2],$1[3]); $r = (l == 0) ? (new Float32Array(4)) : $1.map(function (e) {return e/l;})"
    js_signum :: QFloat -> QFloat

foreign import javascript unsafe "$r = new Float32Array(4); $r[3] = $1;"
    js_toQuaternion :: Float -> QFloat

{-# RULES
"realToFrac/FloatQFloat"  realToFrac = js_toQuaternion
"realToFrac/aQFloat"       realToFrac = js_toQuaternion . realToFrac
"fromIntegral/aQFloat"   fromIntegral = js_toQuaternion . fromIntegral
  #-}


--------------------------------------------------------------------------
-- Fractional
--------------------------------------------------------------------------

instance Fractional QFloat where
    {-# INLINE recip #-}
    recip = js_recip
    {-# INLINE (/) #-}
    (/) p = js_times p . js_recip
    {-# INLINE fromRational #-}
    fromRational = js_toQuaternion . fromRational

foreign import javascript unsafe "new Float32Array(h$easytensor_qrecip($1))"
    js_recip :: QFloat -> QFloat


--------------------------------------------------------------------------
-- Floating
--------------------------------------------------------------------------

instance  Floating QFloat where
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
    atan q = if square imq == 0
             then js_toQuaternion (atan $ taker q)
             else i / 2 * log ( (i + q) / (i - q) )
        where i = signum imq
              imq = im q
    {-# INLINE asinh #-}
    asinh q = log (q + sqrt (q*q + 1))
    {-# INLINE acosh #-}
    acosh q = log (q + sqrt (q*q - 1))
    {-# INLINE atanh #-}
    atanh q = 0.5 * log ((1+q)/(1-q))

foreign import javascript unsafe "new Float32Array(h$easytensor_qexp($1))"  js_exp  :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qlog($1))"  js_log  :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qsqrt($1))" js_sqrt :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qsin($1))"  js_sin  :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qcos($1))"  js_cos  :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qtan($1))"  js_tan  :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qsinh($1))" js_sinh :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qcosh($1))" js_cosh :: QFloat -> QFloat
foreign import javascript unsafe "new Float32Array(h$easytensor_qtanh($1))" js_tanh :: QFloat -> QFloat


--------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------

instance Eq QFloat where
    {-# INLINE (==) #-}
    (==) = js_eq
    {-# INLINE (/=) #-}
    (/=) = js_neq



foreign import javascript unsafe "$1[0] === $2[0] && $1[1] === $2[1] && $1[2] === $2[2] && $1[3] === $2[3]"
    js_eq :: QFloat -> QFloat -> Bool
foreign import javascript unsafe "$1[0] !== $2[0] || $1[1] !== $2[1] || $1[2] !== $2[2] || $1[3] !== $2[3]"
    js_neq :: QFloat -> QFloat -> Bool



--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QFloat where
    show = unpack' . js_show

foreign import javascript unsafe "$1[3].toPrecision(8)\
                                 \ + ($1[0] >= 0 ? ' + ' :  ' - ') + Math.abs($1[0]).toPrecision(8) + 'i'\
                                 \ + ($1[1] >= 0 ? ' + ' :  ' - ') + Math.abs($1[1]).toPrecision(8) + 'j'\
                                 \ + ($1[2] >= 0 ? ' + ' :  ' - ') + Math.abs($1[2]).toPrecision(8) + 'k'"
    js_show:: QFloat -> JSString


