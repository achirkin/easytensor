{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QFloat
    ( QFloat
    ) where

import Data.Coerce (coerce)

import Numeric.Quaternion.Class
import Numeric.Array.Family

instance Quaternion Float where
    newtype Quater Float = QFloat (ArrayT Float '[4])
    {-# INLINE setQ #-}
    setQ = js_setQF
    {-# INLINE fromVecNum #-}
    fromVecNum = js_fromVecNumF
    {-# INLINE fromVec4 #-}
    fromVec4 = js_fromVec4F
    {-# INLINE toVec4 #-}
    toVec4 = js_toVec4F
    {-# INLINE unpackQ #-}
    unpackQ = js_unpackQF
    {-# INLINE square #-}
    square = js_squareF
    {-# INLINE im #-}
    im = js_imF
    {-# INLINE re #-}
    re = js_reF
    {-# INLINE imVec #-}
    imVec = js_imVecF
    {-# INLINE taker #-}
    taker = js_takerF
    {-# INLINE takei #-}
    takei = js_takeiF
    {-# INLINE takej #-}
    takej = js_takejF
    {-# INLINE takek #-}
    takek = js_takekF
    {-# INLINE conjugate #-}
    conjugate = js_conjugateF
    {-# INLINE rotScale #-}
    rotScale = js_rotScaleF
    {-# INLINE getRotScale #-}
    getRotScale = js_getRotScaleF
    {-# INLINE axisRotation #-}
    axisRotation = js_axisRotationF
    {-# INLINE qArg #-}
    qArg = js_qArgF

instance Quaternion QDouble Double where
    {-# INLINE setQ #-}
    setQ = js_setQD
    {-# INLINE fromVecNum #-}
    fromVecNum = js_fromVecNumD
    {-# INLINE fromVec4 #-}
    fromVec4 = js_fromVec4D
    {-# INLINE toVec4 #-}
    toVec4 = js_toVec4D
    {-# INLINE unpackQ #-}
    unpackQ = js_unpackQD
    {-# INLINE square #-}
    square = js_squareD
    {-# INLINE im #-}
    im = js_imD
    {-# INLINE re #-}
    re = js_reD
    {-# INLINE imVec #-}
    imVec = js_imVecD
    {-# INLINE taker #-}
    taker = js_takerD
    {-# INLINE takei #-}
    takei = js_takeiD
    {-# INLINE takej #-}
    takej = js_takejD
    {-# INLINE takek #-}
    takek = js_takekD
    {-# INLINE conjugate #-}
    conjugate = js_conjugateD
    {-# INLINE rotScale #-}
    rotScale = js_rotScaleD
    {-# INLINE getRotScale #-}
    getRotScale = js_getRotScaleD
    {-# INLINE axisRotation #-}
    axisRotation = js_axisRotationD
    {-# INLINE qArg #-}
    qArg = js_qArgD


{-# INLINE js_setQF #-}
foreign import javascript unsafe "[$1,$2,$3,$4]"
    js_setQF :: Float -> Float -> Float -> Float -> QFloat
{-# INLINE js_setQD #-}
foreign import javascript unsafe "[$1,$2,$3,$4]"
    js_setQD :: Double -> Double -> Double -> Double -> QDouble

{-# INLINE js_fromVecNumF #-}
foreign import javascript unsafe "[$1[0],$1[1],$1[2],$2]"
    js_fromVecNumF :: Vector3 Float -> Float -> QFloat
{-# INLINE js_fromVecNumD #-}
foreign import javascript unsafe "[$1[0],$1[1],$1[2],$2]"
    js_fromVecNumD :: Vector3 Double -> Double -> QDouble

{-# INLINE js_fromVec4F #-}
foreign import javascript unsafe "$1.slice()"
    js_fromVec4F :: Vector4 Float -> QFloat
{-# INLINE js_fromVec4D #-}
foreign import javascript unsafe "$1.slice()"
    js_fromVec4D :: Vector4 Double -> QDouble

{-# INLINE js_toVec4F #-}
foreign import javascript unsafe "$1.slice()"
    js_toVec4F :: QFloat -> Vector4 Float
{-# INLINE js_toVec4D #-}
foreign import javascript unsafe "$1.slice()"
    js_toVec4D :: QDouble -> Vector4 Double

{-# INLINE js_unpackQF #-}
foreign import javascript unsafe "$r1 = $1[0];$r2 = $1[1];$r3 = $1[2];$r4 = $1[3];"
    js_unpackQF :: QFloat -> (Float,Float,Float,Float)
{-# INLINE js_unpackQD #-}
foreign import javascript unsafe "$r1 = $1[0];$r2 = $1[1];$r3 = $1[2];$r4 = $1[3];"
    js_unpackQD :: QDouble -> (Double,Double,Double,Double)


{-# INLINE js_squareF #-}
foreign import javascript unsafe "$1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]"
    js_squareF :: QFloat -> Float
{-# INLINE js_squareD #-}
foreign import javascript unsafe "$1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]"
    js_squareD :: QDouble -> Double

{-# INLINE js_imF #-}
foreign import javascript unsafe "[$1[0],$1[1],$1[2],0]"
    js_imF :: QFloat -> QFloat
{-# INLINE js_imD #-}
foreign import javascript unsafe "[$1[0],$1[1],$1[2],0]"
    js_imD :: QDouble -> QDouble

{-# INLINE js_imVecF #-}
foreign import javascript unsafe "[$1[0],$1[1],$1[2]]"
    js_imVecF :: QFloat -> Vector3 Float
{-# INLINE js_imVecD #-}
foreign import javascript unsafe "[$1[0],$1[1],$1[2]]"
    js_imVecD :: QDouble -> Vector3 Double

{-# INLINE js_reF #-}
foreign import javascript unsafe "[0,0,0,$1[3]]"
    js_reF :: QFloat -> QFloat
{-# INLINE js_reD #-}
foreign import javascript unsafe "[0,0,0,$1[3]]"
    js_reD :: QDouble -> QDouble

{-# INLINE js_takerF #-}
foreign import javascript unsafe "$1[3]"
    js_takerF :: QFloat -> Float
{-# INLINE js_takerD #-}
foreign import javascript unsafe "$1[3]"
    js_takerD :: QDouble -> Double

{-# INLINE js_takeiF #-}
foreign import javascript unsafe "$1[0]"
    js_takeiF :: QFloat -> Float
{-# INLINE js_takeiD #-}
foreign import javascript unsafe "$1[0]"
    js_takeiD :: QDouble -> Double

{-# INLINE js_takejF #-}
foreign import javascript unsafe "$1[1]"
    js_takejF :: QFloat -> Float
{-# INLINE js_takejD #-}
foreign import javascript unsafe "$1[1]"
    js_takejD :: QDouble -> Double

{-# INLINE js_takekF #-}
foreign import javascript unsafe "$1[2]"
    js_takekF :: QFloat -> Float
{-# INLINE js_takekD #-}
foreign import javascript unsafe "$1[2]"
    js_takekD :: QDouble -> Double

{-# INLINE js_conjugateF #-}
foreign import javascript unsafe "[-$1[0],-$1[1],-$1[2],$1[3]]"
    js_conjugateF :: QFloat -> QFloat
{-# INLINE js_conjugateD #-}
foreign import javascript unsafe "[-$1[0],-$1[1],-$1[2],$1[3]]"
    js_conjugateD :: QDouble -> QDouble


{-# INLINE js_rotScaleF #-}
foreign import javascript unsafe "rotScale($1,$2)"
    js_rotScaleF :: QFloat -> Vector3 Float -> Vector3 Float
{-# INLINE js_rotScaleD #-}
foreign import javascript unsafe "rotScale($1,$2)"
    js_rotScaleD :: QDouble -> Vector3 Double -> Vector3 Double

{-# INLINE js_getRotScaleF #-}
foreign import javascript unsafe "getRotScale($1,$2)"
    js_getRotScaleF :: Vector3 Float -> Vector3 Float -> QFloat
{-# INLINE js_getRotScaleD #-}
foreign import javascript unsafe "getRotScale($1,$2)"
    js_getRotScaleD :: Vector3 Double -> Vector3 Double -> QDouble

{-# INLINE js_axisRotationF #-}
foreign import javascript unsafe "axisRotation($1,$2)"
    js_axisRotationF :: Vector3 Float -> Float -> QFloat
{-# INLINE js_axisRotationD #-}
foreign import javascript unsafe "axisRotation($1,$2)"
    js_axisRotationD :: Vector3 Double -> Double -> QDouble


{-# INLINE js_qArgF #-}
foreign import javascript unsafe "qArg($1)"
    js_qArgF :: QFloat -> Float

{-# INLINE js_qArgD #-}
foreign import javascript unsafe "qArg($1)"
    js_qArgD :: QDouble -> Double

--------------------------------------------------------------------------
-- Num
--------------------------------------------------------------------------

instance Num QFloat where
    {-# INLINE (+) #-}
    (+) = js_plusF
    {-# INLINE (-) #-}
    (-) = js_minusF
    {-# INLINE (*) #-}
    (*) = js_timesF
    {-# INLINE abs #-}
    abs = js_absF
    {-# INLINE signum #-}
    signum = js_signumF
    {-# INLINE negate #-}
    negate = js_negateF
    {-# INLINE fromInteger #-}
    fromInteger = js_fromFloat . fromInteger

instance Num QDouble where
    {-# INLINE (+) #-}
    (+) = js_plusD
    {-# INLINE (-) #-}
    (-) = js_minusD
    {-# INLINE (*) #-}
    (*) = js_timesD
    {-# INLINE abs #-}
    abs = js_absD
    {-# INLINE signum #-}
    signum = js_signumD
    {-# INLINE negate #-}
    negate = js_negateD
    {-# INLINE fromInteger #-}
    fromInteger = js_fromDouble . fromInteger


{-# INLINE js_plusF #-}
foreign import javascript unsafe "$1.map(function (e, i) {return e + $2[i];})"
    js_plusF :: QFloat -> QFloat -> QFloat
{-# INLINE js_plusD #-}
foreign import javascript unsafe "$1.map(function (e, i) {return e + $2[i];})"
    js_plusD :: QDouble -> QDouble -> QDouble


{-# INLINE js_minusF #-}
foreign import javascript unsafe "$1.map(function (e, i) {return e - $2[i];})"
    js_minusF :: QFloat -> QFloat -> QFloat
{-# INLINE js_minusD #-}
foreign import javascript unsafe "$1.map(function (e, i) {return e - $2[i];})"
    js_minusD :: QDouble -> QDouble -> QDouble

{-# INLINE js_timesF #-}
foreign import javascript unsafe "[ $1[3]*$2[0] + $1[0]*$2[3] + $1[1]*$2[2] - $1[2]*$2[1]\
                                 \, $1[3]*$2[1] - $1[0]*$2[2] + $1[1]*$2[3] + $1[2]*$2[0]\
                                 \, $1[3]*$2[2] + $1[0]*$2[1] - $1[1]*$2[0] + $1[2]*$2[3]\
                                 \, $1[3]*$2[3] - $1[0]*$2[0] - $1[1]*$2[1] - $1[2]*$2[2] ]"
    js_timesF :: QFloat -> QFloat -> QFloat
{-# INLINE js_timesD #-}
foreign import javascript unsafe "[ $1[3]*$2[0] + $1[0]*$2[3] + $1[1]*$2[2] - $1[2]*$2[1]\
                                 \, $1[3]*$2[1] - $1[0]*$2[2] + $1[1]*$2[3] + $1[2]*$2[0]\
                                 \, $1[3]*$2[2] + $1[0]*$2[1] - $1[1]*$2[0] + $1[2]*$2[3]\
                                 \, $1[3]*$2[3] - $1[0]*$2[0] - $1[1]*$2[1] - $1[2]*$2[2] ]"
    js_timesD :: QDouble -> QDouble -> QDouble

{-# INLINE js_absF #-}
foreign import javascript unsafe "[0,0,0,Math.hypot($1[0],$1[1],$1[2],$1[3])]"
    js_absF :: QFloat -> QFloat
{-# INLINE js_absD #-}
foreign import javascript unsafe "[0,0,0,Math.hypot($1[0],$1[1],$1[2],$1[3])]"
    js_absD :: QDouble -> QDouble

{-# INLINE js_signumF #-}
foreign import javascript unsafe "var l = 1 / Math.hypot($1[0],$1[1],$1[2],$1[3]); $r = $1.map(function (e) {return e * l;})"
    js_signumF :: QFloat -> QFloat
{-# INLINE js_signumD #-}
foreign import javascript unsafe "var l = 1 / Math.hypot($1[0],$1[1],$1[2],$1[3]); $r = $1.map(function (e) {return e * l;})"
    js_signumD :: QDouble -> QDouble

{-# INLINE js_negateF #-}
foreign import javascript unsafe "$1.map(function (e) {return -e;})"
    js_negateF :: QFloat -> QFloat
{-# INLINE js_negateD #-}
foreign import javascript unsafe "$1.map(function (e) {return -e;})"
    js_negateD :: QDouble -> QDouble

{-# INLINE js_fromFloat #-}
foreign import javascript unsafe "[0,0,0,$1]"
    js_fromFloat :: Float -> QFloat
{-# INLINE js_fromDouble #-}
foreign import javascript unsafe "[0,0,0,$1]"
    js_fromDouble :: Double -> QDouble

--------------------------------------------------------------------------
-- Fractional
--------------------------------------------------------------------------

instance Fractional QFloat where
    {-# INLINE recip #-}
    recip = js_recipF
    {-# INLINE (/) #-}
    (/) p = js_timesF p . js_recipF
    {-# INLINE fromRational #-}
    fromRational = js_fromFloat . fromRational

instance Fractional QDouble where
    {-# INLINE recip #-}
    recip = js_recipD
    {-# INLINE (/) #-}
    (/) p = js_timesD p . js_recipD
    {-# INLINE fromRational #-}
    fromRational = js_fromDouble . fromRational

{-# INLINE js_recipF #-}
foreign import javascript unsafe "var c = -1 / ($1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]); $r = [$1[0]*c,$1[1]*c,$1[2]*c,-$1[3]*c];"
    js_recipF :: QFloat -> QFloat
{-# INLINE js_recipD #-}
foreign import javascript unsafe "var c = -1 / ($1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]); $r = [$1[0]*c,$1[1]*c,$1[2]*c,-$1[3]*c];"
    js_recipD :: QDouble -> QDouble


--------------------------------------------------------------------------
-- Floating
--------------------------------------------------------------------------

instance  Floating QFloat where
    {-# INLINE pi #-}
    pi = js_piF
    {-# INLINE exp #-}
    exp = js_expF
    {-# INLINE log #-}
    log = js_logF
    {-# INLINE sqrt #-}
    sqrt = js_sqrtF
    {-# INLINE sin #-}
    sin = js_sinF
    {-# INLINE cos #-}
    cos = js_cosF
    {-# INLINE tan #-}
    tan = js_tanF
    {-# INLINE sinh #-}
    sinh = js_sinhF
    {-# INLINE cosh #-}
    cosh = js_coshF
    {-# INLINE tanh #-}
    tanh =  js_tanhF
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

{-# INLINE js_piF #-}
foreign import javascript unsafe "[0,0,0,Math.PI]"
    js_piF :: QFloat
{-# INLINE js_expF #-}
foreign import javascript unsafe "var mv = Math.hypot($1[0],$1[1],$1[2]), et = Math.exp($1[3]);\n\
                                 \ if(mv === 0) {$r = [0,0,0,et];}\
                                 \ else { var l = et * Math.sin(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l,et*Math.cos(mv)]; }"
    js_expF :: QFloat -> QFloat
{-# INLINE js_logF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {if($1[3] >= 0){$r = [0,0,0,Math.log($1[3])];}else{$r = [Math.PI,0,0,Math.log(-$1[3])];}}\
                                 \ else { var mq = Math.sqrt($1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]);\n\
                                 \ mv = Math.sqrt(mv);\n\
                                 \ var l = Math.atan2( mv, $1[3] ) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l,Math.log(mq)]; }"
    js_logF :: QFloat -> QFloat
{-# INLINE js_sqrtF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {if($1[3] >= 0){$r = [0,0,0,Math.sqrt($1[3])];}else{$r = [Math.sqrt( - $1[3]),0,0,0];}}\
                                 \ else { var l = Math.sqrt(mv + $1[3]*$1[3]);\n\
                                 \ var l2 = Math.sqrt(l);\n\
                                 \ var tq = $1[3] / (l * 2);\n\
                                 \ var sina = Math.sqrt(0.5 - tq) * l2 / Math.sqrt(mv);\n\
                                 \ $r = [$1[0]*sina,$1[1]*sina,$1[2]*sina,Math.sqrt(0.5 + tq) * l2]; }"
    js_sqrtF :: QFloat -> QFloat
{-# INLINE js_sinF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.sin($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = Math.cos($1[3]) * Math.sinh(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sin($1[3])*Math.cosh(mv)]; }"
    js_sinF :: QFloat -> QFloat
{-# INLINE js_cosF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.cos($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = - Math.sin($1[3]) * Math.sinh(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.cos($1[3])*Math.cosh(mv)]; }"
    js_cosF :: QFloat -> QFloat
{-# INLINE js_tanF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.tan($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var chv = Math.cosh(mv), shv = Math.sinh(mv), ct = Math.cos($1[3]), st = Math.sin($1[3]);\n\
                                 \ var cq = 1 / Math.sqrt(ct*ct*chv*chv + st*st*shv*shv);\n\
                                 \ var l = chv*shv / cq;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sin(2*$1[3])* cq * 0.5]; }"
    js_tanF :: QFloat -> QFloat

{-# INLINE js_sinhF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.sinh($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = Math.cosh($1[3]) * Math.sin(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sinh($1[3])*Math.cos(mv)]; }"
    js_sinhF :: QFloat -> QFloat
{-# INLINE js_coshF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.cosh($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = - Math.sinh($1[3]) * Math.sin(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.cosh($1[3])*Math.cos(mv)]; }"
    js_coshF :: QFloat -> QFloat
{-# INLINE js_tanhF #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.tanh($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var chv = Math.cos(mv), shv = Math.sin(mv), ct = Math.cosh($1[3]), st = Math.sinh($1[3]);\n\
                                 \ var cq = 1 / Math.sqrt(ct*ct*chv*chv + st*st*shv*shv);\n\
                                 \ var l = chv*shv/cq;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sinh(2*$1[3]) * cq * 0.5]; }"
    js_tanhF :: QFloat -> QFloat


instance  Floating QDouble where
    {-# INLINE pi #-}
    pi = js_piD
    {-# INLINE exp #-}
    exp = js_expD
    {-# INLINE log #-}
    log = js_logD
    {-# INLINE sqrt #-}
    sqrt = js_sqrtD
    {-# INLINE sin #-}
    sin = js_sinD
    {-# INLINE cos #-}
    cos = js_cosD
    {-# INLINE tan #-}
    tan = js_tanD
    {-# INLINE sinh #-}
    sinh = js_sinhD
    {-# INLINE cosh #-}
    cosh = js_coshD
    {-# INLINE tanh #-}
    tanh =  js_tanhD
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

{-# INLINE js_piD #-}
foreign import javascript unsafe "[0,0,0,Math.PI]"
    js_piD :: QDouble
{-# INLINE js_expD #-}
foreign import javascript unsafe "var mv = Math.hypot($1[0],$1[1],$1[2]), et = Math.exp($1[3]);\n\
                                 \ if(mv === 0) {$r = [0,0,0,et];}\
                                 \ else { var l = et * Math.sin(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l,et*Math.cos(mv)]; }"
    js_expD :: QDouble -> QDouble
{-# INLINE js_logD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {if($1[3] >= 0){$r = [0,0,0,Math.log($1[3])];}else{$r = [Math.PI,0,0,Math.log(-$1[3])];}}\
                                 \ else { var mq = Math.sqrt($1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2] + $1[3]*$1[3]);\n\
                                 \ mv = Math.sqrt(mv);\n\
                                 \ var l = Math.atan2( mv, $1[3] ) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l,Math.log(mq)]; }"
    js_logD :: QDouble -> QDouble
{-# INLINE js_sqrtD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {if($1[3] >= 0){$r = [0,0,0,Math.sqrt($1[3])];}else{$r = [Math.sqrt( - $1[3]),0,0,0];}}\
                                 \ else { var l = Math.sqrt(mv + $1[3]*$1[3]);\n\
                                 \ var l2 = Math.sqrt(l);\n\
                                 \ var tq = $1[3] / (l * 2);\n\
                                 \ var sina = Math.sqrt(0.5 - tq) * l2 / Math.sqrt(mv);\n\
                                 \ $r = [$1[0]*sina,$1[1]*sina,$1[2]*sina,Math.sqrt(0.5 + tq) * l2]; }"
    js_sqrtD :: QDouble -> QDouble
{-# INLINE js_sinD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.sin($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = Math.cos($1[3]) * Math.sinh(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sin($1[3])*Math.cosh(mv)]; }"
    js_sinD :: QDouble -> QDouble
{-# INLINE js_cosD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.cos($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = - Math.sin($1[3]) * Math.sinh(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.cos($1[3])*Math.cosh(mv)]; }"
    js_cosD :: QDouble -> QDouble
{-# INLINE js_tanD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.tan($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var chv = Math.cosh(mv), shv = Math.sinh(mv), ct = Math.cos($1[3]), st = Math.sin($1[3]);\n\
                                 \ var cq = 1 / Math.sqrt(ct*ct*chv*chv + st*st*shv*shv);\n\
                                 \ var l = chv*shv / cq;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sin(2*$1[3])* cq * 0.5]; }"
    js_tanD :: QDouble -> QDouble

{-# INLINE js_sinhD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.sinh($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = Math.cosh($1[3]) * Math.sin(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sinh($1[3])*Math.cos(mv)]; }"
    js_sinhD :: QDouble -> QDouble
{-# INLINE js_coshD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.cosh($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var l = - Math.sinh($1[3]) * Math.sin(mv) / mv;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.cosh($1[3])*Math.cos(mv)]; }"
    js_coshD :: QDouble -> QDouble
{-# INLINE js_tanhD #-}
foreign import javascript unsafe "var mv = $1[0]*$1[0] + $1[1]*$1[1] + $1[2]*$1[2];\n\
                                 \ if(mv === 0) {$r = [0,0,0,Math.tanh($1[3])];}\
                                 \ else { mv = Math.sqrt(mv);\n\
                                 \ var chv = Math.cos(mv), shv = Math.sin(mv), ct = Math.cosh($1[3]), st = Math.sinh($1[3]);\n\
                                 \ var cq = 1 / Math.sqrt(ct*ct*chv*chv + st*st*shv*shv);\n\
                                 \ var l = chv*shv/cq;\n\
                                 \ $r = [$1[0]*l,$1[1]*l,$1[2]*l, Math.sinh(2*$1[3]) * cq * 0.5]; }"
    js_tanhD :: QDouble -> QDouble


--------------------------------------------------------------------------
-- Eq
--------------------------------------------------------------------------

instance Eq QFloat where
    {-# INLINE (==) #-}
    (==) = js_eqF
    {-# INLINE (/=) #-}
    (/=) = js_neqF

instance Eq QDouble where
    {-# INLINE (==) #-}
    (==) = js_eqD
    {-# INLINE (/=) #-}
    (/=) = js_neqD



{-# INLINE js_eqF #-}
foreign import javascript unsafe "$1[0] === $2[0] && $1[1] === $2[1] && $1[2] === $2[2] && $1[3] === $2[3]"
    js_eqF :: QFloat -> QFloat -> Bool
{-# INLINE js_eqD #-}
foreign import javascript unsafe "$1[0] === $2[0] && $1[1] === $2[1] && $1[2] === $2[2] && $1[3] === $2[3]"
    js_eqD :: QDouble -> QDouble -> Bool
{-# INLINE js_neqF #-}
foreign import javascript unsafe "$1[0] !== $2[0] || $1[1] !== $2[1] || $1[2] !== $2[2] || $1[3] !== $2[3]"
    js_neqF :: QFloat -> QFloat -> Bool
{-# INLINE js_neqD #-}
foreign import javascript unsafe "$1[0] !== $2[0] || $1[1] !== $2[1] || $1[2] !== $2[2] || $1[3] !== $2[3]"
    js_neqD :: QDouble -> QDouble -> Bool



--------------------------------------------------------------------------
-- Ord
--------------------------------------------------------------------------

instance Ord QFloat where
    {-# INLINE (>) #-}
    (>) = js_gtF
    {-# INLINE (<) #-}
    (<) = js_ltF
    {-# INLINE (>=) #-}
    (>=) = js_geqF
    {-# INLINE (<=) #-}
    (<=) = js_leqF
    {-# INLINE max #-}
    max = js_maxF
    {-# INLINE min #-}
    min = js_minF
    {-# INLINE compare #-}
    compare a b = case js_compareF a b of
        1  -> GT
        -1 -> LT
        _  -> EQ

instance Ord QDouble where
    {-# INLINE (>) #-}
    (>) = js_gtD
    {-# INLINE (<) #-}
    (<) = js_ltD
    {-# INLINE (>=) #-}
    (>=) = js_geqD
    {-# INLINE (<=) #-}
    (<=) = js_leqD
    {-# INLINE max #-}
    max = js_maxD
    {-# INLINE min #-}
    min = js_minD
    {-# INLINE compare #-}
    compare a b = case js_compareD a b of
        1  -> GT
        -1 -> LT
        _  -> EQ

{-# INLINE js_ltF #-}
foreign import javascript unsafe "$1[0] < $2[0] && $1[1] < $2[1] && $1[2] < $2[2] && $1[3] < $2[3]"
    js_ltF :: QFloat -> QFloat -> Bool
{-# INLINE js_gtF #-}
foreign import javascript unsafe "$1[0] > $2[0] && $1[1] > $2[1] && $1[2] > $2[2] && $1[3] > $2[3]"
    js_gtF :: QFloat -> QFloat -> Bool
{-# INLINE js_leqF #-}
foreign import javascript unsafe "$1[0] <= $2[0] && $1[1] <= $2[1] && $1[2] <= $2[2] && $1[3] <= $2[3]"
    js_leqF :: QFloat -> QFloat -> Bool
{-# INLINE js_geqF #-}
foreign import javascript unsafe "$1[0] >= $2[0] && $1[1] >= $2[1] && $1[2] >= $2[2] && $1[3] >= $2[3]"
    js_geqF :: QFloat -> QFloat -> Bool
{-# INLINE js_maxF #-}
foreign import javascript unsafe "[Math.max($1[0],$2[0]), Math.max($1[1],$2[1]), Math.max($1[2],$2[2]), Math.max($1[3],$2[3])]"
    js_maxF :: QFloat -> QFloat -> QFloat
{-# INLINE js_minF #-}
foreign import javascript unsafe "[Math.min($1[0],$2[0]), Math.min($1[1],$2[1]), Math.min($1[2],$2[2]), Math.min($1[3],$2[3])]"
    js_minF :: QFloat -> QFloat -> QFloat
{-# INLINE js_compareF #-}
foreign import javascript unsafe "$1[3] > $2[3] ? 1 : ( $1[3] < $2[3] ? -1 : 0)"
    js_compareF :: QFloat -> QFloat -> Int

{-# INLINE js_ltD #-}
foreign import javascript unsafe "$1[0] < $2[0] && $1[1] < $2[1] && $1[2] < $2[2] && $1[3] < $2[3]"
    js_ltD :: QDouble -> QDouble -> Bool
{-# INLINE js_gtD #-}
foreign import javascript unsafe "$1[0] > $2[0] && $1[1] > $2[1] && $1[2] > $2[2] && $1[3] > $2[3]"
    js_gtD :: QDouble -> QDouble -> Bool
{-# INLINE js_leqD #-}
foreign import javascript unsafe "$1[0] <= $2[0] && $1[1] <= $2[1] && $1[2] <= $2[2] && $1[3] <= $2[3]"
    js_leqD :: QDouble -> QDouble -> Bool
{-# INLINE js_geqD #-}
foreign import javascript unsafe "$1[0] >= $2[0] && $1[1] >= $2[1] && $1[2] >= $2[2] && $1[3] >= $2[3]"
    js_geqD :: QDouble -> QDouble -> Bool
{-# INLINE js_maxD #-}
foreign import javascript unsafe "[Math.max($1[0],$2[0]), Math.max($1[1],$2[1]), Math.max($1[2],$2[2]), Math.max($1[3],$2[3])]"
    js_maxD :: QDouble -> QDouble -> QDouble
{-# INLINE js_minD #-}
foreign import javascript unsafe "[Math.min($1[0],$2[0]), Math.min($1[1],$2[1]), Math.min($1[2],$2[2]), Math.min($1[3],$2[3])]"
    js_minD :: QDouble -> QDouble -> QDouble
{-# INLINE js_compareD #-}
foreign import javascript unsafe "$1[3] > $2[3] ? 1 : ( $1[3] < $2[3] ? -1 : 0)"
    js_compareD :: QDouble -> QDouble -> Int

--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QFloat where
    show = unpack' . js_showF

instance Show QDouble where
    show = unpack' . js_showD

{-# INLINE js_showF #-}
foreign import javascript unsafe "$1[3].toPrecision(5)\
                                 \ + ($1[0] >= 0 ? ' + ' :  ' - ') + Math.abs($1[0]).toPrecision(5) + 'i'\
                                 \ + ($1[1] >= 0 ? ' + ' :  ' - ') + Math.abs($1[1]).toPrecision(5) + 'j'\
                                 \ + ($1[2] >= 0 ? ' + ' :  ' - ') + Math.abs($1[2]).toPrecision(5) + 'k'"
    js_showF :: QFloat -> JSString
{-# INLINE js_showD #-}
foreign import javascript unsafe "$1[3].toPrecision(8)\
                                 \ + ($1[0] >= 0 ? ' + ' :  ' - ') + Math.abs($1[0]).toPrecision(8) + 'i'\
                                 \ + ($1[1] >= 0 ? ' + ' :  ' - ') + Math.abs($1[1]).toPrecision(8) + 'j'\
                                 \ + ($1[2] >= 0 ? ' + ' :  ' - ') + Math.abs($1[2]).toPrecision(8) + 'k'"
    js_showD:: QDouble -> JSString


--------------------------------------------------------------------------
-- Real
--------------------------------------------------------------------------


instance Real QFloat where
    {-# INLINE toRational #-}
    toRational = toRational . taker

instance Real QDouble where
    {-# INLINE toRational #-}
    toRational = toRational . taker

{-# RULES
"realToFrac/FloatQFloat"    realToFrac = js_fromFloat    :: Float -> QFloat
"realToFrac/DoubleQDouble"  realToFrac = js_fromDouble   :: Double -> QDouble
"realToFrac/QFloatQFloat"   realToFrac = id              :: QFloat -> QFloat
"realToFrac/QDoubleQDouble" realToFrac = id              :: QDouble -> QDouble
"realToFrac/QDoubleQFloat"  realToFrac = coerce          :: QDouble -> QFloat
"realToFrac/QFloatQDouble"  realToFrac = coerce          :: QFloat -> QDouble
"realToFrac/NumQFloat"      realToFrac = js_fromFloat  . realToFrac
"realToFrac/NumQDouble"     realToFrac = js_fromDouble . realToFrac
"realToFrac/QFloatNum"      realToFrac = realToFrac . js_takerF
"realToFrac/QDoubleNum"     realToFrac = realToFrac . js_takerD
    #-}


--------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------

instance Enum QFloat where
    {-# INLINE succ #-}
    succ = js_plusF 1
    {-# INLINE pred #-}
    pred = flip js_minusF 1
    {-# INLINE toEnum #-}
    toEnum = js_fromFloat . fromIntegral
    {-# INLINE fromEnum #-}
    fromEnum = fromEnum . taker
    {-# INLINE enumFrom #-}
    enumFrom = iterate (+1)
    {-# INLINE enumFromThen #-}
    enumFromThen x y = x : iterate (+d) y
        where d = y-x
    {-# INLINE enumFromTo #-}
    enumFromTo x y = if y >= x then iterateWhile (<= y) succ x else iterateWhile (>= y) pred x
        where iterateWhile c f t = if c t then t : iterateWhile c f (f t) else []
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo x y z | z >= x && z >= y && not (y <= x) = iterateWhile (<= z) x
                         | z <= x && z <= y && not (y >= x) = iterateWhile (>= z) x
                         | otherwise                        = []
        where iterateWhile c t = if c t then t : iterateWhile c (t+d) else []
              d = y-x

instance Enum QDouble where
    {-# INLINE succ #-}
    succ = js_plusD 1
    {-# INLINE pred #-}
    pred = flip js_minusD 1
    {-# INLINE toEnum #-}
    toEnum = js_fromDouble . fromIntegral
    {-# INLINE fromEnum #-}
    fromEnum = fromEnum . taker
    {-# INLINE enumFrom #-}
    enumFrom = iterate (+1)
    {-# INLINE enumFromThen #-}
    enumFromThen x y = x : iterate (+d) y
        where d = y-x
    {-# INLINE enumFromTo #-}
    enumFromTo x y = if y >= x then iterateWhile (<= y) succ x else iterateWhile (>= y) pred x
        where iterateWhile c f t = if c t then t : iterateWhile c f (f t) else []
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo x y z | z >= x && z >= y && not (y <= x) = iterateWhile (<= z) x
                         | z <= x && z <= y && not (y >= x) = iterateWhile (>= z) x
                         | otherwise                        = []
        where iterateWhile c t = if c t then t : iterateWhile c (t+d) else []
              d = y-x
