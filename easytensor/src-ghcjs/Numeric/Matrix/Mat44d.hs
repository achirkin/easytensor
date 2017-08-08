{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Matrix.Mat44d () where


import Numeric.DataFrame.Type
import Numeric.Vector
import Numeric.Matrix.Class


instance HomTransform4 Double where
    translate4 = js_translate
    {-# INLINE translate4 #-}
    translate3 = js_translate
    {-# INLINE translate3 #-}
    rotateX = js_rotateX
    {-# INLINE rotateX #-}
    rotateY = js_rotateY
    {-# INLINE rotateY #-}
    rotateZ = js_rotateZ
    {-# INLINE rotateZ #-}
    rotate = js_rotate
    {-# INLINE rotate #-}
    rotateEuler = js_rotateEuler
    {-# INLINE rotateEuler #-}
    lookAt = js_lookAt
    {-# INLINE lookAt #-}
    perspective = js_perspective
    {-# INLINE perspective #-}
    orthogonal = js_orthogonal
    {-# INLINE orthogonal #-}
    toHomPoint = js_toHomPoint
    {-# INLINE toHomPoint #-}
    toHomVector = js_toHomVector
    {-# INLINE toHomVector #-}
    fromHom = js_fromHom
    {-# INLINE fromHom #-}



foreign import javascript unsafe "h$easytensor_m4translate($1)"
    js_translate :: Vector Double n -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4rotateX($1))"
    js_rotateX :: Double -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4rotateY($1))"
    js_rotateY :: Double -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4rotateZ($1))"
    js_rotateZ :: Double -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4rotate($1, $2))"
    js_rotate :: Vector Double 3 -> Double -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4rotateEuler($1, $2, $3))"
    js_rotateEuler :: Double -> Double -> Double -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4lookAt($1,$2,$3))"
    js_lookAt :: Vector Double 3 -> Vector Double 3 -> Vector Double 3 -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4perspective($1, $2, $3, $4))"
    js_perspective :: Double -> Double -> Double -> Double -> Matrix Double 4 4
foreign import javascript unsafe "new Float64Array(h$easytensor_m4orthogonal($1, $2, $3, $4))"
    js_orthogonal :: Double -> Double -> Double -> Double -> Matrix Double 4 4

foreign import javascript unsafe "$r = new $1.constructor(4); $r.set($1); $r[3] = 1;"
    js_toHomPoint :: Vector Double 3 -> Vector Double 4
foreign import javascript unsafe "$r = new $1.constructor(4); $r.set($1); $r[3] = 0;"
    js_toHomVector :: Vector Double 3 -> Vector Double 4
foreign import javascript unsafe "h$easytensor_m4fromHom($1)"
    js_fromHom :: Vector Double 4 -> Vector Double 3



