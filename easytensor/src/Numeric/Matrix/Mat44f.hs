{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Matrix.Mat44f () where


import Numeric.Matrix.Class

notYet :: a
notYet = error "Sorry, this function is not implemented for current platform yet."

instance HomTransform4 Float where
    translate4 = notYet
    {-# INLINE translate4 #-}
    translate3 = notYet
    {-# INLINE translate3 #-}
    rotateX = notYet
    {-# INLINE rotateX #-}
    rotateY = notYet
    {-# INLINE rotateY #-}
    rotateZ = notYet
    {-# INLINE rotateZ #-}
    rotate = notYet
    {-# INLINE rotate #-}
    rotateEuler = notYet
    {-# INLINE rotateEuler #-}
    lookAt = notYet
    {-# INLINE lookAt #-}
    perspective = notYet
    {-# INLINE perspective #-}
    orthogonal = notYet
    {-# INLINE orthogonal #-}
    toHomPoint = notYet
    {-# INLINE toHomPoint #-}
    toHomVector = notYet
    {-# INLINE toHomVector #-}
    fromHom = notYet
    {-# INLINE fromHom #-}

