{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QDouble
    ( QDouble, Quater (..)
    ) where

import Data.Coerce (coerce)

import Numeric.Array
import Numeric.DataFrame.Type
--import Numeric.Vector
--import Numeric.Matrix

import Numeric.Quaternion.Class


type QDouble = Quater Double

notYet :: a
notYet = error "Sorry, this function is not implemented for current platform yet."

instance Quaternion Double where
    newtype Quater Double = QDouble (ArrayD '[4])
    {-# INLINE packQ #-}
    packQ = notYet
    {-# INLINE fromVecNum #-}
    fromVecNum = notYet
    {-# INLINE fromVec4 #-}
    fromVec4 = coerce
    {-# INLINE toVec4 #-}
    toVec4 = coerce
    {-# INLINE unpackQ #-}
    unpackQ = notYet
    {-# INLINE square #-}
    square = notYet
    {-# INLINE im #-}
    im = notYet
    {-# INLINE re #-}
    re = notYet
    {-# INLINE imVec #-}
    imVec = notYet
    {-# INLINE taker #-}
    taker = notYet
    {-# INLINE takei #-}
    takei = notYet
    {-# INLINE takej #-}
    takej = notYet
    {-# INLINE takek #-}
    takek = notYet
    {-# INLINE conjugate #-}
    conjugate = notYet
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

--------------------------------------------------------------------------
-- Num
--------------------------------------------------------------------------

instance Num QDouble where
    {-# INLINE (+) #-}
    (+) = notYet
    {-# INLINE (-) #-}
    (-) = notYet
    {-# INLINE (*) #-}
    (*) = notYet
    {-# INLINE abs #-}
    abs = notYet
    {-# INLINE signum #-}
    signum = notYet
    {-# INLINE negate #-}
    negate = notYet
    {-# INLINE fromInteger #-}
    fromInteger = notYet



--------------------------------------------------------------------------
-- Fractional
--------------------------------------------------------------------------

instance Fractional QDouble where
    {-# INLINE recip #-}
    recip = notYet
    {-# INLINE (/) #-}
    (/) = notYet
    {-# INLINE fromRational #-}
    fromRational = notYet

--------------------------------------------------------------------------
-- Floating
--------------------------------------------------------------------------

instance  Floating QDouble where
    {-# INLINE pi #-}
    pi = notYet
    {-# INLINE exp #-}
    exp = notYet
    {-# INLINE log #-}
    log = notYet
    {-# INLINE sqrt #-}
    sqrt = notYet
    {-# INLINE sin #-}
    sin = notYet
    {-# INLINE cos #-}
    cos = notYet
    {-# INLINE tan #-}
    tan = notYet
    {-# INLINE sinh #-}
    sinh = notYet
    {-# INLINE cosh #-}
    cosh = notYet
    {-# INLINE tanh #-}
    tanh =  notYet
    {-# INLINE asin #-}
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where i = signum . im $ q
    {-# INLINE acos #-}
    acos q = pi/2 - asin q
    {-# INLINE atan #-}
    atan = notYet
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
    (==) = notYet
    {-# INLINE (/=) #-}
    (/=) = notYet



--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QDouble where
    show = notYet
