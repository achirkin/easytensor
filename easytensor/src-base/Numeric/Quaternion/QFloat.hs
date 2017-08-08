-- Note,
--
-- The whole module is made by copying Numeric.Quaternion.QFloat and replacing:
--  Float -> Float
--  ArrayD -> ArrayF
--
-- If we are doing any refactoring of one of these modules, just do the same operation.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.QFloat
    ( QFloat, Quater (..)
    ) where

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

instance Num QFloat where
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

instance Fractional QFloat where
    {-# INLINE recip #-}
    recip = notYet
    {-# INLINE (/) #-}
    (/) = notYet
    {-# INLINE fromRational #-}
    fromRational = notYet

--------------------------------------------------------------------------
-- Floating
--------------------------------------------------------------------------

instance  Floating QFloat where
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

instance Eq QFloat where
    {-# INLINE (==) #-}
    (==) = notYet
    {-# INLINE (/=) #-}
    (/=) = notYet



--------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------

instance Show QFloat where
    show = notYet
