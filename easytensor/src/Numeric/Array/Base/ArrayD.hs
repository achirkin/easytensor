{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.NDArray.Base.ArrayD
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Base.ArrayD () where

import           GHC.Prim
import           GHC.Types

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Array.Base.ArrayTH


-- * Utility functions

$(broadcastArrayDec arrayDDef)

$(mapVDec arrayDDef)

$(zipVDec arrayDDef)

$(accumV2Dec arrayDDef)

-- * Instances

$(instanceElementWiseDec arrayDDef)

$(instanceShowDec arrayDDef)

$(instanceEqDec arrayDDef)

$(instanceOrdDec arrayDDef)

$(instanceNumDec arrayDDef)

type instance ElemRep (ArrayD ds) = 'DoubleRep
$(instancePrimBytesDec arrayDDef)




instance Fractional (ArrayD ds) where
  (/) = zipV (/##)
  {-# INLINE (/) #-}
  recip = mapV (1.0## /##)
  {-# INLINE recip #-}
  fromRational = broadcastArray . fromRational
  {-# INLINE fromRational #-}


instance Floating (ArrayD ds) where
  pi = broadcastArray pi
  {-# INLINE pi #-}
  exp = mapV expDouble#
  {-# INLINE exp #-}
  log = mapV logDouble#
  {-# INLINE log #-}
  sqrt = mapV sqrtDouble#
  {-# INLINE sqrt #-}
  sin = mapV sinDouble#
  {-# INLINE sin #-}
  cos = mapV cosDouble#
  {-# INLINE cos #-}
  tan = mapV tanDouble#
  {-# INLINE tan #-}
  asin = mapV asinDouble#
  {-# INLINE asin #-}
  acos = mapV acosDouble#
  {-# INLINE acos #-}
  atan = mapV atanDouble#
  {-# INLINE atan #-}
  sinh = mapV sinDouble#
  {-# INLINE sinh #-}
  cosh = mapV coshDouble#
  {-# INLINE cosh #-}
  tanh = mapV tanhDouble#
  {-# INLINE tanh #-}
  (**) = zipV (**##)
  {-# INLINE (**) #-}

  logBase = zipV (\x y -> logDouble# y /## logDouble# x)
  {-# INLINE logBase #-}
  asinh = mapV (\x -> logDouble# (x +##
                                sqrtDouble# (1.0## +## x *## x)))
  {-# INLINE asinh #-}
  acosh = mapV (\x ->  case x +## 1.0## of
                 y -> logDouble# ( x +## y *##
                           sqrtDouble# ((x -## 1.0##) /## y)
                        )
               )
  {-# INLINE acosh #-}
  atanh = mapV (\x -> 0.5## *##
                logDouble# ((1.0## +## x) /## (1.0## -## x)))
  {-# INLINE atanh #-}
