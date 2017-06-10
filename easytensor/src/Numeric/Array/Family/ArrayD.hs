{-# LANGUAGE CPP                   #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family.ArrayD
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayD () where


import           GHC.Base             (runRW#)
import           GHC.Prim
import           GHC.TypeLits
import           GHC.Types
import           Data.Proxy

import           Numeric.Array.Family
import           Numeric.Array.ElementWise
import           Numeric.Commons
import           Numeric.Dimensions


#include "MachDeps.h"
#define ARR_TYPE                 ArrayD
#define ARR_FROMSCALAR           FromScalarD#
#define ARR_CONSTR               ArrayD#
#define EL_TYPE_BOXED            Double
#define EL_TYPE_PRIM             Double#
#define EL_RUNTIME_REP           'DoubleRep
#define EL_CONSTR                D#
#define EL_SIZE                  SIZEOF_HSDOUBLE#
#define EL_ALIGNMENT             ALIGNMENT_HSDOUBLE#
#define EL_ZERO                  0.0##
#define EL_ONE                   1.0##
#define EL_MINUS_ONE             -1.0##
#define INDEX_ARRAY              indexDoubleArray#
#define WRITE_ARRAY              writeDoubleArray#
#define OP_EQ                    (==##)
#define OP_NE                    (/=##)
#define OP_GT                    (>##)
#define OP_GE                    (>=##)
#define OP_LT                    (<##)
#define OP_LE                    (<=##)
#define OP_PLUS                  (+##)
#define OP_MINUS                 (-##)
#define OP_TIMES                 (*##)
#define OP_NEGATE                negateDouble#
#include "Array.h"


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
