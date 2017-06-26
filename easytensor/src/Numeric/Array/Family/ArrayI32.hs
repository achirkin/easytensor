{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family.ArrayI32
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayI32 () where

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), RuntimeRep (..), isTrue#)
import           GHC.Int                   (Int32 (..))

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse


#include "MachDeps.h"
#define ARR_TYPE                 ArrayI32
#define ARR_FROMSCALAR           FromScalarI32#
#define ARR_CONSTR               ArrayI32#
#define EL_TYPE_BOXED            Int32
#define EL_TYPE_PRIM             Int#
#define EL_RUNTIME_REP           'IntRep
#define EL_CONSTR                I32#
#define EL_SIZE                  SIZEOF_INT32#
#define EL_ALIGNMENT             ALIGNMENT_INT32#
#define EL_ZERO                  0#
#define EL_ONE                   1#
#define EL_MINUS_ONE             -1#
#define INDEX_ARRAY              indexInt32Array#
#define WRITE_ARRAY              writeInt32Array#
#define OP_EQ                    (==#)
#define OP_NE                    (/=#)
#define OP_GT                    (>#)
#define OP_GE                    (>=#)
#define OP_LT                    (<#)
#define OP_LE                    (<=#)
#define OP_PLUS                  (+#)
#define OP_MINUS                 (-#)
#define OP_TIMES                 (*#)
#define OP_NEGATE                negateInt#
#include "Array.h"


instance Num (ArrayI32 ds) where
  (+) = zipV (+#)
  {-# INLINE (+) #-}
  (-) = zipV (-#)
  {-# INLINE (-) #-}
  (*) = zipV (*#)
  {-# INLINE (*) #-}
  negate = mapV negateInt#
  {-# INLINE negate #-}
  abs = mapV (\x -> if isTrue# (x >=# 0#)
                    then x
                    else negateInt# x
                )
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (x ># 0#)
                       then 1#
                       else if isTrue# (x <# 0#)
                            then -1#
                            else 0#
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}

instance Bounded (ArrayI32 ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
