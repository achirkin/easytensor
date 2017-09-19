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
{-# LANGUAGE BangPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Family.ArrayI64
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayI64 () where

#include "MachDeps.h"
import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), RuntimeRep (..), isTrue#)
import           GHC.Int                   (Int64 (..))
#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse



#if SIZEOF_HSWORD < 8
#define ARR_TYPE                 ArrayI64
#define ARR_FROMSCALAR           FromScalarI64#
#define ARR_CONSTR               ArrayI64#
#define EL_TYPE_BOXED            Int64
#define EL_TYPE_PRIM             Int64#
#define EL_RUNTIME_REP           'Int64Rep
#define EL_CONSTR                I64#
#define EL_SIZE                  SIZEOF_INT64#
#define EL_ALIGNMENT             ALIGNMENT_INT64#
#define EL_ZERO                  (intToInt64# 0#)
#define EL_ONE                   (intToInt64# 1#)
#define EL_MINUS_ONE             (intToInt64# (-1#))
#define INDEX_ARRAY              indexInt64Array#
#define WRITE_ARRAY              writeInt64Array#
#define OP_EQ                    (eqInt64#)
#define OP_NE                    (neInt64#)
#define OP_GT                    (gtInt64#)
#define OP_GE                    (geInt64#)
#define OP_LT                    (ltInt64#)
#define OP_LE                    (leInt64#)
#define OP_PLUS                  (plusInt64#)
#define OP_MINUS                 (minusInt64#)
#define OP_TIMES                 (timesInt64#)
#define OP_NEGATE                negateInt64#
#else
#define ARR_TYPE                 ArrayI64
#define ARR_FROMSCALAR           FromScalarI64#
#define ARR_CONSTR               ArrayI64#
#define EL_TYPE_BOXED            Int64
#define EL_TYPE_PRIM             Int#
#define EL_RUNTIME_REP           'IntRep
#define EL_CONSTR                I64#
#define EL_SIZE                  SIZEOF_INT64#
#define EL_ALIGNMENT             ALIGNMENT_INT64#
#define EL_ZERO                  0#
#define EL_ONE                   1#
#define EL_MINUS_ONE             -1#
#define INDEX_ARRAY              indexInt64Array#
#define WRITE_ARRAY              writeInt64Array#
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
#endif
#include "Array.h"


instance Num (ArrayI64 ds) where
  (+) = zipV OP_PLUS
  {-# INLINE (+) #-}
  (-) = zipV OP_MINUS
  {-# INLINE (-) #-}
  (*) = zipV OP_TIMES
  {-# INLINE (*) #-}
  negate = mapV OP_NEGATE
  {-# INLINE negate #-}
  abs = mapV (\x -> if isTrue# (OP_GE x EL_ZERO)
                    then x
                    else OP_NEGATE x
                )
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (OP_GT x EL_ZERO)
                       then EL_ONE
                       else if isTrue# (OP_LT x EL_ZERO)
                            then EL_MINUS_ONE
                            else EL_ZERO
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}

instance Bounded (ArrayI64 ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
