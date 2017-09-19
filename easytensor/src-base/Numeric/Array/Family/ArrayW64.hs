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
-- Module      :  Numeric.Array.Family.ArrayW64
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayW64 () where

#include "MachDeps.h"
import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), RuntimeRep (..), isTrue#)
import           GHC.Word                  (Word64 (..))
#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse

#if SIZEOF_HSWORD < 8

plusWord64#, minusWord64#, timesWord64#
  :: Word64# -> Word64# -> Word64#
plusWord64# x y  = int64ToWord64# (word64ToInt64# x `plusInt64#`  word64ToInt64# y)
minusWord64# x y = int64ToWord64# (word64ToInt64# x `minusInt64#` word64ToInt64# y)
timesWord64# x y = int64ToWord64# (word64ToInt64# x `timesInt64#` word64ToInt64# y)


#define ARR_TYPE                 ArrayW64
#define ARR_FROMSCALAR           FromScalarW64#
#define ARR_CONSTR               ArrayW64#
#define EL_TYPE_BOXED            Word64
#define EL_TYPE_PRIM             Word64#
#define EL_RUNTIME_REP           'Word64Rep
#define EL_CONSTR                W64#
#define EL_SIZE                  SIZEOF_WORD64#
#define EL_ALIGNMENT             ALIGNMENT_WORD64#
#define EL_ZERO                  (wordToWord64# 0##)
#define EL_ONE                   (wordToWord64# 1##)
#define INDEX_ARRAY              indexWord64Array#
#define WRITE_ARRAY              writeWord64Array#
#define OP_EQ                    (eqWord64#)
#define OP_NE                    (neWord64#)
#define OP_GT                    (gtWord64#)
#define OP_GE                    (geWord64#)
#define OP_LT                    (ltWord64#)
#define OP_LE                    (leWord64#)
#define OP_PLUS                  (plusWord64#)
#define OP_MINUS                 (minusWord64#)
#define OP_TIMES                 (timesWord64#)
#else
#define ARR_TYPE                 ArrayW64
#define ARR_FROMSCALAR           FromScalarW64#
#define ARR_CONSTR               ArrayW64#
#define EL_TYPE_BOXED            Word64
#define EL_TYPE_PRIM             Word#
#define EL_RUNTIME_REP           'WordRep
#define EL_CONSTR                W64#
#define EL_SIZE                  SIZEOF_WORD64#
#define EL_ALIGNMENT             ALIGNMENT_WORD64#
#define EL_ZERO                  0##
#define EL_ONE                   1##
#define INDEX_ARRAY              indexWord64Array#
#define WRITE_ARRAY              writeWord64Array#
#define OP_EQ                    eqWord#
#define OP_NE                    neWord#
#define OP_GT                    gtWord#
#define OP_GE                    geWord#
#define OP_LT                    ltWord#
#define OP_LE                    leWord#
#define OP_PLUS                  plusWord#
#define OP_MINUS                 minusWord#
#define OP_TIMES                 timesWord#
#endif
#include "Array.h"

instance Num (ArrayW64 ds) where
  (+) = zipV OP_PLUS
  {-# INLINE (+) #-}
  (-) = zipV OP_MINUS
  {-# INLINE (-) #-}
  (*) = zipV OP_TIMES
  {-# INLINE (*) #-}
#if SIZEOF_HSWORD < 8
  negate = mapV (\x -> int64ToWord64# (negateInt64# (word64ToInt64# x)))
#else
  negate = mapV (\x -> int2Word# (negateInt# (word2Int# x)))
#endif
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (OP_GT x EL_ZERO)
                       then EL_ONE
                       else EL_ZERO
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}


instance Bounded (ArrayW64 ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
