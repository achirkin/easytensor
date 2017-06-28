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
{-# LANGUAGE Strict                #-}
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

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), RuntimeRep (..), isTrue#)
import           GHC.Word                  (Word64 (..))

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse


#include "MachDeps.h"
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
#define EL_MINUS_ONE             -1#
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
#include "Array.h"

instance Num (ArrayW64 ds) where
  (+) = zipV plusWord#
  {-# INLINE (+) #-}
  (-) = zipV minusWord#
  {-# INLINE (-) #-}
  (*) = zipV timesWord#
  {-# INLINE (*) #-}
  negate = mapV (\x -> int2Word# (negateInt# (word2Int# x)))
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (gtWord# x 0##)
                       then 1##
                       else 0##
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}


instance Bounded (ArrayW64 ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
