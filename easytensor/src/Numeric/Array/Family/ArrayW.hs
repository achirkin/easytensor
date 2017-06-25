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
-- Module      :  Numeric.Array.Family.ArrayW
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayW () where

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Word (..), Int (..), RuntimeRep (..), isTrue#)

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse
import           Numeric.TypeLits


#include "MachDeps.h"
#define ARR_TYPE                 ArrayW
#define ARR_FROMSCALAR           FromScalarW#
#define ARR_CONSTR               ArrayW#
#define EL_TYPE_BOXED            Word
#define EL_TYPE_PRIM             Word#
#define EL_RUNTIME_REP           'WordRep
#define EL_CONSTR                W#
#define EL_SIZE                  SIZEOF_HSWORD#
#define EL_ALIGNMENT             ALIGNMENT_HSWORD#
#define EL_ZERO                  0##
#define EL_ONE                   1##
#define EL_MINUS_ONE             -1#
#define INDEX_ARRAY              indexWordArray#
#define WRITE_ARRAY              writeWordArray#
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

instance Num (ArrayW ds) where
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


instance Bounded (ArrayW ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
