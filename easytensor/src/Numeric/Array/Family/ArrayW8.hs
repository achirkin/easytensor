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
-- Module      :  Numeric.Array.Family.ArrayW8
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayW8 () where

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), RuntimeRep (..), isTrue#)
import           GHC.Word                  (Word8 (..))

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse


#include "MachDeps.h"
#define ARR_TYPE                 ArrayW8
#define ARR_FROMSCALAR           FromScalarW8#
#define ARR_CONSTR               ArrayW8#
#define EL_TYPE_BOXED            Word8
#define EL_TYPE_PRIM             Word#
#define EL_RUNTIME_REP           'WordRep
#define EL_CONSTR                W8#
#define EL_SIZE                  SIZEOF_WORD8#
#define EL_ALIGNMENT             ALIGNMENT_WORD8#
#define EL_ZERO                  0##
#define EL_ONE                   1##
#define EL_MINUS_ONE             -1#
#define INDEX_ARRAY              indexWord8Array#
#define WRITE_ARRAY              writeWord8Array#
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

instance Num (ArrayW8 ds) where
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


instance Bounded (ArrayW8 ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
