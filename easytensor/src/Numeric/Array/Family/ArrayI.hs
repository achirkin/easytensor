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
-- Module      :  Numeric.Array.Family.ArrayI
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Family.ArrayI () where

import           GHC.Base             (runRW#)
import           GHC.Prim
import           GHC.TypeLits
import           GHC.Types
import           Data.Proxy

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions


#include "MachDeps.h"
#define ARR_TYPE                 ArrayI
#define ARR_FROMSCALAR           FromScalarI#
#define ARR_CONSTR               ArrayI#
#define EL_TYPE_BOXED            Int
#define EL_TYPE_PRIM             Int#
#define EL_RUNTIME_REP           'IntRep
#define EL_CONSTR                I#
#define EL_SIZE                  SIZEOF_HSINT#
#define EL_ALIGNMENT             ALIGNMENT_HSINT#
#define EL_ZERO                  0#
#define EL_ONE                   1#
#define EL_MINUS_ONE             -1#
#define INDEX_ARRAY              indexIntArray#
#define WRITE_ARRAY              writeIntArray#
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

instance Bounded (ArrayI ds) where
    minBound = broadcastArray minBound
    maxBound = broadcastArray maxBound
