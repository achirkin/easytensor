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
-- Module      :  Numeric.NDArray.Base.ArrayI
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.Base.ArrayI () where

import           GHC.Types

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Array.Base.ArrayTH


-- * Utility functions

$(broadcastArrayDec arrayIDef)

$(mapVDec arrayIDef)

$(zipVDec arrayIDef)

$(accumV2Dec arrayIDef)

-- * Instances

$(instanceElementWiseDec arrayIDef)

$(instanceShowDec arrayIDef)

$(instanceEqDec arrayIDef)

$(instanceOrdDec arrayIDef)

$(instanceNumDec arrayIDef)

type instance ElemRep (ArrayI ds) = 'IntRep
$(instancePrimBytesDec arrayIDef)
