{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -dcore-lint #-}
-- {-# OPTIONS_GHC -ddump-tc-trace #-}
-- {-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.TestInference
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type Idx that enumerates through multiple dimensions.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-- Some of the type-level list operations are implemented using type families
--   and weirdly duplicated for kinds k,Nat,XNat:
--   This duplication is needed to workaround some GHC bugs (panic with "no skolem info")
-----------------------------------------------------------------------------

module Numeric.Dimensions.TestInference where

import           Numeric.Dimensions.Inference
import           Numeric.Dimensions.List

class ( ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
      ) => A (as :: [k]) (bs :: [k]) (asbs :: [k]) where


instance ( asbs ~ EvalList ('Concat (ToList as) (ToList bs))
        --  , ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
         , asbsL ~ ToList asbs
        --  , asbs ~ EvalCons asbsL
        --  , ToList as ~ asL
        --  , bsL ~ ToList bs
         )
         => A as bs asbs where
