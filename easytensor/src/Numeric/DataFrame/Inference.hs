{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Inference
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- The module provides data types and functions to infer typeclasses at runtime.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Inference
    ( PrimBytesEvidence, inferPrimBytes
    , ElementWiseEvidence, inferElementWise
    , NumericFrameEvidence, inferNumericFrame
    ) where

import           Numeric.Array
import           Numeric.Array.ElementWise
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.TypeLits


-- | Evidence for PrimBytes class
type PrimBytesEvidence t (ds :: [Nat])
  = Evidence (PrimBytes (DataFrame t ds))

-- | Evidence for ElementWise class
type ElementWiseEvidence t (ds :: [Nat])
  = Evidence (ElementWise (Idx ds) t (DataFrame t ds))

-- | Allow all common operations on available data frames
type NumericFrameEvidence t (ds :: [Nat])
  = Evidence ( NumericFrame t ds)

inferPrimBytes :: forall t (ds :: [Nat])
                . ( ArrayInstanceInference t ds
                  , Dimensions ds
                  )
               => PrimBytesEvidence t ds
inferPrimBytes = case getArrayInstance @t @ds of
    AIScalar   -> case elemTypeInstance @t of
      ETFloat  -> Evidence
      ETDouble -> Evidence
      ETInt    -> Evidence
      ETInt8   -> Evidence
      ETInt16  -> Evidence
      ETInt32  -> Evidence
      ETInt64  -> Evidence
      ETWord   -> Evidence
      ETWord8  -> Evidence
      ETWord16 -> Evidence
      ETWord32 -> Evidence
      ETWord64 -> Evidence
    AIArrayF   -> Evidence
    AIArrayD   -> Evidence
    AIArrayI   -> Evidence
    AIArrayI8  -> Evidence
    AIArrayI16 -> Evidence
    AIArrayI32 -> Evidence
    AIArrayI64 -> Evidence
    AIArrayW   -> Evidence
    AIArrayW8  -> Evidence
    AIArrayW16 -> Evidence
    AIArrayW32 -> Evidence
    AIArrayW64 -> Evidence
    AIFloatX2  -> Evidence
    AIFloatX3  -> Evidence
    AIFloatX4  -> Evidence

inferElementWise :: forall t (ds :: [Nat])
                . ( ArrayInstanceInference t ds
                  , Dimensions ds
                  )
                 => ElementWiseEvidence t ds
inferElementWise = case getArrayInstance @t @ds of
    AIScalar   -> Evidence
    AIArrayF   -> Evidence
    AIArrayD   -> Evidence
    AIArrayI   -> Evidence
    AIArrayI8  -> Evidence
    AIArrayI16 -> Evidence
    AIArrayI32 -> Evidence
    AIArrayI64 -> Evidence
    AIArrayW   -> Evidence
    AIArrayW8  -> Evidence
    AIArrayW16 -> Evidence
    AIArrayW32 -> Evidence
    AIArrayW64 -> Evidence
    AIFloatX2  -> Evidence
    AIFloatX3  -> Evidence
    AIFloatX4  -> Evidence


inferNumericFrame :: forall t (ds :: [Nat])
                   . ( ArrayInstanceInference t ds
                     , Dimensions ds
                     )
                   => NumericFrameEvidence t ds
inferNumericFrame = case getArrayInstance @t @ds of
    AIFloatX2  -> Evidence
    AIFloatX3  -> Evidence
    AIFloatX4  -> Evidence
    AIScalar   -> case elemTypeInstance @t of
      ETFloat  -> Evidence
      ETDouble -> Evidence
      ETInt    -> Evidence
      ETInt8   -> Evidence
      ETInt16  -> Evidence
      ETInt32  -> Evidence
      ETInt64  -> Evidence
      ETWord   -> Evidence
      ETWord8  -> Evidence
      ETWord16 -> Evidence
      ETWord32 -> Evidence
      ETWord64 -> Evidence
    AIArrayF   -> Evidence
    AIArrayD   -> Evidence
    AIArrayI   -> Evidence
    AIArrayI8  -> Evidence
    AIArrayI16 -> Evidence
    AIArrayI32 -> Evidence
    AIArrayI64 -> Evidence
    AIArrayW   -> Evidence
    AIArrayW8  -> Evidence
    AIArrayW16 -> Evidence
    AIArrayW32 -> Evidence
    AIArrayW64 -> Evidence
