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
    ( PrimBytesEvidence (..), inferPrimBytes
    , ElementWiseEvidence (..), inferElementWise
    , NumericFrameEvidence (..), inferNumericFrame
    ) where

import           Numeric.Array
import           Numeric.Array.ElementWise
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions


-- | Evidence for PrimBytes class
data PrimBytesEvidence t (ds :: [Nat])
  = PrimBytes (DataFrame t ds) => PrimBytesEvidence

-- | Evidence for ElementWise class
data ElementWiseEvidence t (ds :: [Nat])
  = ElementWise (Idx ds) t (DataFrame t ds) => ElementWiseEvidence

-- | Allow all common operations on available data frames
data NumericFrameEvidence t (ds :: [Nat])
  = ( NumericFrame t ds
  ) => NumericFrameEvidence

inferPrimBytes :: forall t (ds :: [Nat])
                . ( ArrayInstanceInference t ds
                  , Dimensions'' ds
                  )
               => PrimBytesEvidence t ds
inferPrimBytes = case getArrayInstance @t @ds of
    AIScalar   -> case elemTypeInstance @t of
      ETFloat  -> PrimBytesEvidence
      ETDouble -> PrimBytesEvidence
      ETInt    -> PrimBytesEvidence
      ETInt8   -> PrimBytesEvidence
      ETInt16  -> PrimBytesEvidence
      ETInt32  -> PrimBytesEvidence
      ETInt64  -> PrimBytesEvidence
      ETWord   -> PrimBytesEvidence
      ETWord8  -> PrimBytesEvidence
      ETWord16 -> PrimBytesEvidence
      ETWord32 -> PrimBytesEvidence
      ETWord64 -> PrimBytesEvidence
    AIArrayF   -> PrimBytesEvidence
    AIArrayD   -> PrimBytesEvidence
    AIArrayI   -> PrimBytesEvidence
    AIArrayI8  -> PrimBytesEvidence
    AIArrayI16 -> PrimBytesEvidence
    AIArrayI32 -> PrimBytesEvidence
    AIArrayI64 -> PrimBytesEvidence
    AIArrayW   -> PrimBytesEvidence
    AIArrayW8  -> PrimBytesEvidence
    AIArrayW16 -> PrimBytesEvidence
    AIArrayW32 -> PrimBytesEvidence
    AIArrayW64 -> PrimBytesEvidence
    AIFloatX2  -> PrimBytesEvidence
    AIFloatX3  -> PrimBytesEvidence
    AIFloatX4  -> PrimBytesEvidence

inferElementWise :: forall t (ds :: [Nat])
                . ( ArrayInstanceInference t ds
                  , Dimensions ds
                  )
                 => ElementWiseEvidence t ds
inferElementWise = case getArrayInstance @t @ds of
    AIScalar   -> ElementWiseEvidence
    AIArrayF   -> ElementWiseEvidence
    AIArrayD   -> ElementWiseEvidence
    AIArrayI   -> ElementWiseEvidence
    AIArrayI8  -> ElementWiseEvidence
    AIArrayI16 -> ElementWiseEvidence
    AIArrayI32 -> ElementWiseEvidence
    AIArrayI64 -> ElementWiseEvidence
    AIArrayW   -> ElementWiseEvidence
    AIArrayW8  -> ElementWiseEvidence
    AIArrayW16 -> ElementWiseEvidence
    AIArrayW32 -> ElementWiseEvidence
    AIArrayW64 -> ElementWiseEvidence
    AIFloatX2  -> ElementWiseEvidence
    AIFloatX3  -> ElementWiseEvidence
    AIFloatX4  -> ElementWiseEvidence


inferNumericFrame :: forall t (ds :: [Nat])
                   . ( ArrayInstanceInference t ds
                     , Dimensions ds
                     )
                   => NumericFrameEvidence t ds
inferNumericFrame = case getArrayInstance @t @ds of
    AIFloatX2  -> NumericFrameEvidence
    AIFloatX3  -> NumericFrameEvidence
    AIFloatX4  -> NumericFrameEvidence
    AIScalar   -> case elemTypeInstance @t of
      ETFloat  -> NumericFrameEvidence
      ETDouble -> NumericFrameEvidence
      ETInt    -> NumericFrameEvidence
      ETInt8   -> NumericFrameEvidence
      ETInt16  -> NumericFrameEvidence
      ETInt32  -> NumericFrameEvidence
      ETInt64  -> NumericFrameEvidence
      ETWord   -> NumericFrameEvidence
      ETWord8  -> NumericFrameEvidence
      ETWord16 -> NumericFrameEvidence
      ETWord32 -> NumericFrameEvidence
      ETWord64 -> NumericFrameEvidence
    AIArrayF   -> NumericFrameEvidence
    AIArrayD   -> NumericFrameEvidence
    AIArrayI   -> NumericFrameEvidence
    AIArrayI8  -> NumericFrameEvidence
    AIArrayI16 -> NumericFrameEvidence
    AIArrayI32 -> NumericFrameEvidence
    AIArrayI64 -> NumericFrameEvidence
    AIArrayW   -> NumericFrameEvidence
    AIArrayW8  -> NumericFrameEvidence
    AIArrayW16 -> NumericFrameEvidence
    AIArrayW32 -> NumericFrameEvidence
    AIArrayW64 -> NumericFrameEvidence
