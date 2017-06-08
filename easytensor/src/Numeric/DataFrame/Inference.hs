{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
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

import GHC.TypeLits (Nat)

import Numeric.Dimensions
import Numeric.Commons
import Numeric.Array
import Numeric.DataFrame.Type


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
    AIArrayI8  -> notImplemented
    AIArrayI16 -> notImplemented
    AIArrayI32 -> notImplemented
    AIArrayI64 -> notImplemented
    AIArrayW   -> notImplemented
    AIArrayW8  -> notImplemented
    AIArrayW16 -> notImplemented
    AIArrayW32 -> notImplemented
    AIArrayW64 -> notImplemented
    AIFloatX2  -> PrimBytesEvidence
    AIFloatX3  -> notImplemented
    AIFloatX4  -> notImplemented

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
    AIArrayI8  -> notImplemented
    AIArrayI16 -> notImplemented
    AIArrayI32 -> notImplemented
    AIArrayI64 -> notImplemented
    AIArrayW   -> notImplemented
    AIArrayW8  -> notImplemented
    AIArrayW16 -> notImplemented
    AIArrayW32 -> notImplemented
    AIArrayW64 -> notImplemented
    AIFloatX2  -> ElementWiseEvidence
    AIFloatX3  -> notImplemented
    AIFloatX4  -> notImplemented


inferNumericFrame :: forall t (ds :: [Nat])
                   . ( ArrayInstanceInference t ds
                     , Dimensions ds
                     )
                   => NumericFrameEvidence t ds
inferNumericFrame = case getArrayInstance @t @ds of
    AIFloatX2  -> NumericFrameEvidence
    AIFloatX3  -> notImplemented
    AIFloatX4  -> notImplemented
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
    AIArrayI8  -> notImplemented
    AIArrayI16 -> notImplemented
    AIArrayI32 -> notImplemented
    AIArrayI64 -> notImplemented
    AIArrayW   -> notImplemented
    AIArrayW8  -> notImplemented
    AIArrayW16 -> notImplemented
    AIArrayW32 -> notImplemented
    AIArrayW64 -> notImplemented





notImplemented :: a
notImplemented = error "Sorry, not implemented for this data type yet"
