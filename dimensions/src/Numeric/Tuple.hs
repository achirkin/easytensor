{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Tuple
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
module Numeric.Tuple
    ( module TS
    , toStrict, fromStrict
    ) where

import Numeric.Tuple.Strict as TS
import qualified Numeric.Tuple.Lazy as TL
import Unsafe.Coerce (unsafeCoerce)

toStrict :: TL.Tuple xs -> TS.Tuple xs
toStrict U = U
toStrict (TL.Id x :* xs)
  = let !y = x `seq` TS.Id x
        !ys = toStrict xs
    in y :* ys
#if __GLASGOW_HASKELL__ >= 802
#else
toStrict _ = error "Tuple.toStrict: impossible argument"
#endif

fromStrict :: TS.Tuple xs -> TL.Tuple xs
fromStrict = unsafeCoerce
{-# INLINE fromStrict #-}
