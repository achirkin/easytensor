{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Tuple
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-----------------------------------------------------------------------------
module Numeric.Tuple
    ( module TS
    , toStrict, fromStrict
    ) where

import qualified Numeric.Tuple.Lazy   as TL
import           Numeric.Tuple.Strict as TS
import           Unsafe.Coerce        (unsafeCoerce)

toStrict :: TL.Tuple xs -> TS.Tuple xs
toStrict U = U
toStrict (TL.Id x :* xs)
  = let !y = x `seq` TS.Id x
        !ys = toStrict xs
    in y :* ys

fromStrict :: TS.Tuple xs -> TL.Tuple xs
fromStrict = unsafeCoerce
{-# INLINE fromStrict #-}
