{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Commons
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Commons
  ( PrimBytes (..)
  ) where

import GHC.Ptr
import GHC.Prim
import GHC.Types
import Foreign.Storable


class PrimBytes a where
  -- | Store content of a data type in a primitive byte array
  toBytes :: a -> ByteArray#
  -- | Load content of a data type from a primitive byte array
  fromBytes :: ByteArray# -> a
  -- | Size of a data type in bytes
  byteSize :: a -> Int#
  -- | Alignment of a data type in bytes
  byteAlign :: a -> Int#


instance PrimBytes a => Storable a where
  sizeOf x = I# (byteSize x)
  alignment x = I# (byteAlign x)
  peekElemOff ptr (I# offset) = peekByteOff ptr (I# (offset *# byteSize (undefined :: a)))
  pokeElemOff ptr (I# offset) = pokeByteOff ptr (I# (offset *# byteSize (undefined :: a)))
  peekByteOff (Ptr addr) (I# offset) = IO $ \s0 -> case newByteArray# bsize s0 of
        (# s1, marr #) -> case copyAddrToByteArray# (addr `plusAddr#` offset) marr 0# bsize s1 of
          s2 -> case unsafeFreezeByteArray# marr s2 of
            (# s3, arr #) -> (# s3, fromBytes arr #)
    where
      bsize = byteSize (undefined :: a)
  pokeByteOff (Ptr addr) (I# offset) x = IO $ \s0 -> case copyByteArrayToAddr# (toBytes x)
                                                                               0#
                                                                               (addr `plusAddr#` offset)
                                                                               bsize s0 of
       s2 -> (# s2, () #)
    where
      bsize = byteSize (undefined :: a)
  peek ptr = peekByteOff ptr 0
  poke ptr = pokeByteOff ptr 0
