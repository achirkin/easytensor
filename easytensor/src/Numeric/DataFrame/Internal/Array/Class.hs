{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UnboxedTuples          #-}
module Numeric.DataFrame.Internal.Array.Class
  ( PrimArray (..)
  , ixOff, unsafeFromFlatList
  ) where

import           GHC.Base          (ByteArray#, Int#, Int (..))
import           Numeric.PrimBytes


class PrimBytes t => PrimArray t a | a -> t where
    -- | Broadcast element into array
    broadcast :: t -> a
    -- | Index an array given an offset
    ix# :: Int# -> a -> t
    -- | Generate an array using an accumulator funtion
    gen# :: Int# -- ^ number of elements, not checked!
                 --   Avoid using this argument if possible.
         -> (s -> (# s, t #))
         -> s -> (# s, a #)
    -- | update a single element in an array given an offset
    upd# :: Int# -- ^ number of elements, not checked!
                 --   Avoid using this argument if possible.
         -> Int# -> t -> a -> a

    -- | Offset of an array in number of elements
    elemOffset :: a -> Int#

    -- | Number of elements in an array.
    --   Returns zero if this information is not available at runtime.
    --   This is possible only if all elements are same in an array.
    elemSize0 :: a -> Int#

    -- | Get array by its offset and size in a ByteArray.
    --   Both offset and size are given in element number.
    fromElems :: Int# -> Int# -> ByteArray# -> a

-- | Index array by an integer offset (starting from 0).
ixOff :: PrimArray t a => Int -> a -> t
ixOff (I# i) = ix# i

-- | Construct an array from a flat list and length
unsafeFromFlatList :: PrimArray t a => Int -> [t] -> a
unsafeFromFlatList (I# n) vs = case gen# n f vs of (# _, r #) -> r
  where
    f [] = (# [], undefined #)
    f (x:xs) = (# xs, x #)
