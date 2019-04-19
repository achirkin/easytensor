{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE UnboxedTuples          #-}
module Numeric.DataFrame.Internal.Array.Class
  ( PrimArray (..), CumulDims (..)
  , cumulDims, cdTotalDim, cdTotalDim#, cdIx
  , ixOff, unsafeFromFlatList
  ) where

import           GHC.Base           (ByteArray#, Int (..), Int#, Word (..),
                                     word2Int#)
import           Numeric.Dimensions
import           Numeric.PrimBytes

-- | Given @Dims ns@, @CumulativeDims@ is a list of length @Length ns + 1@;
--   which cumulative @totalDim@ accumulated on the right.
--   In particular, its first element is @totalDim ds@,
--   its last element is always is always @1@.
newtype CumulDims = CumulDims { unCumulDims :: [Word] }

-- | Calculate cumulative dims
cumulDims :: Dims (ns :: [k]) -> CumulDims
cumulDims = CumulDims . uncurry (:)
          . foldr (\d (c, cs) -> (c*d,c:cs)) (1, []) . listDims

-- | Get the total number of elements
cdTotalDim :: CumulDims -> Word
cdTotalDim ~(CumulDims ~(n:_)) = n

cdTotalDim# :: CumulDims -> Int#
cdTotalDim# ~(CumulDims ~(n:_)) = case n of W# w -> word2Int# w

-- | Calculate offset of an Idxs
--
--   Note, you can take offset of subspace with CumulDims of larger space
--     - very convenient!
cdIx :: CumulDims -> Idxs ns -> Int
cdIx ~(CumulDims ~(_:steps))
  = fromIntegral . sum . zipWith (*) steps . listIdxs

class PrimBytes t => PrimArray t a | a -> t where
    -- | Broadcast element into array
    broadcast :: t -> a
    -- | Index an array given an offset
    ix# :: Int# -> a -> t
    -- | Generate an array using an accumulator funtion
    gen# :: CumulDims
            -- ^ Dimensionality of the result array;
            --   Be careful! @ns@ depends on @a@, but this is not reflected in
            --   types and is not checked at runtime.
         -> (s -> (# s, t #))
         -> s -> (# s, a #)
    -- | update a single element in an array given an offset
    upd# :: CumulDims
            -- ^ Dimensionality of the result array;
            --   Be careful! @ns@ depends on @a@, but this is not reflected in
            --   types and is not checked at runtime.
         -> Int# -> t -> a -> a

    -- | Offset of an array as a number of elements
    offsetElems :: a -> Int#

    -- | Normally, this returns a cumulative @totalDim@s.
    --   However, if a particular implementation does not have the dimensionality
    --   information, it cannot return @CumulDims@;
    --   In this case, it is a sign that all elements of an array are same.
    --   Thus, it is possible to return the single element value instead.
    --
    --   Note, this function returns the only unique element only if it is
    --   a such by construction (there is no equality checks involved).
    uniqueOrCumulDims :: a -> Either t CumulDims

    -- | Get array by its offset and cumulative dims in a ByteArray.
    --   Both offset and dims are given in element number (not in bytes).
    --
    --   It is better to use this function instead of @fromBytes@ to avoid
    --   recalculating @CumulDims@ for implementations that require it.
    fromElems :: CumulDims -> Int# -> ByteArray# -> a

-- | Index array by an integer offset (starting from 0).
ixOff :: PrimArray t a => Int -> a -> t
ixOff (I# i) = ix# i

-- | Construct an array from a flat list and @Dims@;
--   Be careful! @ns@ depends on @a@, but this is not reflected in
--   types and is not checked at runtime.
unsafeFromFlatList :: PrimArray t a => Dims ns -> [t] -> a
unsafeFromFlatList ds vs = case gen# (cumulDims ds) f vs of (# _, r #) -> r
  where
    f :: [t] -> (# [t], t #)
    f []     = (# [], undefined #)
    f (x:xs) = (# xs, x #)
