{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE UnboxedTuples          #-}
module Numeric.DataFrame.Internal.PrimArray
  ( PrimArray (..), CumulDims (..)
  , cumulDims, cdTotalDim, cdTotalDim#, cdIx
  , ixOff, unsafeFromFlatList, getSteps, fromSteps
  ) where

import Data.Monoid        as Mon (Monoid (..))
import Data.Semigroup     as Sem (Semigroup (..))
import GHC.Base           (ByteArray#, Int (..), Int#, Word (..), word2Int#)
import Numeric.Dimensions
import Numeric.PrimBytes

-- | Given @Dims ns@, @CumulativeDims@ is a list of length @Length ns + 1@;
--   which cumulative @totalDim@ accumulated on the right.
--   In particular, its first element is @totalDim ds@,
--   its last element is always is always @1@.
newtype CumulDims = CumulDims { unCumulDims :: [Word] }

instance Sem.Semigroup CumulDims where
    CumulDims as <> CumulDims bs = CumulDims $ map (head bs *) (init as) ++ bs

instance Mon.Monoid CumulDims where
    mempty = CumulDims [1]
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif


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

-- | Try to get @CumulDims@ from an array,
--   and create it using @Dims@ if failed.
getSteps :: PrimArray t a => Dims (ns :: [k]) -> a -> CumulDims
getSteps dds df = case uniqueOrCumulDims df of
   Left  _  -> cumulDims dds
   Right ds -> ds
{-# INLINE getSteps #-}

-- | Get @Dims@ by "de-accumulating" @CumulDims@.
fromSteps :: CumulDims -> SomeDims
fromSteps = someDimsVal . f . unCumulDims
  where
    -- ignore last value, which is always 1
    f :: [Word] -> [Word]
    f []       = []
    f [_]      = []
    f [n,_]    = [n]
    f (a:b:cs) = a `quot` b : f (b:cs)
{-# INLINE fromSteps #-}

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

    -- | If the array represented as a single broadcasted value,
    --   return this value.
    --   Otherwise, return full array content:
    --    @CumulDims@, array offset (elements), byte array with the content.
    arrayContent# :: a -> (# t | (# CumulDims, Int#, ByteArray# #) #)

    -- | Offset of an array as a number of elements
    offsetElems :: a -> Int#
    offsetElems a = case arrayContent# a of
      (# _ | #)             -> 0#
      (# | (# _, o, _ #) #) -> o
    {-# INLINE offsetElems #-}

    -- | Normally, this returns a cumulative @totalDim@s.
    --   However, if a particular implementation does not have the dimensionality
    --   information, it cannot return @CumulDims@;
    --   In this case, it is a sign that all elements of an array are same.
    --   Thus, it is possible to return the single element value instead.
    --
    --   Note, this function returns the only unique element only if it is
    --   a such by construction (there is no equality checks involved).
    uniqueOrCumulDims :: a -> Either t CumulDims
    uniqueOrCumulDims a = case arrayContent# a of
      (# x | #)              -> Left x
      (# | (# cd, _, _ #) #) -> Right cd
    {-# INLINE uniqueOrCumulDims #-}

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
unsafeFromFlatList :: PrimArray t a => Dims ns -> t -> [t] -> a
unsafeFromFlatList ds x0 vs = case gen# (cumulDims ds) f vs of (# _, r #) -> r
  where
    f []     = (# [], x0 #)
    f (x:xs) = (# xs, x #)
