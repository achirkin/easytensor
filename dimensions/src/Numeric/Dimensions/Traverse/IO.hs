{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Traverse.IO
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Map a function over all dimensions provided dimension indices or offsets.
-- This module provides a variant of traversal that lives in IO monad.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Traverse.IO
  ( overDim, overDim_, overDimIdx, overDimIdx_, overDimOff, overDimOff_, overDimPart
  , foldDim, foldDimIdx, foldDimIdxOff
  ) where


import           GHC.Exts
import           GHC.IO                 (IO (..))

import           Numeric.Dimensions.Dim
import           Numeric.Dimensions.Idx
import           Numeric.Dimensions.Traverse



-- | Traverse over all dimensions keeping track of index and offset
overDim :: Dim (ds :: [Nat])
        -> (Idx ds -> Int# -> a -> IO a)
        -> Int# -> a -> IO a
overDim ds stf off0# = IO . overDim# ds (\i off# a -> case stf i off# a of
                                                           IO f -> f
                                         ) off0#
{-# INLINE overDim #-}

-- | Traverse over all dimensions keeping track of indices
overDimIdx :: Dim (ds :: [Nat])
           -> (Idx ds -> a -> IO a)
           -> a -> IO a
overDimIdx ds stf = IO . overDimIdx# ds (\i a -> case stf i a of IO f -> f)
{-# INLINE overDimIdx #-}

-- | Traverse over all dimensions keeping track of total offset
overDimOff :: Dim (ds :: [Nat])
        -> (Idx ds -> Int# -> a -> IO a)
        -> Int# -> a -> IO a
overDimOff ds stf off0# = IO . overDim# ds (\i off# a -> case stf i off# a of
                                                           IO f -> f
                                         ) off0#
{-# INLINE overDimOff #-}



-- | Same as overDim#, but with no return value
overDim_ :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> IO ())
         -> Int# -> IO ()
overDim_ ds stf off0# = IO $ overDim_# ds (\i off# -> case stf i off# of
                                                           IO f -> f
                                          ) off0#
{-# INLINE overDim_ #-}

-- | Traverse over all dimensions keeping track of indices, with no return value
overDimIdx_ :: Dim (ds :: [Nat])
            -> (Idx ds -> IO ())
            -> IO ()
overDimIdx_ ds stf = IO $ overDimIdx_# ds (\i -> case stf i of IO f -> f)
{-# INLINE overDimIdx_ #-}


-- | Traverse over all dimensions keeping track of total offset, with not return value
overDimOff_ :: Dim (ds :: [Nat])
            -> (Int# -> IO ())
            -> Int# -> IO ()
overDimOff_ ds stf off0# = IO $ overDimOff_# ds (\off#-> case stf off# of
                                                           IO f -> f
                                         ) off0#
{-# INLINE overDimOff_ #-}

-- | Traverse from the first index to the second index in each dimension.
--   Indices must be within Dim range, which is not checked.
--   You can combine positive and negative traversal directions along different dimensions.
overDimPart :: forall (ds :: [Nat]) a
             . Dimensions ds
            => Idx ds -> Idx ds
            -> (Idx ds -> Int# -> a -> IO a)
            -> Int# -> a -> IO a
overDimPart iMin iMax stf off0# = IO . overDimPart# iMin iMax (\i off# a -> case stf i off# a of
                                                                   IO f -> f
                                                              ) off0#
{-# INLINE overDimPart #-}
