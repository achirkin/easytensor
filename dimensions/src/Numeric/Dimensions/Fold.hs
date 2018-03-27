{-# LANGUAGE PolyKinds #-}
-- Workaround weird behavior of GHC 8.4
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Fold
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Fold a function over all dimensions provided dimension indices or offsets.
--
-----------------------------------------------------------------------------
module Numeric.Dimensions.Fold
  ( overDim, overDim_, overDimIdx, overDimIdx_
  , overDimOff, overDimOff_
  , overDimReverse, overDimReverseIdx
  , foldDim, foldDimIdx, foldDimOff
  , foldDimReverse, foldDimReverseIdx
  , overDimPart, overDimPartIdx
  ) where


import           Control.Monad           ((>=>))
import           Numeric.Dimensions.Idxs

-- | Go over all dimensions keeping track of index and offset
overDim :: Monad m
        => Dims ds -- ^ Shape of a space
        -> (Idxs ds -> Int -> a -> m a) -- ^ Function to call on each dimension
        -> Int -- ^ Initial offset
        -> Int -- ^ Offset step
        -> a -- ^ Initial value
        -> m a
overDim U k offset _step = k U offset
overDim (d :* ds) k offset step = overDim ds k' offset (di * step)
  where
    dw = dimVal d
    di = fromIntegral dw
    k' is = go 1
      where
        go i off
          | i > dw = return
          | otherwise = k (Idx i :* is) off >=> go (i+1) (off+step)
{-# INLINE overDim #-}

-- | Go over all dimensions in reverse order keeping track of index and offset
overDimReverse :: Monad m
               => Dims ds -- ^ Shape of a space
               -> (Idxs ds -> Int -> a -> m a) -- ^ Function to call on each dimension
               -> Int -- ^ Initial offset
               -> Int -- ^ Offset step (substracted from initial offset)
               -> a -- ^ Initial value
               -> m a
overDimReverse U k offset _step = k U offset
overDimReverse (d :* ds) k offset step = overDimReverse ds k' offset (di * step)
  where
    dw = dimVal d
    di = fromIntegral dw
    k' is = go dw
      where
        go i off
          | i <= 0 = return
          | otherwise = k (Idx i :* is) off >=> go (i-1) (off-step)
{-# INLINE overDimReverse #-}

-- | Go over all dimensions keeping track of index and offset
overDim_ :: Monad m
         => Dims ds -- ^ Shape of a space
         -> (Idxs ds -> Int -> m ()) -- ^ Function to call on each dimension
         -> Int -- ^ Initial offset
         -> Int -- ^ Offset step
         -> m ()
overDim_ U k offset _step = k U offset
overDim_ (d :* ds) k offset step = overDim_ ds k' offset (di * step)
  where
    dw = dimVal d
    di = fromIntegral dw
    k' is = go 1
      where
        go i off
          | i > dw = return ()
          | otherwise = k (Idx i :* is) off >> go (i+1) (off+step)
{-# INLINE overDim_ #-}

-- | Go over all dimensions keeping track of index
overDimIdx :: Monad m
           => Dims ds -- ^ Shape of a space
           -> (Idxs ds -> a -> m a) -- ^ Function to call on each dimension
           -> a -- ^ Initial value
           -> m a
overDimIdx U k = k U
overDimIdx (d :* ds) k = overDimIdx ds k'
  where
    dw = dimVal d
    k' is = go 1
      where
        go i
          | i > dw = return
          | otherwise = k (Idx i :* is) >=> go (i+1)
{-# INLINE overDimIdx #-}

-- | Go over all dimensions keeping track of index
overDimIdx_ :: Monad m
            => Dims ds -- ^ Shape of a space
            -> (Idxs ds -> m ()) -- ^ Function to call on each dimension
            -> m ()
overDimIdx_ U k = k U
overDimIdx_ (d :* ds) k = overDimIdx_ ds k'
  where
    dw = dimVal d
    k' is = go 1
      where
        go i
          | i > dw = return ()
          | otherwise = k (Idx i :* is) >> go (i+1)
{-# INLINE overDimIdx_ #-}


-- | Go over all dimensions keeping track of total offset
overDimOff :: Monad m
           => Dims ds -- ^ Shape of a space
           -> (Int -> a -> m a) -- ^ Function to call with each offset value
           -> Int -- ^ Initial offset
           -> Int -- ^ Offset step
           -> a -- ^ Initial value
           -> m a
overDimOff ds k offset step = go (totalDim ds) offset
  where
    go i off
          | i == 0 = return
          | otherwise = k off >=> go (i-1) (off+step)
{-# INLINE overDimOff #-}

-- | Go over all dimensions keeping track of total offset
overDimOff_ :: Monad m
            => Dims ds -- ^ Shape of a space
            -> (Int -> m ()) -- ^ Function to call with each offset value
            -> Int -- ^ Initial offset
            -> Int -- ^ Offset step
            -> m ()
overDimOff_ ds k offset step = go (totalDim ds) offset
  where
    go i off
          | i == 0 = return ()
          | otherwise = k off >> go (i-1) (off+step)
{-# INLINE overDimOff_ #-}


-- | Go over all dimensions in reverse order keeping track of index
overDimReverseIdx :: Monad m
                  => Dims ds -- ^ Shape of a space
                  -> (Idxs ds -> a -> m a) -- ^ Function to call on each dimension
                  -> a -- ^ Initial value
                  -> m a
overDimReverseIdx U k = k U
overDimReverseIdx (d :* ds) k = overDimReverseIdx ds k'
  where
    dw = dimVal d
    k' is = go dw
      where
        go i
          | i <= 0 = return
          | otherwise = k (Idx i :* is) >=> go (i-1)
{-# INLINE overDimReverseIdx #-}



-- | Fold over all dimensions keeping track of index and offset
foldDim :: Dims ds -- ^ Shape of a space
        -> (Idxs ds -> Int -> a -> a) -- ^ Function to call on each dimension
        -> Int -- ^ Initial offset
        -> Int -- ^ Offset step
        -> a -- ^ Initial value
        -> a
foldDim U k offset _step = k U offset
foldDim (d :* ds) k offset step = foldDim ds k' offset (di * step)
  where
    dw = dimVal d
    di = fromIntegral dw
    k' is = go 1
      where
        go i off
          | i > dw = id
          | otherwise = go (i+1) (off+step) . k (Idx i :* is) off
{-# INLINE foldDim #-}

-- | Fold over all dimensions in reverse order keeping track of index and offset
foldDimReverse :: Dims ds -- ^ Shape of a space
               -> (Idxs ds -> Int -> a -> a) -- ^ Function to call on each dimension
               -> Int -- ^ Initial offset
               -> Int -- ^ Offset step (substracted from initial offset)
               -> a -- ^ Initial value
               -> a
foldDimReverse U k offset _step = k U offset
foldDimReverse (d :* ds) k offset step = foldDimReverse ds k' offset (di * step)
  where
    dw = dimVal d
    di = fromIntegral dw
    k' is = go dw
      where
        go i off
          | i <= 0 = id
          | otherwise = go (i-1) (off-step) . k (Idx i :* is) off
{-# INLINE foldDimReverse #-}



-- | Fold over all dimensions keeping track of index
foldDimIdx :: Dims ds -- ^ Shape of a space
           -> (Idxs ds -> a -> a) -- ^ Function to call on each dimension
           -> a -- ^ Initial value
           -> a
foldDimIdx U k = k U
foldDimIdx (d :* ds) k = foldDimIdx ds k'
  where
    dw = dimVal d
    k' is = go 1
      where
        go i
          | i > dw = id
          | otherwise = go (i+1) . k (Idx i :* is)
{-# INLINE foldDimIdx #-}


-- | Fold over all dimensions keeping track of total offset
foldDimOff :: Dims ds -- ^ Shape of a space
           -> (Int -> a -> a) -- ^ Function to call on each dimension
           -> Int -- ^ Initial offset
           -> Int -- ^ Offset step
           -> a -- ^ Initial value
           -> a
foldDimOff ds k offset step = go (totalDim ds) offset
  where
    go i off
          | i == 0 = id
          | otherwise = go (i-1) (off+step) . k off
{-# INLINE foldDimOff #-}


-- | Fold over all dimensions in reverse order keeping track of index
foldDimReverseIdx :: Dims ds -- ^ Shape of a space
                  -> (Idxs ds -> a -> a) -- ^ Function to call on each dimension
                  -> a -- ^ Initial value
                  -> a
foldDimReverseIdx U k = k U
foldDimReverseIdx (d :* ds) k = foldDimReverseIdx ds k'
  where
    dw = dimVal d
    k' is = go dw
      where
        go i
          | i <= 0 = id
          | otherwise = go (i-1) . k (Idx i :* is)
{-# INLINE foldDimReverseIdx #-}


-- | Traverse from the first index to the second index in each dimension.
--   You can combine positive and negative traversal directions
--   along different dimensions.
--
--   Note, initial and final indices are included in the range;
--   the argument function is guaranteed to execute at least once.
overDimPart :: (Dimensions ds, Monad m)
            => Idxs ds -- ^ Initial indices
            -> Idxs ds -- ^ Final indices
            -> (Idxs ds -> Int -> a -> m a)
                       -- ^ Function to call on each dimension
            -> Int     -- ^ Initial offset (at index @minBound :: Idxs ds@)
                       --   Note, this is not an offset value at initial indices.
            -> Int     -- ^ Offset step
            -> a       -- ^ initial value
            -> m a
overDimPart imin imax f offset step = overDimPart' stepSizes imin imax f offset
    where
      stepSizes = createStepSizes (dims `inSpaceOf` imin) step

      createStepSizes :: Dims ns -> Int -> TypedList StepSize ns
      createStepSizes U _ = U
      createStepSizes (d :* ds) k
        = StepSize k :* createStepSizes ds (k * fromIntegral (dimVal d))

overDimPart' :: Monad m
             => TypedList StepSize ns
             -> Idxs ds -> Idxs ds
             -> (Idxs ds -> Int -> a -> m a)
             -> Int
             -> a -> m a
overDimPart' U U U k off0 = k U off0
overDimPart' (siW :* iws) (Idx iStart :* starts) (Idx iEnd :* ends) k off0
  | iEnd >= iStart = overDimPart' iws starts ends (loop iStart) (off0 + headOff)
  | otherwise      = overDimPart' iws starts ends (looi iStart) (off0 + headOff)
  where
    StepSize iW = siW
    headOff = iW * (fromIntegral iStart - 1)
    loop i js off
      | i > iEnd = return
      | otherwise = k (Idx i :* js) off >=> loop (i+1) js (off + iW)
    looi i js off
      | i < iEnd = return
      | otherwise = k (Idx i :* js) off >=> looi (i-1) js (off - iW)


newtype StepSize n = StepSize Int

-- | Traverse from the first index to the second index in each dimension.
--   You can combine positive and negative traversal directions
--   along different dimensions.
--
--   Note, initial and final indices are included in the range;
--   the argument function is guaranteed to execute at least once.
overDimPartIdx :: Monad m
               => Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> a -> m a)
                          -- ^ Function to call on each dimension
               -> a       -- ^ initial value
               -> m a
overDimPartIdx U U k = k U
overDimPartIdx (start :* starts) (end :* ends) k
  | iEnd >= iStart = overDimPartIdx starts ends (loop iStart)
  | otherwise      = overDimPartIdx starts ends (looi iStart)
  where
    Idx iStart = start
    Idx iEnd   = end
    loop i is
      | i > iEnd = return
      | otherwise = k (Idx i :* is) >=> loop (i+1) is
    looi i is
      | i < iEnd = return
      | otherwise = k (Idx i :* is) >=> looi (i-1) is
