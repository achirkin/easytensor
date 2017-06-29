{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE Strict                    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Traverse
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Map a function over all dimensions provided dimension indices or offsets.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Traverse
  ( overDim#, overDim_#, overDimIdx#, overDimIdx_#, overDimOff#, overDimOff_#
  , overDimPart#
  , foldDim, foldDimIdx, foldDimOff
  , foldDimReverse, foldDimReverseIdx
  ) where


import           GHC.Exts

import           Numeric.Dimensions.Dim
import           Numeric.Dimensions.Idx



-- | Traverse over all dimensions keeping track of index and offset
overDim# :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #)) -- ^ function to map over each dimension
         -> Int# -- ^ Initial offset
         -> Int# -- ^ offset step
         -> a
         -> State# s
         -> (# State# s, a #)
overDim# ds f off0# step# a0 s0 = case overDim'# ds g off0# a0 s0 of
                              (# s1, _, a1 #) -> (# s1, a1 #)
  where
    g i off# a s = case f i off# a s of
                    (# t, b #) -> (# t, off# +# step#, b #)
{-# INLINE overDim# #-}

-- | Fold over all dimensions keeping track of index and offset
foldDim :: Dim (ds :: [Nat])
        -> (Idx ds -> Int# -> a -> a) -- ^ function to map over each dimension
        -> Int# -- ^ Initial offset
        -> Int# -- ^ offset step
        -> a -> a
foldDim ds f off0# step# a0 = case foldDim' ds g off0# a0 of
                              (# _, a1 #) -> a1
  where
    g i off# a = (# off# +# step#, f i off# a #)
{-# INLINE foldDim #-}

-- | Fold over all dimensions in reverse order keeping track of index and offset
foldDimReverse :: Dim (ds :: [Nat])
               -> (Idx ds -> Int# -> a -> a) -- ^ function to map over each dimension
               -> Int# -- ^ Initial offset
               -> Int# -- ^ offset step (substracted from initial offset)
               -> a -> a
foldDimReverse ds f off0# step# a0 = case foldDimReverse' ds g (off0# +# n# *# step# -# step#) a0 of
                              (# _, a1 #) -> a1
  where
    !(I# n#) = dimVal ds
    g i off# a = (# off# -# step#, f i off# a #)
{-# INLINE foldDimReverse #-}



-- | Same as overDim#, but with no return value
overDim_# :: Dim (ds :: [Nat])
          -> (Idx ds -> Int# -> State# s -> State# s) -- ^ function to map over each dimension
          -> Int# -- ^ Initial offset
          -> Int# -- ^ offset step
          -> State# s
          -> State# s
overDim_# ds f off0# step# s0 = case overDim_'# ds g off0# s0 of
                              (# s1, _ #) -> s1
  where
    g i off# s = (# f i off# s, off# +# step# #)
{-# INLINE overDim_# #-}

-- | Traverse over all dimensions keeping track of indices
overDimIdx# :: Dim (ds :: [Nat])
            -> (Idx ds -> a -> State# s -> (# State# s, a #))
            -> a
            -> State# s
            -> (# State# s, a #)
overDimIdx# D f = f Z
overDimIdx# ((Dn :: Dim n) :* (!ds)) f = overDimIdx# ds (loop 1)
  where
    n = dimVal' @n
    loop i js a s | i > n = (# s,  a #)
                  | otherwise = case f (i:!js) a s of
                            (# s', b #) -> loop (i+1) js b s'

-- | Fold all dimensions keeping track of indices
foldDimIdx :: Dim (ds :: [Nat])
            -> (Idx ds -> a -> a)
            -> a -> a
foldDimIdx D f = f Z
foldDimIdx ((Dn :: Dim n) :* (!ds)) f = foldDimIdx ds (loop 1)
  where
    n = dimVal' @n
    loop i js a | i > n = a
                | otherwise = loop (i+1) js $! f (i:!js) a

-- | Fold all dimensions in reverse order keeping track of indices
foldDimReverseIdx :: Dim (ds :: [Nat])
                  -> (Idx ds -> a -> a)
                  -> a -> a
foldDimReverseIdx D f = f Z
foldDimReverseIdx ((Dn :: Dim n) :* (!ds)) f = foldDimReverseIdx ds (loop n)
  where
    n = dimVal' @n
    loop i js a | i > n = a
                | otherwise = loop (i-1) js $! f (i:!js) a



-- | Traverse over all dimensions keeping track of indices, with no return value
overDimIdx_# :: Dim (ds :: [Nat])
             -> (Idx ds -> State# s -> State# s)
             -> State# s
             -> State# s
overDimIdx_# D f = f Z
overDimIdx_# ((Dn :: Dim n) :* (!ds)) f = overDimIdx_# ds (loop 1)
  where
    n = dimVal' @n
    loop i js s | i > n = s
                | otherwise =  loop (i+1) js (f (i:!js) s)

-- | Traverse over all dimensions keeping track of total offset
overDimOff# :: Dim (ds :: [Nat])
            -> (Int# -> a -> State# s -> (# State# s, a #)) -- ^ function to map over each dimension
            -> Int# -- ^ Initial offset
            -> Int# -- ^ offset step
            -> a -> State# s -> (# State# s, a #)
overDimOff# ds f off0# step# = loop off0#
  where
    off1# = case dimVal ds of I# n# -> n# *# step# +# off0#
    cond# = if isTrue# (off1# >=# off0#)
             then \off -> isTrue# (off >=# off1#)
             else \off -> isTrue# (off <=# off1#)
    loop off# a s | cond# off# = (# s,  a #)
                  | otherwise = case f off# a s of
                                  (# s', b #) -> loop (off# +# step#) b s'

-- | Fold over all dimensions keeping track of total offset
foldDimOff :: Dim (ds :: [Nat])
           -> (Int# -> a -> a) -- ^ function to map over each dimension
           -> Int# -- ^ Initial offset
           -> Int# -- ^ offset step
           -> a -> a
foldDimOff ds f off0# step# = loop off0#
  where
    off1# = case dimVal ds of I# n# -> n# *# step# +# off0#
    cond# = if isTrue# (off1# >=# off0#)
             then \off -> isTrue# (off >=# off1#)
             else \off -> isTrue# (off <=# off1#)
    loop off# a | cond# off# = a
                | otherwise  = loop (off# +# step#) $! f off# a


-- | Traverse over all dimensions keeping track of total offset, with not return value
overDimOff_# :: Dim (ds :: [Nat])
             -> (Int# -> State# s -> State# s) -- ^ function to map over each dimension
             -> Int# -- ^ Initial offset
             -> Int# -- ^ offset step
             -> State# s -> State# s
overDimOff_# ds f off0# step# = loop off0#
  where
    off1# = case dimVal ds of I# n# -> n# *# step# +# off0#
    cond# = if isTrue# (off1# >=# off0#)
            then \off -> isTrue# (off >=# off1#)
            else \off -> isTrue# (off <=# off1#)
    loop off# s | cond# off# = s
                | otherwise = loop (off# +# step#) (f off# s)

-- | Traverse from the first index to the second index in each dimension.
--   Indices must be within Dim range, which is not checked.
--   You can combine positive and negative traversal directions along different dimensions.
overDimPart# :: forall (ds :: [Nat]) a s
              . Dimensions ds
             => Idx ds
             -> Idx ds
             -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #)) -- ^ function to map over each dimension
             -> Int# -- ^ Initial offset
             -> Int# -- ^ offset step
             -> a
             -> State# s
             -> (# State# s, a #)
overDimPart# imin imax f off0 step = overDimPart'# offs imin imax f off0
    where
      offs = createOffsets (dim @ds) (I# step)
      createOffsets :: forall (ns :: [Nat]) . Dim ns -> Int -> Idx ns
      createOffsets D _ = Z
      createOffsets ((Dn :: Dim n) :* (!ds)) k = k :! createOffsets ds (k * dimVal' @n)






overDim'# :: Dim (ds :: [Nat])
          -> (Idx ds -> Int# -> a -> State# s -> (# State# s, Int#, a #)) -- ^ function to map over each dimension
          -> Int# -- ^ Initial offset
          -> a
          -> State# s
          -> (# State# s, Int#,  a #)
overDim'# D f = f Z
overDim'# ((Dn :: Dim n) :* (!ds)) f = overDim'# ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# a s | i > n = (# s , off# , a #)
                       | otherwise = case f (i:!js) off# a s of
                                 (# s', off1#, b #) -> loop (i+1) js off1# b s'



foldDim' :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> a -> (# Int#, a #)) -- ^ function to map over each dimension
         -> Int# -- ^ Initial offset
         -> a -> (# Int#,  a #)
foldDim' D f = f Z
foldDim' ((Dn :: Dim n) :* (!ds)) f = foldDim' ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# a | i > n = (#  off#, a #)
                     | otherwise = case f (i:!js) off# a of
                               (# off1#, b #) -> loop (i+1) js off1# b

foldDimReverse' :: Dim (ds :: [Nat])
                -> (Idx ds -> Int# -> a -> (# Int#, a #)) -- ^ function to map over each dimension
                -> Int# -- ^ Initial offset
                -> a -> (# Int#,  a #)
foldDimReverse' D f = f Z
foldDimReverse' ((Dn :: Dim n) :* (!ds)) f = foldDimReverse' ds (loop n)
  where
    n = dimVal' @n
    loop i js off# a | i <= 0 = (#  off#, a #)
                     | otherwise = case f (i:!js) off# a of
                                (# off1#, b #) -> loop (i-1) js off1# b



overDim_'# :: Dim (ds :: [Nat])
           -> (Idx ds -> Int# -> State# s -> (# State# s, Int# #)) -- ^ function to map over each dimension
           -> Int# -- ^ Initial offset
           -> State# s
           -> (# State# s, Int# #)
overDim_'# D f = f Z
overDim_'# ((Dn :: Dim n) :* (!ds)) f = overDim_'# ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# s | i > n = (# s , off#  #)
                     | otherwise = case f (i:!js) off# s of
                               (# s', off1# #) -> loop (i+1) js off1# s'


overDimPart'# :: Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #)) -- ^ function to map over each dimension
              -> Int# -- ^ Initial offset
              -> a
              -> State# s
              -> (# State# s, a #)
overDimPart'# _ Z Z f off0# = f Z off0#
overDimPart'# (I# iW:!iws) (iMin:!mins) (iMax:!maxs) f off0#
    | iMax >= iMin = overDimPart'# iws mins maxs (loop iMin) (off0# +# minOff#)
    | otherwise    = overDimPart'# iws mins maxs (looi iMin) (off0# +# minOff#)
  where
    minOff# = case iMin of I# i -> iW *# (i -# 1#)
    loop i js off# a s | i > iMax = (# s, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', b #) -> loop (i+1) js (off# +# iW) b s'
    looi i js off# a s | i < iMax = (# s, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', b #) -> looi (i-1) js (off# -# iW) b s'
