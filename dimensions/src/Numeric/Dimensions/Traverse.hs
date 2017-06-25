{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UnboxedTuples             #-}
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
  , foldDim, foldDimIdx, foldDimIdxOff
  ) where


import           GHC.Exts

import           Numeric.Dimensions.Dim
import           Numeric.Dimensions.Idx



-- | Traverse over all dimensions keeping track of index and offset
overDim# :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #))
         -> Int#
         -> a
         -> State# s
         -> (# State# s, a #)
overDim# ds f off0# a0 s0 = case overDim'# ds g off0# a0 s0 of
                              (# s1, _, a1 #) -> (# s1, a1 #)
  where
    g i off# a s = case f i off# a s of
                    (# t, b #) -> (# t, off# +# 1#, b #)
{-# INLINE overDim# #-}

-- | Fold over all dimensions keeping track of index and offset
foldDim :: Dim (ds :: [Nat])
        -> (Idx ds -> Int# -> a -> a)
        -> Int# -> a -> a
foldDim ds f off0# a0 = case foldDim' ds g off0# a0 of
                              (# _, a1 #) -> a1
  where
    g i off# a = (# off# +# 1#, f i off# a #)
{-# INLINE foldDim #-}


-- | Same as overDim#, but with no return value
overDim_# :: Dim (ds :: [Nat])
          -> (Idx ds -> Int# -> State# s -> (# State# s, () #))
          -> Int#
          -> State# s
          -> (# State# s, () #)
overDim_# ds f off0# s0 = case overDim_'# ds g off0# s0 of
                              (# s1, _ #) -> (# s1, () #)
  where
    g i off# s = case f i off# s of
                    (# t, _ #) -> (# t, off# +# 1# #)
{-# INLINE overDim_# #-}

-- | Traverse over all dimensions keeping track of indices
overDimIdx# :: Dim (ds :: [Nat])
            -> (Idx ds -> a -> State# s -> (# State# s, a #))
            -> a
            -> State# s
            -> (# State# s, a #)
overDimIdx# D f = f Z
overDimIdx# ((Dn :: Dim n) :* ds) f = overDimIdx# ds (loop 1)
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
foldDimIdx ((Dn :: Dim n) :* ds) f = foldDimIdx ds (loop 1)
  where
    n = dimVal' @n
    loop i js a | i > n = a
                | otherwise = case f (i:!js) a of b -> loop (i+1) js b



-- | Traverse over all dimensions keeping track of indices, with no return value
overDimIdx_# :: Dim (ds :: [Nat])
             -> (Idx ds -> State# s -> (# State# s, () #))
             -> State# s
             -> (# State# s, () #)
overDimIdx_# D f = f Z
overDimIdx_# ((Dn :: Dim n) :* ds) f = overDimIdx_# ds (loop 1)
  where
    n = dimVal' @n
    loop i js s | i > n = (# s,  () #)
                | otherwise = case f (i:!js) s of
                          (# s', _ #) -> loop (i+1) js s'

-- | Traverse over all dimensions keeping track of total offset
overDimOff# :: Dim (ds :: [Nat])
            -> (Int# -> a -> State# s -> (# State# s, a #))
            -> Int# -> a -> State# s -> (# State# s, a #)
overDimOff# ds f off0# = loop off0#
  where
    n = case dimVal ds of I# n# -> n# +# off0#
    loop off# a s | isTrue# (off# >=# n) = (# s,  a #)
                  | otherwise = case f off# a s of
                                  (# s', b #) -> loop (off# +# 1#) b s'

-- | Fold over all dimensions keeping track of total offset
foldDimIdxOff :: Dim (ds :: [Nat])
            -> (Int# -> a -> a)
            -> Int# -> a -> a
foldDimIdxOff ds f off0# = loop off0#
  where
    n = case dimVal ds of I# n# -> n# +# off0#
    loop off# a | isTrue# (off# >=# n) = a
                | otherwise = case f off# a of b -> loop (off# +# 1#) b


-- | Traverse over all dimensions keeping track of total offset, with not return value
overDimOff_# :: Dim (ds :: [Nat])
             -> (Int# -> State# s -> (# State# s, () #))
             -> Int# -> State# s -> (# State# s, () #)
overDimOff_# ds f off0# = loop off0#
  where
    n = case dimVal ds of I# n# -> n# +# off0#
    loop off# s | isTrue# (off# >=# n) = (# s,  () #)
                | otherwise = case f off# s of
                                  (# s', () #) -> loop (off# +# 1#) s'

-- | Traverse from the first index to the second index in each dimension.
--   Indices must be within Dim range, which is not checked.
--   You can combine positive and negative traversal directions along different dimensions.
overDimPart# :: forall (ds :: [Nat]) a s
              . Dimensions ds
             => Idx ds
             -> Idx ds
             -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #))
             -> Int#
             -> a
             -> State# s
             -> (# State# s, a #)
overDimPart# = overDimPart'# offs
    where
      offs = createOffsets (dim @ds) 1
      createOffsets :: forall (ns :: [Nat]) . Dim ns -> Int -> Idx ns
      createOffsets D _ = Z
      createOffsets ((Dn :: Dim n) :* ds) k = k :! createOffsets ds (k * dimVal' @n)






overDim'# :: Dim (ds :: [Nat])
          -> (Idx ds -> Int# -> a -> State# s -> (# State# s, Int#, a #))
          -> Int#
          -> a
          -> State# s
          -> (# State# s, Int#,  a #)
overDim'# D f = f Z
overDim'# ((Dn :: Dim n) :* ds) f = overDim'# ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# a s | i > n = (# s, off#, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', off1#, b #) -> loop (i+1) js off1# b s'



foldDim' :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> a -> (# Int#, a #))
         -> Int# -> a -> (# Int#,  a #)
foldDim' D f = f Z
foldDim' ((Dn :: Dim n) :* ds) f = foldDim' ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# a | i > n = (#  off#, a #)
                     | otherwise = case f (i:!js) off# a of
                               (# off1#, b #) -> loop (i+1) js off1# b


overDim_'# :: Dim (ds :: [Nat])
           -> (Idx ds -> Int# -> State# s -> (# State# s, Int# #))
           -> Int#
           -> State# s
           -> (# State# s, Int# #)
overDim_'# D f = f Z
overDim_'# ((Dn :: Dim n) :* ds) f = overDim_'# ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# s | i > n = (# s, off# #)
                     | otherwise = case f (i:!js) off# s of
                               (# s', off1# #) -> loop (i+1) js off1# s'


overDimPart'# :: Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #))
              -> Int#
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
