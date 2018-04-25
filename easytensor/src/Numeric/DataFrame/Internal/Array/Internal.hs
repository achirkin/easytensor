{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE UnboxedTuples    #-}
-- | Internal primitive functions shared across modules
module Numeric.DataFrame.Internal.Array.Internal where

import           GHC.Base
import           Numeric.Dimensions

inftyD :: Double
inftyD = read "Infinity"
{-# INLINE inftyD #-}

inftyF :: Float
inftyF = read "Infinity"
{-# INLINE inftyF #-}



minInt# :: Int# -> Int# -> Int#
minInt# a b | isTrue# (a ># b) = b
            | otherwise        = a
{-# INLINE minInt# #-}


loop# :: Int# -- ^ initial value
      -> Int# -- ^ step
      -> Int# -- ^ final value (LESS THAN condition)
      -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop# n0 di n1 f = loop0 n0
  where
    loop0 i s | isTrue# (i >=# n1) = s
              | otherwise = loop0 (i +# di) (f i s)
{-# INLINE loop# #-}


-- | Loop with given increment, plus keep the step number
--   in the first argument of the iterated function
loopWithI# :: Int# -- ^ initial value
           -> Int# -- ^ step
           -> Int# -- ^ final value (LESS THAN condition)
           -> (Int# -> Int# -> State# s -> State# s) -> State# s -> State# s
loopWithI# n0 di n1 f = loop0 0# n0
  where
    loop0 j i s | isTrue# (i >=# n1) = s
                | otherwise = loop0 (j +# 1#) (i +# di) (f j i s)
{-# INLINE loopWithI# #-}


-- | Do something in a loop for int i from 0 to (n-1)
loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop1# n f = loop0 0#
  where
    loop0 i s | isTrue# (i ==# n) = s
              | otherwise = loop0 (i +# 1#) (f i s)
{-# INLINE loop1# #-}

-- | Do something in a loop for int i from 0 to (n-1)
loop1a# :: Int# -> (Int# -> a -> a) -> a -> a
loop1a# n f = loop0 0#
  where
    loop0 i s | isTrue# (i ==# n) = s
              | otherwise = s `seq` case f i s of s1 -> s1 `seq` loop0 (i +# 1#) s1
{-# INLINE loop1a# #-}


-- | Same as overDim#, but with no return value
overDim_# :: Dims (ds :: [k])
          -> (Idxs ds -> Int# -> State# s -> State# s) -- ^ function to map over each dimension
          -> Int# -- ^ Initial offset
          -> Int# -- ^ offset step
          -> State# s
          -> State# s
overDim_# ds f off0# step# s0 = case overDim_'# ds g off0# s0 of
                              (# s1, _ #) -> s1
  where
    g i off# s = (# f i off# s, off# +# step# #)
{-# INLINE overDim_# #-}


overDim_'# :: Dims (ds :: [k])
           -> (Idxs ds -> Int# -> State# s -> (# State# s, Int# #)) -- ^ function to map over each dimension
           -> Int# -- ^ Initial offset
           -> State# s
           -> (# State# s, Int# #)
overDim_'# U f = f U
overDim_'# (d :* ds) f = overDim_'# ds (loop 1)
  where
    n = dimVal d
    loop i js off# s | i > n = (# s , off#  #)
                     | otherwise = case f (Idx i :* js) off# s of
                         (# s', off1# #) -> loop (i+1) js off1# s'
