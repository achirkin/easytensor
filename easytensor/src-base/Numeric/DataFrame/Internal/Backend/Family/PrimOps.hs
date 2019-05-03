{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
-- | Internal primitive functions shared across modules
module Numeric.DataFrame.Internal.Backend.Family.PrimOps where

import           GHC.Base

inftyD :: Double
inftyD = read "Infinity"
{-# INLINE inftyD #-}

inftyF :: Float
inftyF = read "Infinity"
{-# INLINE inftyF #-}


loop# :: Int# -- ^ initial value
      -> Int# -- ^ step
      -> Int# -- ^ final value (LESS THAN condition)
      -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop# n0 di n1 f = loop0 n0
  where
    loop0 i s | isTrue# (i >=# n1) = s
              | otherwise = loop0 (i +# di) (f i s)
{-# INLINE loop# #-}


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
