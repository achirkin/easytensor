{-# LANGUAGE MagicHash, UnboxedTuples, DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix.Class
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix.Class
  ( MatrixCalculus (..)
  , SquareMatrixCalculus (..)
  -- , Matrix2x2 (..)
  , MatrixProduct (..)
  , MatrixInverse (..)
  , prodF, prodD
  , prodI8, prodI16, prodI32, prodI64
  , prodW8, prodW16, prodW32, prodW64
  ) where

import GHC.Base (runRW#)
import GHC.Prim
import GHC.Types

#include "MachDeps.h"
#include "HsBaseConfig.h"

import Numeric.Commons
-- import Numeric.Vector.Class
-- import Numeric.Vector.Family (Vector)
-- import Numeric.Matrix.Family (Matrix)



class MatrixCalculus t (n :: Nat) (m :: Nat) v | v -> t, v -> n, v -> m where
    -- -- | Fill Mat with the same value
    -- broadcastMat :: t -> v
    -- -- | Get element by its index
    -- indexMat :: Int -> Int -> v -> t
    -- | Transpose Mat
    transpose :: (MatrixCalculus t m n w, PrimBytes w) => v -> w
    -- -- | First dimension size of a matrix
    -- dimN :: v -> Int
    -- -- | Second dimension size of a matrix
    -- dimM :: v -> Int
    -- -- | Get vector column by its index
    -- indexCol :: (VectorCalculus t n w, PrimBytes w) => Int -> v -> w
    -- -- | Get vector row by its index
    -- indexRow :: (VectorCalculus t m w, PrimBytes w) => Int -> v -> w

class SquareMatrixCalculus t (n :: Nat) v | v -> t, v -> n where
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: v
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: t -> v
    -- | Determinant of  Mat
    det :: v -> t
    -- | Sum of diagonal elements
    trace :: v -> t
    -- -- | Get the diagonal elements from Mat into Vec
    -- fromDiag :: (VectorCalculus t n w, PrimBytes w) => v -> w
    -- -- | Set Vec values into the diagonal elements of Mat
    -- toDiag :: (VectorCalculus t n w, PrimBytes w) => w -> v


-- class Matrix2x2 t where
--   -- | Compose a 2x2D matrix
--   mat22 :: Vector t 2 -> Vector t 2 -> Matrix t 2 2
--   rowsOfM22 :: Matrix t 2 2 -> (Vector t 2, Vector t 2)
--   colsOfM22 :: Matrix t 2 2 -> (Vector t 2, Vector t 2)

-- class ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
--    => MatrixProduct t (m :: Nat) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
--                     | as bs -> asbs, asbs as -> bs, asbs bs -> as where
--   -- | matrix-matrix or matrix-vector product
--   prod :: t (as +: m) -> t (m :+ bs) -> t asbs

class MatrixProduct a b c | a b -> c, a c -> b, b c -> a where
  -- | matrix-matrix or matrix-vector product
  prod :: a -> b -> c

class MatrixInverse a where
  inverse :: a -> a



prodF :: (FloatBytes a, FloatBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodF n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusFloat#` timesFloat# (ixF (i +# n *# l) x)
                                                                                           (ixF (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeFloatArray# marr (i +# n *# j) (loop' i j 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# SIZEOF_HSFLOAT#
{-# INLINE prodF #-}

prodD :: (DoubleBytes a, DoubleBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodD n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +## (*##) (ixD (i +# n *# l) x)
                                                                            (ixD (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeDoubleArray# marr (i +# n *# j) (loop' i j 0# 0.0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# SIZEOF_HSDOUBLE#
{-# INLINE prodD #-}

prodI8 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI8 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ixI (i +# n *# l) x)
                                                                          (ixI (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt8Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodI8 #-}


prodI16 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI16 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ixI (i +# n *# l) x)
                                                                          (ixI (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt16Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodI16 #-}


prodI32 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI32 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ixI (i +# n *# l) x)
                                                                          (ixI (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt32Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodI32 #-}


prodI64 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI64 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ixI (i +# n *# l) x)
                                                                          (ixI (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt64Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodI64 #-}


prodW8 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW8 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ixW (i +# n *# l) x)
                                                                                         (ixW (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord8Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodW8 #-}


prodW16 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW16 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ixW (i +# n *# l) x)
                                                                                         (ixW (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord16Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodW16 #-}

prodW32 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW32 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ixW (i +# n *# l) x)
                                                                                         (ixW (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord32Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodW32 #-}

prodW64 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW64 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ixW (i +# n *# l) x)
                                                                                         (ixW (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord64Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k
{-# INLINE prodW64 #-}


-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s) -> State# s -> State# s
loop2# n m f = loop' 0# 0#
  where
    loop' i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop' 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop' (i +# 1#) j s1
{-# INLINE loop2# #-}
