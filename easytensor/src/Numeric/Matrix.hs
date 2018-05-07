{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedSums               #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix
  ( MatrixTranspose (..)
  , SquareMatrix (..)
  , MatrixDeterminant (..)
  , MatrixInverse (..)
  , MatrixLU (..), LUFact (..)
  , Matrix
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  , Mat22d, Mat23d, Mat24d
  , Mat32d, Mat33d, Mat34d
  , Mat42d, Mat43d, Mat44d
  , mat22, mat33, mat44
  , (%*)
  , pivotMat, luSolve
  ) where


import           Data.List                               (delete)
import           Data.Foldable (foldl')
import           GHC.Base
import           Numeric.DataFrame.Contraction           ((%*))
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family as AFam
import           Numeric.DataFrame.Shape
import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Matrix.Class
import           Numeric.PrimBytes
import           Numeric.Scalar
import           Numeric.Vector

import Data.Foldable
import Control.Monad
import           Control.Monad.ST
import           Numeric.DataFrame.ST



-- | Compose a 2x2D matrix
mat22 :: ( PrimBytes (Vector t 2)
         , PrimBytes (Matrix t 2 2)
         )
      => Vector t 2 -> Vector t 2 -> Matrix t 2 2
mat22 = (<::>)

-- | Compose a 3x3D matrix
mat33 :: ( PrimBytes t
         , PrimBytes (Vector t 3)
         , PrimBytes (Matrix t 3 3)
         )
      => Vector t 3 -> Vector t 3 -> Vector t 3 -> Matrix t 3 3
mat33 a b c = runST $ do
  mmat <- newDataFrame
  copyDataFrame a (1:*1:*U) mmat
  copyDataFrame b (1:*2:*U) mmat
  copyDataFrame c (1:*3:*U) mmat
  unsafeFreezeDataFrame mmat

-- | Compose a 4x4D matrix
mat44 :: forall t
       . ( PrimBytes t
         , PrimBytes (Vector t (4 :: Nat))
         , PrimBytes (Matrix t (4 :: Nat) (4 :: Nat))
         )
      => Vector t (4 :: Nat)
      -> Vector t (4 :: Nat)
      -> Vector t (4 :: Nat)
      -> Vector t (4 :: Nat)
      -> Matrix t (4 :: Nat) (4 :: Nat)
mat44 a b c d = runST $ do
  mmat <- newDataFrame
  copyDataFrame a (1:*1:*U) mmat
  copyDataFrame b (1:*2:*U) mmat
  copyDataFrame c (1:*3:*U) mmat
  copyDataFrame d (1:*4:*U) mmat
  unsafeFreezeDataFrame mmat



instance ( KnownDim n, KnownDim m
         , PrimArray t (Matrix t n m)
         , PrimArray t (Matrix t m n)
         ) => MatrixTranspose t (n :: Nat) (m :: Nat) where
    transpose df = case elemSize0 df of
      0# -> broadcast (ix# 0# df)
      nm | I# m <- fromIntegral $ dimVal' @m
         , I# n <- fromIntegral $ dimVal' @n
         -> let f ( I# j,  I# i )
                  | isTrue# (j ==# m) = f ( 0 , I# (i +# 1#) )
                  | otherwise         = (# ( I# (j +# 1#), I# i )
                                         , ix# (j *# n +# i) df
                                         #)
            in case gen# nm f (0,0) of
              (# _, r #) -> r

instance MatrixTranspose (t :: Type) (xn :: XNat) (xm :: XNat) where
    transpose (XFrame (df :: DataFrame t ns))
      | ((D :: Dim n) :* (D :: Dim m) :* U) <- dims @Nat @ns
      , E <- AFam.inferPrimElem @t @n @'[m]
      = XFrame (transpose df :: Matrix t m n)
    transpose _ = error "MatrixTranspose/transpose: impossible argument"

instance (KnownDim n, PrimArray t (Matrix t n n), Num t)
      => SquareMatrix t n where
    eye
      | n@(I# n#) <- fromIntegral $ dimVal' @n
      = let f 0 = (# n, 1 #)
            f k = (# k - 1, 0 #)
        in case gen# (n# *# n#) f 0 of
            (# _, r #) -> r
    diag se
      | n@(I# n#) <- fromIntegral $ dimVal' @n
      , e <- unScalar se
      = let f 0 = (# n, e #)
            f k = (# k - 1, 0 #)
        in case gen# (n# *# n#) f 0 of
            (# _, r #) -> r
    trace df
      | I# n <- fromIntegral $ dimVal' @n
      , n1 <- n +# 1#
      = let f 0# = ix# 0# df
            f k  = ix# k  df + f (k -# n1)
        in scalar $ f (n *# n -# 1#)


instance ( KnownDim n, Ord t, Fractional t
         , PrimBytes t
         , PrimArray t (Matrix t n n)
         , PrimArray t (Vector t n)
         , PrimBytes (Vector t n)
         , PrimBytes (Matrix t n n)
         )
         => MatrixInverse t n where
  inverse m = ewmap (luSolve (lu m)) eye


instance ( KnownDim n, Ord t, Fractional t
         , PrimBytes t, PrimArray t (Matrix t n n))
         => MatrixDeterminant t n where
  det m = prodF (luUpper f) * prodF (luLower f) * luPermSign f
    where
      f = lu m
      !(I# n) = fromIntegral $ dimVal' @n
      n1 = n +# 1#
      nn1 = n *# n -# 1#
      prodF a = scalar $ prodF' nn1 a
      prodF' 0# a = ix# 0# a
      prodF' k  a  = ix# k a * prodF' (k -# n1) a


instance ( KnownDim n, Ord t, Fractional t
         , PrimBytes t, PrimArray t (Matrix t n n))
         => MatrixLU t n where
    lu m' = case runRW# go of
        (# _, (# bu, bl #) #) -> LUFact
            { luLower    = fromElems 0# nn bl
            , luUpper    = fromElems 0# nn bu
            , luPerm     = p
            , luPermSign = si
            }
      where
        (m, p, si) = pivotMat m'
        !(I# n) = fromIntegral $ dimVal' @n
        nn = n *# n
        tbs = byteSize @t undefined
        bsize = nn *# tbs
        ixm i j = ix# (i +# n *# j) m
        loop :: (Int# -> a -> State# s -> (# State# s, a #))
             -> Int# -> Int# -> a -> State# s -> (# State# s, a #)
        loop f i k x s
          | isTrue# (i ==# k) = (# s, x #)
          | otherwise = case f i x s of
              (# s', y #) -> loop f ( i+# 1# ) k y s'
        go s0
          | (# s1, mbl #) <- newByteArray# bsize s0
          , (# s2, mbu #) <- newByteArray# bsize s1
          , s3 <- setByteArray# mbl 0# bsize 0# s2
          , s4 <- setByteArray# mbu 0# bsize 0# s3
          , readL <- \i j -> readArray @t mbl (i +# n *# j)
          , readU <- \i j -> readArray @t mbu (i +# n *# j)
          , writeL <- \i j -> writeArray @t mbl (i +# n *# j)
          , writeU <- \i j -> writeArray @t mbu (i +# n *# j)
          , computeU <- \i j ->
              let f k x s
                    | (# s' , ukj #) <- readU k j s
                    , (# s'', lik #) <- readL i k s'
                    = (# s'', x - ukj * lik #)
              in loop f 0# i (ixm i j)
          , computeL' <- \i j ->
              let f k x s
                    | (# s' , ukj #) <- readU k j s
                    , (# s'', lik #) <- readL i k s'
                    = (# s'', x - ukj * lik #)
              in loop f 0# j (ixm i j)
          , (# sr, () #) <-
              loop
                ( \j _ sj -> case sj of
                    sj0
                      | sj1 <- writeL j j 1 sj0
                      , (# sj2, () #) <- loop
                          ( \i _ sij0 -> case computeU i j sij0 of
                               (# sij1, uij #) -> (# writeU i j uij sij1, () #)
                          ) 0# j () sj1
                      , (# sj3, ujj #) <- computeU j j sj2
                      , sj4 <- writeU j j ujj sj3
                        -> case ujj of
                          0 -> loop
                            ( \i _ sij -> (# writeL i j 0 sij, () #)
                            ) (j +# 1#) n () sj4
                          x -> loop
                            ( \i _ sij0 -> case computeL' i j sij0 of
                                (# sij1, lij #) -> (# writeL i j (lij / x) sij1, () #)
                            ) (j +# 1#) n () sj4
                ) 0# n () s4
          , (# sf0, bl #) <- unsafeFreezeByteArray# mbl sr
          , (# sf1, bu #) <- unsafeFreezeByteArray# mbu sf0
          = (# sf1, (# bu, bl #) #)


-- | Solve @Ax = b@ problem given LU decomposition of A.
luSolve :: forall t n
         . ( KnownDim n, Ord t, Fractional t
           , PrimBytes t, PrimArray t (Matrix t n n), PrimArray t (Vector t n))
        => LUFact t n -> Vector t n -> Vector t n
luSolve LUFact {..} b = x
  where
    pb = luPerm %* b
    !n@(I# n#) = fromIntegral $ dimVal' @n
    y :: Vector t n
    y = runST $ do
      my <- newDataFrame
      let ixA (I# i) (I# j) = scalar $ ix# (i +# n# *# j) luLower
          ixB (I# i) = scalar $ ix# i pb
          wr = writeDataFrameOff my
          rr = readDataFrameOff my
      for_ [0..n-1] $ \i -> do
        v <- foldM ( \x j -> do
                      dj <- rr j
                      return $ x - dj * ixA i j
                   ) (ixB i) [0..i-1]
        wr i v
      unsafeFreezeDataFrame my
    x = runST $ do
      mx <- newDataFrame
      let ixA (I# i) (I# j) = scalar $ ix# (i +# n# *# j) luUpper
          ixB (I# i) = scalar $ ix# i y
          wr = writeDataFrameOff mx
          rr = readDataFrameOff mx
      for_ [n-1, n-2 .. 0] $ \i -> do
        v <- foldM ( \x j -> do
                      dj <- rr j
                      return $ x - dj * ixA i j
                   ) (ixB i) [i..n-1]
        wr i (v / ixA i i)
      unsafeFreezeDataFrame mx


-- | Permute rows that the largest magnitude elements in columns are on diagonals.
--
--   Invariants of result matrix:
--     * forall j >= i: |M[i,i]| >= M[j,i]
--     * if M[i,i] == 0 then forall j >= i: |M[i+1,i+1]| >= M[j,i+1]
pivotMat :: forall (t :: Type) (n :: k)
          . (KnownDim n, PrimArray t (Matrix t n n), Ord t, Num t)
         => Matrix t n n -> ( Matrix t n n -- permutated matrix
                            , Matrix t n n -- permutation matrix
                            , Scalar t -- sign of permutation matrix
                            )
pivotMat m
    = ( let f ( j, [] )   = f (j+1, rowOrder)
            f ( j, i:is ) = (# (j, is), ix i j #)
        in case gen# nn f (0,rowOrder) of
            (# _, r #) -> r
      , let f ( _, [] ) = (# (0, []), 0 #)
            f ( i, x:xs )
               | i == x    = (# ( i+1, x:xs), 1 #)
               | i == n    = f  ( 0, xs)
               | otherwise = (# ( i+1, x:xs), 0 #)
        in case gen# nn f (0,rowOrder) of
            (# _, r #) -> r
      , if countMisordered rowOrder `rem` 2 == 1
        then -1 else 1
      )
  where
    -- permuted row ordering
    rowOrder = uncurry fillPass $ searchPass 0 [0..n-1]
    -- matrix size
    !n@(I# n#) = fromIntegral $ dimVal' @n
    -- sign of permutations
    countMisordered :: [Int] -> Int
    countMisordered [] = 0
    countMisordered (i:is) = foldl' (\c j -> if i > j then succ c else c) 0 is
                           + countMisordered is
    nn = n# *# n#
    ix (I# i) (I# j) = ix# (i +# j *# n#) m
    findMax :: Int -> [Int] -> (t, Int)
    findMax j = foldl' (\(ox, oi) i -> let x = abs (ix i j)
                                       in if x > ox then (x, i)
                                                    else (ox, oi)
                      ) (0, 0)
    -- search maximums, leaving Nothing where all rows are 0
    searchPass :: Int -> [Int] -> ([Int], [Maybe Int])
    searchPass j is
      | j == n    = (is, [])
      | otherwise = case findMax j is of
          (0, _) -> (Nothing:) <$> searchPass (j+1) is
          (_, i) -> (Just i:) <$> searchPass (j+1) (delete i is)
    -- replace Nothings with remaining row numbers
    fillPass :: [Int] -> [Maybe Int] -> [Int]
    fillPass _ []                  = []
    fillPass js (Just i : is)      = i : fillPass js is
    fillPass (j:js) (Nothing : is) = j : fillPass js is
    fillPass [] (Nothing : is)     = 0 : fillPass [] is
