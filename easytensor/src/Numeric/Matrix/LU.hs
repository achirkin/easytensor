{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.Matrix.LU
  ( MatrixLU (..), LU (..)
  , pivotMat, luSolve
  , detViaLU, inverseViaLU
  ) where

import Control.Monad                        (foldM)
import Control.Monad.ST
import Data.Foldable                        (foldl', forM_)
import Data.List                            (delete)
import GHC.Base
import Numeric.DataFrame.Contraction        ((%*))
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
import Numeric.PrimBytes
import Numeric.Scalar.Internal
import Numeric.Vector.Internal





-- | Result of LU factorization with Partial Pivoting
--   @ PA = LU @.
data LU t n
  = LU
  { luLower    :: Matrix t n n
    -- ^ Lower triangular matrix @L@.
    --   All elements on the diagonal of @L@ equal @1@.
  , luUpper    :: Matrix t n n
    -- ^ Upper triangular matrix @U@
  , luPerm     :: Matrix t n n
    -- ^ Row permutation matrix @P@
  , luPermSign :: Scalar t
    -- ^ Sign of permutation @luPermSign == det . luPerm@
  }

deriving instance (Show t, PrimBytes t, KnownDim n) => Show (LU t n)
deriving instance (Eq (Matrix t n n), Eq t) => Eq (LU t n)

class MatrixLU t (n :: Nat) where
    -- | Compute LU factorization with Partial Pivoting
    lu :: Matrix t n n -> LU t n


instance ( KnownDim n, Ord t, Fractional t
         , PrimBytes t, PrimArray t (Matrix t n n))
         => MatrixLU t n where
    lu m' = case runRW# go of
        (# _, (# bu, bl #) #) -> LU
            { luLower    = fromElems steps 0# bl
            , luUpper    = fromElems steps 0# bu
            , luPerm     = p
            , luPermSign = si
            }
      where
        dn = dimVal' @n
        dnn = dn * dn
        steps = CumulDims [dnn, dn, 1]
        (m, p, si) = pivotMat m'
        n  = case dn of W# w -> word2Int# w
        nn = case dnn of W# w -> word2Int# w
        tbs = byteSize @t undefined
        bsize = nn *# tbs
        ixm i j = ix# (i *# n +# j) m
        loop :: (Int# -> a -> State# s -> (# State# s, a #))
             -> Int# -> Int# -> a -> State# s -> (# State# s, a #)
        loop f i k x s
          | isTrue# (i ==# k) = (# s, x #)
          | otherwise = case f i x s of
              (# s', y #) -> loop f ( i +# 1# ) k y s'
        go s0
          | (# s1, mbl #) <- newByteArray# bsize s0
          , (# s2, mbu #) <- newByteArray# bsize s1
          , s3 <- setByteArray# mbl 0# bsize 0# s2
          , s4 <- setByteArray# mbu 0# bsize 0# s3
          , readL <- \i j -> readArray @t mbl (i *# n +# j)
          , readU <- \i j -> readArray @t mbu (i *# n +# j)
          , writeL <- \i j -> writeArray @t mbl (i *# n +# j)
          , writeU <- \i j -> writeArray @t mbu (i *# n +# j)
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
luSolve :: forall (t :: Type) (n :: Nat)
         . ( KnownDim n, Fractional t
           , PrimArray t (Matrix t n n), PrimArray t (Vector t n))
        => LU t n -> Vector t n -> Vector t n
luSolve LU {..} b = x
  where
    -- Pb = LUx
    pb = luPerm %* b
    n# = case dimVal' @n of W# w -> word2Int# w
    n = I# n#
    -- Ly = Pb
    y :: Vector t n
    y = runST $ do
      my <- newDataFrame
      let ixA (I# i) (I# j) = scalar $ ix# (i *# n# +# j) luLower
          ixB (I# i) = scalar $ ix# i pb
      forM_ [0..n-1] $ \i -> do
        v <- foldM ( \v j -> do
                      dj <- readDataFrameOff my j
                      return $ v - dj * ixA i j
                   ) (ixB i) [0..i-1]
        writeDataFrameOff my i v
      unsafeFreezeDataFrame my
    -- Ux = y
    x = runST $ do
      mx <- newDataFrame
      let ixA (I# i) (I# j) = scalar $ ix# (i *# n# +# j) luUpper
          ixB (I# i) = scalar $ ix# i y
      forM_ [n-1, n-2 .. 0] $ \i -> do
        v <- foldM ( \v j -> do
                      dj <- readDataFrameOff mx j
                      return $ v - dj * ixA i j
                   ) (ixB i) [i+1..n-1]
        writeDataFrameOff mx i (v / ixA i i)
      unsafeFreezeDataFrame mx


-- | Permute rows that the largest magnitude elements in columns are on diagonals.
--
--   Invariants of result matrix:
--     * forall j >= i: |M[i,i]| >= M[j,i]
--     * if M[i,i] == 0 then forall j >= i: |M[i+1,i+1]| >= M[j,i+1]
pivotMat :: forall (t :: Type) (n :: Nat)
          . (KnownDim n, PrimArray t (Matrix t n n), Ord t, Num t)
         => Matrix t n n -> ( Matrix t n n -- permutated matrix
                            , Matrix t n n -- permutation matrix
                            , Scalar t -- sign of permutation matrix
                            )
pivotMat mat
    = ( let f ( _, [] )   = undefined
            f ( j, i:is )
              | j == n    = f (0, is)
              | otherwise = (# (j+1, i:is), ix i j #)
        in case gen# steps f (0,rowOrder) of
            (# _, r #) -> r
      , let f ( _, [] ) = undefined
            f ( j, x:xs )
               | j == n    = f (0, xs)
               | otherwise = (# ( j + 1, x:xs), if j == x then 1 else 0 #)
        in case gen# steps f (0,rowOrder) of
            (# _, r #) -> r
      , if countMisordered rowOrder `rem` 2 == 1
        then -1 else 1
      )
  where
    dn = dimVal' @n
    steps = CumulDims [dn * dn, dn, 1]
    -- permuted row ordering
    rowOrder = uncurry fillPass $ searchPass 0 [0..n-1]
    -- matrix size
    n = fromIntegral dn
    n# = case n of I# x -> x
    -- sign of permutations
    countMisordered :: [Int] -> Int
    countMisordered [] = 0
    countMisordered (i:is) = foldl' (\c j -> if i > j then succ c else c) 0 is
                           + countMisordered is
    ix (I# i) (I# j) = ix# (i *# n# +# j) mat
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


inverseViaLU :: forall (t :: Type) (n :: Nat)
              . ( KnownDim n, Ord t, Fractional t
                , PrimArray t (Matrix t n n)
                , PrimArray t (Vector t n)
                , PrimBytes (Vector t n)
                , PrimBytes (Matrix t n n)
                )
             => Matrix t n n -> Matrix t n n
inverseViaLU m = runST $ do
      m' <- newDataFrame
      indexWise_ @t @'[n] @'[n] @'[n, n] @_ @()
        (\is ->
          indexWise_ @t @'[n] @'[] @'[n] @_ @()
            (\(j:*U) -> writeDataFrame m' (j:*is)) . luSolve luM
        ) eye
      unsafeFreezeDataFrame m'
    where
      luM = lu m

    -- much nicer, but probably slower version:
    --   (it is not particularly fast to do an extra transpose
    --       before calculating the inverse)
    -- ewmap (luSolve . lu $ transpose m) eye


detViaLU :: forall (t :: Type) (n :: Nat)
          . ( KnownDim n, Ord t, Fractional t
            , PrimArray t (Matrix t n n))
         => Matrix t n n -> Scalar t
detViaLU m = prodF (luUpper f) * prodF (luLower f) * luPermSign f
    where
      f = lu m
      n = case dimVal' @n of W# w -> word2Int# w
      n1 = n +# 1#
      nn1 = n *# n -# 1#
      prodF a = scalar $ prodF' nn1 a
      prodF' 0# a = ix# 0# a
      prodF' k  a = ix# k a * prodF' (k -# n1) a
