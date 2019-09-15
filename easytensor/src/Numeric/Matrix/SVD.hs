{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.Matrix.SVD
  ( MatrixSVD (..), SVD (..)
  , svd1, svd2, svd3, svd3q
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.Basics
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Bidiagonal
import Numeric.Matrix.Internal
import Numeric.Quaternion.Internal
import Numeric.Scalar.Internal
import Numeric.Subroutine.Sort
import Numeric.Tuple
import Numeric.Vector.Internal

-- | Result of SVD factorization
--   @ M = svdU %* asDiag svdS %* transpose svdV @.
--
--   Invariants:
--
--   * Singular values `svdS` are in non-increasing order and are non-negative.
--   * svdU and svdV are orthogonal matrices
--   * det svdU == 1
--
--   NB: <https://en.wikipedia.org/wiki/Singular_value_decomposition SVD on wiki>
data SVD t n m
  = SVD
  { svdU :: Matrix t n n
    -- ^ Left-singular basis matrix
  , svdS :: Vector t (Min n m)
    -- ^ Vector of singular values
  , svdV :: Matrix t m m
    -- ^ Right-singular basis matrix
  }

deriving instance ( Show t, PrimBytes t
                  , KnownDim n, KnownDim m, KnownDim (Min n m))
                  => Show (SVD t n m)
deriving instance ( Eq (Matrix t n n)
                  , Eq (Matrix t m m)
                  , Eq (Vector t (Min n m)))
                  => Eq (SVD t n m)

class RealFloatExtras t
    => MatrixSVD (t :: Type) (n :: Nat) (m :: Nat) where
    -- | Compute SVD factorization of a matrix
    svd :: Matrix t n m -> SVD t n m

-- | Obvious dummy implementation of SVD for 1x1 matrices
svd1 :: (PrimBytes t, Num t, Eq t) => Matrix t 1 1 -> SVD t 1 1
svd1 m = SVD
    { svdU = 1
    , svdS = broadcast $ abs x
    , svdV = broadcast $ if x == 0 then 1 else signum x
    }
  where
    x = ixOff 0 m

-- | SVD of a 2x2 matrix can be computed analytically
--
--   Related discussion:
--
--   https://scicomp.stackexchange.com/questions/8899/robust-algorithm-for-2-times-2-svd/
--
--   https://ieeexplore.ieee.org/document/486688
svd2 :: forall t . RealFloatExtras t => Matrix t 2 2 -> SVD t 2 2
svd2 (DF2 (DF2 m00 m01) (DF2 m10 m11)) =
    SVD
    { svdU = DF2 (DF2         uc  us)
                 (DF2 (negate us) uc)
    , svdS = DF2 sigma1 (abs sigma2)
    , svdV = DF2 (DF2 vc  (sg2 (negate vs)))
                 (DF2 vs  (sg2 vc         ))
    }
  where
    x1 = m00 - m11 -- 2F
    x2 = m00 + m11 -- 2E
    y1 = m01 + m10 -- 2G
    y2 = m01 - m10 -- 2H
    yy = y1*y2
    h1 = hypot x1 y1 -- h1 >= abs x1
    h2 = hypot x2 y2 -- h2 >= abs x2
    sigma1 = 0.5 * (h2 + h1)  -- sigma1 >= abs sigma2
    sigma2 = 0.5 * (h2 - h1)  -- can be negative, which is accounted by sg2
    sg2 = negateUnless (sigma2 >= 0)
    hx1 = h1 + x1
    hx2 = h2 + x2
    hxhx = hx1*hx2
    hxy  = hx1*y2
    yhx  = y1*hx2
    (uc', us', vc', vs') = case (x1 > 0 || y1 /= 0, x2 > 0 || y2 /= 0) of
      (True , True ) -> (hxhx + yy, hxy - yhx, hxhx - yy, hxy + yhx)
      (True , False) -> (y1,  hx1, -y1, hx1)
      (False, True ) -> (y2, -hx2, -y2, hx2)
      (False, False) -> (1, 0, -1, 0)
    ru = recip $ hypot uc' us'
    rv = recip $ hypot vc' vs'
    uc = uc' * ru
    us = us' * ru
    vc = vc' * rv
    vs = vs' * rv

-- | Get SVD decomposition of a 3x3 matrix using `svd3q` function.
--
--   This function reorders the singular components under the hood to make sure
--   @s1 >= s2 >= s3 >= 0@.
--   Thus, it has some overhead on top of `svd3q`.
svd3 :: forall t . (Quaternion t, RealFloatExtras t) => Matrix t 3 3 -> SVD t 3 3
svd3 m = SVD
        { svdU = toMatrix33 u
        , svdS = DF3 s1 s2 s3'
        , svdV = neg3If (s3 < 0) (toMatrix33 v)
        }
  where
    (u, (DF3 s1 s2 s3), v) = svd3q m
    s3' = abs s3
    neg3If :: Bool -> Matrix t 3 3 -> Matrix t 3 3
    neg3If False = id
    neg3If True  = ewmap @t @'[3] neg3
    neg3 :: Vector t 3 -> Vector t 3
    neg3 (DF3 a b c) = DF3 a b (negate c)


-- | Get SVD decomposition of a 3x3 matrix, with orthogonal matrices U and V
--   represented as quaternions.
--   Important: U and V are bound to be rotations at the expense of the last
--              singular value being possibly negative.
--
--   This is an adoptation of a specialized 3x3 SVD algorithm described in
--     "Computing the Singular Value Decomposition of 3x3 matrices
--      with minimal branching and elementary floating point operations",
--   by  A. McAdams, A. Selle, R. Tamstorf, J. Teran, E. Sifakis.
--
--   http://pages.cs.wisc.edu/~sifakis/papers/SVD_TR1690.pdf
svd3q :: forall t . (Quaternion t, RealFloatExtras t)
      => Matrix t 3 3 -> (Quater t, Vector t 3, Quater t)
svd3q m = (u, s, v)
  where
    v = jacobiEigenQ (transpose m %* m)
    (s, u) = uncurry fixSigns $ qrDecomposition3 (m %* toMatrix33 v)
    -- last bit: make sure    s1 >= s2 >= 0
    fixSigns :: Vector t 3 -> Quater t -> (Vector t 3, Quater t)
    fixSigns (DF3 s1 s2 s3) q@(Quater a b c d) = case (s1 >= 0, s2 >= 0) of
      (True , True ) -> (mk3 s1 s2 s3, q)
      (False, True ) -> (mk3 (negate s1) s2 (negate s3), Quater (-c)  d   a  (-b))
      (True , False) -> (mk3 s1 (negate s2) (negate s3), Quater   d   c (-b) (-a))
      (False, False) -> (mk3 (negate s1) (negate s2) s3, Quater   b (-a)  d  (-c))
    -- one more thing:
    --   the singular values are ordered, but may have small errors;
    --   as a result adjacent values may seem to be out of order by a very small number
    mk3 :: Scalar t -> Scalar t -> Scalar t -> Vector t 3
    mk3 s1 s2 s3' = case (s1 >= s2, s1 >= abs s3, s2 >= abs s3) of
        (True , True , True ) -> DF3 s1 s2     s3' -- s1 >= s2 >= s3
        (True , True , False) -> DF3 s1 s3 (cs s2) -- s1 >= s3 >  s2
        (True , False, _    ) -> DF3 s3 s1 (cs s2) -- s3 >  s1 >= s2
        (False, True , True ) -> DF3 s2 s1     s3' -- s2 >  s1 >= s3
        (False, _    , False) -> DF3 s3 s2 (cs s1) -- s3 >  s2 >  s1
        (False, False, True ) -> DF3 s2 s3 (cs s1) -- s2 >= s3 >  s1
      where
        s3 = abs s3'
        cs = negateUnless (s3' >= 0)


-- | Approximate values for cos (a/2) and sin (a/2) of a Givens rotation for
--    a 2x2 symmetric matrix. (Algorithm 2)
jacobiGivensQ :: forall t . RealFloatExtras t => t -> t -> t -> (t, t)
jacobiGivensQ aii aij ajj
    | g*sh*sh < ch*ch = (w * ch, w * sh)
    | otherwise       = (c', s')
  where
    ch = 2 * (aii-ajj)
    sh = aij
    w = recip $ hypot ch sh
    g  = 5.82842712474619 :: t  -- 3 + sqrt 8
    c' = 0.9238795325112867 :: t -- cos (pi/8)
    s' = 0.3826834323650898 :: t -- sin (pi/8)


-- | A quaternion for a QR Givens iteration
qrGivensQ :: forall t . RealFloatExtras t => t -> t -> (t, t)
qrGivensQ a1 a2
    | a1 < 0    = (sh * w, ch * w)
    | otherwise = (ch * w, sh * w)
  where
    rho2 = a1*a1 + a2*a2
    sh = if rho2 > M_EPS then a2 else 0
    ch = abs a1 + sqrt (max rho2 M_EPS)
    w = recip $ hypot ch sh -- TODO: consider something like a hypot


-- | One iteration of the Jacobi algorithm on a symmetric 3x3 matrix
--
--   The three words arguments are indices:
--     0 <= i /= j /= k <= 2
jacobiEigen3Iteration :: (Quaternion t, RealFloatExtras t)
                     => Int -> Int -> Int
                     -> STDataFrame s t '[3,3]
                     -> ST s (Quater t)
jacobiEigen3Iteration i j k sPtr = do
    sii <- readDataFrameOff sPtr ii
    sij <- readDataFrameOff sPtr ij
    sjj <- readDataFrameOff sPtr jj
    sik <- readDataFrameOff sPtr ik
    sjk <- readDataFrameOff sPtr jk
    -- Coefficients for a quaternion corresponding to a Givens rotation
    let (ch, sh) = jacobiGivensQ sii sij sjj
        a = ch*ch - sh*sh
        b = 2 * sh*ch
        aa = a * a
        ab = a * b
        bb = b * b
    -- update the matrix
    writeDataFrameOff sPtr ii $
      aa * sii + 2 * ab * sij + bb * sjj
    writeDataFrameOff sPtr ij $
      ab * (sjj - sii) + (aa - bb) * sij
    writeDataFrameOff sPtr jj $
      bb * sii - 2 * ab * sij + aa * sjj
    writeDataFrameOff sPtr ik $ a * sik + b * sjk
    writeDataFrameOff sPtr jk $ a * sjk - b * sik

    -- write the quaternion
    qPtr <- unsafeThawDataFrame 0
    writeDataFrameOff qPtr k (negate sh)
    writeDataFrameOff qPtr 3 ch
    fromVec4 <$> unsafeFreezeDataFrame qPtr
  where
    ii = i*3 + i
    ij = if i < j then i*3 + j else j*3 + i
    jj = j*3 + j
    ik = if i < k then i*3 + k else k*3 + i
    jk = if j < k then j*3 + k else k*3 + j


-- | Total number of the Givens rotations during the Jacobi eigendecomposition
--   part of the 3x3 SVD equals eigenItersX3*3.
--   Value `eigenItersX3 = 6` corresponds to 18 iterations and gives a good precision.
eigenItersX3 :: Int
eigenItersX3 = 12

-- | Run a few iterations of the Jacobi algorithm on a real-valued 3x3 symmetric matrix.
--   The eigenvectors basis of such matrix is orthogonal, and can be represented as
--   a quaternion.
jacobiEigenQ :: forall t
              . (Quaternion t, RealFloatExtras t)
             => Matrix t 3 3 -> Quater t
jacobiEigenQ m = runST $ do
    mPtr <- thawDataFrame m
    q  <- go eigenItersX3 mPtr 1
    s1 <- readDataFrameOff mPtr 0
    s2 <- readDataFrameOff mPtr 4
    s3 <- readDataFrameOff mPtr 8
    return $ sortQ s1 s2 s3 * q
  where
    go :: Int -> STDataFrame s t '[3,3] -> Quater t -> ST s (Quater t)
    go 0 _ q = pure q

    -- -- primitive cyclic iteration;
    -- --   fast, but the convergence is not perfect
    -- --
    -- -- set eigenItersX3 = 6 for good precision
    -- go n p q = do
    --   q1 <- jacobiEigen3Iteration 0 1 2 p
    --   q2 <- jacobiEigen3Iteration 1 2 0 p
    --   q3 <- jacobiEigen3Iteration 2 0 1 p
    --   go (n - 1) p (q3 * q2 * q1 * q)

    -- Pick the largest element on lower triangle;
    --   slow because of branching, but has a better convergence
    --
    --  set eigenItersX3 = 12 for good precision
    --    (slightly faster than the cyclic version with -O0)
    go n p q = do
      a10 <- abs <$> readDataFrameOff p 1
      a20 <- abs <$> readDataFrameOff p 2
      a21 <- abs <$> readDataFrameOff p 5
      q' <- jiter n p a10 a20 a21
      go (n - 1) p (q' * q)
    jiter :: Int -> STDataFrame s t '[3,3]
          -> Scalar t -> Scalar t -> Scalar t -> ST s (Quater t)
    jiter n p a10 a20 a21
      | gt2 a10 a20 a21
        = jacobiEigen3Iteration 0 1 2 p
      | gt2 a20 a10 a21
        = jacobiEigen3Iteration 2 0 1 p
      | gt2 a21 a10 a20
        = jacobiEigen3Iteration 1 2 0 p
      | otherwise
        = case mod n 3 of
            0 -> jacobiEigen3Iteration 0 1 2 p
            1 -> jacobiEigen3Iteration 2 0 1 p
            _ -> jacobiEigen3Iteration 1 2 0 p
    gt2 :: Scalar t -> Scalar t -> Scalar t -> Bool
    gt2 a b c = case compare a b of
                  GT -> a >= c
                  EQ -> a >  c
                  LT -> False

    -- Make such a quaternion that rotates the matrix so that:
    -- abs s1 >= abs s2 >= abs s3
    -- Note, the corresponding singular values may be negative, which must be
    -- taken into account later.
    sortQ :: Scalar t -> Scalar t -> Scalar t -> Quater t
    sortQ s1 s2 s3 = sortQ' (s1 >= s2) (s1 >= s3) (s2 >= s3)
    sortQ' :: Bool -> Bool -> Bool -> Quater t
    sortQ' True  True  True  = Quater 0 0 0 1                    -- s1 >= s2 >= s3
    sortQ' True  True  False = Quater M_SQRT1_2 0 0 (-M_SQRT1_2) -- s1 >= s3 >  s2
    sortQ' True  False _     = Quater 0.5 0.5 0.5 0.5            -- s3 >  s1 >= s2
    sortQ' False True  True  = Quater 0 0 M_SQRT1_2 (-M_SQRT1_2) -- s2 >  s1 >= s3
    sortQ' False _     False = Quater 0 M_SQRT1_2 0 (-M_SQRT1_2) -- s3 >  s2 >  s1
    sortQ' False False True  = Quater 0.5 0.5 0.5 (-0.5)         -- s2 >= s3 >  s1


-- | One Givens rotation for a QR algorithm on a 3x3 matrix
--
--   The three words arguments are indices:
--     0 <= i /= j /= k <= 2
--
--     if i < j then the eigen values are already sorted!
qrDecomp3Iteration :: (Quaternion t, RealFloatExtras t)
                   => Int -> Int -> Int
                   -> STDataFrame s t '[3,3]
                   -> ST s (Quater t)
qrDecomp3Iteration i j k sPtr = do
    sii <- readDataFrameOff sPtr ii
    sij <- readDataFrameOff sPtr ij
    sji <- readDataFrameOff sPtr ji
    sjj <- readDataFrameOff sPtr jj
    sik <- readDataFrameOff sPtr ik
    sjk <- readDataFrameOff sPtr jk
    -- Coefficients for a quaternion corresponding to a Givens rotation
    let (ch, sh) = qrGivensQ sii sji
        a = ch*ch - sh*sh
        b = 2 * sh*ch
    -- update the matrix
    writeDataFrameOff sPtr ii $ a * sii + b * sji
    writeDataFrameOff sPtr ij $ a * sij + b * sjj
    writeDataFrameOff sPtr ik $ a * sik + b * sjk
    writeDataFrameOff sPtr ji 0 --  a * sji - b * sii
    writeDataFrameOff sPtr jj $ a * sjj - b * sij
    writeDataFrameOff sPtr jk $ a * sjk - b * sik

    -- write the quaternion
    qPtr <- unsafeThawDataFrame 0
    writeDataFrameOff qPtr k (negateUnless leftTriple sh)
    writeDataFrameOff qPtr 3 ch
    fromVec4 <$> unsafeFreezeDataFrame qPtr
  where
    leftTriple = (j - i) /= 1 && (k - j) /= 1
    i3 = i*3
    j3 = j*3
    ii = i3 + i
    ij = i3 + j
    ik = i3 + k
    ji = j3 + i
    jj = j3 + j
    jk = j3 + k

-- | Run QR decomposition in context of 3x3 svd: AV = US = QR
--   The input here is matrix AV
--   The R upper-triangular matrix here is in fact a diagonal matrix Sigma;
--   The Q orthogonal matrix is matrix U in the svd decomposition,
--     represented here as a quaternion.
qrDecomposition3 :: (Quaternion t, RealFloatExtras t)
                 => Matrix t 3 3 -> (Vector t 3, Quater t)
qrDecomposition3 m = runST $ do
    mPtr <- thawDataFrame m
    q1 <- qrDecomp3Iteration 0 1 2 mPtr
    q2 <- qrDecomp3Iteration 0 2 1 mPtr
    q3 <- qrDecomp3Iteration 1 2 0 mPtr
    sig0 <- readDataFrameOff mPtr 0
    sig1 <- readDataFrameOff mPtr 4
    sig2 <- readDataFrameOff mPtr 8
    return (DF3 sig0 sig1 sig2, q3 * q2 * q1)


instance RealFloatExtras t => MatrixSVD t 1 1 where
    svd = svd1

instance RealFloatExtras t => MatrixSVD t 2 2 where
    svd = svd2

instance (RealFloatExtras t, Quaternion t) => MatrixSVD t 3 3 where
    svd = svd3

instance {-# INCOHERENT #-}
         ( RealFloatExtras t, KnownDim n, KnownDim m)
         => MatrixSVD t n m where
    svd a = runST $ do
      D <- pure dnm
      Dict <- pure $ minIsSmaller dn dm -- GHC is not convinced :(
      alphas <- unsafeThawDataFrame bdAlpha
      betas <- unsafeThawDataFrame bdBeta
      uPtr <- unsafeThawDataFrame bdU
      vPtr <- unsafeThawDataFrame bdV

      -- remove last beta if m > n
      bLast <- readDataFrameOff betas nm1
      when (abs bLast > M_EPS) $
        svdGolubKahanZeroCol alphas betas vPtr nm1

      -- main routine for a bidiagonal matrix
      svdBidiagonalInplace alphas betas uPtr vPtr nm (3*nm) -- number of tries

      -- sort singular values
      sUnsorted <- unsafeFreezeDataFrame alphas
      let sSorted :: Vector (Tuple '[t, Word]) (Min n m)
          sSorted = sortBy (\(S (x :! _)) (S (y :! _)) -> compare y x)
                  $ iwmap @_ @_ @'[] (\(Idx i :* U) (S x) -> S (abs x :! i :! U) ) sUnsorted
          svdS = ewmap @t @_ @'[] (\(S (x :! _)) -> S x) sSorted
          perm = ewmap @Word @_ @'[] (\(S (_ :! i :! U)) -> S i) sSorted
          pCount =
             if nm < 2
             then 0 :: Word
             else foldl (\s (i, j) -> if perm!i > perm!j then succ s else s)
                        0 [(i, j) | i <- [0..nm2w], j <- [i+1..nm2w+1]]
          pPositive = even pCount

      -- alphas and svdS are now out of sync, but that is not a problem

      -- make sure det U == 1
      when ((bdUDet < 0) == pPositive) $ do
        readDataFrameOff alphas 0 >>= writeDataFrameOff alphas 0 . negate
        forM_ [0..n - 1] $ \i ->
          readDataFrameOff uPtr (i*n) >>= writeDataFrameOff uPtr (i*n) . negate

      -- negate negative singular values
      forM_ [0..nm1] $ \i -> do
        s <- readDataFrameOff alphas i
        when (s < 0) $ do
          writeDataFrameOff alphas i $ negate s
          forM_ [0..m - 1] $ \j ->
            readDataFrameOff vPtr (j*m + i)
              >>= writeDataFrameOff vPtr (j*m + i) . negate

      -- apply permutations if necessary
      if pCount == 0
      then do
        svdU <- unsafeFreezeDataFrame uPtr
        svdV <- unsafeFreezeDataFrame vPtr
        return SVD {..}
      else do
        svdU' <- unsafeFreezeDataFrame uPtr
        svdV' <- unsafeFreezeDataFrame vPtr
        let svdU = iwgen @_ @_ @'[] $ \(i :* Idx j :* U) ->
              if j >= dimVal dnm
              then index (i :* Idx j :* U) svdU'
              else index (i :* Idx (unScalar $ perm!j) :* U) svdU'
            svdV = iwgen @_ @_ @'[] $ \(i :* Idx j :* U) ->
              if j >= dimVal dnm
              then index (i :* Idx j :* U) svdV'
              else index (i :* Idx (unScalar $ perm!j) :* U) svdV'
        return SVD {..}
      where
        n = fromIntegral $ dimVal dn :: Int
        m = fromIntegral $ dimVal dm :: Int
        dn = dim @n
        dm = dim @m
        dnm = minDim dn dm
        nm1 = nm - 1
        nm = fromIntegral (dimVal dnm) :: Int
        nm2w = fromIntegral (max (nm - 2) 0) :: Word
        -- compute the bidiagonal form b first, solve svd for b.
        BiDiag {..} = bidiagonalHouseholder a



{- Compute svd for a square bidiagonal matrix inplace
   \( B = U S V^\intercal \)

@
  B = | a1 b1 0     ... 0 |
      | 0  a2 b2 0  ... 0 |
      | 0  0 a3 b3  ... 0 |
      | ................. |
      | 0  0  ... an1 bn1 |
      | 0  0  ...  0  an  | bn? (in case if n > m)
@
 -}
svdBidiagonalInplace ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat) (nm :: Nat)
     . ( RealFloatExtras t
       , KnownDim n, KnownDim m, KnownDim nm, nm ~ Min n m)
    => STDataFrame s t '[nm] -- ^ the main diagonal of B and then the singular values.
    -> STDataFrame s t '[nm] -- ^ first upper diagonal of B
    -> STDataFrame s t '[n,n] -- ^ U
    -> STDataFrame s t '[m,m] -- ^ V
    -> Int -- ^ 0 < q <= nm -- size of a reduced matrix, such that leftover is diagonal
    -> Int -- iters
    -> ST s ()
svdBidiagonalInplace _ _ _ _ 0 _ = pure ()
svdBidiagonalInplace _ _ _ _ 1 _ = pure ()
svdBidiagonalInplace aPtr bPtr uPtr vPtr q' iter = do
    Dict <- pure $ minIsSmaller (dim @n) (dim @m)
    (p, q) <- findCounters q'
    when (iter == 0) $ error "Too many iterations."
    when (q /= 0) $ do
      findZeroDiagonal p q >>= \case
        Just k
          | k == q-1  -> svdGolubKahanZeroCol aPtr bPtr vPtr (k-1)
          | otherwise -> svdGolubKahanZeroRow aPtr bPtr uPtr k
        Nothing -> svdGolubKahanStep aPtr bPtr uPtr vPtr p q
      svdBidiagonalInplace aPtr bPtr uPtr vPtr q (iter - 1)
  where
    -- nm = fromIntegral $ dimVal' @nm :: Int

    -- Check if off-diagonal elements are close to zero and nullify them
    -- if they are small along the way.
    -- And find such indices p and q that satisfy condition in alg. 8.6.2
    -- on p. 492. of "Matrix Computations " (4-th edition).
    -- Except these are inverted:
    --   p -- is the starting index of B22 (last submatrix with non-zero superdiagonal)
    --   q -- is the starting index of B33 (diagonal submatrix)
    --
    -- that is, p and q determine the index and the size of next work piece.
    findCounters :: Int -> ST s (Int, Int)
    findCounters = goQ
      where
        checkEps :: Int -> ST s Bool
        checkEps k = do
          b <- abs <$> readDataFrameOff bPtr (k-1)
          if b == 0
          then return True
          else do
            a1 <- abs <$> readDataFrameOff aPtr (k-1)
            a2 <- abs <$> readDataFrameOff aPtr  k
            if b <= M_EPS * (max (a1 + a2) 1)
            then True <$ writeDataFrameOff bPtr (k-1) 0
            else return False
        goQ :: Int -> ST s (Int, Int)
        goQ 0 = pure (0, 0) -- guard against calling with q == 0
        goQ 1 = pure (0, 0) -- 1x1 matrix is always diagonal
        goQ k = checkEps (k-1) >>= \case
          True  -> goQ (k-1)
          False -> flip (,) k <$> goP (k-2)
        goP :: Int -> ST s Int
        goP 0 = pure 0
        goP k = checkEps k >>= \case
          True  -> return k
          False -> goP (k-1)

    -- For indices p and q (p < q), find the biggest index (< q) such that
    --  a[p] == 0
    findZeroDiagonal :: Int -> Int -> ST s (Maybe Int)
    findZeroDiagonal p q
      | k < p     = pure Nothing
      | otherwise = do
        ak <- readDataFrameOff aPtr k
        if ak == 0
        then pure $ Just k
        else if abs ak <= M_EPS
             then Just k <$ writeDataFrameOff aPtr k 0
             else findZeroDiagonal p k
      where
        k = q - 1


-- | Apply a series of column transformations to make b[k] (and whole column k+1) zero
--    (page 491, 1st paragraph) when a[k+1] == 0.
--   To make this element zero, I apply a series of Givens transforms on columns
--   (multiply on the right).
--
--   Prerequisites:
--     * a[k+1] == 0
--     * 0 <= k < min n (m-1)
--   Invariants:
--     * matrix \(B :: n \times m \) is bidiagonal, represented by two diagonals;
--     * matrix V is orthogonal
--     * \( B = A V^\intercal \), where \(A :: n \times m\) is an implicit original matrix
--   Results:
--     * Same bidiagonal matrix with b[k] == 0; i.e. (k+1)-th column is zero.
--     * matrix V is updated (multiplied on the right)
--
--   NB: All changes are made inplace.
--
svdGolubKahanZeroCol ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat)
     . (RealFloatExtras t, KnownDim n, KnownDim m, n <= m)
    => STDataFrame s t '[n] -- ^ the main diagonal of \(B\)
    -> STDataFrame s t '[n] -- ^ first upper diagonal of \(B\)
    -> STDataFrame s t '[m,m] -- ^ \(V\)
    -> Int -- ^ 0 <= k < min (n+1) m
    -> ST s ()
svdGolubKahanZeroCol aPtr bPtr vPtr k
  | k < 0 || k >= lim = error $ unwords
      [ "svdGolubKahanZeroCol: k =", show k
      , "is outside of a valid range 0 <= k <", show lim]
    -- this trick is to convince GHC that constraint (n <= m) is not redundant
  | Dict <- Dict @(n <= m) = do
    b <- readDataFrameOff bPtr k
    writeDataFrameOff bPtr k 0
    foldM_ goGivens b [k, k-1 .. 0]
  where
    n = fromIntegral $ dimVal' @n :: Int
    m = fromIntegral $ dimVal' @m :: Int
    lim = min n (m-1)
    goGivens :: Scalar t -> Int -> ST s (Scalar t)
    goGivens 0 _ = return 0 -- non-diagonal element is nullified prematurely
    goGivens b i = do
      ai <- readDataFrameOff aPtr i
      let rab = recip $ hypot b ai
          c = ai*rab
          s = b *rab
      updateGivensMat vPtr i (k+1) c s
      writeDataFrameOff aPtr i $ ai*c + b*s -- B[i,i]
      if i == 0
      then return 0
      else do
        bi1 <- readDataFrameOff bPtr (i - 1)  -- B[i,i-1]
        writeDataFrameOff bPtr (i - 1) $ bi1 * c
        return $ negate (bi1 * s)

-- | Apply a series of row transformations to make b[k] (and whole column k) zero
--    (page 490, last paragraph) when a[k] == 0.
--   To make this element zero, I apply a series of Givens transforms on rows
--   (multiply on the left).
--
--   Prerequisites:
--     * a[k] == 0
--     * 0 <= k < n - 1
--   Invariants:
--     * matrix \(B :: m \times n \) is bidiagonal, represented by two diagonals;
--     * matrix U is orthogonal
--     * \( B = U A \), where \(A :: m \times n \) is an implicit original matrix
--   Results:
--     * Same bidiagonal matrix with b[k] == 0; i.e. k-th column is zero.
--     * matrix U is updated (multiplied on the right)
--
--   NB: All changes are made inplace.
--
svdGolubKahanZeroRow ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat)
     . (RealFloatExtras t, KnownDim n, KnownDim m, n <= m)
    => STDataFrame s t '[n] -- ^ the main diagonal of B
    -> STDataFrame s t '[n] -- ^ first upper diagonal of B
    -> STDataFrame s t '[m,m] -- ^ U
    -> Int -- ^ 0 <= k < n - 1
    -> ST s ()
svdGolubKahanZeroRow aPtr bPtr uPtr k
  | k < 0 || k >= n1 = error $ unwords
      [ "svdGolubKahanZeroRow: k =", show k
      , "is outside of a valid range 0 <= k <", show n1]
    -- this trick is to convince GHC that constraint (n <= m) is not redundant
  | Dict <- Dict @(n <= m) = do
    b <- readDataFrameOff bPtr k
    writeDataFrameOff bPtr k 0
    foldM_ goGivens b [k+1..n1]
  where
    n = fromIntegral $ dimVal' @n :: Int
    n1 = n - 1
    goGivens :: Scalar t -> Int -> ST s (Scalar t)
    goGivens 0 _ = return 0 -- non-diagonal element is nullified prematurely
    goGivens b j = do
      aj <- readDataFrameOff aPtr j
      bj <- readDataFrameOff bPtr j
      let rab = recip $ hypot b aj
          c = aj*rab
          s =  b*rab
      updateGivensMat uPtr k j c (negate s)
      writeDataFrameOff aPtr j $ b*s + aj*c
      writeDataFrameOff bPtr j $ bj*c
      return $ negate (bj * s)

-- | A Golub-Kahan bidiagonal SVD step on an unreduced matrix
svdGolubKahanStep ::
       forall (s :: Type) (t :: Type) (n :: Nat) (m :: Nat) (nm :: Nat)
     . ( RealFloatExtras t
       , KnownDim n, KnownDim m, KnownDim nm, nm ~ Min n m)
    => STDataFrame s t '[nm] -- ^ the main diagonal of B and then the singular values.
    -> STDataFrame s t '[nm] -- ^ first upper diagonal of B
    -> STDataFrame s t '[n,n] -- ^ U
    -> STDataFrame s t '[m,m] -- ^ V
    -> Int -- ^ p : 0 <= p < q <= nm; p <= q - 2
    -> Int -- ^ q : 0 <= p < q <= nm; p <= q - 2
    -> ST s ()
svdGolubKahanStep aPtr bPtr uPtr vPtr p q
  | p > q - 2 || p < 0 || q > nm
    = error $ unwords
        [ "svdGolubKahanStep: p =", show p, "and q =", show q
        , "do not satisfy p <= q - 2 or 0 <= p < q <=", show nm]
  | Dict <- Dict @(nm ~ Min n m) = do
    (y,z) <- getWilkinsonShiftYZ
    goGivens2 y z p
  where
    nm = fromIntegral $ dimVal' @nm :: Int

    -- get initial values for one recursion sweep.
    -- Note, input must satisfy: q >= p+2
    getWilkinsonShiftYZ :: ST s (Scalar t, Scalar t)
    getWilkinsonShiftYZ  = do
      a1 <- readDataFrameOff aPtr p
      b1 <- readDataFrameOff bPtr p
      am <- readDataFrameOff aPtr (q-2)
      an <- readDataFrameOff aPtr (q-1)
      bm <- if q >= p + 3
            then readDataFrameOff bPtr (q-3)
            else pure 0
      bn <- readDataFrameOff bPtr (q-2)
      let t11 = a1*a1
          t12 = a1*b1
          tmm = am*am + bm*bm
          tnn = an*an + bn*bn
          tnm = am*bn
          d   = 0.5*(tmm - tnn)
          mu  = tnn + d - negateUnless (d >= 0) (hypot d tnm)
      return (t11 - mu, t12)

    -- yv = b[k-1]; zv = B[k-1,k+1] -- to be eliminated by 1st Givens r
    -- yu = a[k];   zu = B[k+1,k-1] -- to be eliminated by 2nd Givens r
    goGivens2 :: Scalar t -> Scalar t -> Int -> ST s ()
    goGivens2 yv zv k = do
          a1 <- readDataFrameOff aPtr k     -- B[k,k]
          a2 <- readDataFrameOff aPtr (k+1) -- B[k+1,k+1]
          b1 <- readDataFrameOff bPtr k     -- B[k,k+1]
          let a1' = a1*cv + b1*sv  -- B[k,k] == yu
              a2' = a2*cv          -- B[k+1,k+1]
              b0' = yv*cv + zv*sv  -- B[k-1,k]
              b1' = b1*cv - a1*sv  -- B[k,k+1]
              yu  = a1'            -- B[k,k]
              zu  = a2*sv          -- B[k+1,k]
              ryzu = recip $ hypot yu zu
              cu = yu * ryzu
              su = zu * ryzu
              a1'' = yu *cu + zu *su
              a2'' = a2'*cu - b1'*su
              b1'' = b1'*cu + a2'*su
          updateGivensMat vPtr k (k+1) cv sv
          updateGivensMat uPtr k (k+1) cu su

          when (k > p) $ writeDataFrameOff bPtr (k-1) b0'
          writeDataFrameOff bPtr k b1''
          writeDataFrameOff aPtr k a1''
          writeDataFrameOff aPtr (k+1) a2''
          when (k < q - 2) $ do
            b2 <- readDataFrameOff bPtr (k+1) -- B[k+1,k+2]
            let b2'' = b2*cu
                zvn  = b2*su
            writeDataFrameOff bPtr (k+1) b2''
            goGivens2 b1'' zvn (k+1)
        where
          ryzv = recip $ hypot yv zv
          cv = yv * ryzv
          sv = zv * ryzv

-- | Update a transformation matrix with a Givens transform (on the right)
updateGivensMat ::
       forall (s :: Type) (t :: Type) (n :: Nat)
     . (PrimBytes t, Num t, KnownDim n)
    => STDataFrame s t '[n,n]
    -> Int -> Int
    -> Scalar t -> Scalar t -> ST s ()
updateGivensMat p i j c s = forM_ [0..n-1] $ \k -> do
    let nk = n*k
        ioff = nk + i
        joff = nk + j
    uki <- readDataFrameOff p ioff
    ukj <- readDataFrameOff p joff
    writeDataFrameOff p ioff $ uki*c + ukj*s
    writeDataFrameOff p joff $ ukj*c - uki*s
  where
    n = fromIntegral $ dimVal' @n :: Int


minIsSmaller :: forall (n :: Nat) (m :: Nat)
              . Dim n -> Dim m -> Dict (Min n m <= n, Min n m <= m)
minIsSmaller dn dm
  | Just Dict <- lessOrEqDim dnm dn
  , Just Dict <- lessOrEqDim dnm dm
    = Dict
  | otherwise
    = error "minIsSmaller: impossible type-level comparison"
  where
    dnm = minDim dn dm
