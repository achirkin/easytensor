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
module Numeric.Matrix.SVD
  ( MatrixSVD (..), SVD (..)
  , svd1, svd2, svd3, svd3q
  , checkSvd3q
  ) where

import Control.Monad.ST
-- import Numeric.DataFrame.Contraction
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.SubSpace
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
import Numeric.Quaternion.Internal
-- import Numeric.PrimBytes
import Numeric.Scalar.Internal
import Numeric.Vector.Internal


-- | Precision for rounding
eps :: Fractional t => t
eps = 10e-12

sqrt05 :: Fractional t => t
sqrt05 = 0.7071067811865476

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

class MatrixSVD t (n :: Nat) (m :: Nat) where
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
svd2 :: forall t . (PrimBytes t, RealFloat t) => Matrix t 2 2 -> SVD t 2 2
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
    xx = x1*x2
    yy = y1*y2
    -- xy = x1*y2
    -- yx = y1*x2
    h1 = sqrt $ y1*y1 + x1*x1 -- h1 >= abs x1
    h2 = sqrt $ y2*y2 + x2*x2 -- h2 >= abs x2
    sigma1 = 0.5 * (h2 + h1)  -- sigma1 >= abs sigma2
    sigma2 = 0.5 * (h2 - h1)  -- can be negative, which is accounted by sg2
    sg2 = mneg (sigma2 >= 0 && h2 /= 0)
    hh = h1*h2 -- NB: d == 2 hh
    rd = 0.5 / hh
    ucos = xx + yy
    vcos = xx - yy
    uhhs = hh - ucos
    uhhc = hh + ucos
    vhhs = hh - vcos
    vhhc = hh + vcos
    (uc, us, vc, vs)
      = case (h1 == 0, h2 == 0) of
          (True , True ) -> (1, 0, 1, 0)
          (False, False) ->
             ( sqrt (uhhc * rd)
             , mneg (  (x1 * y2 >  y1 * x2)
                    || (x1 * y2 == y1 * x2 && (m10 < 0 || m11 < 0))
                                           -- (y2 > y1 || x1 > x2)
                    ) . sqrt $ uhhs * rd
             , mneg (h1 * x2 >= negate h2 * x1) . sqrt $ vhhc * rd
             , mneg (h1 * y2 >= negate h2 * y1) . sqrt $ vhhs * rd
             )
          (_    , _    ) -> let rm = recip . sqrt $ m00*m00 + m01*m01
                            in  (1, 0, m00 * rm, m01 * rm)
    mneg :: Bool -> Scalar t -> Scalar t
    mneg a b = if a then b else negate b


-- | Get SVD decomposition of a 3x3 matrix using `svd3q` function.
--
--   This function reorders the singular components under the hood to make sure
--   @s1 >= s2 >= s3 >= 0@.
--   Thus, it has some overhead on top of `svd3q`.
svd3 :: forall t . Quaternion t => Matrix t 3 3 -> SVD t 3 3
svd3 m
    | s3' > s1 = SVD
        { svdU = toMatrix33 (q1 * u)
        , svdS = DF3 s3' s1 s2
        , svdV = mapIf (s3 < 0) neg1 $ toMatrix33 (q1 * v)
        }
    | s3' > s2 = SVD
        { svdU = toMatrix33 (q2 * u)
        , svdS = DF3 s1 s3' s2
        , svdV = mapIf (s3 < 0) neg2 $ toMatrix33 (q2 * v)
        }
    | otherwise = SVD
        { svdU = toMatrix33 u
        , svdS = DF3 s1 s2 s3'
        , svdV = mapIf (s3 < 0) neg3 $ toMatrix33 v
        }
  where
    (u, (DF3 s1 s20 s30), v) = svd3q m
    s2 = if s1 * eps > s20 then 0 else s20
    s3 = if s1 * eps > abs s30 then 0 else s30
    s3' = abs s3
    -- put s3 in front of s1 and s2 if it is big negative
    q1 = Quater 0.5 0.5 0.5 0.5 :: Quater t
    -- put s3 in front of s2 if it is big negative
    q2 = Quater sqrt05 0 0 sqrt05 :: Quater t
    mapIf :: Bool -> (Vector t 3 -> Vector t 3) -> Matrix t 3 3 -> Matrix t 3 3
    mapIf False _ = id
    mapIf True  f = ewmap @t @'[3] f
    neg1, neg2, neg3 :: Vector t 3 -> Vector t 3
    neg1 (DF3 a b c) = DF3 (negate a) b c
    neg2 (DF3 a b c) = DF3 a (negate b) c
    neg3 (DF3 a b c) = DF3 a b (negate c)


-- | Get SVD decomposition of a 3x3 matrix, which orthogonal matrices U and V
--   represented as quaternions.
--   Important: U and V are bound to be rotations at the expense of the last
--              singular value being possible negative.
--
--   This is an adoptation of a specialized 3x3 SVD algorithm described in
--     "Computing the Singular Value Decomposition of 3x3 matrices
--      with minimal branching and elementary floating point operations",
--   by  A. McAdams, A. Selle, R. Tamstorf, J. Teran, E. Sifakis.
--
--   http://pages.cs.wisc.edu/~sifakis/papers/SVD_TR1690.pdf
svd3q :: Quaternion t => Matrix t 3 3 -> (Quater t, Vector t 3, Quater t)
svd3q m = (u, s, v)
  where
    v = jacobiEigenQ (transpose m %* m)
    (s, u) = qrDecomposition3 (m %* toMatrix33 v)



-- | Approximate values for cos (a/2) and sin (a/2) of a Givens rotation for
--    a 2x2 symmetric matrix.
--
--   To make sure the Givens rotations converge, the angle a is limited by pi/4
--   in the base version of an algorithm.
--
--   This is a modified version, that chooses such rotation that always makes
--   the first coefficient bigger.
--   It does so by adding extra rotation pi/4, which just swaps coefficients
--   on the diagonal.
--   It does not affect the aij component, so it should not change the convergence rate.
jacobiGivensQ :: forall t . (Ord t, Floating t) => t -> t -> t -> (t, t)
jacobiGivensQ aii aij ajj
    | g*sh'*sh' < ch'*ch' = ( w * ch, w * sh)
    | otherwise           = ( c', s')
  where
    ch' = 2 * (aii-ajj)
    sh' = aij
    ch = if ch' >= 0 then ch' else negate (ch' - sh')
    sh = if ch' >= 0 then sh' else negate (ch' + sh')
    w = recip . sqrt $ ch * ch + sh * sh -- TODO: consider something like a hypot
    g  = 5.82842712474619 :: t  -- 3 + sqrt 8
    c' = if sh' == 0
         then if ch' >= 0 then 1 else sqrt05
         else if ch' >= 0 then 0.9238795325112867 else 0.3826834323650898 -- sin (pi/8)
    s' = if sh' == 0
         then if ch' >= 0 then 0 else sqrt05
         else if ch' >= 0 then 0.3826834323650898 else 0.9238795325112867 -- cos (pi/8)

-- | A quaternion for a QR Givens iteration
qrGivensQ :: forall t . (Ord t, Floating t) => t -> t -> (t, t)
qrGivensQ a1 a2
    | a1 < 0    = (sh * w, ch * w)
    | otherwise = (ch * w, sh * w)
  where
    rho2 = a1*a1 + a2*a2
    sh = if rho2 > eps then a2 else 0
    ch = abs a1 + sqrt (max rho2 eps)
    w = recip . sqrt $ ch * ch + sh * sh -- TODO: consider something like a hypot


-- | One iteration of the Jacobi algorithm on a symmetric 3x3 matrix
--
--   The three words arguments are indices:
--     0 <= i /= j /= k <= 2
--
--     if i < j then the eigen values are already sorted!
jacobiEigen3Iteration :: Quaternion t
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
        sh' = if rightTriple then negate sh else sh
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
    writeDataFrameOff qPtr k sh'
    writeDataFrameOff qPtr 3 ch
    fromVec4 <$> unsafeFreezeDataFrame qPtr
  where
    rightTriple = (j - i) == 1 || (k - j) == 1
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
jacobiEigenQ :: forall t . Quaternion t => Matrix t 3 3 -> Quater t
jacobiEigenQ m = runST $ do
    mPtr <- thawDataFrame m
    go eigenItersX3 mPtr 1
  where
    go :: Int -> STDataFrame s t '[3,3] -> Quater t -> ST s (Quater t)
    go 0 _ q = pure q

    -- -- primitive cyclic iteration;
    -- --   fast, but the convergence is not perfect
    --
    --  set eigenItersX3 = 6 for good precision
    -- go n p q = do
    --   q1 <- jacobiEigen3Iteration 0 1 2 p
    --   q2 <- jacobiEigen3Iteration 1 2 0 p
    --   q3 <- jacobiEigen3Iteration 0 2 1 p
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
        = jacobiEigen3Iteration 0 2 1 p
      | gt2 a21 a10 a20
        = jacobiEigen3Iteration 1 2 0 p
      | otherwise
        = case mod n 3 of
            0 -> jacobiEigen3Iteration 0 1 2 p
            1 -> jacobiEigen3Iteration 0 2 1 p
            _ -> jacobiEigen3Iteration 1 2 0 p
    gt2 :: Scalar t -> Scalar t -> Scalar t -> Bool
    gt2 a b c = case compare a b of
                  GT -> a >= c
                  EQ -> a >  c
                  LT -> False



-- | One Givens rotation for a QR algorithm on a 3x3 matrix
--
--   The three words arguments are indices:
--     0 <= i /= j /= k <= 2
--
--     if i < j then the eigen values are already sorted!
qrDecomp3Iteration :: Quaternion t
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
        sh' = if rightTriple then negate sh else sh
        a = ch*ch - sh*sh
        b = 2 * sh*ch
    -- update the matrix
    writeDataFrameOff sPtr ii $ a * sii + b * sji
    writeDataFrameOff sPtr ij $ a * sij + b * sjj
    writeDataFrameOff sPtr ik $ a * sik + b * sjk
    writeDataFrameOff sPtr ji 0 -- $ a * sji - b * sii
    writeDataFrameOff sPtr jj $ a * sjj - b * sij
    writeDataFrameOff sPtr jk $ a * sjk - b * sik

    -- write the quaternion
    qPtr <- unsafeThawDataFrame 0
    writeDataFrameOff qPtr k sh'
    writeDataFrameOff qPtr 3 ch
    fromVec4 <$> unsafeFreezeDataFrame qPtr
  where
    rightTriple = (j - i) == 1 || (k - j) == 1
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
qrDecomposition3 :: Quaternion t => Matrix t 3 3 -> (Vector t 3, Quater t)
qrDecomposition3 m = runST $ do
    mPtr <- thawDataFrame m
    q1 <- qrDecomp3Iteration 0 1 2 mPtr
    q2 <- qrDecomp3Iteration 0 2 1 mPtr
    q3 <- qrDecomp3Iteration 1 2 0 mPtr
    sig0 <- readDataFrameOff mPtr 0
    sig1 <- readDataFrameOff mPtr 4
    sig2 <- readDataFrameOff mPtr 8
    return (DF3 sig0 sig1 sig2, q3 * q2 * q1)




pprM :: (PrimBytes t, Show t) => Matrix t 3 3 -> String
pprM (DF3 a b c)
    =  unlines [pprV a, pprV b, pprV c]

pprV :: (PrimBytes t, Show t) => Vector t 3 -> String
pprV (DF3 (S a) (S b) (S c)) = show a ++ "\t" ++ show b ++ "\t" ++ show c



checkSvd3q :: Quaternion Double => Matrix Double 3 3 -> IO ()
checkSvd3q m = do
    putStrLn $ "A = \n" ++ pprM m
    putStrLn $ "u     = " ++ show u
    putStrLn $ "sigma = " ++ pprV s
    putStrLn $ "v     = " ++ show v ++ "\n"

    putStrLn $ "U %* S %* Vt = \n" ++
      pprM (toMatrix33 u %* asDiag s %* toMatrix33 (conjugate v))
  where
    (u, s, v) = svd3q m
