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
module Numeric.Matrix.Internal
  ( MatrixTranspose (..)
  , SquareMatrix (..)
  , MatrixDeterminant (..)
  , MatrixInverse (..)
  , MatrixLU (..), LUFact (..)
  , Matrix
  , HomTransform4 (..)
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
import Numeric.PrimBytes
import Numeric.Scalar.Internal
import Numeric.Vector.Internal

-- | Alias for DataFrames of rank 2
type Matrix (t :: l) (n :: k) (m :: k) = DataFrame t '[n,m]

class MatrixTranspose t (n :: k) (m :: k) where
    -- | Transpose Mat
    transpose :: Matrix t n m -> Matrix t m n

class SquareMatrix t (n :: Nat) where
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: Matrix t n n
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: Scalar t -> Matrix t n n
    -- | Sum of diagonal elements
    trace :: Matrix t n n -> Scalar t

class MatrixDeterminant t (n :: Nat) where
    -- | Determinant of  Mat
    det :: Matrix t n n -> Scalar t

class MatrixInverse t (n :: Nat) where
    -- | Matrix inverse
    inverse :: Matrix t n n -> Matrix t n n


-- | Result of LU factorization with Partial Pivoting
--   @ PA = LU @.
data LUFact t n
  = LUFact
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

deriving instance (Show t, PrimBytes t, KnownDim n) => Show (LUFact t n)
deriving instance (Eq (Matrix t n n), Eq t) => Eq (LUFact t n)

class MatrixLU t (n :: Nat) where
    -- | Compute LU factorization with Partial Pivoting
    lu :: Matrix t n n -> LUFact t n


-- | Operations on 4x4 transformation matrices and vectors in homogeneous coordinates.
--   All angles are specified in radians.
--
--   Note: since version 2 of @easytensor@, DataFrames and matrices are row-major.
--         A good SIMD implementation may drastically improve performance
--         of 4D vector-matrix products of the form @v %* m@, but not so much
--         for products of the form @m %* v@ (due to memory layout).
--         Thus, all operations here assume the former form to benefit more from
--         SIMD in future.
class HomTransform4 t where
    -- | Create a translation matrix from a vector.  The 4th coordinate is ignored.
    --
    --   If @p ! 3 == 1@ and @v ! 3 == 0@, then
    --
    --   > p %* translate4 v == p + v
    --
    translate4  :: Vector t 4 -> Matrix t 4 4
    -- | Create a translation matrix from a vector.
    --
    --   If @p ! 3 == 1@, then
    --
    --   > p %* translate3 v == p + toHomVector v
    --
    translate3  :: Vector t 3 -> Matrix t 4 4
    -- | Rotation matrix for a rotation around the X axis, angle is given in radians.
    --   e.g. @p %* rotateX (pi/2)@ rotates point @p@ around @Ox@ by 90 degrees.
    rotateX     :: t -> Matrix t 4 4
    -- | Rotation matrix for a rotation around the Y axis, angle is given in radians.
    --   e.g. @p %* rotateY (pi/2)@ rotates point @p@ around @Oy@ by 90 degrees.
    rotateY     :: t -> Matrix t 4 4
    -- | Rotation matrix for a rotation around the Z axis, angle is given in radians.
    --   e.g. @p %* rotateZ (pi/2)@ rotates point @p@ around @Oz@ by 90 degrees.
    rotateZ     :: t -> Matrix t 4 4
    -- | Rotation matrix for a rotation around an arbitrary normalized vector
    --   e.g. @p %* rotate (pi/2) v@ rotates point @p@ around @v@ by 90 degrees.
    rotate      :: Vector t 3 -> t -> Matrix t 4 4
    -- | Rotation matrix from the Euler angles roll (axis @Z@), yaw (axis @Y'@), and pitch (axis @X''@).
    --   This order is known as Tait-Bryan angles (@Z-Y'-X''@ intrinsic rotations), or nautical angles, or Cardan angles.
    --
    --   > rotateEuler pitch yaw roll == rotateZ roll %* rotateY yaw %* rotateX pitch
    --
    --   https://en.wikipedia.org/wiki/Euler_angles#Conventions_2
    rotateEuler :: t -- ^ pitch (axis @X''@)
                -> t -- ^ yaw (axis @Y'@)
                -> t -- ^ roll (axis @Z@)
                -> Matrix t 4 4
    -- | Create a transform matrix using up direction, camera position and a point to look at.
    --   Just the same as GluLookAt.
    lookAt      :: Vector t 3 -- ^ The up direction, not necessary unit length or perpendicular to the view vector
                -> Vector t 3 -- ^ The viewers position
                -> Vector t 3 -- ^ The point to look at
                -> Matrix t 4 4
    -- | A perspective symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
    --   http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
    perspective :: t -- ^ Near plane clipping distance (always positive)
                -> t -- ^ Far plane clipping distance (always positive)
                -> t -- ^ Field of view of the y axis, in radians
                -> t -- ^ Aspect ratio, i.e. screen's width\/height
                -> Matrix t 4 4
    -- | An orthogonal symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
    --   http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
    orthogonal  :: t -- ^ Near plane clipping distance
                -> t -- ^ Far plane clipping distance
                -> t -- ^ width
                -> t -- ^ height
                -> Matrix t 4 4
    -- | Add one more dimension and set it to 1.
    toHomPoint  :: Vector t 3 -> Vector t 4
    -- | Add one more dimension and set it to 0.
    toHomVector :: Vector t 3 -> Vector t 4
    -- | Transform a homogenous vector or point into a normal 3D vector.
    --   If the last coordinate is not zero, divide the rest by it.
    fromHom     :: Vector t 4 -> Vector t 3

-- Type abbreviations

type Mat22f = Matrix Float 2 2
type Mat32f = Matrix Float 3 2
type Mat42f = Matrix Float 4 2
type Mat23f = Matrix Float 2 3
type Mat33f = Matrix Float 3 3
type Mat43f = Matrix Float 4 3
type Mat24f = Matrix Float 2 4
type Mat34f = Matrix Float 3 4
type Mat44f = Matrix Float 4 4

type Mat22d = Matrix Double 2 2
type Mat32d = Matrix Double 3 2
type Mat42d = Matrix Double 4 2
type Mat23d = Matrix Double 2 3
type Mat33d = Matrix Double 3 3
type Mat43d = Matrix Double 4 3
type Mat24d = Matrix Double 2 4
type Mat34d = Matrix Double 3 4
type Mat44d = Matrix Double 4 4


-- | Compose a 2x2D matrix
mat22 :: PrimBytes (t :: Type)
      => Vector t 2 -> Vector t 2 -> Matrix t 2 2
mat22 = packDF @_ @2 @'[2]
{-# INLINE mat22 #-}

-- | Compose a 3x3D matrix
mat33 :: PrimBytes (t :: Type)
      => Vector t 3 -> Vector t 3 -> Vector t 3 -> Matrix t 3 3
mat33 = packDF @_ @3 @'[3]
{-# INLINE mat33 #-}

-- | Compose a 4x4D matrix
mat44 :: PrimBytes (t :: Type)
      => Vector t 4
      -> Vector t 4
      -> Vector t 4
      -> Vector t 4
      -> Matrix t 4 4
mat44 = packDF @_ @4 @'[4]
{-# INLINE mat44 #-}



instance ( KnownDim n, KnownDim m
         , PrimArray t (Matrix t n m)
         , PrimArray t (Matrix t m n)
         ) => MatrixTranspose t (n :: Nat) (m :: Nat) where
    transpose df = case uniqueOrCumulDims df of
      Left a -> broadcast a
      Right _
         | wm <- dimVal' @m
         , wn <- dimVal' @n
         , m <- case wm of W# w -> word2Int# w
         , n <- case wn of W# w -> word2Int# w
         -> let f ( I# i,  I# j )
                  | isTrue# (i ==# n) = f ( 0, I# (j +# 1#) )
                  | otherwise         = (# ( I# (i +# 1#), I# j )
                                         , ix# (i *# m +# j) df
                                         #)
            in case gen# (CumulDims [wm*wn, wn, 1]) f (0,0) of (# _, r #) -> r

instance MatrixTranspose (t :: Type) (xn :: XNat) (xm :: XNat) where
    transpose (XFrame (df :: DataFrame t ns))
      | ((D :: Dim n) :* (D :: Dim m) :* U) <- dims @ns
      , Dict <- inferPrimElem df
      = XFrame (transpose df :: Matrix t m n)
    transpose _ = error "MatrixTranspose/transpose: impossible argument"

instance (KnownDim n, PrimArray t (Matrix t n n), Num t)
      => SquareMatrix t n where
    eye
      | n <- dimVal' @n
      = let f 0 = (# n    , 1 #)
            f k = (# k - 1, 0 #)
        in case gen# (CumulDims [n*n, n, 1]) f 0 of
            (# _, r #) -> r
    diag se
      | n <- dimVal' @n
      , e <- unScalar se
      = let f 0 = (# n    , e #)
            f k = (# k - 1, 0 #)
        in case gen# (CumulDims [n*n, n, 1]) f 0 of
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
  inverse m = runST $ do
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



instance ( KnownDim n, Ord t, Fractional t
         , PrimBytes t, PrimArray t (Matrix t n n))
         => MatrixDeterminant t n where
  det m = prodF (luUpper f) * prodF (luLower f) * luPermSign f
    where
      f = lu m
      n = case dimVal' @n of W# w -> word2Int# w
      n1 = n +# 1#
      nn1 = n *# n -# 1#
      prodF a = scalar $ prodF' nn1 a
      prodF' 0# a = ix# 0# a
      prodF' k  a = ix# k a * prodF' (k -# n1) a


instance ( KnownDim n, Ord t, Fractional t
         , PrimBytes t, PrimArray t (Matrix t n n))
         => MatrixLU t n where
    lu m' = case runRW# go of
        (# _, (# bu, bl #) #) -> LUFact
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
        => LUFact t n -> Vector t n -> Vector t n
luSolve LUFact {..} b = x
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
