{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedSums               #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}
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
  ( MatrixCalculus (..)
  , SquareMatrixCalculus (..)
  , MatrixInverse (..)
  , HomTransform4 (..)
  , Matrix
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  , Mat22d, Mat23d, Mat24d
  , Mat32d, Mat33d, Mat34d
  , Mat42d, Mat43d, Mat44d
  , mat22, mat33, mat44
  , (%*)
  ) where


import           GHC.Base
import           Numeric.DataFrame.Contraction                     ((%*))
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.DataFrame.Shape
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Matrix.Class
import           Numeric.PrimBytes
import           Numeric.Vector
import           Numeric.Scalar

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
         ) => MatrixCalculus t (n :: Nat) (m :: Nat) where
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

instance MatrixCalculus t (xn :: XNat) (xm :: XNat) where
    transpose (XFrame (df :: DataFrame t ns))
      | ((D :: Dim n) :* (D :: Dim m) :* U) <- dims @Nat @ns
      , E <- inferPrimElem @t @n @'[m]
      = XFrame (transpose df :: Matrix t m n)
    transpose _ = error "MatrixCalculus/transpose: impossible argument"


instance (KnownDim n, PrimArray t (Matrix t n n), Num t)
      => SquareMatrixCalculus t n where
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
    det _ = error "Not implemented yet, sorry" -- TODO: implement!
    trace df
      | I# n <- fromIntegral $ dimVal' @n
      , n1 <- n +# 1#
      = let f 0# = ix# 0# df
            f k  = ix# k  df + f (k -# n1)
        in scalar $ f (n *# n -# 1#)
