{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
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



#ifdef ghcjs_HOST_OS
import           Numeric.DataFrame.Internal.Array.Family (ElemTypeInference)
#endif

import           GHC.Types                     (Type)

import           Numeric.Commons
import           Numeric.DataFrame.Contraction ((%*))
import           Numeric.DataFrame.Shape
import           Numeric.Dimensions            (Nat, Idx (..))
import           Numeric.Matrix.Class
import           Numeric.Matrix.Mat44d         ()
import           Numeric.Matrix.Mat44f         ()
import           Numeric.Vector

import           Control.Monad.ST
import           Numeric.DataFrame.ST



-- | Compose a 2x2D matrix
mat22 :: ( PrimBytes (Vector t 2)
         , PrimBytes (Matrix t 2 2)
         )
      => Vector t 2 -> Vector t 2 -> Matrix t 2 2
mat22 = (<::>)

-- | Compose a 3x3D matrix
mat33 :: (
#ifdef ghcjs_HOST_OS
           ElemTypeInference t, MutableFrame t '[3,3]
#else
           PrimBytes t
         , PrimBytes (Vector t 3)
         , PrimBytes (Matrix t 3 3)
#endif
         )
      => Vector t 3 -> Vector t 3 -> Vector t 3 -> Matrix t 3 3
mat33 a b c = runST $ do
  mmat <- newDataFrame
  copyDataFrame a (1:!1:!Z) mmat
  copyDataFrame b (1:!2:!Z) mmat
  copyDataFrame c (1:!3:!Z) mmat
  unsafeFreezeDataFrame mmat

-- | Compose a 4x4D matrix
mat44 :: forall (t :: Type)
       . (
#ifdef ghcjs_HOST_OS
           ElemTypeInference t, MutableFrame t '[4,4]
#else
           PrimBytes t
         , PrimBytes (Vector t (4 :: Nat))
         , PrimBytes (Matrix t (4 :: Nat) (4 :: Nat))
#endif
         )
      => Vector t (4 :: Nat) -> Vector t (4 :: Nat) -> Vector t (4 :: Nat) -> Vector t (4 :: Nat)
      -> Matrix t (4 :: Nat) (4 :: Nat)
mat44 a b c d = runST $ do
  mmat <- newDataFrame
  copyDataFrame a (1:!1:!Z) mmat
  copyDataFrame b (1:!2:!Z) mmat
  copyDataFrame c (1:!3:!Z) mmat
  copyDataFrame d (1:!4:!Z) mmat
  unsafeFreezeDataFrame mmat
