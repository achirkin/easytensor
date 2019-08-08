module Numeric.Matrix
  ( MatrixTranspose (..)
  , SquareMatrix (..)
  , MatrixDeterminant (..)
  , MatrixInverse (..)
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
  ) where

import Numeric.DataFrame.Internal.Backend ()
import Numeric.Matrix.Internal
import Numeric.Matrix.Internal.Double     ()
import Numeric.Matrix.Internal.Float      ()
