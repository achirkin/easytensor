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
  ) where

-- import Control.Monad.ST
-- import Numeric.DataFrame.Contraction
-- import Numeric.DataFrame.Internal.PrimArray
-- import Numeric.DataFrame.ST
-- import Numeric.DataFrame.SubSpace
-- import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.Matrix.Internal
-- import Numeric.PrimBytes
-- import Numeric.Scalar.Internal
import Numeric.Vector.Internal

-- | Result of SVD factorization
--   @ M = svdU %* asDiag svdS %* svdV @.
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

class MatrixSVD t (n :: Nat) (m :: Nat) where
    -- | Compute SVD factorization of a matrix
    svd :: Matrix t n m -> SVD t n m
