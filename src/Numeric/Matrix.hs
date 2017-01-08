{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeOperators    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Matrix
  ( Matrix
  -- * Type classes
  , MatrixCalculus (..), SquareMatrixCalculus (..)
  , Matrix2x2 (..)
  -- * Type abbreviations
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  -- * Common functions on vectors
--  , (<:>)
  , MatrixProduct (..)
  , MatrixInverse (..)
  ) where

--import GHC.Base (runRW#)
--import GHC.Prim
--import GHC.TypeLits
--
--import Numeric.Commons
import           Numeric.Matrix.Class         (Matrix2x2 (..),
                                               MatrixCalculus (..),
                                               MatrixInverse (..),
                                               MatrixProduct (..),
                                               SquareMatrixCalculus (..))
import           Numeric.Matrix.Family        (Matrix)


-- Import instances
import           Numeric.Matrix.Base.FloatXNM ()


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



---- | Append one vector to another, adding up their dimensionality
--(<:>) :: ( PrimBytes (Matrix t n1 m1)
--         , PrimBytes (Matrix t m)
--         , PrimBytes (Matrix t (n+m))
--         )
--      => Matrix t n -> Matrix t m -> Matrix t (n + m)
--a <:> b = case (# toBytes a, toBytes b, byteSize a, byteSize b #) of
--  (# arr1, arr2, n, m #) -> case runRW#
--     ( \s0 -> case newByteArray# (n +# m) s0 of
--         (# s1, marr #) -> case copyByteArray# arr1 0# marr 0# n s1 of
--           s2 -> case copyByteArray# arr2 0# marr n m s2 of
--             s3 -> unsafeFreezeByteArray# marr s3
--     ) of (# _, r #) -> fromBytes r
--infixl 5 <:>
