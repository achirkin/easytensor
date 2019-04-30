{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Contraction
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This modules provides generalization of a matrix product:
--  tensor-like contraction.
-- For matrices and vectors this is a normal matrix*matrix or vector*matrix or matrix*vector product,
-- for larger dimensions it calculates the scalar product of "adjacent" dimesnions of a tensor.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Contraction
  ( Contraction (..), (%*)
  ) where

import           GHC.Base

import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.Dimensions



class ConcatList as bs asbs
      => Contraction (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                             | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Generalization of a matrix product: take scalar product over one dimension
    --   and, thus, concatenate other dimesnions
    contract :: ( KnownDim m
                , PrimArray t (DataFrame t (as +: m))
                , PrimArray t (DataFrame t (m :+ bs))
                , PrimArray t (DataFrame t asbs)
                )
             => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs

-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.
(%*) :: ( ConcatList as bs (as ++ bs)
        , Contraction t as bs asbs
        , KnownDim m
        , PrimArray t (DataFrame t (as +: m))
        , PrimArray t (DataFrame t (m :+ bs))
        , PrimArray t (DataFrame t (as ++ bs))
        )  => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t (as ++ bs)
(%*) = contract
{-# INLINE (%*) #-}
infixl 7 %*



instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Num t
         ) => Contraction t as bs asbs where
    contract :: forall m .
                ( KnownDim m
                , PrimArray t (DataFrame t (as +: m))
                , PrimArray t (DataFrame t (m :+ bs))
                , PrimArray t (DataFrame t asbs)
                )
             => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs
    contract x y = case (# uniqueOrCumulDims x, uniqueOrCumulDims y #) of
      (# Left x0, Left y0 #) -> broadcast (x0 * y0)
      (# ux, uy #)
        | dm <- dim @_ @m
        , (ixX, xs) <- getStepsAndIx (Snoc dims dm) x ux
        , (ixY, ys) <- getStepsAndIx (Cons dm dims) y uy
        , (# n, m, k, steps #) <- conSteps xs ys ->
          let loop i j l r | isTrue# (l ==# m) = r
                           | otherwise = loop i j (l +# 1#)
                              (r + ixX (i *# m +# l) * ixY (l *# k +# j))

              loop2 (T# i j) | isTrue# (i ==# n) = (# T# i j, 0 #)
                             | isTrue# (j ==# k) = loop2 (T# (i +# 1#) 0#)
                             | otherwise = (# T# i (j +# 1#), loop i j 0# 0 #)
          in case gen# steps loop2 (T# 0# 0#) of
              (# _, r #) -> r
      where
        getStepsAndIx :: forall (ns :: [Nat])
                       . PrimArray t (DataFrame t ns)
                      => Dims ns
                      -> DataFrame t ns
                      -> Either t CumulDims
                      -> (Int# -> t, CumulDims)
        getStepsAndIx _  df (Right cds) = (\i -> ix# i df, cds)
        getStepsAndIx ds _  (Left  e)   = (\_ -> e, cumulDims ds)
        conSteps (CumulDims xs) (CumulDims ys) = case conSteps' xs ys of
          (W# n, W# m, W# k, zs)
            -> (# word2Int# n, word2Int# m, word2Int# k, CumulDims zs #)
        conSteps' :: [Word] -> [Word] -> (Word, Word, Word, [Word])
        conSteps' [m, _] (_:ys@(k:_)) = (1, m, k, ys)
        conSteps' (nm:ns) cys
          | (_, m, k, ys) <- conSteps' ns cys
          , n <- nm `quot` m
            = (n, m, k, n*k : ys )
        conSteps' _ _ = error "Numeric.DataFrame.Contraction: impossible match"

data T# = T# Int# Int#
