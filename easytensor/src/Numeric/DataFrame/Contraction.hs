{-# LANGUAGE CPP                    #-}
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

#include "MachDeps.h"


import           GHC.Base
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Type
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
    contract x y
        | I# m <- fromIntegral $ dimVal' @m
        , I# n <- fromIntegral $ totalDim' @as
        , I# k <- fromIntegral $ totalDim' @bs
        , nk <- n *# k
        = let loop i j l r | isTrue# (l ==# m) = r
                           | otherwise = loop i j (l +# 1#)
                              (r + ix# (i +# n *# l) x * ix# (l +# m *# j) y)

              loop2 (T# i j) | isTrue# (j ==# k) = (# T# i j, 0 #)
                             | isTrue# (i ==# n) = loop2 (T# 0# (j +# 1#))
                             | otherwise = (# T# (i +# 1#) j, loop i j 0# 0 #)
          in case gen# nk loop2 (T# 0# 0#) of
              (# _, r #) -> r

data T# = T# Int# Int#
