{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE UnliftedFFITypes       #-}
{-# LANGUAGE JavaScriptFFI          #-}
{-# LANGUAGE GHCForeignImportPrim   #-}
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

import           Data.Type.Equality     ((:~:) (..))
import           GHC.Prim
import           GHC.Types              (Int (..), Type)
import           Unsafe.Coerce          (unsafeCoerce)

import           Numeric.Array.Family
import           Numeric.DataFrame.Type
import           Numeric.Dimensions



class ConcatList as bs asbs
      => Contraction (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                             | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Generalization of a matrix product: take scalar product over one dimension
    --   and, thus, concatenate other dimesnions
    contract :: ( KnownDim m )
             => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs

-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.
(%*) :: ( ConcatList as bs (as ++ bs)
        , Contraction t as bs asbs
        , KnownDim m
        )  => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t (as ++ bs)
(%*) = contract
{-# INLINE (%*) #-}
infixl 7 %*

instance {-# OVERLAPPABLE #-}
         ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , asbs ~ (a' ': sbs')
         ) => Contraction t as bs asbs where
    contract :: forall m . KnownDim m => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs
    contract dx dy
        | Refl <- unsafeCoerce Refl :: Array t asbs :~: ArrayT t asbs
        , Refl <- unsafeCoerce Refl :: Array t (as +: m) :~: ArrayT t (as +: m)
        , Evidence <- inferConcatDimensions @as @bs
        = KnownDataFrame $ js_contract @t @as @m @bs (dimVal (dim @as)) (dimVal' @m) (dimVal (dim @bs)) (coerce dx) (coerce dy)


foreign import javascript unsafe "h$easytensor_contract($1,$2,$3,$4,$5)"
    js_contract  :: forall t as m bs . Int -> Int -> Int -> ArrayT t (as +: m) -> ArrayT t (m :+ bs) -> ArrayT t (as ++ bs)

instance {-# OVERLAPPING #-}
         Contraction t '[] '[] '[] where
    contract :: forall m . KnownDim m => DataFrame t '[m] -> DataFrame t '[m] -> DataFrame t ('[] :: [Nat])
    contract dx dy
        = KnownDataFrame $ unsafeCoerce (js_contract0 (coerce dx) (coerce dy))

foreign import javascript unsafe "$1.reduce(function (r, e, i) { return e*$2[i] + r;}, 0)"
    js_contract0  :: ArrayT t '[m] -> ArrayT t '[m] -> Any


