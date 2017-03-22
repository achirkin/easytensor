{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-- {-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Contraction
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
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

import           Data.Proxy             (Proxy (..))
import           Data.Type.Equality
import           GHC.TypeLits           (KnownNat, Nat)
import           GHC.Types              (Type)
import           Numeric.Dimensions
import qualified Numeric.Matrix.Class   as M
import           Unsafe.Coerce

import           Numeric.DataFrame.Type

-- | Generalization of the matrix product
class ( ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
      , ToList as   ~ SimplifyList ('Prefix (ToList bs) (ToList asbs))
      , ToList bs   ~ SimplifyList ('Suffix (ToList as) (ToList asbs))
      ) => Contraction t (m :: Nat) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                             | as bs -> asbs, asbs as -> bs, asbs bs -> as where
  -- | Generalization of a matrix product: take scalar product over one dimension
  --   and, thus, concatenate other dimesnions
  contract :: DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs

instance ( ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
         , ToList as   ~ SimplifyList ('Prefix (ToList bs) (ToList asbs))
         , ToList bs   ~ SimplifyList ('Suffix (ToList as) (ToList asbs))
         , M.MatrixProduct (DataFrame t (as +: m))
                           (DataFrame t (m :+ bs))
                           (DataFrame t asbs)
         ) => Contraction t m as bs asbs where
  contract = M.prod
  {-# INLINE contract #-}

-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.
(%*) :: ( Contraction t m as bs asbs )
     => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs
(%*) = contract
{-# INLINE (%*) #-}
infixl 7 %*



-- contract' :: forall (t :: Type) (m :: Nat) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
--            . ( ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
--              , ToList as   ~ SimplifyList ('Prefix (ToList bs) (ToList asbs))
--              , ToList bs   ~ SimplifyList ('Suffix (ToList as) (ToList asbs))
--              , Dimensions asbs
--              , Dimensions (as +: m)
--              , Dimensions (m :+ bs)
--              , KnownNat m
--              , ElementDataType t
--              )
--           => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs
-- contract' x y = case dim @asbs of
--   D -> case ( unsafeCoerce Refl :: as :~: '[]
--             , unsafeCoerce Refl :: bs :~: '[]
--             ) of
--     (Refl, Refl) -> case edtRefl (Proxy @t) of
--         EDTFloat -> contract x y
--   _ :* (sbs :: Dim (sbs :: [Nat])) -> case edtRefl (Proxy @t) of
--       EDTFloat -> contract x y
    --    case ( unsafeCoerce Refl :: EvalConsNat (SimplifyList (ToListNat sbs)) :~: sbs
    --         , unsafeCoerce Refl :: SimplifyList (ToListNat bs) :~: ToListNat bs
    --         , unsafeCoerce Refl :: ToList (as +: m) :~: SimplifyList (ToList (as +: m))
    --         ) of
    -- (Refl, Refl, Refl) -> case edtRefl (Proxy @t) of
    --     EDTFloat -> contract x y
