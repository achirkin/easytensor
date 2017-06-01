{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Tensor
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Tensor
  ( Tensor
  , module Numeric.DataFrame
  , module Numeric.Dimensions
  , module Numeric.Commons
  ) where

import GHC.TypeLits
--import GHC.Prim
--import Data.Proxy



-- import Numeric.NDArray.Base.Float ()
import Numeric.Dimensions
import Numeric.DataFrame
import Numeric.Commons


type Tensor t (ds :: [Nat]) = DataFrame t ds



-- -- | Fill whole tensor with a single value
-- fill :: M.MatrixCalculus t n m (Tensor t n m) => Tensor t '[] -> Tensor t ds
-- fill = M.broadcastMat . _unScalar . _unT
-- {-# INLINE fill #-}
