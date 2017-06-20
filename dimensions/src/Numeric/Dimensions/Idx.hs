{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeInType                #-}
-- {-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Idx
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type Idx that enumerates through multiple dimensions.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-- Some of the type-level list operations are implemented using type families
--   and weirdly duplicated for kinds k,Nat,XNat:
--   This duplication is needed to workaround some GHC bugs (panic with "no skolem info")
-----------------------------------------------------------------------------

module Numeric.Dimensions.Idx
  ( -- * Data types
    Idx (..)
  ) where


-- import           Control.Arrow           (first)
-- import           Data.Maybe              (isJust)
-- import           Data.Type.Equality      ((:~:) (..))
-- import           GHC.Exts                (Constraint, IsList (..), Proxy#,
--                                           State#, proxy#)
-- import           GHC.TypeLits            (type (+), type (-), type (<=),
--                                           type (<=?), ErrorMessage (..),
--                                           KnownNat, Nat, SomeNat (..),
--                                           TypeError, natVal, natVal', sameNat,
--                                           someNatVal)
import GHC.Exts
import GHC.TypeLits

import           Unsafe.Coerce           (unsafeCoerce)


-- | Type-level dimensional indexing with arbitrary Int values inside
data Idx (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Idx '[]
   -- | List-like concatenation of indices
   (:!) :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !(Idx ds) -> Idx (d ': ds)
infixr 5 :!


instance Show (Idx ds) where
    show Z  = "Idx Ø"
    show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (idxToList xs)

idxToList :: Idx ds -> [Int]
idxToList Z         = []
idxToList (x :! xs) = x : idxToList xs

idxFromList :: [Int] -> Idx ds
idxFromList []     = unsafeCoerce Z
idxFromList (x:xs) = unsafeCoerce $ x :! unsafeCoerce (idxFromList xs)
