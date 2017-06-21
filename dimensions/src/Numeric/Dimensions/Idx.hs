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
-- import GHC.Exts
import GHC.TypeLits
import GHC.Exts                (IsList (..))
import Numeric.Dimensions.Dim
--import Numeric.Dimensions.List

import           Unsafe.Coerce           (unsafeCoerce)


-- | Type-level dimensional indexing with arbitrary Int values inside
data Idx (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Idx '[]
   -- | List-like concatenation of indices
   (:!) :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !(Idx ds) -> Idx (d ': ds)
infixr 5 :!

idxToList :: Idx ds -> [Int]
idxToList Z         = []
idxToList (x :! xs) = x : idxToList xs

idxFromList :: [Int] -> Idx ds
idxFromList []     = unsafeCoerce Z
idxFromList (x:xs) = unsafeCoerce $ x :! unsafeCoerce (idxFromList xs)

-- appendIdx :: Idx as -> Int -> Idx (as +: b)
-- appendIdx Z i = i :! Z
-- appendIdx jjs@(j :! js) i = case proofCons jjs js of
--     Refl -> unsafeCoerce $ j :! appendIdx js i
--   where
--     proofCons :: Idx as -> Idx bs -> as :~: (b :+ bs)
--     proofCons _ _ = unsafeCoerce Refl
-- {-# INLINE appendIdx #-}
--
-- splitIdx :: FiniteList as => Idx (as ++ bs) -> (Idx as, Idx bs)
-- splitIdx idx = rez
--   where
--     getAs :: (Idx as, Idx bs) -> Proxy as
--     getAs _ = Proxy
--     rez = splitN (order $ getAs rez) idx
--     splitN :: Int -> Idx (as ++ bs) -> (Idx as, Idx bs)
--     splitN 0 js = unsafeCoerce (Z, js)
--     splitN n (j :! js) = first (unsafeCoerce . (j :!))
--                        $ splitN (n-1) (unsafeCoerce js)
--     splitN _ Z  = unsafeCoerce (Z, Z)
-- {-# INLINE splitIdx #-}


instance Show (Idx ds) where
    show Z  = "Idx Ø"
    show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (idxToList xs)

instance Eq (Idx ds) where
    Z == Z = True
    (a:!as) == (b:!bs) = a == b && as == bs
    Z /= Z = False
    (a:!as) /= (b:!bs) = a /= b || as /= bs


-- | With this instance we can slightly reduce indexing expressions
--   e.g. x ! (1 :! 2 :! 4) == x ! (1 :! 2 :! 4 :! Z)
instance Num (Idx '[n]) where
    (a:!Z) + (b:!Z) = (a+b) :! Z
    (a:!Z) - (b:!Z) = (a-b) :! Z
    (a:!Z) * (b:!Z) = (a*b) :! Z
    signum (a:!Z)   = signum a :! Z
    abs (a:!Z)      = abs a :! Z
    fromInteger i   = fromInteger i :! Z


instance Ord (Idx ds) where
    compare Z Z             = EQ
    compare (a:!as) (b:!bs) = compare as bs `mappend` compare a b

instance Dimensions ds => Bounded (Idx ds) where
    maxBound = f (dim @ds)
      where
        f :: forall ns . Dim ns -> Idx ns
        f D = Z
        f ((Dn :: Dim n) :* ds) = dimVal' @n :! f ds
        f _ = undefined
    {-# INLINE maxBound #-}
    minBound = f (dim @ds)
      where
        f :: forall (ns :: [Nat]) . Dim ns -> Idx ns
        f D = Z
        f (Dn :* ds) = 1 :! f ds
        f _ = undefined
    {-# INLINE minBound #-}

instance IsList (Idx ds) where
    type Item (Idx ds) = Int
    -- | Very unsafe way to convert Haskell list into Idx.
    --   If the length of a list is not equal to the length of type-level
    --   dimensions, behavior is undefined (going to crash likely).
    fromList = idxFromList
    toList = idxToList

instance Dimensions ds => Enum (Idx ds) where
    succ Z = Z
    succ (i:!is) | i < _     = succ i :! is
                 | otherwise =      1 :! succ is
    {-# INLINE succ #-}
    pred Z = Z
    {-# INLINE pred #-}
    -- toEnum = toIdx
    -- {-# INLINE toEnum #-}
    -- fromEnum = fromIdx
    -- {-# INLINE fromEnum #-}
    -- enumFrom x = take (diffIdx maxBound x + 1) $ iterate succ x
    -- {-# INLINE enumFrom #-}
    -- enumFromTo x y | x >= y    = take (diffIdx x y + 1) $ iterate pred x
    --                | otherwise = take (diffIdx y x + 1) $ iterate succ x
    -- {-# INLINE enumFromTo #-}
    -- enumFromThen x x' = take n $ iterate (stepIdx dn) x
    --   where
    --     dn = diffIdx x' x
    --     n  = 1 + if dn == 0 then 0
    --                         else if dn > 0 then diffIdx maxBound x `div` dn
    --                                        else diffIdx x minBound `div` negate dn
    -- {-# INLINE enumFromThen #-}
    -- enumFromThenTo x x' y = take n $ iterate (stepIdx dn) x
    --   where
    --     dn = diffIdx x' x
    --     n  = 1 + if dn == 0 then 0
    --                         else diffIdx y x `div` dn
    -- {-# INLINE enumFromThenTo #-}
