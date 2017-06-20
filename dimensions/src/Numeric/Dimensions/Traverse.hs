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
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeInType                #-}
-- {-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Traverse
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

module Numeric.Dimensions.Traverse
  ( module Numeric.Dimensions.Traverse
  , module Numeric.Dimensions.Dim
  , module Numeric.Dimensions.Idx
  -- , module Control.Monad.ST
  , module GHC.Exts
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
import           GHC.IO                 (IO(..))

import           Unsafe.Coerce           (unsafeCoerce)

import Numeric.Dimensions.Dim
import Numeric.Dimensions.Idx



-- | Traverse over all dimensions keeping track of index and offset
overDim# :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #))
         -> Int#
         -> a
         -> State# s
         -> (# State# s, a #)
overDim# ds f off0# a0 s0 = case overDim'# ds g off0# a0 s0 of
                              (# s1, _, a1 #) -> (# s1, a1 #)
  where
    g i off# a s = case f i off# a s of
                    (# t, b #) -> (# t, off# +# 1#, b #)
{-# INLINE overDim# #-}

-- | Same as overDim#, but with no return value
overDim_# :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> State# s -> (# State# s, () #))
         -> Int#
         -> State# s
         -> (# State# s, () #)
overDim_# ds f off0# s0 = case overDim_'# ds g off0# s0 of
                              (# s1, _ #) -> (# s1, () #)
  where
    g i off# s = case f i off# s of
                    (# t, _ #) -> (# t, off# +# 1# #)
{-# INLINE overDim_# #-}

-- | Traverse over all dimensions keeping track of indices
overDimIdx# :: Dim (ds :: [Nat])
            -> (Idx ds -> a -> State# s -> (# State# s, a #))
            -> a
            -> State# s
            -> (# State# s, a #)
overDimIdx# D f = f Z
overDimIdx# ((Dn :: Dim n) :* ds) f = overDimIdx# ds (loop 1)
  where
    n = dimVal' @n
    loop i js a s | i > n = (# s,  a #)
                  | otherwise = case f (i:!js) a s of
                            (# s', b #) -> loop (i+1) js b s'
overDimIdx# _ _ = error "Impossible Dim structure in overDim# function."

-- | Traverse over all dimensions keeping track of indices
overDimIdx_# :: Dim (ds :: [Nat])
            -> (Idx ds -> State# s -> (# State# s, () #))
            -> State# s
            -> (# State# s, () #)
overDimIdx_# D f = f Z
overDimIdx_# ((Dn :: Dim n) :* ds) f = overDimIdx_# ds (loop 1)
  where
    n = dimVal' @n
    loop i js s | i > n = (# s,  () #)
                | otherwise = case f (i:!js) s of
                          (# s', _ #) -> loop (i+1) js s'
overDimIdx_# _ _ = error "Impossible Dim structure in overDim# function."

-- | Traverse over all dimensions keeping track of total offset
overDimOff# :: Dim (ds :: [Nat])
            -> (Int# -> a -> State# s -> (# State# s, a #))
            -> Int# -> a -> State# s -> (# State# s, a #)
overDimOff# ds f off0# = loop off0#
  where
    n = case dimVal ds of I# n# -> n# +# off0#
    loop off# a s | isTrue# (off# >=# n) = (# s,  a #)
                  | otherwise = case f off# a s of
                                  (# s', b #) -> loop (off# +# 1#) b s'

-- | Traverse over all dimensions keeping track of total offset
overDimOff_# :: Dim (ds :: [Nat])
            -> (Int# -> State# s -> (# State# s, () #))
            -> Int# -> State# s -> (# State# s, () #)
overDimOff_# ds f off0# = loop off0#
  where
    n = case dimVal ds of I# n# -> n# +# off0#
    loop off# s | isTrue# (off# >=# n) = (# s,  () #)
                | otherwise = case f off# s of
                                  (# s', () #) -> loop (off# +# 1#) s'






overDim'# :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> a -> State# s -> (# State# s, Int#, a #))
         -> Int#
         -> a
         -> State# s
         -> (# State# s, Int#,  a #)
overDim'# D f = f Z
overDim'# ((Dn :: Dim n) :* ds) f = overDim'# ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# a s | i > n = (# s, off#, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', off1#, b #) -> loop (i+1) js off1# b s'
overDim'# _ _ = error "Impossible Dim structure in overDim# function."


overDim_'# :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> State# s -> (# State# s, Int# #))
         -> Int#
         -> State# s
         -> (# State# s, Int# #)
overDim_'# D f = f Z
overDim_'# ((Dn :: Dim n) :* ds) f = overDim_'# ds (loop 1)
  where
    n = dimVal' @n
    loop i js off# s | i > n = (# s, off# #)
                     | otherwise = case f (i:!js) off# s of
                               (# s', off1# #) -> loop (i+1) js off1# s'
overDim_'# _ _ = error "Impossible Dim structure in overDim_# function."


overDimPart'# :: Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> (Idx ds -> Int# -> a -> State# s -> (# State# s, Int#, a #))
              -> Int#
              -> a
              -> State# s
              -> (# State# s, Int#,  a #)
overDimPart'# Z Z f = f Z
overDimPart'# (iMin:!mins) (iMax:!maxs) f = overDimPart'# mins maxs (loop iMin)
  where
    loop i js off# a s | i > iMax = (# s, off#, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', off1#, b #) -> loop (i+1) js off1# b s'
-- overDim'# _ _ = error "Impossible Dim structure in overDim# function."











overDim :: Dim (ds :: [Nat])
        -> (Idx ds -> Int# -> a -> IO a)
        -> Int# -> a -> IO a
overDim ds stf off0# = IO . overDim# ds (\i off# a -> case stf i off# a of
                                                           IO f -> f
                                         ) off0#
{-# INLINE overDim #-}

overDimIdx :: Dim (ds :: [Nat])
           -> (Idx ds -> a -> IO a)
           -> a -> IO a
overDimIdx ds stf = IO . overDimIdx# ds (\i a -> case stf i a of IO f -> f)
{-# INLINE overDimIdx #-}

overDimOff :: Dim (ds :: [Nat])
        -> (Idx ds -> Int# -> a -> IO a)
        -> Int# -> a -> IO a
overDimOff ds stf off0# = IO . overDim# ds (\i off# a -> case stf i off# a of
                                                           IO f -> f
                                         ) off0#
{-# INLINE overDimOff #-}



overDim_ :: Dim (ds :: [Nat])
         -> (Idx ds -> Int# -> IO ())
         -> Int# -> IO ()
overDim_ ds stf off0# = IO $ overDim_# ds (\i off# -> case stf i off# of
                                                           IO f -> f
                                          ) off0#
{-# INLINE overDim_ #-}

overDimIdx_ :: Dim (ds :: [Nat])
            -> (Idx ds -> IO ())
            -> IO ()
overDimIdx_ ds stf = IO $ overDimIdx_# ds (\i -> case stf i of IO f -> f)
{-# INLINE overDimIdx_ #-}


overDimOff_ :: Dim (ds :: [Nat])
            -> (Int# -> IO ())
            -> Int# -> IO ()
overDimOff_ ds stf off0# = IO $ overDimOff_# ds (\off#-> case stf off# of
                                                           IO f -> f
                                         ) off0#
{-# INLINE overDimOff_ #-}
