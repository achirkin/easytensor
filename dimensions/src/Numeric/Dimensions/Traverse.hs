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

-- import           Unsafe.Coerce           (unsafeCoerce)

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

-- | Traverse from the first index to the second index in each dimension.
--   Indices must be within Dim range, which is not checked.
--   You can combine positive and negative traversal directions along different dimensions.
overDimPart# :: forall (ds :: [Nat]) a s
              . Dimensions ds
             => Idx ds
             -> Idx ds
             -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #))
             -> Int#
             -> a
             -> State# s
             -> (# State# s, a #)
overDimPart# = overDimPart'# offs
    where
      offs = createOffsets (dim @ds) 1
      createOffsets :: forall (ns :: [Nat]) . Dim ns -> Int -> Idx ns
      createOffsets D _ = Z
      createOffsets ((Dn :: Dim n) :* ds) k = k :! createOffsets ds (k * dimVal' @n)






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


overDimPart'# :: Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> Idx (ds :: [Nat])
              -> (Idx ds -> Int# -> a -> State# s -> (# State# s, a #))
              -> Int#
              -> a
              -> State# s
              -> (# State# s, a #)
overDimPart'# _ Z Z f off0# = f Z off0#
overDimPart'# (I# iW:!iws) (iMin:!mins) (iMax:!maxs) f off0#
    | iMax >= iMin = overDimPart'# iws mins maxs (loop iMin) (off0# +# minOff#)
    | otherwise    = overDimPart'# iws mins maxs (looi iMin) (off0# +# minOff#)
  where
    minOff# = case iMin of I# i -> iW *# (i -# 1#)
    loop i js off# a s | i > iMax = (# s, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', b #) -> loop (i+1) js (off# +# iW) b s'
    looi i js off# a s | i < iMax = (# s, a #)
                       | otherwise = case f (i:!js) off# a s of
                               (# s', b #) -> looi (i-1) js (off# -# iW) b s'








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

overDimPart :: forall (ds :: [Nat]) a
             . Dimensions ds
            => Idx ds -> Idx ds
            -> (Idx ds -> Int# -> a -> IO a)
            -> Int# -> a -> IO a
overDimPart iMin iMax stf off0# = IO . overDimPart# iMin iMax (\i off# a -> case stf i off# a of
                                                                   IO f -> f
                                                              ) off0#
{-# INLINE overDimPart #-}
