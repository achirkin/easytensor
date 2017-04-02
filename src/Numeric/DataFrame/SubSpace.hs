{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE InstanceSigs           #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpace
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.SubSpace
  ( SubSpace (..), (!)
  , ewfoldMap, iwfoldMap
  , ewzip, iwzip
  ) where

import           Data.Proxy
import           GHC.Base (runRW#)
import           GHC.Prim
import           GHC.TypeLits (Nat)
import           GHC.Types
import qualified Numeric.Commons as NCommons
import           Numeric.Dimensions


import           Numeric.DataFrame.Type


-- | Operations on DataFrames
-- as is an element dimensionality
-- bs is an indexing dimensionality
-- t is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatDim as bs asbs
      , Dimensions as
      , Dimensions bs
      , Dimensions asbs
      , NCommons.PrimBytes (DataFrame t as)
      , NCommons.PrimBytes (DataFrame t asbs)
      ) => SubSpace (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Get an element
    (!.) :: Idx bs -> DataFrame t asbs -> DataFrame t as
    -- | Map a function over each element of DataFrame
    ewmap  :: SubSpace s as' bs asbs'
           => proxy bs
           -> (DataFrame t as -> DataFrame s as')
           -> DataFrame t asbs -> DataFrame s asbs'
    -- | Map a function over each element with its index of DataFrame
    iwmap  :: SubSpace s as' bs asbs'
           => (Idx bs -> DataFrame t as -> DataFrame s as')
           -> DataFrame t asbs -> DataFrame s asbs'
    -- | Generate a DataFrame by repeating an element
    ewgen :: proxy bs -> DataFrame t as -> DataFrame t asbs
    -- | Generate a DataFrame by iterating a function (index -> element)
    iwgen :: (Idx bs -> DataFrame t as) -> DataFrame t asbs
    -- | Left-associative fold of a DataFrame
    ewfoldl :: proxy bs -> (b -> DataFrame t as -> b) -> b -> DataFrame t asbs -> b
    -- | Left-associative fold of a DataFrame with an index
    iwfoldl :: (Idx bs -> b -> DataFrame t as -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame
    ewfoldr :: proxy bs -> (DataFrame t as -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame with an index
    iwfoldr :: (Idx bs -> DataFrame t as -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Apply an applicative functor on each element (Lens-like traversal)
    elementWise :: ( Applicative f
                   , SubSpace s as' bs asbs'
                   )
                => Dim bs
                -> (DataFrame t as -> f (DataFrame s as'))
                -> DataFrame t asbs -> f (DataFrame s asbs')
    -- | Apply an applicative functor on each element with its index
    --     (Lens-like indexed traversal)
    indexWise :: ( Applicative f
                 , SubSpace s as' bs asbs'
                 )
              => (Idx bs -> DataFrame t as -> f (DataFrame s as'))
              -> DataFrame t asbs -> f (DataFrame s asbs')
    -- -- | Generalization of a matrix product: take scalar product over one dimension
    -- --   and, thus, concatenate other dimesnions
    -- contract :: DataFrame t (as +: m) -> DataFrame t (m ': bs) -> DataFrame t asbs
infixr 4 !.

-- | Index an element (reverse of !.)
(!) :: SubSpace t as bs asbs
    => DataFrame t asbs -> Idx bs -> DataFrame t as
(!) = flip (!.)
infixl 4 !
{-# INLINE (!) #-}


ewfoldMap :: ( Monoid m, SubSpace t as bs asbs)
          => Dim bs -> (DataFrame t as -> m) -> DataFrame t asbs -> m
ewfoldMap bs f = ewfoldl bs (\b -> mappend b . f) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: ( Monoid m, SubSpace t as bs asbs)
          => (Idx bs -> DataFrame t as -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldl (\i b -> mappend b . f i) mempty
{-# INLINE iwfoldMap #-}


instance ( ConcatDim as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , NCommons.PrimBytes (DataFrame t as)
         , NCommons.PrimBytes (DataFrame t asbs)
         ) => SubSpace t as bs asbs where

    i !. d = r
        where
          r = case (# NCommons.toBytes d, fromIdx i, totalDim r #) of
                (# (# off, _, arr #), I# i#, I# l# #)
                  -> NCommons.fromBytes (# off +# i# *# l#, l#, arr #)
    {-# INLINE (!.) #-}

    ewmap _ f df = case (# NCommons.toBytes df, totalDim (Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> case runRW#
           ( \s0 -> case newByteArray# (len *# elS) s0 of
               (# s1, marr #) -> case go off (off +# len) l# arr marr s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> NCommons.fromBytes (# 0#, len, r #)
      where
        elS = NCommons.elementByteSize df
        go pos lim step arr marr s
          | isTrue# (pos >=# lim) = s
          | otherwise = case NCommons.toBytes (f (NCommons.fromBytes (# pos, step, arr #))) of
             (# offX, _, arrX #) -> go (pos +# step) lim step arr marr
               (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (step *# elS) s)

    iwmap f df = case (# NCommons.toBytes df, totalDim (Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> case runRW#
           ( \s0 -> case newByteArray# (len *# elS) s0 of
               (# s1, marr #) -> case go off (off +# len) l# arr marr dimMin s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> NCommons.fromBytes (# 0#, len, r #)
      where
        elS = NCommons.elementByteSize df
        go pos lim step arr marr curI s
          | isTrue# (pos >=# lim) = s
          | otherwise = case NCommons.toBytes (f curI (NCommons.fromBytes (# pos, step, arr #))) of
             (# offX, _, arrX #) -> go (pos +# step) lim step arr marr (succIdx curI)
               (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (step *# elS) s)

    ewgen _ x = case (# NCommons.toBytes x, totalDim (Proxy @asbs) #) of
        (# tobytesX , I# len# #) -> case runRW#
           ( \s0 -> case newByteArray# (len# *# elS) s0 of
               (# s1, marr #) -> case go 0# len# tobytesX marr s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> NCommons.fromBytes (# 0#, len#, r #)
      where
        elS = NCommons.elementByteSize (undefined :: DataFrame t as)
        go pos lim tobytesX@(# offX, step, arrX #) marr s
          | isTrue# (pos >=# lim) = s
          | otherwise = go (pos +# step) lim tobytesX marr
               (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (step *# elS) s)

    iwgen f = case (# totalDim (Proxy @as), totalDim (Proxy @asbs) #) of
        (# I# asl# , I# len# #) -> case runRW#
           ( \s0 -> case newByteArray# (len# *# elS) s0 of
               (# s1, marr #) -> case go 0# len# asl# marr dimMin s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> NCommons.fromBytes (# 0#, len#, r #)
      where
        elS = NCommons.elementByteSize (undefined :: DataFrame t as)
        go pos lim step marr curI s
          | isTrue# (pos >=# lim) = s
          | otherwise = case NCommons.toBytes (f curI) of
             (# offX, _, arrX #) -> go (pos +# step) lim step marr (succIdx curI)
               (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (step *# elS) s)

    ewfoldl _ f x0 df = case (# NCommons.toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go off (off +# len) l# arr x0
      where
        go pos lim step arr acc
          | isTrue# (pos >=# lim) = acc
          | otherwise = go (pos +# step) lim step arr
              ( f acc (NCommons.fromBytes (# pos, step, arr #)) )

    iwfoldl f x0 df = case (# NCommons.toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go off (off +# len) l# arr dimMin x0
      where
        go pos lim step arr curI acc
          | isTrue# (pos >=# lim) = acc
          | otherwise = go (pos +# step) lim step arr (succIdx curI)
              ( f curI acc (NCommons.fromBytes (# pos, step, arr #)) )

    ewfoldr _ f x0 df = case (# NCommons.toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go (off +# len -# l#) off l# arr x0
      where
        go pos lim step arr acc
          | isTrue# (pos <# lim) = acc
          | otherwise = go (pos -# step) lim step arr
              ( f (NCommons.fromBytes (# pos, step, arr #)) acc )

    iwfoldr f x0 df = case (# NCommons.toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go (off +# len -# l#) off l# arr dimMin x0
      where
        go pos lim step arr curI acc
          | isTrue# (pos <# lim) = acc
          | otherwise = go (pos -# step) lim step arr (succIdx curI)
              ( f curI (NCommons.fromBytes (# pos, step, arr #)) acc )

    -- implement elementWise in terms of indexWise
    elementWise _ = indexWise . const

    indexWise :: forall (s :: Type) (f :: Type -> Type) (as' :: [Nat]) (asbs' :: [Nat])
               . ( Applicative f
                 , SubSpace s as' bs asbs'
                 )
              => (Idx bs -> DataFrame t as -> f (DataFrame s as'))
              -> DataFrame t asbs -> f (DataFrame s asbs')
    indexWise f df = runWithState <$> iwfoldl applyF (pure initialState) df
      where
        -- run a state-based continuation within RW
        runWithState :: ( State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                     -> DataFrame s asbs'
        runWithState g = case runRW#
                           ( \s0 -> case g s0 of
                                (# s1, (# marr, _ #) #) -> unsafeFreezeByteArray# marr s1
                           ) of (# _, arr #) -> NCommons.fromBytes (# 0#, rezLength#, arr #)

        -- Prepare empty byte array for the result DataFrame and keep a current position counter
        -- Input: state
        -- Output: state +
        --     ( current mutable byte array + current write position )
        initialState :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #)
        initialState s0 = case newByteArray# (rezLength# *# rezElBSize#) s0 of
                            (# s1, marr #) -> (# s1, (# marr, 0# #) #)

        -- Given the result chunk, write it into a mutable array
        updateChunk :: (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                    -> DataFrame s as'
                    -> (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        updateChunk g dfChunk = case NCommons.toBytes dfChunk of
            (# off#, _, arr#  #) -> \s -> case g s of
                                        (# s1, (# marr#, pos# #) #) -> case
                                            copyByteArray# arr# (off# *# rezElBSize#)
                                                           marr# (pos# *# rezElBSize#)
                                                           (rezStepN# *# rezElBSize#) s1 of
                                          s2 -> (# s2, (# marr#, pos# +# rezStepN# #) #)

        -- Apply applicative functor on each chunk and update a state.
        applyF :: Idx bs
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
               -> DataFrame t as
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        applyF idx s dfChunk = updateChunk <$> s <*> f idx dfChunk

        -- Element byte size of the result DataFrame (byte size of s)
        rezElBSize# = NCommons.elementByteSize (undefined :: DataFrame s asbs')
        -- Number of primitive elements in the result DataFrame chunk
        !(I# rezStepN#) = totalDim (Proxy @as')
        -- Number of primitive elements in the result DataFrame
        !(I# rezLength#) = totalDim (Proxy @asbs')

-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip :: ( SubSpace t as bs asbs
         , SubSpace s as' bs asbs'
         , SubSpace r as'' bs asbs''
         )
      => (Idx bs -> DataFrame t as -> DataFrame s as' -> DataFrame r as'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
iwzip f dft dfs = iwmap g dft
  where
    g i dft' = f i dft' (i !. dfs)
{-# INLINE iwzip #-}

-- | Zip two spaces on a specified subspace element-wise (without index)
ewzip :: ( SubSpace t as bs asbs
         , SubSpace s as' bs asbs'
         , SubSpace r as'' bs asbs''
         )
      => proxy bs
      -> (DataFrame t as -> DataFrame s as' -> DataFrame r as'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
ewzip _ = iwzip . const
{-# INLINE ewzip #-}
