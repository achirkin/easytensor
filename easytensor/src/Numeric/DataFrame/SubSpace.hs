{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UnboxedTuples           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpace
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.SubSpace
  ( SubSpace (..), (!), element
  , ewfoldMap, iwfoldMap
  , ewzip, iwzip
  ) where

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), Type, isTrue#)


import qualified Numeric.Array.ElementWise as EW
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse
import           Numeric.TypeLits
import           Numeric.Scalar

-- | Operations on DataFrames
-- as is an element dimensionality
-- bs is an indexing dimensionality
-- t is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatList as bs asbs
      , Dimensions as
      , Dimensions bs
      , Dimensions asbs
      ) => SubSpace (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Get an element
    (!.) :: Idx bs -> DataFrame t asbs -> DataFrame t as
    -- | Set a new value to an element
    update :: Idx bs -> DataFrame t as -> DataFrame t asbs -> DataFrame t asbs
    -- | Map a function over each element of DataFrame
    ewmap  :: forall s (as' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as' bs asbs'
           => (DataFrame s as' -> DataFrame t as)
           -> DataFrame s asbs' -> DataFrame t asbs
    -- | Map a function over each element with its index of DataFrame
    iwmap  :: forall s (as' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as' bs asbs'
           => (Idx bs -> DataFrame s as' -> DataFrame t as)
           -> DataFrame s asbs' -> DataFrame t asbs
    -- | Generate a DataFrame by repeating an element
    ewgen :: DataFrame t as -> DataFrame t asbs
    -- | Generate a DataFrame by iterating a function (index -> element)
    iwgen :: (Idx bs -> DataFrame t as) -> DataFrame t asbs
    -- | Left-associative fold of a DataFrame
    ewfoldl :: (b -> DataFrame t as -> b) -> b -> DataFrame t asbs -> b
    -- | Left-associative fold of a DataFrame with an index
    iwfoldl :: (Idx bs -> b -> DataFrame t as -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame
    ewfoldr :: (DataFrame t as -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame with an index
    iwfoldr :: (Idx bs -> DataFrame t as -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Apply an applicative functor on each element (Lens-like traversal)
    elementWise :: forall s (as' :: [Nat]) (asbs' :: [Nat]) f
                 . ( Applicative f
                   , SubSpace s as' bs asbs'
                   )
                => (DataFrame s as' -> f (DataFrame t as))
                -> DataFrame s asbs' -> f (DataFrame t asbs)
    -- | Apply an applicative functor on each element with its index
    --     (Lens-like indexed traversal)
    indexWise :: forall s (as' :: [Nat]) (asbs' :: [Nat]) f
               . ( Applicative f
                 , SubSpace s as' bs asbs'
                 )
              => (Idx bs -> DataFrame s as' -> f (DataFrame t as))
              -> DataFrame s asbs' -> f (DataFrame t asbs)
infixr 4 !.

-- | Apply a functor over a single element (simple lens)
element :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) f
         . (SubSpace t as bs asbs, Applicative f)
        => Idx bs
        -> (DataFrame t as -> f (DataFrame t as))
        -> DataFrame t asbs -> f (DataFrame t asbs)
element i f df = flip (update i) df <$> f (i !. df)
{-# INLINE element #-}

-- | Index an element (reverse of !.)
(!) :: SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
    => DataFrame t asbs -> Idx bs -> DataFrame t as
(!) = flip (!.)
infixl 4 !
{-# INLINE (!) #-}


ewfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . (Monoid m, SubSpace t as bs asbs)
          => (DataFrame t as -> m) -> DataFrame t asbs -> m
ewfoldMap f = ewfoldl (\b -> mappend b . f) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . ( Monoid m, SubSpace t as bs asbs)
          => (Idx bs -> DataFrame t as -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldl (\i b -> mappend b . f i) mempty
{-# INLINE iwfoldMap #-}



-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (as' :: [Nat]) (asbs' :: [Nat])
                r (as'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
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
ewzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (as' :: [Nat]) (asbs' :: [Nat])
                r (as'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as' bs asbs'
         , SubSpace r as'' bs asbs''
         )
      => (DataFrame t as -> DataFrame s as' -> DataFrame r as'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
ewzip = iwzip . const
{-# INLINE ewzip #-}



instance {-# OVERLAPPABLE #-}
         ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , PrimBytes (DataFrame t as)
         , PrimBytes (DataFrame t asbs)
         , as ~ (a'' ': as'')
         , asbs ~ (a'' ': asbs'')
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where

    i !. d = r
        where
          r = case (# toBytes d, fromEnum i, totalDim r #) of
                (# (# off, _, arr #), I# i#, I# l# #)
                  -> fromBytes (# off +# i# *# l#, l#, arr #)
    {-# INLINE (!.) #-}

    ewmap = iwmap . const
    {-# INLINE ewmap #-}

    iwmap f df
      | elS <- elementByteSize (undefined :: DataFrame t asbs)
      , I# lenBS <- totalDim (Proxy @bs)
      , I# lenAS <- totalDim (Proxy @as)
      = case runRW#
          ( \s0 -> case newByteArray# (lenAS *# lenBS *# elS) s0 of
              (# s1, marr #) -> case foldDimIdx
                  (dim @bs)
                  ( \i (SO# pos s) -> case toBytes $ f i (i !. df) of
                      (# offX, _, arrX #) -> SO#
                        (pos +# lenAS)
                        (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (lenAS *# elS) s)
                  )
                  (SO# 0# s1) of
                (SO# _ s2) -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenAS *# lenBS, r #)

    ewgen x
      | (# offX, lenX, arrX #) <- toBytes x
      , I# lenASBS <- totalDim (Proxy @asbs)
      , elS <- elementByteSize x
      = case runRW#
          ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
              (# s1, marr #) -> case foldDimIdx (dim @bs)
                  ( \_ (SO# pos s) -> SO#
                        (pos +# lenX)
                        (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (lenX *# elS) s)
                  ) (SO# 0# s1) of
                (SO# _ s2) -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenASBS, r #)

    iwgen f
      | I# lenASBS <- totalDim (Proxy @asbs)
      , elS <- elementByteSize (undefined :: DataFrame t asbs)
      = case runRW#
          ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
              (# s1, marr #) -> case foldDimIdx (dim @bs)
                  ( \i (SO# pos s) -> case toBytes $ f i of
                      (# offX, lenX, arrX #) -> SO#
                        (pos +# lenX)
                        (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (lenX *# elS) s)
                  ) (SO# 0# s1) of
                (SO# _ s2) -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenASBS, r #)

    ewfoldl f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go off (off +# len) l# arr x0
      where
        go pos lim step arr acc
          | isTrue# (pos >=# lim) = acc
          | otherwise = go (pos +# step) lim step arr
              ( f acc (fromBytes (# pos, step, arr #)) )

    iwfoldl f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go off (off +# len) l# arr minBound x0
      where
        go pos lim step arr curI acc
          | isTrue# (pos >=# lim) = acc
          | otherwise = go (pos +# step) lim step arr (succ curI)
              ( f curI acc (fromBytes (# pos, step, arr #)) )

    ewfoldr f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go (off +# len -# l#) off l# arr x0
      where
        go pos lim step arr acc
          | isTrue# (pos <# lim) = acc
          | otherwise = go (pos -# step) lim step arr
              ( f (fromBytes (# pos, step, arr #)) acc )

    iwfoldr f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# l# #) -> go (off +# len -# l#) off l# arr minBound x0
      where
        go pos lim step arr curI acc
          | isTrue# (pos <# lim) = acc
          | otherwise = go (pos -# step) lim step arr (succ curI)
              ( f curI (fromBytes (# pos, step, arr #)) acc )

    -- implement elementWise in terms of indexWise
    elementWise = indexWise . const
    {-# INLINE elementWise #-}

    indexWise :: forall (s :: Type) (f :: Type -> Type) (as' :: [Nat]) (asbs' :: [Nat])
               . ( Applicative f
                 , SubSpace s as' bs asbs'
                 )
              => (Idx bs -> DataFrame s as' -> f (DataFrame t as))
              -> DataFrame s asbs' -> f (DataFrame t asbs)
    indexWise f df = runWithState <$> iwfoldl applyF (pure initialState) df
      where
        -- run a state-based continuation within RW
        runWithState :: ( State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                     -> DataFrame t asbs
        runWithState g = case runRW#
                           ( \s0 -> case g s0 of
                                (# s1, (# marr, _ #) #) -> unsafeFreezeByteArray# marr s1
                           ) of (# _, arr #) -> fromBytes (# 0#, rezLength#, arr #)

        -- Prepare empty byte array for the result DataFrame and keep a current position counter
        -- Input: state
        -- Output: state +
        --     ( current mutable byte array + current write position )
        initialState :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #)
        initialState s0 = case newByteArray# (rezLength# *# rezElBSize#) s0 of
                            (# s1, marr #) -> (# s1, (# marr, 0# #) #)

        -- Given the result chunk, write it into a mutable array
        updateChunk :: (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                    -> DataFrame t as
                    -> (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        updateChunk g dfChunk = case toBytes dfChunk of
            (# off#, _, arr#  #) -> \s -> case g s of
                                        (# s1, (# marr#, pos# #) #) -> case
                                            copyByteArray# arr# (off# *# rezElBSize#)
                                                           marr# (pos# *# rezElBSize#)
                                                           (rezStepN# *# rezElBSize#) s1 of
                                          s2 -> (# s2, (# marr#, pos# +# rezStepN# #) #)

        -- Apply applicative functor on each chunk and update a state.
        applyF :: Idx bs
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
               -> DataFrame s as'
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        applyF idx s dfChunk = updateChunk <$> s <*> f idx dfChunk

        -- Element byte size of the result DataFrame (byte size of s)
        rezElBSize# = elementByteSize (undefined :: DataFrame t asbs)
        -- Number of primitive elements in the result DataFrame chunk
        !(I# rezStepN#) = totalDim (Proxy @as)
        -- Number of primitive elements in the result DataFrame
        !(I# rezLength#) = totalDim (Proxy @asbs)

    update ei x df
      | I# i <- fromEnum ei
      , (# off, len, arr #) <- toBytes df
      , (# offX, lenX, arrX #) <- toBytes x
      , elS <- elementByteSize df
      = case runRW#
          ( \s0 -> case newByteArray# ( len *# elS ) s0 of
            (# s1, marr #) -> case copyByteArray# arr (off *# elS) marr 0# (len *# elS) s1 of
              s2 -> case copyByteArray# arrX (offX *# elS) marr (lenX *# i *# elS) (lenX *# elS) s2 of
                s3 -> unsafeFreezeByteArray# marr s3
          ) of (# _, r #) -> fromBytes (# 0#, len, r #)


-- | This datatype is used to carry compound state in loops
data StateOffset = SO# Int# (State# RealWorld)


-- | Specialized instance of SubSpace for operating on scalars.
instance {-# OVERLAPPING #-}
         ( Dimensions bs
         , EW.ElementWise (Idx bs) t (DataFrame t bs)
         , PrimBytes (DataFrame t bs)
         ) => SubSpace t ('[] :: [Nat]) (bs :: [Nat]) (bs :: [Nat]) where
    i !. x =  scalar $ x EW.! i
    {-# INLINE (!.) #-}
    ewmap = iwmap . const
    {-# INLINE ewmap #-}
    iwmap f x = EW.ewgen (\i -> unScalar $ f i (i !. x))
    {-# INLINE iwmap #-}
    ewgen = EW.broadcast . unScalar
    {-# INLINE ewgen #-}
    iwgen f = EW.ewgen (unScalar . f)
    {-# INLINE iwgen #-}
    ewfoldl f r0 x = foldDimIdx (dim @bs) (\i r -> f r (i !. x)) r0
    {-# INLINE ewfoldl #-}
    iwfoldl f r0 x = foldDimIdx (dim @bs) (\i r -> f i r (i !. x)) r0
    {-# INLINE iwfoldl #-}
    ewfoldr f r0 x = foldDimReverseIdx (dim @bs) (\i r -> f (i !. x) r) r0
    {-# INLINE ewfoldr #-}
    iwfoldr f r0 x = foldDimReverseIdx (dim @bs) (\i r -> f i (i !. x) r) r0
    {-# INLINE iwfoldr #-}
    elementWise = indexWise . const
    {-# INLINE elementWise #-}
    indexWise f x = EW.ewgenA (\i -> unScalar <$> f i (i !. x))
    {-# INLINE indexWise #-}
    update i x = EW.update i (unScalar x)
    {-# INLINE update #-}
