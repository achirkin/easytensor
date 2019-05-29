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
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.SubSpace
  ( SubSpace (..), (!), element
  , ewfoldMap, iwfoldMap
  , ewzip, iwzip
  , indexWise_, elementWise_
  ) where

import Numeric.Semigroup (Semigroup (..))

import Data.Kind
import GHC.Exts

import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.PrimBytes


-- | Operations on DataFrames
--
-- @as@ is an indexing dimensionality
--
-- @bs@ is an element dimensionality
--
-- @t@ is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatList as bs asbs
      , Dimensions as
      , Dimensions bs
      , Dimensions asbs
      , PrimArray t (DataFrame t bs)
      , PrimArray t (DataFrame t asbs)
      , PrimBytes   (DataFrame t bs)
      , PrimBytes   (DataFrame t asbs)
      ) => SubSpace (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where

    -- | Unsafely get a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element)
    --   size (aka `totalDim` of sub dataframe)
    --
    --   Normal indexing can be expressed in terms of `indexOffset#`:
    --
    --   > index i = case (# dimVal (dim @bs), fromEnum i #) of (# I# n, I# j #) -> indexOffset# (n *# j)
    indexOffset# :: Int# -- ^ Prim element offset
                 -> DataFrame t asbs -> DataFrame t bs
    indexOffset# off df = case uniqueOrCumulDims df of
      Left a      -> broadcast a
      Right steps -> fromElems (dropPref (dims @as) steps) (offsetElems df +# off) (getBytes df)
    {-# INLINE [1] indexOffset# #-}

    -- | Unsafely update a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element)
    --   size (aka `totalDim` of sub dataframe)
    --
    --   Normal updating can be expressed in terms of `indexOffset#`:
    --
    --   > update i = case (# dimVal (dim @bs), fromEnum i #) of (# I# n, I# j #) -> updateOffset# (n *# j)
    updateOffset# :: Int# -- ^ Prim element offset
                  -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
    updateOffset# off x df
      | steps <- getSteps (dims @asbs) df
      , elemBS <- byteSize @t undefined = case runRW#
        ( \s0 -> case newByteArray# (cdTotalDim# steps *# elemBS) s0 of
          (# s1, mba #) -> unsafeFreezeByteArray# mba
            ( writeBytes mba (off *# elemBS) x
              ( writeBytes mba 0# df s1 )
            )
        ) of (# _, r #) -> fromElems steps 0# r
    {-# INLINE [1] updateOffset# #-}

    -- | Get an element by its index in the dataframe
    index :: Idxs as -> DataFrame t asbs -> DataFrame t bs
    index i df = case uniqueOrCumulDims df of
      Left a      -> broadcast a
      Right steps -> case cdIx steps i of
        I# off -> fromElems (dropPref (dims @as) steps) (offsetElems df +# off) (getBytes df)
    {-# INLINE [1] index #-}

    -- | Set a new value to an element
    update :: Idxs as -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
    update i x df
      | steps <- getSteps (dims @asbs) df
      , I# off <- cdIx steps i
      , elemBS <- byteSize @t undefined = case runRW#
        ( \s0 -> case newByteArray# (cdTotalDim# steps *# elemBS) s0 of
          (# s1, mba #) -> unsafeFreezeByteArray# mba
            ( writeBytes mba (off *# elemBS) x
              ( writeBytes mba 0# df s1 )
            )
        ) of (# _, r #) -> fromElems steps 0# r
    {-# INLINE [1] update #-}

    -- | Map a function over each element of DataFrame
    ewmap  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as bs' asbs'
           => (DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs
    ewmap f df
      | bsizeT    <- byteSize @t undefined
      , stepsAS   <- cumulDims $ dims @as
      , stepsBS   <- cumulDims $ dims @bs
      , stepsBS'  <- cumulDims $ dims @bs'
      , stepsASBS <- stepsAS <> stepsBS
      , lenAS     <- cdTotalDim# stepsAS
      , lenBS     <- cdTotalDim# stepsBS
      , lenBS'    <- cdTotalDim# stepsBS'
      , lenBSB    <- lenBS *# bsizeT
      , lenASBSB  <- lenAS *# lenBSB
      = let go mba sourceOffE destOffB s
              | isTrue# (destOffB >=# lenASBSB)
                = s
              | otherwise
                = go mba (sourceOffE +# lenBS') (destOffB +# lenBSB)
                     (writeBytes mba destOffB (f (indexOffset# sourceOffE df)) s)
        in  case runRW#
          ( \s0 -> case newByteArray# lenASBSB s0 of
            (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba 0# 0# s1 )
          ) of (# _, r #) -> fromElems stepsASBS 0# r
    {-# INLINE [1] ewmap #-}

    -- | Map a function over each element with its index of DataFrame
    iwmap  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as bs' asbs'
           => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs
    iwmap f df
      | bsizeT    <- byteSize @t undefined
      , as@KnownDims <- dims @as
      , stepsAS   <- cumulDims as
      , stepsBS   <- cumulDims $ dims @bs
      , stepsBS'  <- cumulDims $ dims @bs'
      , stepsASBS <- stepsAS <> stepsBS
      , lenAS     <- cdTotalDim# stepsAS
      , lenBS     <- cdTotalDim# stepsBS
      , lenBS'    <- cdTotalDim# stepsBS'
      , lenBSB    <- lenBS *# bsizeT
      , lenASBSB  <- lenAS *# lenBSB
      = let go _ [] _ _ s = s
            go mba (i:is) sourceOffE destOffB s
                = go mba is (sourceOffE +# lenBS') (destOffB +# lenBSB)
                     (writeBytes mba destOffB (f i (indexOffset# sourceOffE df)) s)
        in  case runRW#
          ( \s0 -> case newByteArray# lenASBSB s0 of
            (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba [minBound..maxBound] 0# 0# s1 )
          ) of (# _, r #) -> fromElems stepsASBS 0# r
    iwmap _ _ = error "iwmap: impossible args"
    {-# INLINE [1] iwmap #-}

    -- | Generate a DataFrame by repeating an element
    ewgen :: DataFrame t bs -> DataFrame t asbs
    ewgen df = case uniqueOrCumulDims df of
      Left a -> broadcast a
      Right stepsBS
        | stepsAS <- cumulDims $ dims @as
        , stepsASBS <- stepsAS <> stepsBS
        , elS       <- byteSize @t undefined
        , lenBSB    <- cdTotalDim# stepsBS *# elS
        , lenASBSB  <- cdTotalDim# stepsASBS *# elS
          -> let go mba destOffB s
                   | isTrue# (destOffB >=# lenASBSB)
                     = s
                   | otherwise
                     = go mba (destOffB +# lenBSB)
                          (writeBytes mba destOffB df s)
             in  case runRW#
              ( \s0 -> case newByteArray# lenASBSB s0 of
                (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba 0# s1 )
              ) of (# _, r #) -> fromElems stepsASBS 0# r
    {-# INLINE [1] ewgen #-}

    -- | Generate a DataFrame by iterating a function (index -> element)
    iwgen :: (Idxs as -> DataFrame t bs) -> DataFrame t asbs
    iwgen f
        | as@KnownDims <- dims @as
        , stepsAS <- cumulDims as
        , stepsBS <- cumulDims $ dims @bs
        , stepsASBS <- stepsAS <> stepsBS
        , elS       <- byteSize @t undefined
        , lenBSB    <- cdTotalDim# stepsBS *# elS
        , lenASBSB  <- cdTotalDim# stepsASBS *# elS
          = let go _ [] _ s = s
                go mba (i:is) destOffB s
                  = go mba is (destOffB +# lenBSB) (writeBytes mba destOffB (f i) s)
            in  case runRW#
              ( \s0 -> case newByteArray# lenASBSB s0 of
                (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba [minBound..maxBound] 0# s1 )
              ) of (# _, r #) -> fromElems stepsASBS 0# r
    iwgen _ = error "iwgen: impossible args"
    {-# INLINE [1] iwgen #-}

    -- | Left-associative fold of a DataFrame.
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    ewfoldl :: (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
    ewfoldl f x0 df = case uniqueOrCumulDims df of
      Left a ->
        let b = broadcast a
            go 0 x = x
            go n x = go (n-1) $! f x b
        in  go (totalDim' @as) x0
      Right stepsASBS
        | stepsBS <- dropPref (dims @as) stepsASBS
        , lenBS   <- cdTotalDim# stepsBS
        , lenASBS <- cdTotalDim# stepsASBS
          -> let go sourceOffE x
                  | isTrue# (sourceOffE >=# lenASBS)
                    = x
                  | otherwise
                    = go (sourceOffE +# lenBS) $! f x (indexOffset# sourceOffE df)
             in  go 0# x0

    -- | Left-associative fold of a DataFrame with an index
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    iwfoldl :: (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
    iwfoldl f x0 df
      | as@KnownDims <- dims @as = case uniqueOrCumulDims df of
      Left a ->
        let b = broadcast a
            go [] x     = x
            go (i:is) x = go is $! f i x b
        in  go [minBound..maxBound] x0
      Right stepsASBS
        | stepsBS <- dropPref as stepsASBS
        , lenBS   <- cdTotalDim# stepsBS
          -> let go [] _ x = x
                 go (i:is) sourceOffE x
                    = go is (sourceOffE +# lenBS) $! f i x (indexOffset# sourceOffE df)
             in  go [minBound..maxBound] 0# x0
    iwfoldl _ _ _ = error "iwfoldl: impossible args"

    -- | Right-associative fold of a DataFrame
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    ewfoldr :: (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
    ewfoldr f x0 df = case uniqueOrCumulDims df of
      Left a ->
        let b = broadcast a
            go 0 x = x
            go n x = go (n-1) $! f b x
        in  go (totalDim' @as) x0
      Right stepsASBS
        | stepsBS <- dropPref (dims @as) stepsASBS
        , lenBS   <- cdTotalDim# stepsBS
        , lenASBS <- cdTotalDim# stepsASBS
          -> let go sourceOffE x
                  | isTrue# (sourceOffE <# 0#)
                    = x
                  | otherwise
                    = go (sourceOffE -# lenBS) $! f (indexOffset# sourceOffE df) x
             in  go (lenASBS -# lenBS) x0

    -- | Right-associative fold of a DataFrame with an index
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    iwfoldr :: (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
    iwfoldr f x0 df
      | as@KnownDims <- dims @as = case uniqueOrCumulDims df of
      Left a ->
        let b = broadcast a
            go [] x     = x
            go (i:is) x = go is $! f i b x
        in  go [maxBound, pred maxBound .. minBound] x0
      Right stepsASBS
        | stepsBS <- dropPref as stepsASBS
        , lenBS   <- cdTotalDim# stepsBS
        , lenASBS <- cdTotalDim# stepsASBS
          -> let go [] _ x = x
                 go (i:is) sourceOffE x
                    = go is (sourceOffE -# lenBS) $! f i (indexOffset# sourceOffE df) x
             in  go [maxBound, pred maxBound .. minBound] (lenASBS -# lenBS) x0
    iwfoldr _ _ _ = error "iwfoldr: impossible args"

    -- | Apply an applicative functor on each element (Lens-like traversal)
    elementWise :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
                 . ( Applicative f
                   , SubSpace s as bs' asbs'
                   )
                => (DataFrame s bs' -> f (DataFrame t bs))
                -> DataFrame s asbs' -> f (DataFrame t asbs)
    elementWise = indexWise . const
    {-# INLINE [1] elementWise #-}

    -- | Apply an applicative functor on each element with its index
    --     (Lens-like indexed traversal)
    indexWise :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
               . ( Applicative f
                 , SubSpace s as bs' asbs'
                 )
              => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
              -> DataFrame s asbs' -> f (DataFrame t asbs)
    indexWise f df = runWithState <$> iwfoldl applyF (pure initialState) df
      where
        steps = cumulDims $ dims @asbs
        -- run a state-based continuation within RW
        runWithState :: ( State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                     -> DataFrame t asbs
        runWithState g = case runRW#
                           ( \s0 -> case g s0 of
                                (# s1, (# marr, _ #) #) -> unsafeFreezeByteArray# marr s1
                           ) of (# _, arr #) -> fromElems steps 0# arr

        -- Prepare empty byte array for the result DataFrame and keep a current position counter
        -- Input: state
        -- Output: state +
        --     ( current mutable byte array + current write position )
        initialState :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #)
        initialState s0 = case newByteArray# (rezLength# *# rezElBSize#) s0 of
                            (# s1, marr #) -> (# s1, (# marr, 0# #) #)

        -- Given the result chunk, write it into a mutable array
        updateChunk :: (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                    -> DataFrame t bs
                    -> (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        updateChunk g dfChunk = case (# byteOffset dfChunk, getBytes dfChunk #) of
            (# off#, arr#  #) -> \s -> case g s of
                                        (# s1, (# marr#, pos# #) #) -> case
                                            copyByteArray# arr# (off# *# rezElBSize#)
                                                           marr# (pos# *# rezElBSize#)
                                                           (rezStepN# *# rezElBSize#) s1 of
                                          s2 -> (# s2, (# marr#, pos# +# rezStepN# #) #)

        -- Apply applicative functor on each chunk and update a state.
        applyF :: Idxs as
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
               -> DataFrame s bs'
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        applyF idx s dfChunk = idx `seq` dfChunk `seq` updateChunk <$> s <*> f idx dfChunk

        -- Element byte size of the result DataFrame (byte size of s)
        rezElBSize# = byteSize @t undefined
        -- Number of primitive elements in the result DataFrame chunk
        !(I# rezStepN#) = fromIntegral $ totalDim' @bs
        -- Number of primitive elements in the result DataFrame
        rezLength# = cdTotalDim# steps



-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal)
indexWise_ :: forall t as bs asbs f b
            . (SubSpace t as bs asbs, Applicative f)
           => (Idxs as -> DataFrame t bs -> f b)
           -> DataFrame t asbs -> f ()
indexWise_ f = iwfoldr (\i -> (*>) . f i) (pure ())

-- | Apply an applicative functor on each element (Lens-like traversal)
elementWise_ :: forall t as bs asbs f b
              . (SubSpace t as bs asbs, Applicative f)
             => (DataFrame t bs -> f b)
             -> DataFrame t asbs -> f ()
elementWise_ f = ewfoldr ((*>) . f) (pure ())


-- | Apply a functor over a single element (simple lens)
element :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) f
         . (SubSpace t as bs asbs, Applicative f)
        => Idxs as
        -> (DataFrame t bs -> f (DataFrame t bs))
        -> DataFrame t asbs -> f (DataFrame t asbs)
element i f df = flip (update i) df <$> f (index i df)
{-# INLINE element #-}

-- | Index an element (reverse arguments of `index`)
(!) :: SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
    => DataFrame t asbs -> Idxs as -> DataFrame t bs
(!) = flip index
{-# INLINE (!) #-}
infixl 4 !



ewfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . (Monoid m, SubSpace t as bs asbs)
          => (DataFrame t bs -> m) -> DataFrame t asbs -> m
ewfoldMap f = ewfoldl (\m b -> m `seq` (mappend m $! f b)) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . ( Monoid m, SubSpace t as bs asbs)
          => (Idxs as -> DataFrame t bs -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldl (\i m b -> m `seq` (mappend m $! f i b)) mempty
{-# INLINE iwfoldMap #-}



-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (bs' :: [Nat]) (asbs' :: [Nat])
                r (bs'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as bs' asbs'
         , SubSpace r as bs'' asbs''
         )
      => (Idxs as -> DataFrame t bs -> DataFrame s bs' -> DataFrame r bs'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
iwzip f dft dfs = iwmap g dft
  where
    g i dft' = f i dft' (index i dfs)
{-# INLINE iwzip #-}

-- | Zip two spaces on a specified subspace element-wise (without index)
ewzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (bs' :: [Nat]) (asbs' :: [Nat])
                r (bs'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as bs' asbs'
         , SubSpace r as bs'' asbs''
         )
      => (DataFrame t bs -> DataFrame s bs' -> DataFrame r bs'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
ewzip = iwzip . const
{-# INLINE ewzip #-}


instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , PrimArray t (DataFrame t bs)
         , PrimArray t (DataFrame t asbs)
         , PrimBytes   (DataFrame t bs)
         , PrimBytes   (DataFrame t asbs)
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where



dropPref :: Dims (ns :: [Nat]) -> CumulDims -> CumulDims
dropPref ds = CumulDims . drop (length $ listDims ds) . unCumulDims


unSc :: DataFrame (t :: Type) ('[] :: [Nat]) -> t
unSc = unsafeCoerce#

{-# RULES
"ewgen/broadcast" ewgen = broadcast . unSc

  #-}
