{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UnboxedTuples           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpace
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.SubSpace
  ( SubSpace (..), (.!), element
  , ewzip, iwzip
  , indexOffset, updateOffset
  , index, update, slice, updateSlice
  , ewgen, iwgen, ewmap, iwmap
  , elementWise, elementWise_, indexWise, indexWise_
  , ewfoldl, ewfoldr, ewfoldMap
  , iwfoldl, iwfoldr, iwfoldMap
  ) where

import Data.Kind
import GHC.Exts

-- import Control.Monad.ST
import Numeric.DataFrame.Internal.PrimArray
-- import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Numeric.PrimBytes


-- | Unsafely get a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
--   Arguments are zero-based primitive element offset and subset ("bs" element)
--   size (aka `totalDim` of sub dataframe).
--
indexOffset ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => Int -- ^ Prim element offset
    -> DataFrame t asbs -> DataFrame t bs
indexOffset = indexOffsetI @_ @t @as @bs @asbs
{-# INLINE[1] indexOffset #-}

-- | Unsafely updateI a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
--   Arguments are zero-based primitive element offset and subset ("bs" element)
--   size (aka `totalDim` of sub dataframe).
--
updateOffset ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => Int -- ^ Prim element offset
    -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
updateOffset = updateOffsetI @_ @t @as @bs @asbs
{-# INLINE[1] updateOffset #-}

-- | Get an element by its indexI in the dataframe.
index ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => Idxs as -> DataFrame t asbs -> DataFrame t bs
index = indexI @_ @t @as @bs @asbs
{-# INLINE[1] index #-}

-- | Get a few contiguous elements.
--
--   In a sense, this is just a more complicated version of `indexI`.
slice ::
       forall t as bs asbs bi bd b' bs'
     . ( SubSpace t as bs asbs
       , SubFrameIndexCtx b' bi bd
       , bs ~ (b' :+ bs'), KnownDim bd )
    => Idxs (as +: bi) -> DataFrame t asbs -> DataFrame t (bd :+ bs')
slice = sliceI @_ @t @as @bs @asbs @bi @bd @b'
{-# INLINE[1] slice #-}


-- | Set a new value to an element.
update ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => Idxs as -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
update = updateI @_ @t @as @bs @asbs
{-# INLINE[1] update #-}

-- | Update a few contiguous elements.
--
--   In a sense, this is just a more complicated version of `updateI`.
updateSlice ::
       forall t as bs asbs bi bd b' bs'
     . ( SubSpace t as bs asbs
       , SubFrameIndexCtx b' bi bd
       , bs ~ (b' :+ bs'), KnownDim bd )
    => Idxs (as +: bi) -> DataFrame t (bd :+ bs') -> DataFrame t asbs -> DataFrame t asbs
updateSlice = updateSliceI @_ @t @as @bs @asbs @bi @bd @b'
{-# INLINE[1] updateSlice #-}

-- | Map a function over each element of DataFrame.
ewmap ::
       forall t as bs asbs s bs' asbs'
     . (SubSpace t as bs asbs, SubSpace s as bs' asbs')
    => (DataFrame s bs' -> DataFrame t bs)
    -> DataFrame s asbs' -> DataFrame t asbs
ewmap = ewmapI @_ @t @as @bs @asbs @s @bs' @asbs'
{-# INLINE[1] ewmap #-}

-- | Map a function over each element with its indexI of DataFrame.
iwmap ::
       forall t as bs asbs s bs' asbs'
     . (SubSpace t as bs asbs, SubSpace s as bs' asbs')
    => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
    -> DataFrame s asbs' -> DataFrame t asbs
iwmap = iwmapI @_ @t @as @bs @asbs @s @bs' @asbs'
{-# INLINE[1] iwmap #-}

-- | Generate a DataFrame by repeating an element.
ewgen ::
       forall t as bs asbs
     . (SubSpace t as bs asbs, Dimensions as)
    => DataFrame t bs -> DataFrame t asbs
ewgen = ewgenI @_ @t @as @bs @asbs
{-# INLINE[1] ewgen #-}

-- | Generate a DataFrame by iterating a function (indexI -> element).
iwgen ::
       forall t as bs asbs
     . (SubSpace t as bs asbs, Dimensions as)
    => (Idxs as -> DataFrame t bs) -> DataFrame t asbs
iwgen = iwgenI @_ @t @as @bs @asbs
{-# INLINE[1] iwgen #-}

-- | Left-associative fold of a DataFrame.
--   Same rules apply as for `foldl`.
ewfoldl ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
ewfoldl = ewfoldlI @_ @t @as @bs @asbs @b
{-# INLINE[1] ewfoldl #-}

-- | Left-associative fold of a DataFrame with an indexI.
--   Same rules apply as for `foldl`.
iwfoldl ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
iwfoldl = iwfoldlI @_ @t @as @bs @asbs @b
{-# INLINE[1] iwfoldl #-}

-- | Right-associative fold of a DataFrame.
--   Same rules apply as for `foldr`.
ewfoldr ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
ewfoldr = ewfoldrI @_ @t @as @bs @asbs @b
{-# INLINE[1] ewfoldr #-}

-- | Right-associative fold of a DataFrame with an indexI.
--   Same rules apply as for `foldr`.
iwfoldr ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
iwfoldr = iwfoldrI @_ @t @as @bs @asbs @b
{-# INLINE[1] iwfoldr #-}

-- | Apply an applicative functor on each element (Lens-like traversal).
elementWise ::
       forall t as bs asbs s bs' asbs' f
     . (SubSpace t as bs asbs, SubSpace s as bs' asbs', Applicative f)
    => (DataFrame s bs' -> f (DataFrame t bs))
    -> DataFrame s asbs' -> f (DataFrame t asbs)
elementWise = elementWiseI @_ @t @as @bs @asbs @s @bs' @asbs' @f
{-# INLINE[1] elementWise #-}

-- | Apply an applicative functor on each element with its indexI
--     (Lens-like indexIed traversal).
indexWise ::
       forall t as bs asbs s bs' asbs' f
     . (SubSpace t as bs asbs, SubSpace s as bs' asbs', Applicative f)
    => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
    -> DataFrame s asbs' -> f (DataFrame t asbs)
indexWise = indexWiseI @_ @t @as @bs @asbs @s @bs' @asbs' @f
{-# INLINE[1] indexWise #-}

-- | Operations on DataFrames
--
-- @as@ is an indexIing dimensionality
--
-- @bs@ is an element dimensionality
--
-- @t@ is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatList as bs asbs
      , BoundedDims as
      , Dimensions bs
      , BoundedDims asbs
      , PrimArray t (DataFrame t bs)
      , PrimArray t (DataFrame t asbs)
      , PrimBytes   (DataFrame t bs)
      , PrimBytes   (DataFrame t asbs)
      ) => SubSpace (t :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where

    -- | Unsafely get a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element)
    --   size (aka `totalDim` of sub dataframe).
    --
    indexOffsetI :: Int -- ^ Prim element offset
                -> DataFrame t asbs -> DataFrame t bs

    -- | Unsafely updateI a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element)
    --   size (aka `totalDim` of sub dataframe).
    --
    updateOffsetI :: Int -- ^ Prim element offset
                 -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs

    -- | Get an element by its indexI in the dataframe.
    indexI :: Idxs as -> DataFrame t asbs -> DataFrame t bs

    -- | Get a few contiguous elements.
    --
    --   In a sense, this is just a more complicated version of `indexI`.
    sliceI :: forall (bi :: k) (bd :: k) (b' :: k) (bs' :: [k])
           . (SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd)
          => Idxs (as +: bi) -> DataFrame t asbs -> DataFrame t (bd :+ bs')

    -- | Set a new value to an element.
    updateI :: Idxs as -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs

    -- | Update a few contiguous elements.
    --
    --   In a sense, this is just a more complicated version of `updateI`.
    updateSliceI :: forall (bi :: k) (bd :: k) (b' :: k) (bs' :: [k])
           . (SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd)
          => Idxs (as +: bi) -> DataFrame t (bd :+ bs') -> DataFrame t asbs -> DataFrame t asbs

    -- | Map a function over each element of DataFrame.
    ewmapI  :: forall s (bs' :: [k]) (asbs' :: [k])
            . SubSpace s as bs' asbs'
           => (DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs

    -- | Map a function over each element with its indexI of DataFrame.
    iwmapI  :: forall s (bs' :: [k]) (asbs' :: [k])
            . SubSpace s as bs' asbs'
           => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs

    -- | Generate a DataFrame by repeating an element.
    ewgenI :: Dimensions as => DataFrame t bs -> DataFrame t asbs

    -- | Generate a DataFrame by iterating a function (indexI -> element).
    iwgenI :: Dimensions as => (Idxs as -> DataFrame t bs) -> DataFrame t asbs

    -- | Left-associative fold of a DataFrame.
    --   Same rules apply as for `foldl`.
    ewfoldlI :: (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b

    -- | Left-associative fold of a DataFrame with an indexI.
    --   Same rules apply as for `foldl`.
    iwfoldlI :: (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b

    -- | Right-associative fold of a DataFrame.
    --   Same rules apply as for `foldr`.
    ewfoldrI :: (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b

    -- | Right-associative fold of a DataFrame with an indexI.
    --   Same rules apply as for `foldr`.
    iwfoldrI :: (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b

    -- | Apply an applicative functor on each element (Lens-like traversal).
    elementWiseI :: forall (s :: Type) (bs' :: [k]) (asbs' :: [k]) (f :: Type -> Type)
                 . ( Applicative f
                   , SubSpace s as bs' asbs'
                   )
                => (DataFrame s bs' -> f (DataFrame t bs))
                -> DataFrame s asbs' -> f (DataFrame t asbs)

    -- | Apply an applicative functor on each element with its indexI
    --     (Lens-like indexIed traversal).
    indexWiseI :: forall (s :: Type) (bs' :: [k]) (asbs' :: [k]) (f :: Type -> Type)
               . ( Applicative f
                 , SubSpace s as bs' asbs'
                 )
              => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
              -> DataFrame s asbs' -> f (DataFrame t asbs)



instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , PrimArray t (DataFrame t bs)
         , PrimArray t (DataFrame t asbs)
         , PrimBytes   (DataFrame t bs)
         , PrimBytes   (DataFrame t asbs)
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where

    indexOffsetI (I# off) df = case uniqueOrCumulDims df of
      Left a      -> broadcast a
      Right steps -> fromElems (dropPref (dims @as) steps) (offsetElems df +# off) (getBytes df)
    {-# INLINE [1] indexOffsetI #-}

    updateOffsetI (I# off) x df
      | steps <- getSteps (dims @asbs) df
      , elemBS <- byteSize @t undefined
      , elems <- cdTotalDim# steps
        = if isTrue# (elems ==# 0#)
          then fromElems steps 0# (error "Empty DataFrame (DF0)")
          else case runRW#
            ( \s0 -> case newByteArray# (elems *# elemBS) s0 of
              (# s1, mba #) -> unsafeFreezeByteArray# mba
                ( writeBytes mba (off *# elemBS) x
                  ( writeBytes mba 0# df s1 )
                )
            ) of (# _, r #) -> fromElems steps 0# r
    {-# INLINE [1] updateOffsetI #-}

    indexI i df = case uniqueOrCumulDims df of
      Left a      -> broadcast a
      Right steps -> case cdIx steps i of
        I# off -> fromElems (dropPref (dims @as) steps) (offsetElems df +# off) (getBytes df)
    {-# INLINE [1] indexI #-}


    sliceI :: forall (bi :: Nat) (bd :: Nat) (b' :: Nat) (bs' :: [Nat])
           . (bs ~ (b' :+ bs'), KnownDim bd)
          => Idxs (as +: bi) -> DataFrame t asbs -> DataFrame t (bd :+ bs')
    sliceI  i df
      | _ :* Dims <- dims @bs
      , Dict <- inferKnownBackend @t @(bd ': bs')
        = case uniqueOrCumulDims df of
        Left a      -> broadcast a
        Right steps -> case cdIx steps i of
          I# off
            | bsteps <- repHead (dimVal' @bd) (dropPref (dims @as) steps)
              -> fromElems bsteps (offsetElems df +# off) (getBytes df)
      | otherwise = error "SubSpace/sliceI: impossible arguments"
      where
        repHead y (CumulDims (_:x:xs)) = CumulDims (y*x:x:xs)
        repHead _ steps                = steps

    updateI i x df
      | steps <- getSteps (dims @asbs) df
      , I# off <- cdIx steps i
      , elemBS <- byteSize @t undefined
      , elems <- cdTotalDim# steps
        = if isTrue# (elems ==# 0#)
          then fromElems steps 0# (error "Empty DataFrame (DF0)")
          else case runRW#
            ( \s0 -> case newByteArray# (elems *# elemBS) s0 of
              (# s1, mba #) -> unsafeFreezeByteArray# mba
                ( writeBytes mba (off *# elemBS) x
                  ( writeBytes mba 0# df s1 )
                )
            ) of (# _, r #) -> fromElems steps 0# r
    {-# INLINE [1] updateI #-}

    updateSliceI :: forall (bi :: Nat) (bd :: Nat) (b' :: Nat) (bs' :: [Nat])
           . (bs ~ (b' :+ bs'), KnownDim bd)
          => Idxs (as +: bi) -> DataFrame t (bd :+ bs') -> DataFrame t asbs -> DataFrame t asbs
    updateSliceI i x df
      | _ :* Dims <- dims @bs
      , Dict <- inferKnownBackend @t @(bd ': bs')
      , steps <- getSteps (dims @asbs) df
      , I# off <- cdIx steps i
      , elemBS <- byteSize @t undefined
      , elems <- cdTotalDim# steps
        = if isTrue# (elems ==# 0#)
          then fromElems steps 0# (error "Empty DataFrame (DF0)")
          else case runRW#
            ( \s0 -> case newByteArray# (cdTotalDim# steps *# elemBS) s0 of
              (# s1, mba #) -> unsafeFreezeByteArray# mba
                ( writeBytes mba (off *# elemBS) x
                  ( writeBytes mba 0# df s1 )
                )
            ) of (# _, r #) -> fromElems steps 0# r
      | otherwise = error "SubSpace/updateSliceI: impossible arguments"
    {-# INLINE [1] updateSliceI #-}

    ewmapI  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as bs' asbs'
           => (DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs
    ewmapI f df
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
                     (writeBytes mba destOffB (f (indexOffsetI (I# sourceOffE) df)) s)
        in if isTrue# (lenASBSB ==# 0#)
           then fromElems stepsASBS 0# (error "Empty DataFrame (DF0)")
           else case runRW#
            ( \s0 -> case newByteArray# lenASBSB s0 of
              (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba 0# 0# s1 )
            ) of (# _, r #) -> fromElems stepsASBS 0# r
    {-# INLINE [1] ewmapI #-}

    iwmapI  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as bs' asbs'
           => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs
    iwmapI f df
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
      = let go _ [] _ _ s = s
            go mba (i:is) sourceOffE destOffB s
                = go mba is (sourceOffE +# lenBS') (destOffB +# lenBSB)
                     (writeBytes mba destOffB (f i (indexOffsetI (I# sourceOffE) df)) s)
        in if isTrue# (lenASBSB ==# 0#)
           then fromElems stepsASBS 0# (error "Empty DataFrame (DF0)")
           else case runRW#
            ( \s0 -> case newByteArray# lenASBSB s0 of
              (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba [minBound..maxBound] 0# 0# s1 )
            ) of (# _, r #) -> fromElems stepsASBS 0# r
    {-# INLINE [1] iwmapI #-}

    ewgenI df = case uniqueOrCumulDims df of
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
             in if isTrue# (lenASBSB ==# 0#)
                then fromElems stepsASBS 0# (error "Empty DataFrame (DF0)")
                else case runRW#
                  ( \s0 -> case newByteArray# lenASBSB s0 of
                    (# s1, mba #) -> unsafeFreezeByteArray# mba ( go mba 0# s1 )
                  ) of (# _, r #) -> fromElems stepsASBS 0# r
    {-# INLINE [1] ewgenI #-}

    iwgenI f
        | stepsAS <- cumulDims $ dims @as
        , stepsBS <- cumulDims $ dims @bs
        , stepsASBS <- stepsAS <> stepsBS
        , elS       <- byteSize @t undefined
        , lenBSB    <- cdTotalDim# stepsBS *# elS
        , lenASBSB  <- cdTotalDim# stepsASBS *# elS
          = let go _ [] _ s = s
                go mba (i:is) destOffB s
                  = go mba is (destOffB +# lenBSB) (writeBytes mba destOffB (f i) s)
            in if isTrue# (lenASBSB ==# 0#)
               then fromElems stepsASBS 0# (error "Empty DataFrame (DF0)")
               else case runRW#
                ( \s0 -> case newByteArray# lenASBSB s0 of
                  (# s1, mba #) -> unsafeFreezeByteArray# mba
                                     ( go mba [minBound..maxBound] 0# s1 )
                ) of (# _, r #) -> fromElems stepsASBS 0# r
    {-# INLINE [1] iwgenI #-}

    ewfoldlI f x0 df = case uniqueOrCumulDims df of
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
                    = go (sourceOffE +# lenBS) $! f x (indexOffsetI (I# sourceOffE) df)
             in  go 0# x0

    iwfoldlI f x0 df = case uniqueOrCumulDims df of
      Left a ->
        let b = broadcast a
            go [] x     = x
            go (i:is) x = go is $! f i x b
        in  go [minBound..maxBound] x0
      Right stepsASBS
        | stepsBS <- dropPref (dims @as) stepsASBS
        , lenBS   <- cdTotalDim# stepsBS
          -> let go [] _ x = x
                 go (i:is) sourceOffE x
                    = go is (sourceOffE +# lenBS) $! f i x (indexOffsetI (I# sourceOffE) df)
             in  go [minBound..maxBound] 0# x0

    ewfoldrI f x0 df = case uniqueOrCumulDims df of
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
                    = go (sourceOffE -# lenBS) $! f (indexOffsetI (I# sourceOffE) df) x
             in  go (lenASBS -# lenBS) x0


    iwfoldrI f x0 df = case uniqueOrCumulDims df of
      Left a ->
        let b = broadcast a
            go [] x     = x
            go (i:is) x = go is $! f i b x
        in  go [maxBound, pred maxBound .. minBound] x0
      Right stepsASBS
        | stepsBS <- dropPref (dims @as) stepsASBS
        , lenBS   <- cdTotalDim# stepsBS
        , lenASBS <- cdTotalDim# stepsASBS
          -> let go [] _ x = x
                 go (i:is) sourceOffE x
                    = go is (sourceOffE -# lenBS) $! f i (indexOffsetI (I# sourceOffE) df) x
             in  go [maxBound, pred maxBound .. minBound] (lenASBS -# lenBS) x0

    elementWiseI :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
                 . ( Applicative f
                   , SubSpace s as bs' asbs'
                   )
                => (DataFrame s bs' -> f (DataFrame t bs))
                -> DataFrame s asbs' -> f (DataFrame t asbs)
    elementWiseI = indexWiseI . const
    {-# INLINE [1] elementWiseI #-}

    indexWiseI :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
               . ( Applicative f
                 , SubSpace s as bs' asbs'
                 )
              => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
              -> DataFrame s asbs' -> f (DataFrame t asbs)
    indexWiseI f df = runWithState <$> iwfoldlI applyF (pure initialState) df
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
        initialState s0
          | isTrue# (rezLength# ==# 0#) = (# s0, (# error "Empty DataFrame (DF0)", 0# #) #)
          | otherwise = case newByteArray# (rezLength# *# rezElBSize#) s0 of
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

        -- Apply applicative functor on each chunk and updateI a state.
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



-- | Apply an applicative functor on each element with its indexI
--     (Lens-like indexIed traversal)
indexWise_ :: forall t as bs asbs f b
            . (SubSpace t as bs asbs, Applicative f)
           => (Idxs as -> DataFrame t bs -> f b)
           -> DataFrame t asbs -> f ()
indexWise_ f = iwfoldr (\i -> (*>) . f i) (pure ())
{-# INLINE indexWise_ #-}

-- | Apply an applicative functor on each element (Lens-like traversal)
elementWise_ :: forall t as bs asbs f b
              . (SubSpace t as bs asbs, Applicative f)
             => (DataFrame t bs -> f b)
             -> DataFrame t asbs -> f ()
elementWise_ f = ewfoldr ((*>) . f) (pure ())
{-# INLINE elementWise_ #-}


-- | Apply a functor over a single element (simple lens)
element :: forall t as bs asbs f
         . (SubSpace t as bs asbs, Applicative f)
        => Idxs as
        -> (DataFrame t bs -> f (DataFrame t bs))
        -> DataFrame t asbs -> f (DataFrame t asbs)
element i f df = flip (updateI i) df <$> f (indexI i df)
{-# INLINE element #-}

-- | Index an element (reverse arguments of `indexI`)
(.!) :: SubSpace t as bs asbs
     => DataFrame t asbs -> Idxs as -> DataFrame t bs
(.!) = flip index
{-# INLINE (.!) #-}
infixl 4 .!


ewfoldMap :: forall t as bs asbs m
           . (Monoid m, SubSpace t as bs asbs)
          => (DataFrame t bs -> m) -> DataFrame t asbs -> m
ewfoldMap f = ewfoldlI (\m b -> m `seq` (mappend m $! f b)) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: forall t as bs asbs m
           . (Monoid m, SubSpace t as bs asbs)
          => (Idxs as -> DataFrame t bs -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldlI (\i m b -> m `seq` (mappend m $! f i b)) mempty
{-# INLINE iwfoldMap #-}


-- | Zip two spaces on a specified subspace indexI-wise (with indexI)
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
iwzip f dft dfs = iwmapI g dft
  where
    g i dft' = f i dft' (indexI i dfs)
{-# INLINE iwzip #-}

-- | Zip two spaces on a specified subspace element-wise (without indexI)
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




dropPref :: Dims (ns :: [Nat]) -> CumulDims -> CumulDims
dropPref ds = CumulDims . drop (length $ listDims ds) . unCumulDims


unSc :: DataFrame (t :: Type) ('[] :: [Nat]) -> t
unSc = unsafeCoerce#

{-# RULES
"ewgenI/broadcast" ewgenI = broadcast . unSc

  #-}
