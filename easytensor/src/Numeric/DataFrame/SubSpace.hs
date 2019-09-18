{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
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
  ( SubSpace (), (.!), element
  , joinDataFrame, indexOffset, updateOffset
  , index, update, slice, updateSlice
  , ewgen, iwgen, ewmap, iwmap, ewzip, iwzip
  , elementWise, elementWise_, indexWise, indexWise_
  , ewfoldl, ewfoldl', ewfoldr, ewfoldr', ewfoldMap
  , iwfoldl, iwfoldl', iwfoldr, iwfoldr', iwfoldMap
  ) where

import GHC.Exts (Int (..), (+#))

import Control.Monad
import Control.Monad.ST
import Data.Kind
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions

-- | Flatten a nested DataFrame, analogous to `Control.Monad.join`.
joinDataFrame ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => DataFrame (DataFrame t bs) as -> DataFrame t asbs
joinDataFrame = joinDataFrameI @_ @t @as @bs @asbs
{-# INLINE[1] joinDataFrame #-}


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

-- | Unsafely update a sub-dataframe by its primitive element offset.
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

-- | Get an element by its index in the dataframe.
index ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => Idxs as -> DataFrame t asbs -> DataFrame t bs
index = indexI @_ @t @as @bs @asbs
{-# INLINE[1] index #-}

-- | Get a few contiguous elements.
--
--   In a sense, this is just a more complicated version of `index`.
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
--   In a sense, this is just a more complicated version of `update`.
updateSlice ::
       forall t as bs asbs bi bd b' bs'
     . ( SubSpace t as bs asbs
       , SubFrameIndexCtx b' bi bd
       , bs ~ (b' :+ bs'), KnownDim bd )
    => Idxs (as +: bi) -> DataFrame t (bd :+ bs')
    -> DataFrame t asbs -> DataFrame t asbs
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

-- | Map a function over each element with its index of DataFrame.
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

-- | Generate a DataFrame by iterating a function (index -> element).
iwgen ::
       forall t as bs asbs
     . (SubSpace t as bs asbs, Dimensions as)
    => (Idxs as -> DataFrame t bs) -> DataFrame t asbs
iwgen = iwgenI @_ @t @as @bs @asbs
{-# INLINE[1] iwgen #-}

-- | Left-associative lazy fold of a DataFrame.
--   Same rules apply as for `foldl`.
ewfoldl ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
ewfoldl = ewfoldlI @_ @t @as @bs @asbs @b
{-# INLINE[1] ewfoldl #-}

-- | Left-associative strict fold of a DataFrame.
--   Same rules apply as for `foldl'`.
ewfoldl' ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
ewfoldl' f z0 xs = ewfoldr f' id xs z0
  where
    f' x k z = k $! f z x
{-# INLINE ewfoldl' #-}

-- | Left-associative lazy fold of a DataFrame with an index.
--   Same rules apply as for `foldl`.
iwfoldl ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
iwfoldl = iwfoldlI @_ @t @as @bs @asbs @b
{-# INLINE[1] iwfoldl #-}

-- | Left-associative strict fold of a DataFrame with an index.
--   Same rules apply as for `foldl'`.
iwfoldl' ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
iwfoldl' f z0 xs = iwfoldr f' id xs z0
  where
    f' i x k z = k $! f i z x
{-# INLINE[1] iwfoldl' #-}

-- | Right-associative lazy fold of a DataFrame.
--   Same rules apply as for `foldr`.
ewfoldr ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
ewfoldr = ewfoldrI @_ @t @as @bs @asbs @b
{-# INLINE[1] ewfoldr #-}

-- | Right-associative strict fold of a DataFrame.
--   Same rules apply as for `foldr'`.
ewfoldr' ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
ewfoldr' f z0 xs = ewfoldl f' id xs z0
  where
    f' k x z = k $! f x z
{-# INLINE ewfoldr' #-}

-- | Right-associative lazy fold of a DataFrame with an index.
--   Same rules apply as for `foldr`.
iwfoldr ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
iwfoldr = iwfoldrI @_ @t @as @bs @asbs @b
{-# INLINE[1] iwfoldr #-}

-- | Right-associative strict fold of a DataFrame with an index.
--   Same rules apply as for `foldr'`.
iwfoldr' ::
       forall t as bs asbs b
     . SubSpace t as bs asbs
    => (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
iwfoldr' f z0 xs = iwfoldl f' id xs z0
  where
    f' i k x z = k $! f i x z
{-# INLINE[1] iwfoldr' #-}

-- | Apply an applicative functor on each element (Lens-like traversal).
elementWise ::
       forall t as bs asbs s bs' asbs' f
     . (SubSpace t as bs asbs, SubSpace s as bs' asbs', Applicative f)
    => (DataFrame s bs' -> f (DataFrame t bs))
    -> DataFrame s asbs' -> f (DataFrame t asbs)
elementWise = elementWiseI @_ @t @as @bs @asbs @s @bs' @asbs' @f
{-# INLINE[1] elementWise #-}

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal).
indexWise ::
       forall t as bs asbs s bs' asbs' f
     . (SubSpace t as bs asbs, SubSpace s as bs' asbs', Applicative f)
    => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
    -> DataFrame s asbs' -> f (DataFrame t asbs)
indexWise = indexWiseI @_ @t @as @bs @asbs @s @bs' @asbs' @f
{-# INLINE[1] indexWise #-}

-- | Apply an applicative functor on each element (Lens-like traversal)
elementWise_ ::
       forall t as bs asbs f b
     . (SubSpace t as bs asbs, Applicative f)
    => (DataFrame t bs -> f b) -> DataFrame t asbs -> f ()
elementWise_ f = ewfoldr ((*>) . f) (pure ())
{-# INLINE elementWise_ #-}

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal)
indexWise_ ::
       forall t as bs asbs f b
    . (SubSpace t as bs asbs, Applicative f)
   => (Idxs as -> DataFrame t bs -> f b) -> DataFrame t asbs -> f ()
indexWise_ f = iwfoldr (\i -> (*>) . f i) (pure ())
{-# INLINE indexWise_ #-}

-- | Apply a functor over a single element (simple lens)
element ::
       forall t as bs asbs f
     . (SubSpace t as bs asbs, Applicative f)
    => Idxs as
    -> (DataFrame t bs -> f (DataFrame t bs))
    -> DataFrame t asbs -> f (DataFrame t asbs)
element i f df = flip (update i) df <$> f (index i df)
{-# INLINE element #-}

-- | Index an element (reverse arguments of `index`)
(.!) :: SubSpace t as bs asbs
     => DataFrame t asbs -> Idxs as -> DataFrame t bs
(.!) = flip index
{-# INLINE (.!) #-}
infixl 4 .!

-- | Map each element of the DataFrame to a monoid,
--    and combine the results.
ewfoldMap ::
       forall t as bs asbs m
     . (SubSpace t as bs asbs, Monoid m)
    => (DataFrame t bs -> m) -> DataFrame t asbs -> m
ewfoldMap f = ewfoldr (mappend . f) mempty
{-# INLINE ewfoldMap #-}

-- | Map each element of the DataFrame and its index to a monoid,
--    and combine the results.
iwfoldMap ::
       forall t as bs asbs m
     . (SubSpace t as bs asbs, Monoid m)
    => (Idxs as -> DataFrame t bs -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldr (\i -> mappend . f i) mempty
{-# INLINE iwfoldMap #-}

-- | Zip two spaces on a specified subspace element-wise (without index)
ewzip :: forall t as bs asbs l bsL asbsL r bsR asbsR
     . (SubSpace t as bs asbs, SubSpace l as bsL asbsL, SubSpace r as bsR asbsR)
    => (DataFrame l bsL -> DataFrame r bsR -> DataFrame t bs)
    -> DataFrame l asbsL -> DataFrame r asbsR -> DataFrame t asbs
ewzip = iwzip . const
{-# INLINE ewzip #-}

-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip :: forall t as bs asbs l bsL asbsL r bsR asbsR
     . (SubSpace t as bs asbs, SubSpace l as bsL asbsL, SubSpace r as bsR asbsR)
    => (Idxs as -> DataFrame l bsL -> DataFrame r bsR -> DataFrame t bs)
    -> DataFrame l asbsL -> DataFrame r asbsR -> DataFrame t asbs
iwzip f dft dfs = iwmap g dft
  where
    g i dft' = f i dft' (index i dfs)
{-# INLINE iwzip #-}

-- | Operations on DataFrames
--
-- @as@ is an indexing dimensionality
--
-- @bs@ is an element dimensionality
--
-- @t@ is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatList as bs asbs
      , BoundedDims as
      , Dimensions  bs
      , BoundedDims asbs
      , PrimArray t (DataFrame t bs)
      ) => SubSpace (t :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where

    -- | Flatten a nested DataFrame, analogous to `Control.Monad.join`.
    joinDataFrameI :: DataFrame (DataFrame t bs) as -> DataFrame t asbs

    -- | Unsafely get a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element)
    --   size (aka `totalDim` of sub dataframe).
    --
    indexOffsetI :: Int -- ^ Prim element offset
                 -> DataFrame t asbs -> DataFrame t bs

    -- | Unsafely update a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element)
    --   size (aka `totalDim` of sub dataframe).
    --
    updateOffsetI :: Int -- ^ Prim element offset
                  -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs

    -- | Get an element by its index in the dataframe.
    indexI :: Idxs as -> DataFrame t asbs -> DataFrame t bs

    -- | Get a few contiguous elements.
    --
    --   In a sense, this is just a more complicated version of `index`.
    sliceI :: forall (bi :: k) (bd :: k) (b' :: k) (bs' :: [k])
            . (SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd)
           => Idxs (as +: bi) -> DataFrame t asbs -> DataFrame t (bd :+ bs')

    -- | Set a new value to an element.
    updateI :: Idxs as -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs

    -- | Update a few contiguous elements.
    --
    --   In a sense, this is just a more complicated version of `update`.
    updateSliceI :: forall (bi :: k) (bd :: k) (b' :: k) (bs' :: [k])
                  . (SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd)
                 => Idxs (as +: bi) -> DataFrame t (bd :+ bs')
                 -> DataFrame t asbs -> DataFrame t asbs

    -- | Map a function over each element of DataFrame.
    ewmapI  :: forall s (bs' :: [k]) (asbs' :: [k])
             . SubSpace s as bs' asbs'
            => (DataFrame s bs' -> DataFrame t bs)
            -> DataFrame s asbs' -> DataFrame t asbs

    -- | Map a function over each element with its index of DataFrame.
    iwmapI  :: forall s (bs' :: [k]) (asbs' :: [k])
             . SubSpace s as bs' asbs'
            => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
            -> DataFrame s asbs' -> DataFrame t asbs

    -- | Generate a DataFrame by repeating an element.
    ewgenI :: Dimensions as => DataFrame t bs -> DataFrame t asbs

    -- | Generate a DataFrame by iterating a function (index -> element).
    iwgenI :: Dimensions as => (Idxs as -> DataFrame t bs) -> DataFrame t asbs

    -- | Left-associative fold of a DataFrame.
    --   Same rules apply as for `foldl`.
    ewfoldlI :: (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b

    -- | Left-associative fold of a DataFrame with an index.
    --   Same rules apply as for `foldl`.
    iwfoldlI :: (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b

    -- | Right-associative fold of a DataFrame.
    --   Same rules apply as for `foldr`.
    ewfoldrI :: (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b

    -- | Right-associative fold of a DataFrame with an index.
    --   Same rules apply as for `foldr`.
    iwfoldrI :: (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b

    -- | Apply an applicative functor on each element (Lens-like traversal).
    elementWiseI :: forall (s :: Type) (bs' :: [k]) (asbs' :: [k]) (f :: Type -> Type)
                  . (Applicative f, SubSpace s as bs' asbs')
                 => (DataFrame s bs' -> f (DataFrame t bs))
                 -> DataFrame s asbs' -> f (DataFrame t asbs)
    elementWiseI = indexWiseI . const
    {-# INLINE elementWiseI #-}

    -- | Apply an applicative functor on each element with its index
    --     (Lens-like indexed traversal).
    indexWiseI :: forall (s :: Type) (bs' :: [k]) (asbs' :: [k]) (f :: Type -> Type)
                . (Applicative f, SubSpace s as bs' asbs')
               => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
               -> DataFrame s asbs' -> f (DataFrame t asbs)



instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , PrimBytes   (DataFrame t bs)
         , PrimArray t (DataFrame t bs)
         , PrimArray t (DataFrame t asbs)
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where

    joinDataFrameI asbs
      | Dict <- inferKnownBackend @(DataFrame t bs) @as
        = case arrayContent# asbs of
      (# bs | #) -> ewgenI bs
      (# | (# steps, off, ba #) #)
                 -> fromElems (steps `mappend` cumulDims (dims @bs)) off ba

    indexOffsetI (I# off) asbs = case arrayContent# asbs of
      (# a | #) -> broadcast a
      (# | (# steps, off0, ba #) #)
                -> fromElems (dropPref (dims @as) steps) (off0 +# off) ba
    {-# INLINE indexOffsetI #-}

    updateOffsetI off bs asbs = runST $ do
        asbsM <- case uniqueOrCumulDims asbs of
          Left _ | Dict <- inferKnownBackend @t @asbs
                  -> thawDataFrame asbs
          Right _ -> uncheckedThawDataFrame asbs
        copyDataFrameOff off bs asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE updateOffsetI #-}

    indexI i asbs = case arrayContent# asbs of
      (# a | #) -> broadcast a
      (# | (# steps, off0, ba #) #)
         | I# off <- cdIx steps i
                -> fromElems (dropPref (dims @as) steps) (off0 +# off) ba
    {-# INLINE indexI #-}

    sliceI  i df
      | _ :* bs'Dims@Dims <- dims @bs
      , (ds :: Dims (bd ': bs')) <- D :* bs'Dims
      , Dict <- inferKnownBackend @t @(bd ': bs')
        = case arrayContent# df of
        (# a | #)
             -> broadcast a `inSpaceOf` ds
        (# | (# steps, off0, ba #) #)
           | I# off <- cdIx steps i
           , bsteps <- repHead (dimVal' @bd) (dropPref (dims @as) steps)
             -> fromElems bsteps (off0 +# off) ba
      | otherwise = error "SubSpace/sliceI: impossible arguments"
      where
        repHead y (CumulDims (_:x:xs)) = CumulDims (y*x:x:xs)
        repHead _ steps                = steps
    {-# INLINE sliceI #-}

    updateI i bs asbs = runST $ do
        asbsM <- case uniqueOrCumulDims asbs of
          Left _ | Dict <- inferKnownBackend @t @asbs
                  -> thawDataFrame asbs
          Right _ -> uncheckedThawDataFrame asbs
        copyDataFrame' i bs asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE updateI #-}

    updateSliceI i (bs :: DataFrame t (bd :+ bs')) asbs
      | _ :* Dims <- dims @bs
      , Dict <- inferKnownBackend @t @(bd :+ bs')
        = runST $ do
        asbsM <- case uniqueOrCumulDims asbs of
          Left _ | Dict <- inferKnownBackend @t @asbs
                  -> thawDataFrame asbs
          Right _ -> uncheckedThawDataFrame asbs
        copyDataFrame i bs asbsM
        unsafeFreezeDataFrame asbsM
      | otherwise = error "SubSpace/updateSliceI: impossible pattern"
    {-# INLINE updateSliceI #-}

    ewmapI (f :: DataFrame s bs' -> DataFrame t bd) (asbs' :: DataFrame s asbs')
      | Dict <- inferKnownBackend @s @asbs'
      = case uniqueOrCumulDims asbs' of
      Left e  -> ewgen (f (broadcast e))
      Right _ -> runST $ do
        let asTD  = fromIntegral (totalDim' @as ) :: Int
            bsTD  = fromIntegral (totalDim' @bs ) :: Int
            bs'TD = fromIntegral (totalDim' @bs') :: Int
        asbsM <- newDataFrame
        forM_ [0..asTD-1] $ \i ->
          copyDataFrameOff (i*bsTD) (f (indexOffset (i*bs'TD) asbs')) asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE ewmapI #-}

    iwmapI f (asbs' :: DataFrame s asbs')
      | Dict <- inferKnownBackend @s @asbs'
      = case uniqueOrCumulDims asbs' of
      Left e  -> iwgen (flip f (broadcast e))
      Right _ -> runST $ do
        asbsM <- newDataFrame
        forM_ [minBound..maxBound] $ \i ->
          copyDataFrame' i (f i (index i asbs')) asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE iwmapI #-}

    ewgenI bs = case uniqueOrCumulDims bs of
      Left a -> broadcast a
      Right bsSteps -> runST $ do
        let asTD = fromIntegral (totalDim' @as) :: Int
            bsTD = fromIntegral (cdTotalDim bsSteps)
        asbsM <- newDataFrame
        forM_ [0, bsTD .. asTD * bsTD  - 1] $ \o -> copyDataFrameOff o bs asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE ewgenI #-}

    iwgenI f = runST $ do
        asbsM <- newDataFrame
        forM_ [minBound..maxBound] $ \i -> copyDataFrame' i (f i) asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE iwgenI #-}

    ewfoldlI f x0 asbs = case uniqueOrCumulDims asbs of
      Left a ->
        let b = broadcast a
            go i | i < 0     = x0
                 | otherwise = f (go (i-1)) b
        in  go (fromIntegral (totalDim' @as) - 1 :: Int)
      Right asbsSteps ->
        let bsSteps = dropPref (dims @as) asbsSteps
            bsLen   = min 1 (fromIntegral (cdTotalDim bsSteps)) :: Int
            asbsLen = fromIntegral (cdTotalDim asbsSteps) :: Int
            go i | i < 0     = x0
                 | otherwise = f (go (i - bsLen)) (indexOffset i asbs)
        in  go (asbsLen - bsLen)
    {-# INLINE ewfoldlI #-}

    iwfoldlI f x0 asbs
      | not (nonVoidDims (dims @as)) = x0
      | otherwise = case uniqueOrCumulDims asbs of
        Left a ->
          let b = broadcast a
              go i | i == minBound = f i x0 b
                   | otherwise     = f i (go (pred i)) b
          in  go maxBound
        Right _ ->
          let go i | i == minBound = f i x0 (index i asbs)
                   | otherwise     = f i (go (pred i)) (index i asbs)
          in  go maxBound
    {-# INLINE iwfoldlI #-}

    ewfoldrI f x0 asbs = case uniqueOrCumulDims asbs of
        Left a ->
          let b = broadcast a
              go i | i >= totalDim' @as = x0
                   | otherwise          = f b (go (i+1))
          in  go 0
        Right asbsSteps ->
          let bsSteps = dropPref (dims @as) asbsSteps
              bsLen   = min 1 (fromIntegral (cdTotalDim bsSteps)) :: Int
              asbsLen = fromIntegral (cdTotalDim asbsSteps) :: Int
              go i | i >= asbsLen = x0
                   | otherwise    = f (indexOffset i asbs) (go (i + bsLen))
          in  go 0
    {-# INLINE ewfoldrI #-}

    iwfoldrI f x0 asbs
      | not (nonVoidDims (dims @as)) = x0
      | otherwise = case uniqueOrCumulDims asbs of
        Left a ->
          let b = broadcast a
              go i | i == maxBound = f i b x0
                   | otherwise     = f i b (go (succ i))
          in  go minBound
        Right _ ->
          let go i | i == maxBound = f i (index i asbs) x0
                   | otherwise     = f i (index i asbs) (go (succ i))
          in  go minBound
    {-# INLINE iwfoldrI #-}

    indexWiseI (f :: Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
        = fmap mkSTDF . iwfoldr fST (pure $ MakingDF newDataFrame)
      where
        mkSTDF :: MakingDF t asbs -> DataFrame t asbs
        mkSTDF st = runST (getMDF st >>= unsafeFreezeDataFrame)
        fST :: Idxs as -> DataFrame s bs'
            -> f (MakingDF t asbs) -> f (MakingDF t asbs)
        fST i bs' asbsSTF
          = (\st r -> MakingDF (getMDF st >>= \x -> x <$ copyDataFrame' i r x))
             <$> asbsSTF <*> f i bs'
    {-# INLINE indexWiseI #-}

-- | Hide ST monad state with a DataFrame in works inside a plain type.
newtype MakingDF t asbs
  = MakingDF { getMDF :: forall s . ST s (STDataFrame s t asbs) }

-- | Checks if all of the dimensions are non-zero.
nonVoidDims :: Dims ns -> Bool
nonVoidDims = all (0 <) . listDims

dropPref :: Dims (ns :: [Nat]) -> CumulDims -> CumulDims
dropPref ds = CumulDims . drop (length $ listDims ds) . unCumulDims
