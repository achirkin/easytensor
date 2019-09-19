{-# LANGUAGE AllowAmbiguousTypes     #-}
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
  ( SubSpace (SubSpaceCtx), (.!), element
  , joinDataFrame, indexOffset, updateOffset
  , index, update, slice, updateSlice
  , ewgen, iwgen, ewmap, iwmap, ewzip, iwzip
  , elementWise, elementWise_, indexWise, indexWise_
  , ewfoldl, ewfoldl', ewfoldr, ewfoldr', ewfoldMap
  , iwfoldl, iwfoldl', iwfoldr, iwfoldr', iwfoldMap
  ) where

import GHC.Exts (Int (..), (+#))

import           Control.Monad
import           Control.Monad.ST
import           Data.Kind
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.DataFrame.ST
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import qualified Numeric.TypedList                    as TL
import           Unsafe.Coerce

-- | Flatten a nested DataFrame, analogous to `Control.Monad.join`.
joinDataFrame ::
       forall t as bs asbs
     . (SubSpace t as bs asbs, PrimBytes (DataFrame t bs))
    => DataFrame (DataFrame t bs) as -> DataFrame t asbs
joinDataFrame = joinDataFrameI @_ @t @as @bs @asbs
{-# INLINE[1] joinDataFrame #-}


-- | Unsafely get a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
indexOffset ::
       forall t as bs asbs
     . SubSpace t as bs asbs
    => Int -- ^ Prim element offset
    -> DataFrame t asbs -> DataFrame t bs
indexOffset = indexOffsetI @_ @t @as @bs @asbs
{-# INLINE[1] indexOffset #-}

-- | Unsafely update a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
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
       , bs ~ (b' :+ bs'), KnownDim bd
       , PrimArray t (DataFrame t (bd :+ bs')))
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
       , bs ~ (b' :+ bs'), KnownDim bd
       , PrimArray t (DataFrame t (bd :+ bs')))
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
      , SubSpaceCtx t as bs asbs
      , PrimBytes t
      ) => SubSpace (t :: Type) (as :: [k]) (bs :: [k]) (asbs :: [k])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    type SubSpaceCtx t as bs asbs :: Constraint

    joinDataFrameI :: PrimBytes (DataFrame t bs)
                   => DataFrame (DataFrame t bs) as -> DataFrame t asbs
    indexOffsetI :: Int -> DataFrame t asbs -> DataFrame t bs
    updateOffsetI :: Int -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
    indexI :: Idxs as -> DataFrame t asbs -> DataFrame t bs
    sliceI :: forall (bi :: k) (bd :: k) (b' :: k) (bs' :: [k])
            . ( SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd
              , PrimArray t (DataFrame t (bd :+ bs')))
           => Idxs (as +: bi) -> DataFrame t asbs -> DataFrame t (bd :+ bs')
    updateI :: Idxs as -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
    updateSliceI :: forall (bi :: k) (bd :: k) (b' :: k) (bs' :: [k])
                  . ( SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd
                    , PrimArray t (DataFrame t (bd :+ bs')))
                 => Idxs (as +: bi) -> DataFrame t (bd :+ bs')
                 -> DataFrame t asbs -> DataFrame t asbs
    ewmapI  :: forall s (bs' :: [k]) (asbs' :: [k])
             . SubSpace s as bs' asbs'
            => (DataFrame s bs' -> DataFrame t bs)
            -> DataFrame s asbs' -> DataFrame t asbs
    iwmapI  :: forall s (bs' :: [k]) (asbs' :: [k])
             . SubSpace s as bs' asbs'
            => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
            -> DataFrame s asbs' -> DataFrame t asbs
    ewgenI :: Dimensions as => DataFrame t bs -> DataFrame t asbs
    iwgenI :: Dimensions as => (Idxs as -> DataFrame t bs) -> DataFrame t asbs
    ewfoldlI :: (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
    iwfoldlI :: (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
    ewfoldrI :: (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
    iwfoldrI :: (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
    elementWiseI :: forall (s :: Type) (bs' :: [k]) (asbs' :: [k]) (f :: Type -> Type)
                  . (Applicative f, SubSpace s as bs' asbs')
                 => (DataFrame s bs' -> f (DataFrame t bs))
                 -> DataFrame s asbs' -> f (DataFrame t asbs)
    elementWiseI = indexWiseI . const
    {-# INLINE elementWiseI #-}
    indexWiseI :: forall (s :: Type) (bs' :: [k]) (asbs' :: [k]) (f :: Type -> Type)
                . (Applicative f, SubSpace s as bs' asbs')
               => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
               -> DataFrame s asbs' -> f (DataFrame t asbs)



instance ( ConcatList as bs asbs
         , SubSpaceCtx t as bs asbs
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where
    type SubSpaceCtx t as bs asbs
      = ( Dimensions as, Dimensions bs, Dimensions asbs
        , PrimArray t (DataFrame t asbs), PrimArray t (DataFrame t bs))

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

    sliceI i df = case arrayContent# df of
      (# a | #)
           -> broadcast a
      (# | (# steps, off0, ba #) #)
         | I# off <- cdIx steps i
           -> let r = fromElems bsteps (off0 +# off) ba
                  bsteps = repHead (dimVal bd) (dropPref (dims @as) steps)
                  bd = getBD r
              in  r
      where
        getBD :: KnownDim bd => DataFrame t (bd ': bs') -> Dim bd
        getBD _ = D
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

    updateSliceI i bs asbs = runST $ do
        asbsM <- case uniqueOrCumulDims asbs of
          Left _ | Dict <- inferKnownBackend @t @asbs
                  -> thawDataFrame asbs
          Right _ -> uncheckedThawDataFrame asbs
        copyDataFrame i bs asbsM
        unsafeFreezeDataFrame asbsM
    {-# INLINE updateSliceI #-}

    ewmapI (f :: DataFrame s bs' -> DataFrame t bs) (asbs' :: DataFrame s asbs')
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
            bsLen   = max 1 (fromIntegral (cdTotalDim bsSteps)) :: Int
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
              bsLen   = max 1 (fromIntegral (cdTotalDim bsSteps)) :: Int
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
        = fmap mkSTDF . iwfoldr fST (pure $ MakingDF (const (pure ())))
      where
        mkSTDF :: MakingDF t asbs -> DataFrame t asbs
        mkSTDF st = runST $ do
          df <- newDataFrame
          getMDF st df
          unsafeFreezeDataFrame df
        fST :: Idxs as -> DataFrame s bs'
            -> f (MakingDF t asbs) -> f (MakingDF t asbs)
        fST i bs' asbsSTF
          = (\r st -> MakingDF (\x -> copyDataFrame' i r x *> getMDF st x))
             <$> f i bs' <*> asbsSTF
    {-# INLINE indexWiseI #-}

-- | Hide ST monad state with a DataFrame in works inside a plain type.
newtype MakingDF t asbs
  = MakingDF { getMDF :: forall s . STDataFrame s t asbs -> ST s () }

-- | Checks if all of the dimensions are non-zero.
nonVoidDims :: Dims ns -> Bool
nonVoidDims = all (0 <) . listDims

dropPref :: Dims (ns :: [Nat]) -> CumulDims -> CumulDims
dropPref ds = CumulDims . dropSome (listDims ds) . unCumulDims
  where
    dropSome :: [Word] -> [Word] -> [Word]
    dropSome []     xs = xs
    dropSome (_:as) xs = dropSome as (tail xs)

instance ( ConcatList as bs asbs
         , SubSpaceCtx t as bs asbs
         ) => SubSpace t (as :: [XNat]) (bs :: [XNat]) (asbs :: [XNat]) where
    type SubSpaceCtx t as bs asbs
      = ( Dimensions bs, PrimBytes t -- , PrimArray t (DataFrame t (DimsBound bs))
        , KnownXNatTypes as, KnownXNatTypes bs, KnownXNatTypes asbs)

    joinDataFrameI (XFrame (df :: DataFrame (DataFrame t bs) asN)) =
      let as   = XDims (dims @asN) :: Dims as
          bs   = dims :: Dims bs
          asbs = concatDims as bs :: Dims asbs
      in withLiftedConcatList @('Just asN) @('Just (DimsBound bs)) @'Nothing
                              as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) -> case () of
            _ | Dict <- inferKnownBackend @t @bsn
              , Dict <- inferKnownBackend @t @asbsn
                -> XFrame @_ @t @asbs @asbsn $ case arrayContent# df of
                    (# XFrame e | #)
                      -> ewgen @t @asn @bsn @asbsn (unsafeCoerce e)
                              -- know for sure its DataFrame t bsn
                    (# | (# steps, off, ba #) #)
                      -> fromElems (steps `mappend` cumulDims (dims @bs)) off ba
    {-# INLINE joinDataFrameI #-}

    indexOffsetI off (XFrame (df :: DataFrame t asbsN)) =
      let bs = dims :: Dims bs
          as = dropSufDims bs asbs :: Dims as
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @('Just (DimsBound bs)) @('Just asbsN)
                              as bs asbs $
           \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn)
             -> case inferKnownBackend @t @bsn of
               Dict -> XFrame (indexOffset @t @asn @bsn @asbsn off df)
    {-# INLINE indexOffsetI #-}

    updateOffsetI off (XFrame (bsDf :: DataFrame t bsN))
                      (XFrame (asbsDf :: DataFrame t asbsN)) =
      let bs = dims :: Dims bs
          as = dropSufDims bs asbs :: Dims as
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @('Just bsN) @('Just asbsN)
                              as bs asbs $
           \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn)
             -> XFrame (updateOffset @t @asn @bsn @asbsn off bsDf asbsDf)
    {-# INLINE updateOffsetI #-}

    indexI i (XFrame (df :: DataFrame t asbsN)) =
      let bs = dims :: Dims bs
          as = dropSufDims bs asbs :: Dims as
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN)
                              as bs asbs $
           \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn)
             -> case inferKnownBackend @t @bsn of
               Dict -> XFrame (index @t @asn @bsn @asbsn (unsafeCoerce i) df)
    {-# INLINE indexI #-}

    sliceI ::
           forall (bi :: XNat) (bd :: XNat) (b' :: XNat) (bs' :: [XNat])
         . ( SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd
           , PrimArray t (DataFrame t (bd :+ bs')))
        => Idxs (as +: bi) -> DataFrame t asbs -> DataFrame t (bd :+ bs')
    sliceI i (XFrame (df :: DataFrame t asbsN))
            | bs@(XDims (_ :* Dims :: Dims bsN)) <- dims @bs
            , asbs <- XDims (dims @asbsN) :: Dims asbs
            , as <- dropSufDims bs asbs :: Dims as
      = withLiftedConcatList @'Nothing @('Just bsN) @('Just asbsN) as bs asbs $
           \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn)
             -> withKnownXDim @bd $ case unsafeEqTypes @_ @(Head bsn) @(DimBound b') of
               Dict
                | Dict <- inferKnownBackend @t @(DimBound bd ': Tail bsn)
                , Dict <- inferKnownBackend @t @bsN
                 -> XFrame (slice @t @asn @bsn @asbsn
                                 @(DimBound bi) @(DimBound bd) @(Head bsn)
                                 @(Tail bsn) (unsafeCoerce i) df)
    sliceI _ _ = error "SubSpace[XNat]/sliceI -- impossible pattern"
    {-# INLINE sliceI #-}


    updateI i (XFrame (bsDf :: DataFrame t bsN))
              (XFrame (asbsDf :: DataFrame t asbsN)) =
      let bs = dims :: Dims bs
          as = dropSufDims bs asbs :: Dims as
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @('Just bsN) @('Just asbsN)
                              as bs asbs $
           \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn)
             -> XFrame (update @t @asn @bsn @asbsn (unsafeCoerce i) bsDf asbsDf)
    {-# INLINE updateI #-}

    updateSliceI :: forall (bi :: XNat) (bd :: XNat) (b' :: XNat) (bs' :: [XNat])
                  . ( SubFrameIndexCtx b' bi bd, bs ~ (b' :+ bs'), KnownDim bd
                    , PrimArray t (DataFrame t (bd :+ bs')))
                 => Idxs (as +: bi) -> DataFrame t (bd :+ bs')
                 -> DataFrame t asbs -> DataFrame t asbs
    updateSliceI i (XFrame (dbs'Df :: DataFrame t dbs'N))
                   (XFrame (asbsDf :: DataFrame t asbsN))
            | bs@(XDims ((_ :: Dim b'N) :* (Dims :: Dims bs'N) :: Dims bsN)) <- dims @bs
            , (_ :: Dim bdN) :* (_ :: Dims bs'N2) <- dims @dbs'N
            , asbs <- XDims (dims @asbsN) :: Dims asbs
            , as <- dropSufDims bs asbs :: Dims as
      = withLiftedConcatList @'Nothing @('Just bsN) @('Just asbsN) as bs asbs $
           \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn)
             -> withKnownXDim @bd $ case unsafeEqTypes @_ @b'N @(DimBound b') of
               Dict
                | Dict <- unsafeEqTypes @_ @bs'N @bs'N2
                , Dict <- inferKnownBackend @t @bsN
                 -> XFrame (updateSlice @t @asn @bsn @asbsn
                                 @(DimBound bi) @bdN @b'N
                                 @bs'N (unsafeCoerce i) dbs'Df asbsDf)
    updateSliceI _ _ _ = error "SubSpace[XNat]/updateSliceI -- impossible pattern"
    {-# INLINE updateSliceI #-}

    ewmapI  :: forall s (bs' :: [XNat]) (asbs' :: [XNat])
             . SubSpace s as bs' asbs'
            => (DataFrame s bs' -> DataFrame t bs)
            -> DataFrame s asbs' -> DataFrame t asbs
    ewmapI f (XFrame (df :: DataFrame s asbsN')) =
      let bs    = dims :: Dims bs
          bs'   = dims :: Dims bs'
          as    = dropSufDims bs' asbs' :: Dims as
          asbs' = XDims (dims @asbsN') :: Dims asbs'
          asbs  = concatDims as bs :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN')
                              as bs' asbs' $
         \(Dims :: Dims asn) (Dims :: Dims bsn') (Dims :: Dims asbsn') ->
         withLiftedConcatList @('Just asn) @'Nothing @'Nothing
                                as bs asbs $
         \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) ->
           case inferKnownBackend @t @asbsn of
            Dict
             | Dict <- inferKnownBackend @t @bsn
             , Dict <- inferKnownBackend @s @bsn'
             -> XFrame (ewmap @t @asn @bsn @asbsn @s @bsn' @asbsn'
                        (\x -> case f (XFrame x) of XFrame y -> unsafeCoerce y) df)
    {-# INLINE ewmapI #-}

    iwmapI  :: forall s (bs' :: [XNat]) (asbs' :: [XNat])
             . SubSpace s as bs' asbs'
            => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
            -> DataFrame s asbs' -> DataFrame t asbs
    iwmapI f (XFrame (df :: DataFrame s asbsN')) =
      let bs    = dims :: Dims bs
          bs'   = dims :: Dims bs'
          as    = dropSufDims bs' asbs' :: Dims as
          asbs' = XDims (dims @asbsN') :: Dims asbs'
          asbs  = concatDims as bs :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN')
                              as bs' asbs' $
         \(Dims :: Dims asn) (Dims :: Dims bsn') (Dims :: Dims asbsn') ->
         withLiftedConcatList @('Just asn) @'Nothing @'Nothing
                                as bs asbs $
         \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) -> case () of
           _ | Dict <- inferKnownBackend @t @asbsn
             , Dict <- inferKnownBackend @t @bsn
             , Dict <- inferKnownBackend @s @bsn'
               -> XFrame (iwmap @t @asn @bsn @asbsn @s @bsn' @asbsn'
                           (\i x -> case f (unsafeCoerce i) (XFrame x) of
                                      XFrame y -> unsafeCoerce y) df)
    {-# INLINE iwmapI #-}


    ewgenI (XFrame (df :: DataFrame t bsN)) =
      let as   = dims :: Dims as
          bs   = dims :: Dims bs
          asbs = concatDims as bs :: Dims asbs
      in withLiftedConcatList @'Nothing @('Just bsN) @'Nothing as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) ->
            case inferKnownBackend @t @asbsn of
              Dict -> XFrame (ewgen @t @asn @bsn @asbsn df)
    {-# INLINE ewgenI #-}

    iwgenI f =
      let as   = dims :: Dims as
          bs   = dims :: Dims bs
          asbs = concatDims as bs :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @'Nothing as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) -> case () of
            _ | Dict <- inferKnownBackend @t @asbsn
              , Dict <- inferKnownBackend @t @bsn
                 -> XFrame (iwgen @t @asn @bsn @asbsn
                     (\i -> case f (unsafeCoerce i) of
                              XFrame y -> unsafeCoerce y))
    {-# INLINE iwgenI #-}

    ewfoldlI f x0 (XFrame (df :: DataFrame s asbsN)) =
      let as   = dropSufDims bs asbs :: Dims as
          bs   = dims :: Dims bs
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN) as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) ->
           case inferKnownBackend @t @bsn of
             Dict -> ewfoldl @t @asn @bsn @asbsn (\b x -> f b (XFrame x)) x0 df
    {-# INLINE ewfoldlI #-}

    iwfoldlI f x0 (XFrame (df :: DataFrame s asbsN)) =
      let as   = dropSufDims bs asbs :: Dims as
          bs   = dims :: Dims bs
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN) as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) ->
           case inferKnownBackend @t @bsn of
             Dict -> iwfoldl @t @asn @bsn @asbsn
                       (\i b x -> f (unsafeCoerce i) b (XFrame x)) x0 df
    {-# INLINE iwfoldlI #-}

    ewfoldrI f x0 (XFrame (df :: DataFrame s asbsN)) =
      let as   = dropSufDims bs asbs :: Dims as
          bs   = dims :: Dims bs
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN) as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) ->
           case inferKnownBackend @t @bsn of
             Dict -> ewfoldr @t @asn @bsn @asbsn (\x -> f (XFrame x)) x0 df
    {-# INLINE ewfoldrI #-}

    iwfoldrI f x0 (XFrame (df :: DataFrame s asbsN)) =
      let as   = dropSufDims bs asbs :: Dims as
          bs   = dims :: Dims bs
          asbs = XDims (dims @asbsN) :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN) as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) ->
           case inferKnownBackend @t @bsn of
             Dict -> iwfoldr @t @asn @bsn @asbsn
                       (\i x -> f (unsafeCoerce i) (XFrame x)) x0 df
    {-# INLINE iwfoldrI #-}

    indexWiseI :: forall s (bs' :: [XNat]) (asbs' :: [XNat]) f
                . (Applicative f, SubSpace s as bs' asbs')
               => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
               -> DataFrame s asbs' -> f (DataFrame t asbs)
    indexWiseI f (XFrame (df :: DataFrame s asbsN')) =
      let bs    = dims :: Dims bs
          bs'   = dims :: Dims bs'
          as    = dropSufDims bs' asbs' :: Dims as
          asbs' = XDims (dims @asbsN') :: Dims asbs'
          asbs  = concatDims as bs :: Dims asbs
      in withLiftedConcatList @'Nothing @'Nothing @('Just asbsN')
                              as bs' asbs' $
         \(Dims :: Dims asn) (Dims :: Dims bsn') (Dims :: Dims asbsn') ->
         withLiftedConcatList @('Just asn) @'Nothing @'Nothing
                                as bs asbs $
         \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) -> case () of
           _ | Dict <- inferKnownBackend @t @asbsn
             , Dict <- inferKnownBackend @t @bsn
             , Dict <- inferKnownBackend @s @bsn'
               -> XFrame <$>
                         (indexWise @t @asn @bsn @asbsn @s @bsn' @asbsn'
                           (\i x -> (\(XFrame y) -> unsafeCoerce y)
                                     <$> f (unsafeCoerce i) (XFrame x)
                           ) df
                         )
    {-# INLINE indexWiseI #-}



dropSufDims :: ConcatList as bs asbs
            => Dims bs -> Dims asbs -> Dims as
dropSufDims = unsafeCoerce dropSuf
  where
    dropSuf :: [Word] -> [Word] -> [Word]
    dropSuf bs asbs = fst $ foldr f ([], bs) asbs
    f :: Word -> ([Word], [Word]) -> ([Word], [Word])
    f x (as, [])  = (x:as,[])
    f _ (_, _:rs) = ([], rs)

concatDims :: ConcatList as bs asbs
           => Dims as -> Dims bs -> Dims asbs
concatDims = TL.concat

{-
A trick here is that both alternatives of 'MayEq' have the same representation
of a single equality constraint.
Thus, I can unsafeCoerce an equality later into MayEq to significantly
reduce amount of boilerplate.
 -}
type family MayEq (mns :: (Maybe [Nat])) (ns :: [Nat]) :: Constraint where
    MayEq 'Nothing   bs = bs ~ bs
    MayEq ('Just as) bs = as ~ bs

-- Very unsafe function, because it does not check the content of XDims at all.
withLiftedConcatList ::
       forall (asm :: Maybe [Nat]) (bsm :: Maybe [Nat]) (asbsm :: Maybe [Nat])
              (asx :: [XNat]) (bsx :: [XNat]) (asbsx :: [XNat]) r
     . -- (ConcatList asx bsx asbsx) =>
       Dims asx -> Dims bsx -> Dims asbsx
    -> ( forall (asn :: [Nat]) (bsn :: [Nat]) (asbsn :: [Nat])
          . ( ConcatList asn bsn asbsn
            , FixedDims asx asn, FixedDims bsx bsn, FixedDims asbsx asbsn
            , MayEq asm asn, MayEq bsm bsn, MayEq asbsm asbsn)
         => Dims asn -> Dims bsn -> Dims asbsn -> r
       ) -> r
withLiftedConcatList (XDims (asn :: Dims asn))
                     (XDims (bsn :: Dims bsn))
                     (XDims (asbsn :: Dims asbsn)) k
  | Dict <- unsafeEqTypes @_ @asbsn @(Concat asn bsn)
  , Dict <- unsafeCoerce (Dict @(asn ~ asn)) :: Dict (MayEq asm asn)
  , Dict <- unsafeCoerce (Dict @(bsn ~ bsn)) :: Dict (MayEq bsm bsn)
  , Dict <- unsafeCoerce (Dict @(asbsn ~ asbsn)) :: Dict (MayEq asbsm asbsn)
  , Dict <- inferConcat @asn @bsn @asbsn = k asn bsn asbsn


unsafeEqTypes :: forall k (a :: k) (b :: k)
               . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce (Dict :: Dict (a ~ a))
