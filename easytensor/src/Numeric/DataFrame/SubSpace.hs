{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
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
  ( -- * Class definition
    SubSpace (SubSpaceCtx)
    -- * Simple interface
    --
    --   All functions in this section are named @sxxx@, where @s@ stands for
    --   "simple". These allow using all functions of `SubSpace` with indexing
    --   dimensionality @as@ fixed to a single dim @(as ~ '[a])@.
    --   Use these functions if you are tired of @TypeApplications@ or find the
    --   error messages too cryptic.
  , sindexOffset, supdateOffset
  , sindex, supdate, sslice, supdateSlice
  , sewgen, siwgen, sewmap, siwmap, sewzip, siwzip
  , selement, selementWise, selementWise_, sindexWise, sindexWise_
  , sewfoldl, sewfoldl', sewfoldr, sewfoldr', sewfoldMap
  , siwfoldl, siwfoldl', siwfoldr, siwfoldr', siwfoldMap
    -- * Flexible interface
    --
    --   Functions in this section allow you pick, fold, iterate, or do whatever
    --   you want with arbitrary sub-dataframe dimensionalities:
    --   e.g. a DataFrame of rank 3 can be processed as an 1D array of matrices
    --   or a matrix of vectors, or a 3D array of scalars.
    --   Often, you would need @TypeApplications@ to specify explicitly at least
    --   the indexing subspace (parameter @as@).
  , (.!)
  , joinDataFrame, indexOffset, updateOffset
  , index, update, slice, updateSlice
  , ewgen, iwgen, ewmap, iwmap, ewzip, iwzip
  , element, elementWise, elementWise_, indexWise, indexWise_
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

-- | Unsafely get a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
sindexOffset ::
       forall t a bs
     . SubSpace t '[a] bs (a :+ bs)
    => Int -- ^ Prim element offset
    -> DataFrame t (a :+ bs) -> DataFrame t bs
sindexOffset = indexOffset @t @'[a] @bs @(a :+ bs)
{-# INLINE sindexOffset #-}

-- | Unsafely update a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
supdateOffset ::
       forall t a bs
     . SubSpace t '[a] bs (a :+ bs)
    => Int -- ^ Prim element offset
    -> DataFrame t bs -> DataFrame t (a :+ bs) -> DataFrame t (a :+ bs)
supdateOffset = updateOffset @t @'[a] @bs @(a :+ bs)
{-# INLINE supdateOffset #-}

-- | Get an element by its index in the dataframe.
sindex ::
       forall t a bs
     . SubSpace t '[a] bs (a :+ bs)
    => Idx a -> DataFrame t (a :+ bs) -> DataFrame t bs
sindex = index @t @'[a] @bs @(a :+ bs) . (:* U)
{-# INLINE[1] sindex #-}

-- | Get a few contiguous elements.
--
--   In a sense, this is just a more complicated version of `index`.
sslice ::
       forall t bi bd b bs
     . ( SubSpace t '[] (b :+ bs) (b :+ bs)
       , SubFrameIndexCtx b bi bd
       , KnownDim bd
       , PrimArray t (DataFrame t (bd :+ bs)))
    => Idx bi -> DataFrame t (b :+ bs) -> DataFrame t (bd :+ bs)
sslice = slice @t @'[] @(b :+ bs) @(b :+ bs) @bi @bd @b . (:* U)
{-# INLINE[1] sslice #-}


-- | Set a new value to an element.
supdate ::
       forall t a bs
     . SubSpace t '[a] bs (a :+ bs)
    => Idxs '[a] -> DataFrame t bs -> DataFrame t (a :+ bs) -> DataFrame t (a :+ bs)
supdate = update @t @'[a] @bs @(a :+ bs)
{-# INLINE[1] supdate #-}

-- | Update a few contiguous elements.
--
--   In a sense, this is just a more complicated version of `update`.
supdateSlice ::
       forall t bi bd b bs
     . ( SubSpace t '[] (b :+ bs) (b :+ bs)
       , SubFrameIndexCtx b bi bd
       , KnownDim bd
       , PrimArray t (DataFrame t (bd :+ bs)))
    => Idx bi -> DataFrame t (bd :+ bs)
    -> DataFrame t (b :+ bs) -> DataFrame t (b :+ bs)
supdateSlice = updateSlice @t @'[] @(b :+ bs) @(b :+ bs) @bi @bd @b . (:* U)
{-# INLINE[1] supdateSlice #-}

-- | Map a function over each element of DataFrame.
sewmap ::
       forall t a bs s bs'
     . (SubSpace t '[a] bs (a :+ bs), SubSpace s '[a] bs' (a :+ bs'))
    => (DataFrame s bs' -> DataFrame t bs)
    -> DataFrame s (a :+ bs') -> DataFrame t (a :+ bs)
sewmap = ewmap @t @'[a] @bs @(a :+ bs) @s @bs' @(a :+ bs')
{-# INLINE sewmap #-}

-- | Map a function over each element with its index of DataFrame.
siwmap ::
       forall t a bs s bs'
     . (SubSpace t '[a] bs (a :+ bs), SubSpace s '[a] bs' (a :+ bs'))
    => (Idx a -> DataFrame s bs' -> DataFrame t bs)
    -> DataFrame s (a :+ bs') -> DataFrame t (a :+ bs)
siwmap f = iwmap @t @'[a] @bs @(a :+ bs) @s @bs' @(a :+ bs') (\(i :* U) -> f i)
{-# INLINE[1] siwmap #-}

-- | Generate a DataFrame by repeating an element.
sewgen ::
       forall t a bs
     . (SubSpace t '[a] bs (a :+ bs), Dimensions '[a])
    => DataFrame t bs -> DataFrame t (a :+ bs)
sewgen = ewgen @t @'[a] @bs @(a :+ bs)
{-# INLINE sewgen #-}

-- | Generate a DataFrame by iterating a function (index -> element).
siwgen ::
       forall t a bs
     . (SubSpace t '[a] bs (a :+ bs), Dimensions '[a])
    => (Idx a -> DataFrame t bs) -> DataFrame t (a :+ bs)
siwgen f = iwgen @t @'[a] @bs @(a :+ bs) (\(i :* U) -> f i)
{-# INLINE[1] siwgen #-}

-- | Left-associative lazy fold of a DataFrame.
--   Same rules apply as for `foldl`.
sewfoldl ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (b -> DataFrame t bs -> b) -> b -> DataFrame t (a :+ bs) -> b
sewfoldl = ewfoldl @t @'[a] @bs @(a :+ bs) @b
{-# INLINE sewfoldl #-}

-- | Left-associative strict fold of a DataFrame.
--   Same rules apply as for `foldl'`.
sewfoldl' ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (b -> DataFrame t bs -> b) -> b -> DataFrame t (a :+ bs) -> b
sewfoldl' = ewfoldl' @t @'[a] @bs @(a :+ bs) @b
{-# INLINE sewfoldl' #-}

-- | Left-associative lazy fold of a DataFrame with an index.
--   Same rules apply as for `foldl`.
siwfoldl ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (Idx a -> b -> DataFrame t bs -> b) -> b -> DataFrame t (a :+ bs) -> b
siwfoldl f = iwfoldl @t @'[a] @bs @(a :+ bs) @b (\(i :* U) -> f i)
{-# INLINE[1] siwfoldl #-}

-- | Left-associative strict fold of a DataFrame with an index.
--   Same rules apply as for `foldl'`.
siwfoldl' ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (Idx a -> b -> DataFrame t bs -> b) -> b -> DataFrame t (a :+ bs) -> b
siwfoldl' f = iwfoldl' @t @'[a] @bs @(a :+ bs) @b (\(i :* U) -> f i)
{-# INLINE[1] siwfoldl' #-}

-- | Right-associative lazy fold of a DataFrame.
--   Same rules apply as for `foldr`.
sewfoldr ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (DataFrame t bs -> b -> b) -> b -> DataFrame t (a :+ bs) -> b
sewfoldr = ewfoldr @t @'[a] @bs @(a :+ bs) @b
{-# INLINE sewfoldr #-}

-- | Right-associative strict fold of a DataFrame.
--   Same rules apply as for `foldr'`.
sewfoldr' ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (DataFrame t bs -> b -> b) -> b -> DataFrame t (a :+ bs) -> b
sewfoldr' = ewfoldr' @t @'[a] @bs @(a :+ bs) @b
{-# INLINE sewfoldr' #-}

-- | Right-associative lazy fold of a DataFrame with an index.
--   Same rules apply as for `foldr`.
siwfoldr ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (Idx a -> DataFrame t bs -> b -> b) -> b -> DataFrame t (a :+ bs) -> b
siwfoldr f = iwfoldr @t @'[a] @bs @(a :+ bs) @b (\(i :* U) -> f i)
{-# INLINE[1] siwfoldr #-}

-- | Right-associative strict fold of a DataFrame with an index.
--   Same rules apply as for `foldr'`.
siwfoldr' ::
       forall t a bs b
     . SubSpace t '[a] bs (a :+ bs)
    => (Idx a -> DataFrame t bs -> b -> b) -> b -> DataFrame t (a :+ bs) -> b
siwfoldr' f = iwfoldr' @t @'[a] @bs @(a :+ bs) @b (\(i :* U) -> f i)
{-# INLINE[1] siwfoldr' #-}

-- | Apply an applicative functor on each element (Lens-like traversal).
selementWise ::
       forall t a bs s bs' f
     . (SubSpace t '[a] bs (a :+ bs), SubSpace s '[a] bs' (a :+ bs'), Applicative f)
    => (DataFrame s bs' -> f (DataFrame t bs))
    -> DataFrame s (a :+ bs') -> f (DataFrame t (a :+ bs))
selementWise = elementWise @t @'[a] @bs @(a :+ bs) @s @bs' @(a :+ bs') @f
{-# INLINE selementWise #-}

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal).
sindexWise ::
       forall t a bs s bs' f
     . (SubSpace t '[a] bs (a :+ bs), SubSpace s '[a] bs' (a :+ bs'), Applicative f)
    => (Idx a -> DataFrame s bs' -> f (DataFrame t bs))
    -> DataFrame s (a :+ bs') -> f (DataFrame t (a :+ bs))
sindexWise f = indexWise @t @'[a] @bs @(a :+ bs) @s @bs' @(a :+ bs') @f (\(i :* U) -> f i)
{-# INLINE[1] sindexWise #-}

-- | Apply an applicative functor on each element (Lens-like traversal)
selementWise_ ::
       forall t a bs f b
     . (SubSpace t '[a] bs (a :+ bs), Applicative f)
    => (DataFrame t bs -> f b) -> DataFrame t (a :+ bs) -> f ()
selementWise_ = elementWise_ @t @'[a] @bs @(a :+ bs) @f @b
{-# INLINE selementWise_ #-}

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal)
sindexWise_ ::
       forall t a bs f b
    . (SubSpace t '[a] bs (a :+ bs), Applicative f)
   => (Idx a -> DataFrame t bs -> f b) -> DataFrame t (a :+ bs) -> f ()
sindexWise_ f = indexWise_ @t @'[a] @bs @(a :+ bs) @f @b (\(i :* U) -> f i)
{-# INLINE sindexWise_ #-}

-- | Apply a functor over a single element (simple lens)
selement ::
       forall t a bs f
     . (SubSpace t '[a] bs (a :+ bs), Applicative f)
    => Idx a
    -> (DataFrame t bs -> f (DataFrame t bs))
    -> DataFrame t (a :+ bs) -> f (DataFrame t (a :+ bs))
selement = element @t @'[a] @bs @(a :+ bs) @f . (:* U)
{-# INLINE selement #-}

-- | Map each element of the DataFrame to a monoid,
--    and combine the results.
sewfoldMap ::
       forall t a bs m
     . (SubSpace t '[a] bs (a :+ bs), Monoid m)
    => (DataFrame t bs -> m) -> DataFrame t (a :+ bs) -> m
sewfoldMap = ewfoldMap @t @'[a] @bs @(a :+ bs) @m
{-# INLINE sewfoldMap #-}

-- | Map each element of the DataFrame and its index to a monoid,
--    and combine the results.
siwfoldMap ::
       forall t a bs m
     . (SubSpace t '[a] bs (a :+ bs), Monoid m)
    => (Idx a -> DataFrame t bs -> m) -> DataFrame t (a :+ bs) -> m
siwfoldMap f = iwfoldMap @t @'[a] @bs @(a :+ bs) @m (\(i :* U) -> f i)
{-# INLINE[1] siwfoldMap #-}

-- | Zip two spaces on a specified subspace element-wise (without index)
sewzip ::
       forall t a bs l bsL r bsR
     . (SubSpace t '[a] bs (a :+ bs), SubSpace l '[a] bsL (a :+ bsL), SubSpace r '[a] bsR (a :+ bsR))
    => (DataFrame l bsL -> DataFrame r bsR -> DataFrame t bs)
    -> DataFrame l (a :+ bsL) -> DataFrame r (a :+ bsR) -> DataFrame t (a :+ bs)
sewzip = ewzip @t @'[a] @bs @(a :+ bs) @l @bsL @(a :+ bsL) @r @bsR @(a :+ bsR)
{-# INLINE sewzip #-}

-- | Zip two spaces on a specified subspace index-wise (with index)
siwzip ::
       forall t a bs l bsL r bsR
     . (SubSpace t '[a] bs (a :+ bs), SubSpace l '[a] bsL (a :+ bsL), SubSpace r '[a] bsR (a :+ bsR))
    => (Idx a -> DataFrame l bsL -> DataFrame r bsR -> DataFrame t bs)
    -> DataFrame l (a :+ bsL) -> DataFrame r (a :+ bsR) -> DataFrame t (a :+ bs)
siwzip f = iwzip @t @'[a] @bs @(a :+ bs) @l @bsL @(a :+ bsL) @r @bsR @(a :+ bsR) (\(i :* U) -> f i)
{-# INLINE siwzip #-}

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
iwzip f dfl dfr = case getDimKind dfl of
  DimNat -> iwmap (\i x -> f i x (index i dfr)) dfl
  DimXNat
    | XFrame (_ :: DataFrame l asbsLN) <- dfl
    , XFrame (_ :: DataFrame r asbsRN) <- dfr
    , asbs <- unsafeCoerce -- minimum spans
        $ zipWith min (listDims $ dims @asbsLN) (listDims $ dims @asbsRN) :: Dims asbs
    , bs <- (dims :: Dims bs)
    , as <- (dropSufDims bs asbs :: Dims as)
      -> withLiftedConcatList @'Nothing @('Just (DimsBound bs)) @'Nothing as bs asbs $
          \(Dims :: Dims asn) (Dims :: Dims bsn) (Dims :: Dims asbsn) -> case () of
            _ | Dict <- inferKnownBackend @t @asbsn
              , Dict <- inferKnownBackend @t @bsn
             -> XFrame (iwgen @t @asn @bsn @asbsn
                         (\i -> case f (unsafeCoerce i)
                                       (index (unsafeCoerce i) dfl)
                                       (index (unsafeCoerce i) dfr) of
                                  XFrame r -> unsafeCoerce r
                         )
                       )
  where
    getDimKind :: forall (k :: Type) (ns :: [k]) (p :: [k] -> Type)
                . KnownDimKind k => p ns -> DimKind k
    getDimKind = const (dimKind @k)
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
      , KnownDimKind k
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
        getBD :: KnownDim (bd :: Nat) => DataFrame t (bd ': bs') -> Dim bd
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
               Dict -> XFrame (index @t @asn @bsn @asbsn (liftIdxs i) df)
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
                  -- last indexing Dim (bi) is not checked, but we know it's fixed,
                  --  so that's fine to unsafeCoerce it.
                , ii <- unsafeCoerce (liftIdxs i :: Idxs asn)
                 -> XFrame (slice @t @asn @bsn @asbsn
                                 @(DimBound bi) @(DimBound bd) @(Head bsn)
                                 @(Tail bsn) ii df)
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
             -> XFrame (update @t @asn @bsn @asbsn (liftIdxs i) bsDf asbsDf)
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
                  -- last indexing Dim (bi) is not checked, but we know it's fixed,
                  --  so that's fine to unsafeCoerce it.
                , ii <- unsafeCoerce (liftIdxs i :: Idxs asn)
                 -> XFrame (updateSlice @t @asn @bsn @asbsn
                                 @(DimBound bi) @bdN @b'N
                                 @bs'N ii dbs'Df asbsDf)
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


liftIdxs :: forall (ds :: [Nat]) (xds :: [XNat])
          . Dimensions ds => Idxs xds -> Idxs ds
liftIdxs ixs
#ifndef UNSAFE_INDICES
  | or $ zipWith (>=) (listIdxs ixs) (listDims (dims @ds)) = errorWithoutStackTrace
    $ "SubSpace/liftIdxs: index "
      ++ show ixs ++ " is outside of DataFrame bounds (Dims (" ++ show (dims @ds) ++ ")."
  | otherwise
#endif
    = unsafeCoerce ixs
{-# INLINABLE liftIdxs #-}

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
