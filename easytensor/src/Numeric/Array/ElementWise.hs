{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.ElementWise
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.Array.ElementWise
  ( ElementWise (..)
  ) where

import           Data.Int  (Int16, Int32, Int64, Int8)
import           Data.Word (Word16, Word32, Word64, Word8)

-- | Access elements.
--   i is an index type
--   x is an element
--   t is a container type
class ElementWise i x t | t -> x i where
  -- | Index a container
  (!)   :: t -> i -> x
  -- | map all elements with index
  ewmap :: (i -> x -> x) -> t -> t
  -- | generate data from elements
  ewgen :: (i -> x) -> t
  -- | generate data from elements in applicative functor
  ewgenA :: forall f . Applicative f => (i -> f x) -> f t
  -- | fold all element with index
  ewfold :: (i -> x -> a -> a) -> a -> t -> a
  -- | Apply an applicative functor on each element (Lens-like traversal)
  elementWise :: forall f . Applicative f => (x -> f x) -> t -> f t
  -- | Apply an applicative functor on each element with its index
  --     (Lens-like indexed traversal)
  indexWise :: forall f . Applicative f => (i -> x -> f x) -> t -> f t
  -- | Fill a container with a single value
  broadcast :: x -> t
  -- | Update a single element
  update :: i -> x -> t -> t


instance ElementWise Int Float Float where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}



instance ElementWise Int Double Double where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Int Int where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Int8 Int8 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}

instance ElementWise Int Int16 Int16 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}

instance ElementWise Int Int32 Int32 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Int64 Int64 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Word Word where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Word8 Word8 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Word16 Word16 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Word32 Word32 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}


instance ElementWise Int Word64 Word64 where
  (!) x _ = x
  {-# INLINE (!) #-}
  ewmap f = f 1
  {-# INLINE ewmap #-}
  ewgen f   = f 1
  {-# INLINE ewgen #-}
  ewgenA f  = f 1
  {-# INLINE ewgenA #-}
  ewfold f x0 x = f 1 x x0
  {-# INLINE ewfold #-}
  elementWise = id
  {-# INLINE elementWise #-}
  indexWise f = f 1
  {-# INLINE indexWise #-}
  broadcast = id
  {-# INLINE broadcast #-}
  update _ = const
  {-# INLINE update #-}
