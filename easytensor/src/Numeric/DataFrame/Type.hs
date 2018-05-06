-- {-# LANGUAGE CPP                        #-}
-- {-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Type
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..)
  , SomeDataFrame (..), DataFrame'
  , pattern (:*:), pattern Z
  , ArraySingletons
  --   -- * Bring type classes into scope
  -- , NumericFrame
  --   -- * Utility type families and constraints
  -- , FPFRame, IntegralFrame, NumericVariantFrame, CommonOpFrame
  ) where


import           Foreign.Storable                        (Storable (..))
import           GHC.Base
import           GHC.Ptr (Ptr (..))
import Data.Proxy (Proxy)

import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.Dimensions
import           Numeric.PrimBytes

-- | Single frame
newtype instance DataFrame (t :: Type) (ns :: [Nat])
  = SingleFrame { _getDF :: Array t ns }

-- | Multiple "columns" of data frames of the same shape
newtype instance DataFrame (ts :: [Type]) (ns :: [Nat])
  = MultiFrame { _getDFS ::  TypedList (DataFrame' ns) ts }

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame.
data instance DataFrame (ts :: l) (xns :: [XNat])
  = forall (ns :: [Nat])
  . (KnownXNatTypes xns, FixedDims xns ns, Dimensions ns, ArraySingletons ts ns)
  => XFrame (DataFrame ts ns)

-- | Data frame that has an unknown dimensionality at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame
data SomeDataFrame (t :: l)
  = forall (ns :: [Nat]) . (Dimensions ns, ArraySingletons t ns)
  => SomeDataFrame (DataFrame t ns)

-- | DataFrame with its type arguments swapped.
newtype DataFrame' (xs :: [k]) (t :: l) = DataFrame' (DataFrame t xs)

{-# COMPLETE Z, (:*:) #-}


-- | Constructing a @MultiFrame@ using DataFrame columns
pattern (:*:) :: forall (xs :: [Type]) (ns :: [Nat])
              . ()
              => forall (y :: Type) (ys :: [Type])
              . (xs ~ (y ': ys))
                => DataFrame y ns
                -> DataFrame ys ns -> DataFrame xs ns
pattern (:*:) x xs <- (MultiFrame (DataFrame' x :* (MultiFrame -> xs)))
  where
    (:*:) x (MultiFrame xs) = MultiFrame (DataFrame' x :* xs)
infixr 6 :*:

-- | Empty MultiFrame
pattern Z :: forall (xs :: [Type]) (ns :: [Nat])
           . () => (xs ~ '[]) => DataFrame xs ns
pattern Z = MultiFrame U


--------------------------------------------------------------------------------
-- All Show instances
--------------------------------------------------------------------------------

instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame (t :: Type) (ds :: [Nat])) where
  show (SingleFrame arr) = unlines
                            [ "DF " ++ drop 5 (show $ dims @_ @ds) ++ ":"
                            , show arr
                            ]

instance ( Dimensions ds
         , ImplAllows Show ts ds
         ) => Show (DataFrame (ts :: [Type]) (ds :: [Nat])) where
  show dfs = unlines $
      ("DF " ++ show (order dds) ++ " x "  ++ drop 5 (show dds) ++ ":")
      : showAll 1 dfs
    where
      dds = dims @_ @ds
      showAll :: ImplAllows Show xs ds
              => Word -> DataFrame (xs :: [Type]) ds -> [String]
      showAll _ Z = []
      showAll n (SingleFrame arr :*: fs)
        = ("Var " ++ show n) : show arr : showAll (n+1) fs

instance Show t => Show (DataFrame (t :: Type) (xns :: [XNat])) where
  show (XFrame (df :: DataFrame t ns))
    | E <- inferShow @t @ns = 'X': show df

instance All Show ts => Show (DataFrame (ts :: [Type]) (xns :: [XNat])) where
  show (XFrame (df@(MultiFrame mf) :: DataFrame ts ns))
    | TypeList <- types mf
    , E <- inferShows @ts @ns = 'X': show df
  show _ = error "DataFrame/show: impossible argument"

instance Show t => Show (SomeDataFrame (t :: Type)) where
  show (SomeDataFrame (df :: DataFrame t ns))
    | E <- inferShow @t @ns = "Some" ++ show df

instance All Show ts => Show (SomeDataFrame (ts :: [Type])) where
  show (SomeDataFrame (df@(MultiFrame mf) :: DataFrame ts ns))
    | TypeList <- types mf
    , E <- inferShows @ts @ns = "Some" ++ show df
  show _ = error "DataFrame/show: impossible argument"

--------------------------------------------------------------------------------





type family ImplAllows (f :: Type -> Constraint) (ts :: l) (ds :: [Nat])
                                                             :: Constraint where
    ImplAllows f (t :: Type) ds = f (Array t ds)
    ImplAllows _ ('[] :: [Type]) _ = ()
    ImplAllows f (t ': ts :: [Type]) ds = (f (Array t ds), ImplAllows f ts ds)



type family ArraySingletons (ts :: l) (ns :: [Nat]) :: Constraint where
    ArraySingletons (t :: Type) ns = ArraySingleton t ns
    ArraySingletons ('[] :: [Type]) _ = ()
    ArraySingletons (t ': ts :: [Type]) ns
      = (ArraySingleton t ns, ArraySingletons ts ns)


deriving instance Bounded (Array t ds) => Bounded (DataFrame t ds)
deriving instance Enum (Array t ds) => Enum (DataFrame t ds)
deriving instance Eq (Array t ds) => Eq (DataFrame t ds)
deriving instance Integral (Array t ds)
               => Integral (DataFrame t ds)
deriving instance Num (Array t ds)
               => Num (DataFrame t ds)
deriving instance Fractional (Array t ds)
               => Fractional (DataFrame t ds)
deriving instance Floating (Array t ds)
               => Floating (DataFrame t ds)
deriving instance Ord (Array t ds)
               => Ord (DataFrame t ds)
deriving instance ( Read (Array t ds), Dimensions ds )
               => Read (DataFrame t ds)
deriving instance Real (Array t ds)
               => Real (DataFrame t ds)
deriving instance RealFrac (Array t ds)
               => RealFrac (DataFrame t ds)
deriving instance RealFloat (Array t ds)
               => RealFloat (DataFrame t ds)
deriving instance (PrimArray t (Array t ds), PrimBytes t)
               => PrimArray t (DataFrame t ds)
deriving instance PrimBytes (Array t ds)
               => PrimBytes (DataFrame t ds)



instance PrimBytes (DataFrame t ds) => Storable (DataFrame t ds) where
    sizeOf x = I# (byteSize x)
    alignment x = I# (byteAlign x)
    peekElemOff ptr (I# offset) =
      peekByteOff ptr (I# (offset *# byteSize @(DataFrame t ds) undefined))
    pokeElemOff ptr (I# offset) =
      pokeByteOff ptr (I# (offset *# byteSize @(DataFrame t ds) undefined))
    peekByteOff (Ptr addr) (I# offset)
      | bsize <- byteSize @(DataFrame t ds) undefined
      = IO $ \s0 -> case newByteArray# bsize s0 of
         (# s1, marr #) -> case unsafeFreezeByteArray# marr
                                 ( copyAddrToByteArray#
                                     (addr `plusAddr#` offset)
                                      marr 0# bsize s1
                                 ) of
           (# s2, arr #) -> (# s2, fromBytes 0# arr #)
    pokeByteOff (Ptr addr) (I# offset) x = IO
            $ \s -> (# copyByteArrayToAddr# (getBytes x) (byteOffset x)
                                            (addr `plusAddr#` offset)
                                            (byteSize x) s
                     , () #)
    peek ptr = peekByteOff ptr 0
    poke ptr = pokeByteOff ptr 0



--
-- instance Eq (DataFrame t (ds :: [XNat])) where
--   SomeDataFrame (a :: DataFrame t nsa) == SomeDataFrame (b :: DataFrame t nsb)
--       = case sameDim (dim @nsa) (dim @nsb) of
--           Just Evidence -> a == b
--           Nothing   -> False
--
-- instance Show (DataFrame t (ds :: [XNat])) where
--   show (SomeDataFrame arr) = show arr



inferShows :: forall ts ds
             . (All Show ts, RepresentableList ts, ArraySingletons ts ds, Dimensions ds)
            => Evidence (ImplAllows Show ts ds)
inferShows = case tList @_ @ts of
    U -> E
    (_ :: Proxy t) :* (TypeList :: TypeList ts') ->
      case (inferShow @t @ds, inferShows @ts' @ds) of (E, E) -> E



class DataFrameInference (t :: l) (ds :: [Nat]) where
    -- | Bring an evidence of `ArraySingleton` instance into
    --   a scope at runtime.
    --   This is often used to let GHC infer other complex type class instances,
    --   such as `SubSpace`.
    inferASing :: Evidence (ArraySingletons ts ds)
