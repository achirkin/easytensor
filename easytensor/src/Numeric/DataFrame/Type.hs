{-# LANGUAGE AllowAmbiguousTypes        #-}
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

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..)
  , SomeDataFrame (..), DataFrame'
  , pattern (:*:), pattern Z
    -- * Infer type class instances
  , KnownBackend (), DFBackend, KnownBackends
  , InferKnownBackend (..), inferPrimElem
    -- * Re-exports
  , Dim (..), Idx (..), XNat (..), N, XN, Dims, Idxs, TypedList (..)
  , PrimBytes (), bSizeOf, bAlignOf
  , PrimArray (), ixOff, unsafeFromFlatList
  ) where


import Data.Proxy       (Proxy)
import Foreign.Storable (Storable (..))
import GHC.Base
import GHC.Ptr          (Ptr (..))

import Numeric.DataFrame.Internal.PrimArray
import Numeric.Dimensions
import Numeric.PrimBytes

import {-# SOURCE #-} Numeric.DataFrame.Internal.Backend (DFBackend,
                                                          KnownBackend)
import {-# SOURCE #-} qualified Numeric.DataFrame.Internal.Backend as Backend

-- | Keep data in a primitive data frame
--    and maintain information about Dimensions in the type system
data family DataFrame (t :: l) (xs :: [k])

-- | Single frame
newtype instance DataFrame (t :: Type) (ns :: [Nat])
  = SingleFrame { _getDF :: DFBackend t ns }

-- | Multiple "columns" of data frames of the same shape
newtype instance DataFrame (ts :: [Type]) (ns :: [Nat])
  = MultiFrame { _getDFS ::  TypedList (DataFrame' ns) ts }

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame.
data instance DataFrame (ts :: l) (xns :: [XNat])
  = forall (ns :: [Nat])
  . (KnownXNatTypes xns, FixedDims xns ns, Dimensions ns, KnownBackends ts ns)
  => XFrame (DataFrame ts ns)

-- | Data frame that has an unknown dimensionality at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame
data SomeDataFrame (t :: l)
  = forall (ns :: [Nat]) . (Dimensions ns, KnownBackends t ns)
  => SomeDataFrame (DataFrame t ns)

-- | DataFrame with its type arguments swapped.
newtype DataFrame' (xs :: [k]) (t :: l) = DataFrame' (DataFrame t xs)

{-# COMPLETE Z, (:*:) #-}


-- | Constructing a @MultiFrame@ using DataFrame columns
pattern (:*:) :: forall (xs :: [Type]) (ns :: [Nat])  . ()
              => forall (y :: Type)    (ys :: [Type]) . (xs ~ (y ': ys))
              => DataFrame y ns -> DataFrame ys ns -> DataFrame xs ns
pattern (:*:) x xs <- (MultiFrame (DataFrame' x :* (MultiFrame -> xs)))
  where
    (:*:) x (MultiFrame xs) = MultiFrame (DataFrame' x :* xs)
infixr 6 :*:

-- | Empty MultiFrame
pattern Z :: forall (xs :: [Type]) (ns :: [Nat])
           . () => (xs ~ '[]) => DataFrame xs ns
pattern Z = MultiFrame U

-- | I use this kind-polymorphic constraint to generalize @XFrame@ and @SomeDataFrame@
--   over @SingleFrame@ and @MultiFrame@.
type family KnownBackends (ts :: l) (ns :: [Nat]) :: Constraint where
    KnownBackends ( t      ::  Type ) ns = KnownBackend t ns
    KnownBackends ('[]     :: [Type]) _  = ()
    KnownBackends (t ': ts :: [Type]) ns =
      (KnownBackend t ns, KnownBackends ts ns)

class InferKnownBackend (t :: k) ds where
    inferKnownBackend :: Dict (KnownBackends t ds)

instance (PrimBytes t, Dimensions ds) => InferKnownBackend (t :: Type) ds where
    inferKnownBackend = Backend.inferKnownBackend @t @ds

instance (RepresentableList ts, All PrimBytes ts, Dimensions ds)
      => InferKnownBackend (ts :: [Type]) ds where
    inferKnownBackend = go (tList @_ @ts)
      where
        go :: forall ss . All PrimBytes ss => TypeList ss -> Dict (KnownBackends ss ds)
        go U = Dict
        go ((_ :: Proxy t) :* ts)
             = case Backend.inferKnownBackend @t @ds of
                 Dict -> case go ts of
                   Dict -> Dict


type family AllFrames (f :: Type -> Constraint) (ts :: [Type]) (ds :: [Nat])
                                                             :: Constraint where
    AllFrames _ '[] _ = ()
    AllFrames f (t ': ts) ds = (f (DataFrame t ds), AllFrames f ts ds)


deriving instance Eq (DFBackend t ds)
               => Eq (DataFrame t ds)
deriving instance Ord (DFBackend t ds)
               => Ord (DataFrame t ds)
deriving instance Bounded (DFBackend t ds)
               => Bounded (DataFrame t ds)
deriving instance Enum (DFBackend t ds)
               => Enum (DataFrame t ds)
deriving instance Integral (DFBackend t ds)
               => Integral (DataFrame t ds)
deriving instance Num (DFBackend t ds)
               => Num (DataFrame t ds)
deriving instance Fractional (DFBackend t ds)
               => Fractional (DataFrame t ds)
deriving instance Floating (DFBackend t ds)
               => Floating (DataFrame t ds)
deriving instance Real (DFBackend t ds)
               => Real (DataFrame t ds)
deriving instance RealFrac (DFBackend t ds)
               => RealFrac (DataFrame t ds)
deriving instance RealFloat (DFBackend t ds)
               => RealFloat (DataFrame t ds)
deriving instance PrimBytes (DFBackend t ds)
               => PrimBytes (DataFrame t ds)
deriving instance (PrimArray t (DFBackend t ds), PrimBytes t)
               => PrimArray t (DataFrame t ds)



instance PrimBytes (DataFrame t ds) => Storable (DataFrame t ds) where
    sizeOf x = I# (byteSize x)
    alignment x = I# (byteAlign x)
    peek (Ptr addr) = IO (readAddr addr)
    poke (Ptr addr) a = IO (\s -> (# writeAddr a addr s, () #))


instance AllFrames Eq ts ds => Eq (DataFrame (ts :: [Type]) ds) where
    Z == Z = True
    (a :*: as) == (b :*: bs) = a == b && as == bs

instance Eq t => Eq (DataFrame (t :: Type) (ds :: [XNat])) where
    XFrame dfa == XFrame dfb
      | Just Dict <- sameDims' dfa dfb = dfa == dfb
      | otherwise                      = False

instance All Eq ts => Eq (DataFrame (ts :: [Type]) (ds :: [XNat])) where
    XFrame dfa == XFrame dfb
      | Just Dict <- sameDims' dfa dfb = eqFrames dfa dfb
      | otherwise                      = False

instance Eq t => Eq (SomeDataFrame (t :: Type)) where
    SomeDataFrame dfa == SomeDataFrame dfb
      | Just Dict <- sameDims' dfa dfb = dfa == dfb
      | otherwise                      = False

instance All Eq ts => Eq (SomeDataFrame (ts :: [Type])) where
    SomeDataFrame dfa == SomeDataFrame dfb
      | Just Dict <- sameDims' dfa dfb = eqFrames dfa dfb
      | otherwise                      = False


eqFrames :: forall (xs :: [Type]) (ns :: [Nat])
          . (KnownBackends xs ns, All Eq xs)
         => DataFrame xs ns -> DataFrame xs ns -> Bool
eqFrames Z Z                   = True
eqFrames (a :*: as) (b :*: bs) = a == b && eqFrames as bs



instance ( Show t
         , Dimensions ds
         , KnownBackend t ds
         ) => Show (DataFrame (t :: Type) (ds :: [Nat])) where
    show (SingleFrame df) = unlines
        [ "DF " ++ drop 5 (show $ dims @_ @ds) ++ ":"
        , show df
        ]

instance ( All Show ts
         , Dimensions ds
         , KnownBackends ts ds
         ) => Show (DataFrame (ts :: [Type]) (ds :: [Nat])) where
    show dfs = unlines $
        ("DF " ++ show (order dds) ++ " x "  ++ drop 5 (show dds) ++ ":")
        : showAll 1 dfs
      where
        dds = dims @_ @ds
        showAll :: forall (xs :: [Type]) . (All Show xs, KnownBackends xs ds)
                => Word -> DataFrame xs ds -> [String]
        showAll _ Z = []
        showAll n (SingleFrame arr :*: fs)
          = ("Var " ++ show n) : show arr : showAll (n+1) fs

instance Show t => Show (DataFrame (t :: Type) (xns :: [XNat])) where
    show (XFrame df) = 'X': show df

instance All Show ts => Show (DataFrame (ts :: [Type]) (xns :: [XNat])) where
    show (XFrame df) = 'X': show df

instance Show t => Show (SomeDataFrame (t :: Type)) where
    show (SomeDataFrame df) = "Some" ++ show df

instance All Show ts => Show (SomeDataFrame (ts :: [Type])) where
    show (SomeDataFrame df) = "Some" ++ show df


deriving instance Read (DFBackend t ds)
               => Read (DataFrame t ds)


inferPrimElem
  :: forall (t :: Type) (ds :: [Nat])
   . KnownBackend t ds
  => DataFrame t ds -> Maybe (Dict (PrimBytes t))
inferPrimElem = Backend.inferPrimElem . _getDF
