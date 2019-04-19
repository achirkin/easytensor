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
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..)
  , SomeDataFrame (..), DataFrame'
  , pattern (:*:), pattern Z
    -- * Infer type class instances
  , AllTypes, ImplAllows, ArraySingletons, PrimFrames
  , DataFrameInference (..)
  , inferOrd, inferNum, inferFractional, inferFloating
  , inferOrd', inferNum', inferFractional', inferFloating'
  , inferASing', inferEq', inferShow', inferPrim', inferPrimElem'
    -- * Misc
  , ixOff, unsafeFromFlatList
  , Dim1 (..), Dim2 (..), Dim3 (..)
  , dimSize1, dimSize2, dimSize3
  , bSizeOf, bAlignOf
    -- * Re-exports from dimensions
  , Dim (..), Idx (..), XNat (..), Dims, Idxs, TypedList (..)
  ) where


import           Data.Proxy                              (Proxy)
import           Foreign.Storable                        (Storable (..))
import           GHC.Base
import           GHC.Ptr                                 (Ptr (..))

import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family (Array,
                                                          ArraySingleton (..))
import qualified Numeric.DataFrame.Internal.Array.Family as AFam
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
-- All Eq instances
--------------------------------------------------------------------------------

deriving instance Eq (Array t ds) => Eq (DataFrame (t :: Type) (ds :: [Nat]))

instance ImplAllows Eq ts ds => Eq (DataFrame (ts :: [Type]) ds) where
    Z == Z = True
    (a :*: as) == (b :*: bs) = a == b && as == bs

instance (AllTypes Eq t, DataFrameInference t)
      => Eq (DataFrame (t :: l) (ds :: [XNat])) where
    (XFrame dfa) == (XFrame dfb)
      | Just Dict <- sameDims' dfa dfb
      , Dict <- inferEq dfa = dfa == dfb
    _ == _ = False

instance (AllTypes Eq t, DataFrameInference t)
      => Eq (SomeDataFrame (t :: l)) where
    (SomeDataFrame dfa) == (SomeDataFrame dfb)
      | Just Dict <- sameDims' dfa dfb
      , Dict <- inferEq dfa = dfa == dfb
    _ == _ = False


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


instance (AllTypes Show t, DataFrameInference t)
      => Show (DataFrame (t :: l) (xns :: [XNat])) where
  show (XFrame df)
    | Dict <- inferShow df = 'X': show df

instance (AllTypes Show t, DataFrameInference t)
      => Show (SomeDataFrame (t :: l)) where
  show (SomeDataFrame df)
    | Dict <- inferShow df = "Some" ++ show df

--------------------------------------------------------------------------------



type family AllTypes (f :: Type -> Constraint) (ts :: l) :: Constraint where
    AllTypes f (t :: Type) = f t
    AllTypes f (ts :: [Type]) = All f ts

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

type family PrimFrames (ts :: l) (ns :: [Nat]) :: Constraint where
    PrimFrames (t :: Type) ns
      = (PrimBytes (DataFrame t ns), PrimArray t (DataFrame t ns))
    PrimFrames ('[] :: [Type]) _ = ()
    PrimFrames (t ': ts :: [Type]) ns
      = ( PrimBytes (DataFrame t ns), PrimArray t (DataFrame t ns)
        , PrimFrames ts ns)


deriving instance Bounded (Array t ds) => Bounded (DataFrame t ds)
deriving instance Enum (Array t ds) => Enum (DataFrame t ds)
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
    peek (Ptr addr) = IO (readAddr addr)
    poke (Ptr addr) a = IO (\s -> (# writeAddr a addr s, () #))



class DataFrameInference (t :: l) where
    -- | Bring an evidence of `ArraySingleton` instance into
    --   a scope at runtime.
    --   This is often used to let GHC infer other complex type class instances,
    --   such as `SubSpace`.
    inferASing
        :: (AllTypes PrimBytes t, Dimensions ds)
        => DataFrame t ds -> Dict (ArraySingletons t ds)
    inferEq
        :: (AllTypes Eq t, ArraySingletons t ds)
        => DataFrame t ds -> Dict (Eq (DataFrame t ds))
    inferShow
        :: (AllTypes Show t, ArraySingletons t ds, Dimensions ds)
        => DataFrame t ds -> Dict (Show (DataFrame t ds))
    inferPrim
        :: (AllTypes PrimBytes t, ArraySingletons t ds, Dimensions ds)
        => DataFrame t ds -> Dict (PrimFrames t ds)
    -- | This is a special function, because Scalar does not require PrimBytes.
    --   That is why the dimension list in the argument nust not be empty.
    inferPrimElem
        :: (ArraySingletons t ds, ds ~ (Head ds ': Tail ds))
        => DataFrame t ds -> Dict (AllTypes PrimBytes t)



instance DataFrameInference (t :: Type) where
    inferASing    (_ :: DataFrame t ds)
      = AFam.inferASing @t @ds
    inferEq       (_ :: DataFrame t ds)
      = case AFam.inferEq @t @ds of Dict -> Dict
    inferShow     (_ :: DataFrame t ds)
      = case AFam.inferShow @t @ds of Dict -> Dict
    inferPrim     (_ :: DataFrame t ds)
      = case AFam.inferPrim @t @ds of Dict -> Dict
    inferPrimElem (_ :: DataFrame t ds)
      = case AFam.inferPrimElem @t @(Head ds) @(Tail ds) of Dict -> Dict

inferOrd' :: forall t ds
           . (Ord t, ArraySingleton t ds)
          => Dict (Ord (DataFrame t ds))
inferOrd' = case AFam.inferOrd @t @ds of Dict -> Dict

inferNum' :: forall t ds
           . (Num t, ArraySingletons t ds)
          => Dict (Num (DataFrame t ds))
inferNum' = case AFam.inferNum @t @ds of Dict -> Dict

inferFractional' :: forall t ds
                  . (Fractional t, ArraySingleton t ds)
                 => Dict (Fractional (DataFrame t ds))
inferFractional' = case AFam.inferFractional @t @ds of Dict -> Dict

inferFloating' :: forall t ds
                . (Floating t, ArraySingleton t ds)
               => Dict (Floating (DataFrame t ds))
inferFloating' = case AFam.inferFloating @t @ds of Dict -> Dict


instance RepresentableList ts => DataFrameInference (ts :: [Type]) where
    inferASing    (_ :: DataFrame t ds)
      = inferASings @ts @ds (tList @_ @ts)
    inferEq       (_ :: DataFrame t ds)
      = case inferEqs @ts @ds (tList @_ @ts) of Dict -> Dict
    inferShow     (_ :: DataFrame t ds)
      = case inferShows @ts @ds (tList @_ @ts) of Dict -> Dict
    inferPrim     (_ :: DataFrame t ds)
      = case inferPrims @ts @ds (tList @_ @ts) of Dict -> Dict
    inferPrimElem (_ :: DataFrame t ds)
      = case inferPrimElems @ts @(Head ds) @(Tail ds) (tList @_ @ts) of Dict -> Dict



inferASings :: forall ts ds
             . (All PrimBytes ts, Dimensions ds)
            => TypeList ts -> Dict (ArraySingletons ts ds)
inferASings U = Dict
inferASings ((_ :: Proxy t) :* ts)
  = case (inferASing' @t @ds, inferASings @_ @ds ts) of (Dict, Dict) -> Dict



inferEqs :: forall ts ds
          . (All Eq ts, ArraySingletons ts ds)
         => TypeList ts -> Dict (ImplAllows Eq ts ds)
inferEqs U = Dict
inferEqs ((_ :: Proxy t) :* ts)
  = case (AFam.inferEq @t @ds, inferEqs @_ @ds ts) of (Dict, Dict) -> Dict

inferShows :: forall ts ds
            . (All Show ts, ArraySingletons ts ds, Dimensions ds)
           => TypeList ts -> Dict (ImplAllows Show ts ds)
inferShows U = Dict
inferShows ((_ :: Proxy t) :* ts)
  = case (AFam.inferShow @t @ds, inferShows @_ @ds ts) of (Dict, Dict) -> Dict

inferPrims :: forall ts ds
            . (All PrimBytes ts, ArraySingletons ts ds, Dimensions ds)
           => TypeList ts -> Dict (PrimFrames ts ds)
inferPrims U = Dict
inferPrims ((_ :: Proxy t) :* ts)
  = case (AFam.inferPrim @t @ds, inferPrims @_ @ds ts) of (Dict, Dict) -> Dict

inferPrimElems :: forall ts d ds
             . (ArraySingletons ts (d ': ds))
            => TypeList ts -> Dict (All PrimBytes ts)
inferPrimElems U = Dict
inferPrimElems ((_ :: Proxy t) :* ts)
  = case (AFam.inferPrimElem @t @d @ds, inferPrimElems @_ @d @ds ts) of (Dict, Dict) -> Dict


inferASing' :: forall t ds
            . (DataFrameInference t, AllTypes PrimBytes t, Dimensions ds)
           => Dict (ArraySingletons t ds)
inferASing' = inferASing (undefined :: DataFrame t ds)

inferEq' :: forall t ds
         . (DataFrameInference t, AllTypes Eq t, ArraySingletons t ds)
        => Dict (Eq (DataFrame t ds))
inferEq' = inferEq (undefined :: DataFrame t ds)

inferShow' :: forall t ds
           . ( DataFrameInference t, AllTypes Show t
             , ArraySingletons t ds, Dimensions ds)
          => Dict (Show (DataFrame t ds))
inferShow' = inferShow (undefined :: DataFrame t ds)


inferPrim' :: forall t ds
           . ( DataFrameInference t, AllTypes PrimBytes t
             , ArraySingletons t ds, Dimensions ds)
          => Dict (PrimFrames t ds)
inferPrim' = inferPrim (undefined :: DataFrame t ds)


inferPrimElem' :: forall t ds
               . ( DataFrameInference t, ArraySingletons t ds
                 , ds ~ (Head ds ': Tail ds))
              => Dict (AllTypes PrimBytes t)
inferPrimElem' = inferPrimElem (undefined :: DataFrame t ds)

inferOrd :: forall t ds
          . (Ord t, ArraySingleton t ds)
         => DataFrame t ds -> Dict (Ord (DataFrame t ds))
inferOrd = const (inferOrd' @t @ds)

inferNum :: forall t ds
          . (Num t, ArraySingletons t ds)
         => DataFrame t ds -> Dict (Num (DataFrame t ds))
inferNum = const (inferNum' @t @ds)

inferFractional :: forall t ds
                 . (Fractional t, ArraySingleton t ds)
                => DataFrame t ds -> Dict (Fractional (DataFrame t ds))
inferFractional = const (inferFractional' @t @ds)

inferFloating :: forall t ds
               . (Floating t, ArraySingleton t ds)
              => DataFrame t ds -> Dict (Floating (DataFrame t ds))
inferFloating = const (inferFloating' @t @ds)


--------------------------------------------------------------------------------
-- * Misc
--------------------------------------------------------------------------------

-- | A wrapper on `byteSize`
bSizeOf :: PrimBytes a => a -> Int
bSizeOf a = I# (byteSize a)

-- | A wrapper on `byteAlign`
bAlignOf :: PrimBytes a => a -> Int
bAlignOf a = I# (byteAlign a)

-- | Number of elements along the 1st dimension.
dimSize1 :: Dim1 t ds => t ds -> Word
dimSize1 = dimVal . dim1

-- | Number of elements along the 2nd dimension.
dimSize2 :: Dim2 t ds => t ds -> Word
dimSize2 = dimVal . dim2

-- | Number of elements along the 3rd dimension.
dimSize3 :: Dim3 t ds => t ds -> Word
dimSize3 = dimVal . dim3

class Dim1 (t :: [k] -> Type) (ds :: [k]) where
    dim1 :: t ds -> Dim (Head ds)

class Dim2 (t :: [k] -> Type) (ds :: [k]) where
    dim2 :: t ds -> Dim (Head (Tail ds))

class Dim3 (t :: [k] -> Type) (ds :: [k]) where
    dim3 :: t ds -> Dim (Head (Tail (Tail ds)))


instance {-# OVERLAPPABLE #-}
         Dimensions (d ': ds)
         => Dim1 t (d ': ds :: [k]) where
    dim1 _ = case dims @k @(d ': ds) of d :* _ -> d

instance {-# OVERLAPPING #-}
         Dim1 (TypedList Dim) (d ': ds) where
    dim1 (d :* _) = d

instance {-# OVERLAPPING #-}
         Dim1 (DataFrame l) (d ': ds :: [XNat]) where
    dim1 (XFrame (_ :: DataFrame l ns))
      = case xDims' @(d ': ds) @ns of d :* _  -> d

instance {-# OVERLAPPABLE #-}
         Dimensions (d1 ': d2 ': ds)
         => Dim2 t (d1 ': d2 ': ds :: [k]) where
    dim2 _ = case dims @k @(d1 ': d2 ': ds) of _ :* d :* _ -> d

instance {-# OVERLAPPING #-}
         Dim2 (TypedList Dim) (d1 ': d2 ': ds) where
    dim2 (_ :* d :* _) = d

instance {-# OVERLAPPING #-}
         Dim2 (DataFrame l) (d1 ': d2 ': ds :: [XNat]) where
    dim2 (XFrame (_ :: DataFrame l ns))
      = case xDims' @(d1 ': d2 ': ds) @ns of _ :* d :* _  -> d

instance {-# OVERLAPPABLE #-}
         Dimensions (d1 ': d2 ': d3 ': ds)
         => Dim3 t (d1 ': d2 ': d3 ': ds :: [k]) where
    dim3 _ = case dims @k @(d1 ': d2 ': d3 ': ds) of _ :* _ :* d :* _ -> d

instance {-# OVERLAPPING #-}
         Dim3 (TypedList Dim) (d1 ': d2 ': d3 ': ds) where
    dim3 (_ :* _ :* d :* _) = d

instance {-# OVERLAPPING #-}
         Dim3 (DataFrame l) (d1 ': d2 ': d3 ': ds :: [XNat]) where
    dim3 (XFrame (_ :: DataFrame l ns))
      = case xDims' @(d1 ': d2 ': d3 ': ds) @ns of _ :* _ :* d :* _  -> d
