{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE StandaloneDeriving     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Internal.Array.Family
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Internal.Array.Singleton
  ( ArraySingleton (..)
  , ArraySing (..), aSingEv, inferASing
  , inferPrimElem, inferPrim, inferEq, inferShow, inferOrd, inferNum
  , inferFractional, inferFloating
  ) where


import           GHC.Base


import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family.ArrayBase
import           Numeric.DataFrame.Internal.Array.Family.DoubleX2
import           Numeric.DataFrame.Internal.Array.Family.DoubleX3
import           Numeric.DataFrame.Internal.Array.Family.DoubleX4
import           Numeric.DataFrame.Internal.Array.Family.FloatX2
import           Numeric.DataFrame.Internal.Array.Family.FloatX3
import           Numeric.DataFrame.Internal.Array.Family.FloatX4
import           Numeric.DataFrame.Internal.Array.Family.ScalarBase
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.DataFrame.Family


-- | A framework for using Array type family instances.
class ArraySingleton (t :: Type) (ds :: [Nat]) where
    -- | Get Array type family instance
    aSing :: ArraySing t ds


data ArraySing t (ds :: [Nat]) where
    AScalar :: (Array t ds ~ ScalarBase t)   => ArraySing t      '[]
    AF2     :: (Array t ds ~ FloatX2)        => ArraySing Float  '[2]
    AF3     :: (Array t ds ~ FloatX3)        => ArraySing Float  '[3]
    AF4     :: (Array t ds ~ FloatX4)        => ArraySing Float  '[4]
    AD2     :: (Array t ds ~ DoubleX2)       => ArraySing Double '[2]
    AD3     :: (Array t ds ~ DoubleX3)       => ArraySing Double '[3]
    AD4     :: (Array t ds ~ DoubleX4)       => ArraySing Double '[4]
    ABase   :: ( Array t ds ~ ArrayBase t ds
               , PrimBytes t
               ) => ArraySing t ds

deriving instance Eq (ArraySing t ds)
deriving instance Ord (ArraySing t ds)
deriving instance Show (ArraySing t ds)

-- | This function does GHC's magic to convert user-supplied `aSing` function
--   to create an instance of `ArraySingleton` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyArraySing :: forall r t ds
                . ArraySing t ds -> ( ArraySingleton t ds => r) -> r
reifyArraySing as k
  = unsafeCoerce# (MagicArraySing k :: MagicArraySing t ds r) as
{-# INLINE reifyArraySing #-}
newtype MagicArraySing t (ds :: [Nat]) r
  = MagicArraySing (ArraySingleton t ds => r)

-- | Use `ArraySing` GADT to construct an `ArraySingleton` dictionary.
--   In other words, bring an evidence of `ArraySingleton` instance into
--   a scope at runtime.
aSingEv :: ArraySing t ds -> Evidence (ArraySingleton t ds)
aSingEv ds = reifyArraySing ds E
{-# INLINE aSingEv #-}

-- | Use `ArraySing` GADT to construct an `ArraySingleton` dictionary.
--   The same as `aSingEv`, but relies on `PrimBytes` and `Dimensions`.
inferASing :: forall t ds
            . (PrimBytes t, Dimensions ds)
           => Evidence (ArraySingleton t ds)
inferASing = case (dims @_ @ds, primTag @t undefined) of
  (U, _) -> E
  (d :* U, PTagFloat)
      | Just E <- sameDim (D @2) d -> E
      | Just E <- sameDim (D @3) d -> E
      | Just E <- sameDim (D @4) d -> E
  (d :* U, PTagDouble)
      | Just E <- sameDim (D @2) d -> E
      | Just E <- sameDim (D @3) d -> E
      | Just E <- sameDim (D @4) d -> E
  _ -> case (unsafeCoerce# (E @(ds ~ ds)) :: Evidence (ds ~ '[0])) of E -> E
{-# INLINE inferASing #-}


instance {-# OVERLAPPABLE #-}
         (Array t ds ~ ArrayBase t ds, PrimBytes t)
         => ArraySingleton t ds where
    aSing = ABase
instance {-# OVERLAPPING #-}  ArraySingleton t      '[]    where
    aSing = AScalar
instance {-# OVERLAPPING #-}  ArraySingleton Float  '[2]   where
    aSing = AF2
instance {-# OVERLAPPING #-}  ArraySingleton Float  '[3]   where
    aSing = AF3
instance {-# OVERLAPPING #-}  ArraySingleton Float  '[4]   where
    aSing = AF4
instance {-# OVERLAPPING #-}  ArraySingleton Double '[2]   where
    aSing = AD2
instance {-# OVERLAPPING #-}  ArraySingleton Double '[3]   where
    aSing = AD3
instance {-# OVERLAPPING #-}  ArraySingleton Double '[4]   where
    aSing = AD4

-- | This is a special function, because Scalar does not require PrimBytes.
--   That is why the dimension list in the argument is not empty.
inferPrimElem :: forall t d ds
               . ArraySingleton t (d ': ds)
              => Evidence (PrimBytes t)
inferPrimElem = case (aSing :: ArraySing t (d ': ds)) of
  AF2   -> E
  AF3   -> E
  AF4   -> E
  AD2   -> E
  AD3   -> E
  AD4   -> E
  ABase -> E

-- Rather verbose way to show that there is an instance of a required type class
-- for every instance of the type family.
#define WITNESS case (aSing :: ArraySing t ds) of {\
  AScalar -> E;\
  AF2     -> E;\
  AF3     -> E;\
  AF4     -> E;\
  AD2     -> E;\
  AD3     -> E;\
  AD4     -> E;\
  ABase   -> E}

inferPrim :: forall t ds
           . ( PrimBytes t
             , ArraySingleton t ds
             , Dimensions ds
             )
          => Evidence (PrimBytes (DataFrame t ds), PrimArray t (DataFrame t ds))
inferPrim = WITNESS

inferEq :: forall t ds
         . (Eq t, ArraySingleton t ds)
        => Evidence (Eq (DataFrame t ds))
inferEq = WITNESS

inferOrd :: forall t ds
            . (Ord t, ArraySingleton t ds)
           => Evidence (Ord (DataFrame t ds))
inferOrd = WITNESS

inferNum :: forall t ds
          . (Num t, ArraySingleton t ds)
         => Evidence (Num (DataFrame t ds))
inferNum = WITNESS

inferFractional :: forall t ds
                 . (Fractional t, ArraySingleton t ds)
                => Evidence (Fractional (DataFrame t ds))
inferFractional = WITNESS

inferFloating :: forall t ds
               . (Floating t, ArraySingleton t ds)
              => Evidence (Floating (DataFrame t ds))
inferFloating = WITNESS

inferShow :: forall t ds
           . (Show t, Dimensions ds, ArraySingleton t ds)
          => Evidence (Show (DataFrame t ds))
inferShow = WITNESS
