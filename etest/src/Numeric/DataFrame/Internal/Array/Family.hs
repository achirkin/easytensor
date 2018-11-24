{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
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

module Numeric.DataFrame.Internal.Array.Family
  ( Array
  , UnitBase (..), ScalarBase (..), Vec2Base (..), ListBase (..)
  , ArraySingleton (..)
  , ArraySing (..), aSingEv
  , InferBackendInstance (..)
  ) where


import           GHC.Base
import           Data.Semigroup
import           Debug.Trace

import           Numeric.Dimensions
import           Numeric.Type.Evidence.Internal


type family Array (t :: Type) (n :: Nat) = (v :: Type) | v -> t n where
    Array t  0 = UnitBase t
    Array t  1 = ScalarBase t
    Array t  2 = Vec2Base t
    Array t  n = ListBase t n

data UnitBase (t :: Type) = UnitBase
  deriving (Eq, Ord, Show)

instance Semigroup (UnitBase t) where
  UnitBase <> UnitBase = UnitBase

instance Monoid (UnitBase t) where
  mempty = UnitBase
  mappend = (<>)

-- | Specialize ScalarBase type without any arrays
newtype ScalarBase (t :: Type) = ScalarBase { _unScalarBase :: t }
  deriving (Eq, Ord, Show)

instance Num t => Semigroup (ScalarBase t) where
  ScalarBase a <> ScalarBase b = ScalarBase (a + b)

instance Num t => Monoid (ScalarBase t) where
  mempty = ScalarBase 0
  mappend = (<>)


data Vec2Base (t :: Type) = Vec2Base t t
  deriving (Eq, Ord, Show)


instance Num t => Semigroup (Vec2Base t) where
  Vec2Base a1 a2 <> Vec2Base b1 b2 = Vec2Base (a1 + b1) (a2 * b2)

instance Num t => Monoid (Vec2Base t) where
  mempty = Vec2Base 0 1
  mappend = (<>)

newtype ListBase (t :: Type) (n :: Nat) = ListBase { _unListBase :: [t] }
  deriving (Eq, Ord, Show)


instance Num t => Semigroup (ListBase t n) where
  ListBase as <> ListBase bs = ListBase $ zipWith (*) as bs

instance (Num t, KnownDim n) => Monoid (ListBase t n) where
  mempty = ListBase $ replicate (fromIntegral $ dimVal' @n) 1
  mappend = (<>)


-- | A framework for using Array type family instances.
class ArraySingleton (t :: Type) (n :: Nat) where
    -- | Get Array type family instance
    aSing :: ArraySing t n

data ArraySing t (ds :: Nat) where
    AS0 :: (Array t n ~ UnitBase t)   => ArraySing t 0
    AS1 :: (Array t n ~ ScalarBase t) => ArraySing t 1
    AS2 :: (Array t n ~ Vec2Base t)   => ArraySing t 2
    ASn :: (Array t n ~ ListBase t n) => ArraySing t n

deriving instance Eq (ArraySing t ds)
deriving instance Ord (ArraySing t ds)
deriving instance Show (ArraySing t ds)



-- | This function does GHC's magic to convert user-supplied `aSing` function
--   to create an instance of `ArraySingleton` typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyArraySing :: forall r t n
                . ArraySing t n -> ( ArraySingleton t n => r) -> r
reifyArraySing as k
  = unsafeCoerce# (MagicArraySing k :: MagicArraySing t n r) as
{-# INLINE reifyArraySing #-}
newtype MagicArraySing t (n :: Nat) r
  = MagicArraySing (ArraySingleton t n => r)

-- | Use `ArraySing` GADT to construct an `ArraySingleton` dictionary.
--   In other words, bring an evidence of `ArraySingleton` instance into
--   a scope at runtime.
aSingEv :: ArraySing t n -> Evidence (ArraySingleton t n)
aSingEv ds = reifyArraySing ds E
{-# INLINE aSingEv #-}


instance {-# OVERLAPPABLE #-}
         (Array t n ~ ListBase t n)
         => ArraySingleton t n where
    aSing = ASn
instance {-# OVERLAPPING #-}  ArraySingleton t 0 where
    aSing = AS0
instance {-# OVERLAPPING #-}  ArraySingleton t 1 where
    aSing = AS1
instance {-# OVERLAPPING #-}  ArraySingleton t 2 where
    aSing = AS2


class InferBackendInstance t ds c where
    inferBackendInstance :: BareConstraint (c (Array t ds))

instance ( ArraySingleton t n
         , c (UnitBase t)
         , c (ScalarBase t)
         , c (Vec2Base t)
         , c (ListBase t n)
         ) => InferBackendInstance t n c where
    inferBackendInstance = case ev of EvValue c -> c
      where
        ev = case (aSing :: ArraySing t n) of
          AS0 -> trace "---------- Selecting UnitBase" E
          AS1 -> trace "---------- Selecting ScalarBase" E
          AS2 -> trace "---------- Selecting Vec2Base" E
          ASn -> trace "---------- Selecting ListBase" E
    {-# INLINE inferBackendInstance #-}



-- inferEq :: forall t ds
--          . (Eq t, ArraySingleton t ds)
--         => Evidence (Eq (Array t ds))
-- inferEq = EvValue inferBackendInstance
--
-- inferOrd :: forall t ds
--             . (Ord t, ArraySingleton t ds)
--            => Evidence (Ord (Array t ds))
-- inferOrd = EvValue inferBackendInstance
--
-- inferNum :: forall t ds
--           . (Num t, ArraySingleton t ds)
--          => Evidence (Num (Array t ds))
-- inferNum = EvValue inferBackendInstance
--
-- inferFractional :: forall t ds
--                  . (Fractional t, ArraySingleton t ds)
--                 => Evidence (Fractional (Array t ds))
-- inferFractional = EvValue inferBackendInstance
--
-- inferFloating :: forall t ds
--                . (Floating t, ArraySingleton t ds)
--               => Evidence (Floating (Array t ds))
-- inferFloating = EvValue inferBackendInstance
--
-- inferShow :: forall t ds
--            . (Show t, Dimensions ds, ArraySingleton t ds)
--           => Evidence (Show (Array t ds))
-- inferShow = EvValue inferBackendInstance
