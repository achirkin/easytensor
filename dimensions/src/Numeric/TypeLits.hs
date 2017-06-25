{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.TypeLits
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This modules is based on `GHC.TypeLits` and re-exports its functionality.
-- It provides `KnownDim` class that is similar to `KnownNat`, but keeps
-- `Int`s instead of `Integer`s.
-- A set of utility functions provide inference functionality, so
-- that `KnownDim` can be preserved over some type-level operations.
--
-----------------------------------------------------------------------------

module Numeric.TypeLits
  ( -- * Nats backed by Int
    SomeIntNat (..), someIntNatVal, intNatVal, reifyDim
  , KnownDim (..), KnownDims, dimVal#, Proxy#, proxy#
    -- * Dynamically constructing evidence
  , Evidence (..), withEvidence, sumEvs
  , inferPlusKnownDim, inferMinusKnownDim, inferMinusKnownDimM
  , inferTimesKnownDim
    -- * Re-export original GHC TypeLits
  , module GHC.TypeLits
  , Proxy (..)
  ) where


import           Data.Proxy    (Proxy(..))
import           GHC.Exts      (Constraint, Proxy#, proxy#)
import           GHC.TypeLits
import           GHC.Types     (Type)
import           Unsafe.Coerce (unsafeCoerce)


-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeIntNat = forall (n :: Nat) . KnownDim n => SomeIntNat (Proxy n)



-- | This class gives the int associated with a type-level natural.
--   Valid known dim must be not less than 0.
class KnownDim (n :: Nat) where
    -- | Get value of type-level dim at runtime
    dimVal' :: Int

-- | A constraint family that makes sure all subdimensions are known.
type family KnownDims (ns :: [Nat]) :: Constraint where
    KnownDims '[] = ()
    KnownDims (x ': xs) = ( KnownDim x, KnownDims xs )

-- | A variant of `dimVal'` that gets `Proxy#` as an argument.
dimVal# :: forall (n :: Nat) . KnownDim n => Proxy# n -> Int
dimVal# _ = dimVal' @n
{-# INLINE dimVal# #-}

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Int`.
intNatVal :: forall n proxy . KnownDim n => proxy n -> Int
intNatVal _ = dimVal' @n

instance {-# OVERLAPPABLE #-} KnownNat n => KnownDim n where
    {-# INLINE dimVal' #-}
    dimVal' = fromInteger (natVal' (proxy# :: Proxy# n))

instance {-# OVERLAPPING #-} KnownDim 0  where { {-# INLINE dimVal' #-}; dimVal' = 0 }
instance {-# OVERLAPPING #-} KnownDim 1  where { {-# INLINE dimVal' #-}; dimVal' = 1 }
instance {-# OVERLAPPING #-} KnownDim 2  where { {-# INLINE dimVal' #-}; dimVal' = 2 }
instance {-# OVERLAPPING #-} KnownDim 3  where { {-# INLINE dimVal' #-}; dimVal' = 3 }
instance {-# OVERLAPPING #-} KnownDim 4  where { {-# INLINE dimVal' #-}; dimVal' = 4 }
instance {-# OVERLAPPING #-} KnownDim 5  where { {-# INLINE dimVal' #-}; dimVal' = 5 }
instance {-# OVERLAPPING #-} KnownDim 6  where { {-# INLINE dimVal' #-}; dimVal' = 6 }
instance {-# OVERLAPPING #-} KnownDim 7  where { {-# INLINE dimVal' #-}; dimVal' = 7 }
instance {-# OVERLAPPING #-} KnownDim 8  where { {-# INLINE dimVal' #-}; dimVal' = 8 }
instance {-# OVERLAPPING #-} KnownDim 9  where { {-# INLINE dimVal' #-}; dimVal' = 9 }
instance {-# OVERLAPPING #-} KnownDim 10 where { {-# INLINE dimVal' #-}; dimVal' = 10 }
instance {-# OVERLAPPING #-} KnownDim 11 where { {-# INLINE dimVal' #-}; dimVal' = 11 }
instance {-# OVERLAPPING #-} KnownDim 12 where { {-# INLINE dimVal' #-}; dimVal' = 12 }
instance {-# OVERLAPPING #-} KnownDim 13 where { {-# INLINE dimVal' #-}; dimVal' = 13 }
instance {-# OVERLAPPING #-} KnownDim 14 where { {-# INLINE dimVal' #-}; dimVal' = 14 }
instance {-# OVERLAPPING #-} KnownDim 15 where { {-# INLINE dimVal' #-}; dimVal' = 15 }
instance {-# OVERLAPPING #-} KnownDim 16 where { {-# INLINE dimVal' #-}; dimVal' = 16 }
instance {-# OVERLAPPING #-} KnownDim 17 where { {-# INLINE dimVal' #-}; dimVal' = 17 }
instance {-# OVERLAPPING #-} KnownDim 18 where { {-# INLINE dimVal' #-}; dimVal' = 18 }
instance {-# OVERLAPPING #-} KnownDim 19 where { {-# INLINE dimVal' #-}; dimVal' = 19 }
instance {-# OVERLAPPING #-} KnownDim 20 where { {-# INLINE dimVal' #-}; dimVal' = 20 }


-- | Similar to `someNatVal`, but for a single dimension
someIntNatVal :: Int -> Maybe SomeIntNat
someIntNatVal x | 0 > x     = Nothing
                | otherwise = Just (reifyDim x f)
  where
    f :: forall (n :: Nat) . KnownDim n => Proxy# n -> SomeIntNat
    f _ = SomeIntNat (Proxy @n)
{-# INLINE someIntNatVal #-}


-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall r . Int -> (forall (n :: Nat) . KnownDim n => Proxy# n -> r) -> r
reifyDim n k = unsafeCoerce (MagicDim k :: MagicDim r) n proxy#
{-# INLINE reifyDim #-}
newtype MagicDim r = MagicDim (forall (n :: Nat) . KnownDim n => Proxy# n -> r)


instance Eq SomeIntNat where
  SomeIntNat x == SomeIntNat y = intNatVal x == intNatVal y

instance Ord SomeIntNat where
  compare (SomeIntNat x) (SomeIntNat y) = compare (intNatVal x) (intNatVal y)

instance Show SomeIntNat where
  showsPrec p (SomeIntNat x) = showsPrec p (intNatVal x)

instance Read SomeIntNat where
  readsPrec p xs = do (a,ys) <- readsPrec p xs
                      case someIntNatVal a of
                        Nothing -> []
                        Just n  -> [(n,ys)]


-- | Bring an instance of certain class or constaint satisfaction evidence into scope.
data Evidence :: Constraint -> Type where
    Evidence :: a => Evidence a

sumEvs :: Evidence a -> Evidence b -> Evidence (a,b)
sumEvs Evidence Evidence = Evidence
{-# INLINE sumEvs #-}

withEvidence :: Evidence a -> (a => r) -> r
withEvidence d r = case d of Evidence -> r
{-# INLINE withEvidence #-}

mkKDEv :: forall (m :: Nat) (n :: Nat) . KnownDim n => Proxy# n -> Evidence (KnownDim m)
mkKDEv _ = unsafeCoerce $ Evidence @(KnownDim n)
{-# INLINE mkKDEv #-}

inferPlusKnownDim :: forall n m . (KnownDim n, KnownDim m) => Evidence (KnownDim (n + m))
inferPlusKnownDim = reifyDim (dimVal' @n + dimVal' @m) (mkKDEv @(n + m))
{-# INLINE inferPlusKnownDim #-}

inferMinusKnownDim :: forall n m . (KnownDim n, KnownDim m, m <= n) => Evidence (KnownDim (n - m))
inferMinusKnownDim = reifyDim (dimVal' @n - dimVal' @m) (mkKDEv @(n - m))
{-# INLINE inferMinusKnownDim #-}

inferMinusKnownDimM :: forall n m . (KnownDim n, KnownDim m) => Maybe (Evidence (KnownDim (n - m)))
inferMinusKnownDimM = if v >= 0 then Just $ reifyDim v (mkKDEv @(n - m))
                                else Nothing
  where
    v = dimVal' @n - dimVal' @m
{-# INLINE inferMinusKnownDimM #-}

inferTimesKnownDim :: forall n m . (KnownDim n, KnownDim m) => Evidence (KnownDim (n * m))
inferTimesKnownDim = reifyDim (dimVal' @n * dimVal' @m) (mkKDEv @(n * m))
{-# INLINE inferTimesKnownDim #-}
