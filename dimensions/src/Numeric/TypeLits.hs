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
--
-----------------------------------------------------------------------------

module Numeric.TypeLits
  ( -- * Nats backed by Int
    SomeDim (..), someDimVal, reifyDim
  , KnownDim (..), KnownDims, dimVal#, Proxy#, proxy#
    -- * Dynamically constructing evidence
  , Evidence (..), withEvidence, sumEvs
  , inferPlusKnownDim, inferMinusKnownDim, inferMinusKnownDimM
  , inferTimesKnownDim
    -- * Re-export original GHC TypeLits
  , module GHC.TypeLits
  ) where


import           GHC.Exts      (Constraint, Proxy#, proxy#)
import           GHC.TypeLits
import           GHC.Types     (Type)
import           Unsafe.Coerce (unsafeCoerce)


-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDim = forall (n :: Nat) . KnownDim n => SomeDim (Proxy# n)

-- | This class gives the int associated with a type-level natural.
--   Valid known dim must be not less than 2.
class KnownDim (n :: Nat) where
    -- | Get value of type-level dim at runtime
    dimVal' :: Int

-- | A constraint family that makes sure all subdimensions are known.
type family KnownDims (ns :: [Nat]) :: Constraint where
    KnownDims '[] = ()
    KnownDims (x ': xs) = ( KnownDim x, KnownDims xs )

dimVal# :: forall (n :: Nat) . KnownDim n => Proxy# n -> Int
dimVal# _ = dimVal' @n
{-# INLINE dimVal# #-}

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
someDimVal :: Int -> Maybe SomeDim
someDimVal x | 0 > x     = Nothing
             | otherwise = Just (reifyDim x SomeDim)
{-# INLINE someDimVal #-}


-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDim :: forall r . Int -> (forall (n :: Nat) . KnownDim n => Proxy# n -> r) -> r
reifyDim n k = unsafeCoerce (MagicDim k :: MagicDim r) n proxy#
{-# INLINE reifyDim #-}
newtype MagicDim r = MagicDim (forall (n :: Nat) . KnownDim n => Proxy# n -> r)


instance Eq SomeDim where
  SomeDim x == SomeDim y = dimVal# x == dimVal# y

instance Ord SomeDim where
  compare (SomeDim x) (SomeDim y) = compare (dimVal# x) (dimVal# y)

instance Show SomeDim where
  showsPrec p (SomeDim x) = showsPrec p (dimVal# x)

instance Read SomeDim where
  readsPrec p xs = do (a,ys) <- readsPrec p xs
                      case someDimVal a of
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
