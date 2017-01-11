{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs, TypeInType #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type Idx that enumerates through multiple dimensions.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
-- This is also to encourage column-first matrix enumeration and array layout.
--
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( -- * Data types
    Idx (..), Dim (..), XNat, XN, N, Dimensional (..)
    -- * Operations
  , Dimensions (..)
  , withDim, totalDim#, order, tailDim, headDim
  , type (++), Reverse, Take, Drop, Length, IsPrefixOf, IsSuffixOf
  , type (:<), type (>:), Head, Tail
  , Slice (..)
  ) where



import GHC.TypeLits
import GHC.Prim
import GHC.Types
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool

import Unsafe.Coerce

-- | Type-level dimensionality with arbitrary Int values inside
data Idx (ds :: [k]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Idx '[]
   -- | List-like concatenation of dimensionality
   (:!) :: !Int -> !(Idx ds) -> Idx (d ': ds)
infixr 5 :!

-- | Singleton-valued type-level dimensionality
data Dim (ds :: [k]) where
   -- | Zero-rank dimensionality - scalar
  D   :: Dim '[]
  -- | List-like concatenation of known dimensionality
  (:*) :: !(Proxy (KnownDim d)) -> !(Dim ds) -> Dim (d ': ds)
  -- | List-like concatenation of unknown dimensionality
  (:?) :: !SomeNat -> !(Dim ds) -> Dim (XN ': ds)
infixr 5 :*
infixr 5 :?

-- | Either known or unknown at compile-time natural number
data XNat = XN | N Nat
-- | Unknown natural number
type XN = 'XN
-- | Known natural number
type N (n::Nat) = 'N n

-- | Provide runtime-known dimensions and execute inside functions
--   that require compile-time-known dimensions.
newtype Dimensional (xns :: [XNat]) a = Dimensional
  { runDimensional ::
     (forall (ns :: [Nat]) . FixedDim xns ns
                          => Proxy ns -> a)
  }

instance Functor (Dimensional xns) where
  fmap f d = Dimensional (f . runDimensional d)
  {-# INLINE fmap #-}
instance Applicative (Dimensional xns) where
  pure x = Dimensional $ const x
  {-# INLINE pure #-}
  f <*> v = Dimensional $ \d -> runDimensional f d (runDimensional v d)
  {-# INLINE (<*>) #-}
instance Monad (Dimensional xns) where
  return  = pure
  {-# INLINE return #-}
  m >>= k = Dimensional $ \d -> runDimensional (k $ runDimensional m d) d
  {-# INLINE (>>=) #-}



-- | Fix runtime-obtained dimensions for some function
withDim :: Dim (xns :: [XNat])
        -> (forall (ns :: [Nat]) . FixedDim xns ns
             => Proxy ns -> a)
        -> a
withDim D f = f (Proxy :: Proxy '[])
-- withDim xxs@(p :* xs) f = withDim xs (fixDims' f p)
withDim xxs@(SomeNat p :? xs) f = withDim xs (fixDims' f p)



withDim' :: FixedDim (WrapNats matched) matched
         => Proxy (WrapNats matched)
         -> Dim (xns :: [XNat])
         -> (forall (ns :: [Nat]) . FixedDim (WrapNats matched ++ xns)
                                             (matched  ++ ns)
             => Proxy ns -> a)
         -> a
withDim' matched D f = f Proxy
withDim' matched xxs@(p :* xs) f = withDim'
  (oneMoreMatched matched p) xs (fixDims' f p)
-- withDim' xxs@(SomeNat p :? xs) f = withDim' xs (fixDims' xxs f p)
--
oneMoreMatched :: Proxy ns -> Proxy n -> Proxy (ns +: N n)
oneMoreMatched _ _ = Proxy

oneMore :: Proxy n -> Proxy ns -> Proxy (n :+ ns)
oneMore _ _ = Proxy

fixDims' :: (Proxy (n ': ns) -> a) -> Proxy n -> Proxy ns -> a
fixDims' f _ _ = f Proxy
{-# INLINE fixDims' #-}

type family KnownDim (x::k) :: Nat where
  KnownDim n = n
  KnownDim (N n) = n

type family WrapNats (ns :: k Nat) = (xns :: l XNat) | xns -> ns where
  WrapNats '[] = '[]
  WrapNats (n ': ns) = N n ': WrapNats ns
  WrapNats 'LEmpty = 'LEmpty
  WrapNats ('LSingle n) = 'LSingle (N n)
  WrapNats ('List n ns) = 'List (N n) (WrapNats ns)
  WrapNats 'REmpty = 'REmpty
  WrapNats ('Reversing ns) = 'Reversing (WrapNats ns)

-- LEmpty | LSingle k | List k [k]

type family FixedDim (xns :: [XNat]) (ns :: [Nat]) :: Constraint where
  FixedDim (XN  ': xns) (_ ': ns) = FixedDim xns ns
  FixedDim (N n ': xns) (n ': ns) = FixedDim xns ns
  FixedDim '[]          '[]       = ()
  FixedDim _            _         = TypeError
    ( 'Text
      "Dimensionalities have different lengths."
    )

-- type family IsFixedDim (xns :: [XNat]) (ns :: [Nat]) :: Bool where
--   IsFixedDim (XN  ': xns) (_ ': ns) = IsFixedDim xns ns
--   IsFixedDim (N n ': xns) (m ': ns) = (n == m) && IsFixedDim xns ns
--   IsFixedDim '[]          '[]       = 'True
--   IsFixedDim _            _         = 'False


-- data FixingDims = FixingDims [Nat] [Nat] [XNat]
-- type family UsedFixers (x::FixingDims) :: [Nat] where
--   UsedFixers ('FixingDims xs _ _) = xs
-- type family RemainingFixers (x::FixingDims) :: [Nat] where
--   RemainingFixers ('FixingDims _ xs _) = xs
-- type family UpdatedDims (x::FixingDims) :: [XNat] where
--   UpdatedDims ('FixingDims _ _ xs) = xs
--
-- type family FixDims (ns :: [Nat]) (xns :: [XNat]) :: FixingDims where

-- -- | Type-level checked dimensionality (unboxed)
-- data Idx# (ds :: [k]) where
--    -- | Zero-rank dimensionality - scalar
--    Z# :: Idx# '[]
--    -- | List-like concatenation of dimensionality
--    (:#) :: Int# -> Idx# ds -> Idx# (n':ds)
-- infixr 5 :#

-- data SomeDim (ds :: [XNat])
--   = SomeDim
--   { dimVals ::
--   }

-- data Dimensional a
--   = forall ds . Dimensions ds => Dimensional
--   { _dimVals :: Idx ds
--   , _dimContent :: a
--   }
--
-- someDimVal :: ByteArray# -> a
--            -> Dimensional a
-- someDimVal = undefined

--   =
-- data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)

-- withDimensions :: forall (a :: forall k . [k] -> *) b (xds :: [XNat]) (ds :: [Nat])
--                . a xds -> (a ds -> b) -> b
-- withDimensions = undefined

-- fixDimensions :: forall (a :: forall k . [k] -> *) b (xds :: [XNat]) (ds :: [Nat])
--               . a xds -> Dimensional xds (a ds)
-- fixDimensions = undefined

data Slice (n::Nat) (m::Nat) where
   Get   :: !Int -> Slice n 1
   (:&)  :: !(Slice n m) -> !Int -> Slice n (m + 1)
   Every :: Slice n n
infixl 9 :&



instance Show (Idx ds) where
  show Z = "Idx Ø"
  show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (dimToList xs)

dimToList :: Idx ds -> [Int]
dimToList Z = []
dimToList (x :! xs) = x : dimToList xs

dimFromList :: [Int] -> Idx ds
dimFromList [] = unsafeCoerce Z
dimFromList (x:xs) = unsafeCoerce $ x :! unsafeCoerce (dimFromList xs)

instance Eq (Idx ds) where
  Z == Z = True
  (a:!as) == (b:!bs) = a == b && as == bs
  Z /= Z = False
  (a:!as) /= (b:!bs) = a /= b || as /= bs

instance Ord (Idx ds) where
  compare Z Z = EQ
  compare (a:!as) (b:!bs) = compare as bs `mappend` compare a b

instance Dimensions ds => Bounded (Idx ds) where
  maxBound = dimMax
  {-# INLINE maxBound #-}
  minBound = dimMin
  {-# INLINE minBound #-}

instance Dimensions ds => Enum (Idx ds) where
  succ = succDim
  {-# INLINE succ #-}
  pred = predDim
  {-# INLINE pred #-}
  toEnum = toDim
  {-# INLINE toEnum #-}
  fromEnum = fromDim
  {-# INLINE fromEnum #-}
  enumFrom x = take (diffDim maxBound x + 1) $ iterate succ x
  {-# INLINE enumFrom #-}
  enumFromTo x y | x >= y    = take (diffDim x y + 1) $ iterate pred x
                 | otherwise = take (diffDim y x + 1) $ iterate succ x
  {-# INLINE enumFromTo #-}
  enumFromThen x x' = take n $ iterate (stepDim dn) x
    where
      dn = diffDim x' x
      n  = 1 + if dn == 0 then 0
                          else if dn > 0 then diffDim maxBound x `div` dn
                                         else diffDim x minBound `div` negate dn
  {-# INLINE enumFromThen #-}
  enumFromThenTo x x' y = take n $ iterate (stepDim dn) x
    where
      dn = diffDim x' x
      n  = 1 + if dn == 0 then 0
                          else diffDim y x `div` dn
  {-# INLINE enumFromThenTo #-}


-- | Support for Idx GADT
class Dimensions (ds :: [k]) where
  type TotalDim ds :: l
  -- | Dimensionality in type naturals
  toNats :: Idx ds -> Idx (AllNats ds)
  -- | Dimensionality of a second rank type
  dim :: t ds -> Idx ds
  -- | Total number of elements - product of all dimension sizes (unboxed)
  totalDim :: t ds -> Int
  -- | Run a primitive loop over all dimensions (1..n)
  loopS  :: Idx  ds -> (Idx  ds -> State# s -> State# s) -> State# s -> State# s
  -- | Run a loop over all dimensions keeping a boxed accumulator (1..n)
  loopA  :: Idx  ds -> (Idx  ds -> a -> a) -> a -> a
  -- | Run a loop in a reverse order n..1
  loopReverse :: Idx ds -> (Idx  ds -> a -> a) -> a -> a
  -- | Get index offset: i1 + i2*n1 + i3*n1*n2 + ...
  ioffset   :: Idx ds -> Int
  -- | Drop a number of dimensions
  dropDims  :: KnownNat n => Proxy n -> Idx ds -> Idx (Drop n ds)
  -- | Take a number of dimensions
  takeDims  :: KnownNat n => Proxy n -> Idx ds -> Idx (Take n ds)
  -- | Maximum values of all dimensions
  dimMax    :: Idx ds
  -- | Minimum values -- ones
  dimMin    :: Idx ds
  -- | For Enum
  succDim   :: Idx ds -> Idx ds
  -- | For Enum
  predDim   :: Idx ds -> Idx ds
  -- | For Enum
  fromDim   :: Idx ds -> Int
  -- | For Enum
  toDim     :: Int -> Idx ds
  -- | For Enum -- step dimension index by an Integer offset
  stepDim   :: Int -> Idx ds -> Idx ds
  -- | For Enum -- difference in offsets between two Dims (a `diffDim` b) = a - b
  diffDim   :: Idx ds -> Idx ds -> Int


instance IsList (Idx ds) where
  type Item (Idx ds) = Int
  fromList = dimFromList
  toList = dimToList

-- | Get all but first dimensions
tailDim :: Idx ds -> Idx (Drop 1 ds)
tailDim Z = Z
tailDim (_:!xs) = xs

-- | Get the first dimension
headDim :: t (d ': ds :: [Nat]) -> Proxy d
headDim _ = Proxy

-- | Total number of elements - product of all dimension sizes
totalDim# :: Dimensions ds => t ds -> Int#
totalDim# x = case totalDim x of I# n -> n
{-# INLINE totalDim# #-}

-- | Number of dimensions
order :: Idx ds -> Int
order Z = 0
order (_:!xs) = 1 + order xs
{-# INLINE order #-}


instance Dimensions ('[] :: [Nat]) where
  type TotalDim ('[] :: [Nat]) = 1
  toNats = id
  {-# INLINE toNats #-}
  dim _ = Z
  {-# INLINE dim #-}
  totalDim _ = 1
  {-# INLINE totalDim #-}
  loopS _ f = f Z
  {-# INLINE loopS #-}
  loopA _ f = f Z
  {-# INLINE loopA #-}
  loopReverse _ f = f Z
  {-# INLINE loopReverse #-}
  ioffset _ = 0
  {-# INLINE ioffset #-}
  dropDims _ Z = Z
  {-# INLINE dropDims #-}
  takeDims _ Z = Z
  {-# INLINE takeDims #-}
  dimMax = Z
  {-# INLINE dimMax #-}
  dimMin = Z
  {-# INLINE dimMin #-}
  succDim = id
  {-# INLINE succDim #-}
  predDim = id
  {-# INLINE predDim #-}
  fromDim _ = 0
  {-# INLINE fromDim #-}
  toDim _ = Z
  {-# INLINE toDim #-}
  stepDim _ = id
  {-# INLINE stepDim #-}
  diffDim _ _ = 0
  {-# INLINE diffDim #-}

instance (KnownNat d, KnownNat (TotalDim (d ': ds)), Dimensions ds)
          => Dimensions (d ': ds) where
  type TotalDim (d ': ds) = d GHC.TypeLits.* TotalDim ds
  toNats = id
  {-# INLINE toNats #-}
  dim x = fromIntegral (natVal' (headDim# x)) :! dim (tailDim# x)
  {-# INLINE dim #-}
  totalDim _ = fromIntegral $ natVal ( Proxy :: Proxy (TotalDim (d ': ds)) )
  {-# INLINE totalDim #-}
  loopS (n:!Z) f = loop1 n (\i -> f (i:!Z))
  loopS (n:!ns) f = loopS ns (\js -> loop1 n (\i -> f (i:!js)))
  {-# INLINE loopS #-}
  loopA (n:!Z) f = loopA1 n (f . (:!Z))
  loopA (n:!ns) f = loopA ns (\js -> loopA1 n (f . (:!js)))
  {-# INLINE loopA #-}
  loopReverse (n:!Z) f = loopReverse1 n (f . (:!Z))
  loopReverse (n:!ns) f = loopReverse ns (\js -> loopReverse1 n (f . (:!js)))
  {-# INLINE loopReverse #-}
  ioffset (i:!Z) = i
  ioffset iis@(i:!is) = i + fromIntegral (natVal' (headDim# iis)) * ioffset is
  {-# INLINE ioffset #-}
  dropDims p ds = case (fromInteger (natVal p), order ds) of
          (0, _) -> unsafeCoerce ds
          (n, k) -> if n >= k then unsafeCoerce Z
                              else f n ds
    where
      f 0 ds' = unsafeCoerce ds'
      f i (_:!ds') = unsafeCoerce (f (i-1) $ unsafeCoerce ds')
      f _ Z = unsafeCoerce Z
  {-# INLINE dropDims #-}
  takeDims p ds = case (fromInteger (natVal p), order ds) of
          (0, _) -> unsafeCoerce Z
          (n, k) -> if n >= k then unsafeCoerce ds
                              else f n ds
    where
      f 0 _ = unsafeCoerce Z
      f i (d:!ds') = unsafeCoerce $ d :! unsafeCoerce (f (i-1) $ unsafeCoerce ds')
      f _ Z = unsafeCoerce Z
  {-# INLINE takeDims #-}
  dimMax = ds
    where
      ds = fromInteger (natVal $ headDim ds) :! dimMax
  {-# INLINE dimMax #-}
  dimMin = 1 :! dimMin
  {-# INLINE dimMin #-}
  succDim ds@(i:!is) = case fromInteger (natVal' (headDim# ds)) of
                         n -> if i == n then 1 :! succDim is
                                        else i+1 :! is
  {-# INLINE succDim #-}
  predDim ds@(i:!is) = if i == 1
                       then fromInteger (natVal' (headDim# ds)) :! predDim is
                       else i-1 :! is
  {-# INLINE predDim #-}
  fromDim ds@(i:!is) = i-1 + fromInteger (natVal' (headDim# ds)) * fromDim is
  {-# INLINE fromDim #-}
  toDim j = r
    where
      r = case divMod j $ fromInteger (natVal' (headDim# r)) of
            (j', i) -> i+1 :! toDim j'
  {-# INLINE toDim #-}
  stepDim di ds@(i:!is)
        = case divMod (di + i - 1) $ fromInteger (natVal' (headDim# ds)) of
           (0  , i') -> i'+1 :! is
           (di', i') -> i'+1 :! stepDim di' is
  {-# INLINE stepDim #-}
  diffDim ds@(i1:!is1) (i2:!is2) = i1 - i2
        + fromInteger (natVal' (headDim# ds)) * diffDim is1 is2
  {-# INLINE diffDim #-}

-- instance ( Dimensions (AllNats ds)
--            ) => Dimensions (ds :: [XNat]) where
--   type TotalDim ds = TotalDim (AllNats ds)
--   toNats Z = Z
--   toNats xs@(_ :! _) = unsafeCoerce xs
--   -- TODO: try safe coercion
--   {-# INLINE toNats #-}
--   -- dim = _
--   -- {-# INLINE dim #-}
--   totalDim _ = totalDim ( Proxy :: Proxy (AllNats ds) )
--   {-# INLINE totalDim #-}
--   -- loopS _ f = f Z
--   -- {-# INLINE loopS #-}
--   -- loopA _ f = f Z
--   -- {-# INLINE loopA #-}
--   -- loopReverse _ f = f Z
--   -- {-# INLINE loopReverse #-}
--   -- -- ioffset# _ = 0#
--   -- -- {-# INLINE ioffset# #-}
--   -- ioffset _ = 0
--   -- {-# INLINE ioffset #-}
--   -- dropDims _ Z = Z
--   -- {-# INLINE dropDims #-}
--   -- takeDims _ Z = Z
--   -- {-# INLINE takeDims #-}
--   -- dimMax = Z
--   -- {-# INLINE dimMax #-}
--   -- dimMin = Z
--   -- {-# INLINE dimMin #-}
--   -- -- dimZero# = Z#
--   -- -- {-# INLINE dimZero# #-}
--   -- succDim = id
--   -- {-# INLINE succDim #-}
--   -- predDim = id
--   -- {-# INLINE predDim #-}
--   -- fromDim _ = 0
--   -- {-# INLINE fromDim #-}
--   -- toDim _ = Z
--   -- {-# INLINE toDim #-}
--   -- stepDim _ = id
--   -- {-# INLINE stepDim #-}
--   -- diffDim _ _ = 0
--   -- {-# INLINE diffDim #-}



headDim# :: t (d ': ds :: [k]) -> Proxy# d
headDim# _ = proxy#
{-# INLINE headDim# #-}

tailDim# :: t (d ': ds :: [k]) -> Proxy ds
tailDim# _ = Proxy
{-# INLINE tailDim# #-}

-- -- | Do something in a loop for int i from 0 to n-1
-- loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
-- loop1# n f = loop' 0#
--   where
--     loop' i s | isTrue# (i ==# n) = s
--               | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
-- {-# INLINE loop1# #-}



-- | Do something in a loop for int i from 1 to n
loop1 :: Int -> (Int -> State# s -> State# s) -> State# s -> State# s
loop1 n f = loop' 1
  where
    loop' i s | i > n = s
              | otherwise = case f i s of s1 -> loop' (i + 1) s1
{-# INLINE loop1 #-}

-- | Do something in a loop for int i from 1 to n
loopA1 :: Int -> (Int -> a -> a) -> a -> a
loopA1 n f = loop' 1
  where
    loop' i s | i > n = s
              | otherwise = case f i s of s1 -> loop' (i + 1) s1
{-# INLINE loopA1 #-}

-- | Do something in a loop for int i from n to 1
loopReverse1 :: Int -> (Int -> a -> a) -> a -> a
loopReverse1 n f = loop' n
  where
    loop' i s | i == 0 = s
              | otherwise = case f i s of s1 -> loop' (i - 1) s1
{-# INLINE loopReverse1 #-}


-- type family FixDim (xs :: [XNat]) n :: [XNat] where
--   FixDim (XN ': xs) n = N n ': xs
--   FixDim (N m ': xs) n = N m ': FixDim xs n
--   FixDim '[] n = TypeError
--          (      'Text "Can't fix dimension "
--           ':<>: 'ShowType n
--           ':<>: 'Text " because all dimensions are known already."
--          )
--
--
-- data FixingDims = FixingDims [Nat] [Nat] [XNat]
-- type family UsedFixers (x::FixingDims) :: [Nat] where
--   UsedFixers ('FixingDims xs _ _) = xs
-- type family RemainingFixers (x::FixingDims) :: [Nat] where
--   RemainingFixers ('FixingDims _ xs _) = xs
-- type family UpdatedDims (x::FixingDims) :: [XNat] where
--   UpdatedDims ('FixingDims _ _ xs) = xs
--
-- type family FixDims (ns :: [Nat]) (xns :: [XNat]) :: FixingDims where
--   FixDims ns xns = FixDimsAcc ns xns ('FixingDims '[] '[] '[])
--
-- type family FixDimsAcc (ns :: [Nat]) (xns :: [XNat]) (x::FixingDims)
--                     :: FixingDims where
--   FixDimsAcc '[] '[] fd = fd
--   FixDimsAcc '[] xns ('FixingDims un rn ud)
--     = 'FixingDims un rn (xns ++ ud)
--   FixDimsAcc ns '[] ('FixingDims un rn ud)
--     = 'FixingDims un (ns ++ rn) ud
--   FixDimsAcc ns (N n ': xns) ('FixingDims un rn ud)
--     = FixDimsAcc ns xns ('FixingDims un rn (N n ': ud))
--   FixDimsAcc (n ': ns) (XN ': xns) ('FixingDims un rn ud)
--     = FixDimsAcc ns xns ('FixingDims (n ': un) rn (N n ': ud))

-----------------------------------------------------------------------------
-- * Type-level programming
-----------------------------------------------------------------------------

-- | Substitute a type in a list elements
-- type family (a :: k) <$ (ds :: [l]) :: [k] where
--   _ <$ '[] = '[]
--   x <$ (_ ': bs) = x ': (x <$ bs)

-- | Synonym for a type-level cons
type a :+ as = a ': as
infixl 5 :+
-- | Synonym for a type-level snoc
type (ns :: [k]) +: (n :: k) = GetList (Snoc ns n)
infixl 5 +:

data List k = LEmpty | LSingle k | List k [k]
type family Snoc (ns :: [k]) (n :: k) = (rs :: List k) | rs -> ns n where
  Snoc '[] n        = 'LSingle n
  Snoc (x :+ xs) n  = 'List x (GetList (Snoc xs n))
type family GetList (ts :: List k) = (rs :: [k]) | rs -> ts where
  GetList 'LEmpty = '[]
  GetList ('LSingle x) = '[x]
  GetList ('List y (x ':xs)) = y ': x ': xs

-- | List concatenation
type as ++ bs = GetConcat ('Concat as bs)
infixr 5 ++

data Concat k = Concat [k] [k]
type instance 'Concat as '[] == 'Concat bs '[] = as == bs
type instance 'Concat as (a ': as1) == 'Concat bs (b ': bs1)
  = 'Concat (as +: a) as1 == 'Concat (bs +: b) bs1
type instance 'Concat _ (_ ': _) == 'Concat _ '[] = 'False
type instance 'Concat _ '[] == 'Concat _ (_ ': _) = 'False
type family GetConcat (c :: Concat k) :: [k] where
  GetConcat ('Concat as '[]) = as
  GetConcat ('Concat as (b ': bs)) = GetConcat ('Concat (as +: b) bs)


-- | Synonym for (:+) that ignores Nat values 0 and 1
type family (n :: Nat) :< (ns :: [Nat]) :: [Nat] where
  0 :< ns = ns
  1 :< ns = ns
  n :< ns = n :+ ns
infixr 6 :<

-- | Synonym for (+:) that ignores Nat values 0 and 1
type family (ns :: [Nat]) >: (n :: Nat) :: [Nat] where
  ns >: 0 = ns
  ns >: 1 = ns
  ns >: n = ns +: n
infixl 6 >:





type family AllNats (xs :: [k]) :: [Nat] where
  AllNats '[]           = '[]
  AllNats (xs :: [Nat]) = xs
  AllNats (N n ': xs)   = n ': AllNats xs
  AllNats (XN ': _)     = TypeError ( 'Text
    "Can't map XNat to Nat, because some dimensions aren't known at compile time."
   )

type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x
  Head '[]       = TypeError ( 'Text
    "Head -- empty type-level list."
   )

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  Tail '[]       = TypeError ( 'Text
    "Tail -- empty type-level list."
   )

-- | Reverse a list
type Reverse (xs :: [k]) = Reversed (Reverse' xs)
data Reversing k = REmpty | Reversing (List k)
type family Reverse' (as :: [k]) = (rs :: Reversing k) | rs -> as where
  Reverse' '[] = 'REmpty
  Reverse' (a ': as) = 'Reversing (Snoc (Reversed (Reverse' as)) a)
type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
  Reversed 'REmpty = '[]
  Reversed ('Reversing ('LSingle a)) = '[a]
  Reversed ('Reversing ('List y (x ':xs))) = y ': x ': xs


-- -- xx :: Proxy 'True
-- xx :: Proxy (('[2,3,4] +: 1) == '[2,3,4,1])
-- xx = _
--
-- -- yy :: Proxy 'True
-- yy :: Proxy ((Reverse (Reverse '[2,5,7,1])) == Reverse '[1,7,5,2])
-- yy = _


type family IsPrefixOf (as :: [k]) (bs :: [k]) :: Bool where
  IsPrefixOf '[] _ = 'True
  IsPrefixOf (a ': as)  (b ': bs) = a == b && IsPrefixOf as bs
  IsPrefixOf _ _ = 'False

type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + Length as

type family Take (n::Nat) (as :: [k]) :: [k] where
  Take _ '[] = '[]
  Take 0 _   = '[]
  Take n (a ': as) = a ': Take (n-1) as

type family Drop (n::Nat) (as :: [k]) :: [k] where
  Drop _ '[] = '[]
  Drop 0 as  = as
  Drop n (a ': as) = Drop (n-1) as

type family IsSuffixOf (as :: [k]) (bs :: [k]) :: Bool where
  IsSuffixOf '[] _ = 'True
  IsSuffixOf as bs = If (CmpNat (Length as) (Length bs) == 'GT)
                         'False
                         (as == Drop (Length bs - Length as) bs)
