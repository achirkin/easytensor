{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FunctionalDependencies     #-}
{-# LANGUAGE ConstraintKinds      #-}
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
    Idx (..), Dim (..), XNat, XN, N
  , SomeDim (..), someDimVal
  , Slice (..)
  , Dimensional (..), runDimensional, withDim
    -- * Operations
  , Dimensions, Dimensions' (..), Dimensions'' (..)
  , XDimensions (..)
  , inSpaceOf, order, appendIdx, splitIdx
    -- * Type-level programming
  , FixedDim, FixedXDim, KnownOrder, ValidDims
  , type (++), Reverse, Take, Drop, Length
  , type (:<), type (>:), type (:+), type (+:), Head, Tail
  ) where


import Control.Arrow (first)
import GHC.TypeLits
import GHC.Prim
import GHC.Types
import GHC.Exts
import Data.Proxy
import Data.Type.Equality

import Unsafe.Coerce

-- | Type-level dimensional indexing with arbitrary Int values inside
data Idx (ds :: [Nat]) where
   -- | Zero-rank dimensionality - scalar
   Z :: Idx '[]
   -- | List-like concatenation of indices
   (:!) :: !Int -> !(Idx ds) -> Idx (d ': ds)
infixr 5 :!

-- | Type-level dimensionality
data Dim (ds :: k) where
   -- | Zero-rank dimensionality - scalar
  D   :: Dim '[]
  -- | List-like concatenation of known dimensionality
  (:*) :: KnownNat (KnownDim d)
       => !(Proxy (KnownDim d)) -> !(Dim ds) -> Dim (d ': ds)
  -- | List-like concatenation of unknown dimensionality
  (:?) :: !SomeNat -> !(Dim ds) -> Dim (XN ': ds)
infixr 5 :*
infixr 5 :?

-- | Select a number of element from a given dimension
data Slice (n::Nat) (m::Nat) where
   Get   :: !Int -> Slice n 1
   (:&)  :: !(Slice n m) -> !Int -> Slice n (m + 1)
   Every :: Slice n n
infixl 9 :&


-- | Either known or unknown at compile-time natural number
data XNat = XN | N Nat
-- | Unknown natural number
type XN = 'XN
-- | Known natural number
type N (n::Nat) = 'N n

-- | Similar to SomeNat, hide some dimensions under an existential constructor.
data SomeDim (xns :: [XNat])
  = forall ns . ( Dimensions ns
                , ValidDims ns
                , FixedDim xns ns ~ ns
                , FixedXDim xns ns ~ xns
                ) => SomeDim (Dim ns)

-- | Construct dimensionality at runtime
someDimVal :: Dim (xns :: [XNat]) -> Maybe (SomeDim xns)
someDimVal D = Just $ SomeDim D
someDimVal xxs@(p :* xs) = do
    Refl <- isGoodDim p
    SomeDim ps <- someDimVal xs
    return $ withSuccKnown (order ps) ps
          ( \Refl -> let pps = p :* ps
                     in case ( isFixed xxs pps
                             , isXFixed xxs pps ) of
                          (Refl, Refl) -> SomeDim pps
          )
  where
    -- I know for sure that the constraint (FixedDim xns ns ~ ns) holds,
    --   but I need to convince the compiler that this is the case
    isFixed :: Dim xns -> Dim ns -> FixedDim xns ns :~: ns
    isFixed _ _ = unsafeCoerce Refl
    isXFixed :: Dim xns -> Dim ns -> FixedXDim xns ns :~: xns
    isXFixed _ _ = unsafeCoerce Refl
someDimVal (SomeNat p :? xs) = do
  Refl <- isGoodDim p
  SomeDim ps <- someDimVal xs
  return $ withSuccKnown (order ps) ps
            (\Refl -> SomeDim (p :* ps)
            )

isGoodDim :: KnownNat d => p d -> Maybe ((2 <=? d) :~: 'True)
isGoodDim p = if 2 <= natVal p then unsafeCoerce (Just Refl)
                               else unsafeCoerce Nothing


withSuccKnown :: Int
              -> p xs
              -> ( forall n . KnownNat n => (1 + Length xs) :~: n -> a)
              -> a
withSuccKnown n p g = case someNatVal (fromIntegral n) of
    Just (SomeNat m) -> g (evidence p m)
    Nothing          -> error "Something is terribly wrong. Is the length negative?"
  where
    evidence:: KnownNat m => p xs -> q m -> (1 + Length xs) :~: m
    evidence _ _ = unsafeCoerce Refl


-- | Fix runtime-obtained dimensions for use in some function
withDim :: Dim (xns :: [XNat])
        -> (forall ns . ( Dimensions ns
                        , FixedDim xns ns ~ ns
                        , FixedXDim xns ns ~ xns
                        )  => Dim ns -> a)
        -> Either String a
withDim xds f = case someDimVal xds of
  Just (SomeDim ds) -> Right $ f ds
  Nothing -> Left "Could not extract runtime naturals to construct dimensions."

-- | Provide runtime-known dimensions and execute inside functions
--   that require compile-time-known dimensions.
newtype Dimensional (xns :: [XNat]) a = Dimensional
  { _runDimensional ::
      forall ns . ( Dimensions ns
                  , FixedDim xns ns ~ ns
                  , FixedXDim xns ns ~ xns
                  ) => Dim ns -> a
  }

-- | Run Dimension-enabled computation with dimensionality known at runtime
runDimensional :: Dim xns
               -> Dimensional xns a
               -> Either String a
runDimensional xds d = withDim xds $ _runDimensional d

--------------------------------------------------------------------------------
-- * Dimension-enabled operations
--------------------------------------------------------------------------------

-- -- | Data types that can be parametrized by dimenions
-- --    - either compile-time or run-time
-- class PreservingDim (xns :: [XNat])
--                     (a :: [Nat] -> Type)
--                     (xa :: [XNat] -> Type)
--                    | a -> xa, xa -> a where
--   -- | Apply a function that requires a dixed dimension
--   withShape :: xa xns
--             -> (forall ns . ( Dimensions ns
--                             , FixedDim xns ns ~ ns
--                             , FixedXDim xns ns ~ xns
--                             ) => a ns -> b)
--             -> b
--   -- | Put some of dimensions into existential data type
--   looseDims :: ( FixedXDim xns ns ~ xns
--                , FixedDim xns ns ~ ns
--                , XDimensions ns xns
--                , Dimensions ns
--                ) => a ns -> xa xns

-- | The main constraint type.
--   With this we are sure that all dimension values are known at compile time,
--   plus we have all handy functions for `Dim` and `Idx` types.
type Dimensions xs = ( KnownDims xs
                     , KnownOrder xs
                     , Dimensions' xs
                     , Dimensions'' xs)

-- | Length of a dimension list
order :: KnownOrder xs => t xs -> Int
order = fromInteger . natVal . f
  where
    f :: t xs -> Proxy (Length xs)
    f _ = Proxy
{-# INLINE order #-}

class Dimensions' (ds :: [Nat]) where
  -- | Dimensionality of our space
  dim :: Dim ds

class XDimensions (ds :: [Nat]) (xds :: [XNat]) where
  -- | Loose compile-time information about dimensionalities
  xdim :: FixedXDim xds ds ~ xds => p ds -> Dim xds

-- | Support for Idx GADT
class Dimensions' ds => Dimensions'' (ds :: [Nat]) where
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
  succIdx   :: Idx ds -> Idx ds
  -- | For Enum
  predIdx   :: Idx ds -> Idx ds
  -- | For Enum
  fromIdx   :: Idx ds -> Int
  -- | For Enum
  toIdx     :: Int -> Idx ds
  -- | For Enum -- step dimension index by an Integer offset
  stepIdx   :: Int -> Idx ds -> Idx ds
  -- | For Enum -- difference in offsets between two Indices
  --      (a `diffIdx` b) = a - b
  diffIdx   :: Idx ds -> Idx ds -> Int

-- | Similar to `const` or `asProxyTypeOf`;
--   to be used on such implicit functions as `dim`, `dimMax`, etc.
inSpaceOf :: a ds -> b ds -> a ds
inSpaceOf x _ = x

--------------------------------------------------------------------------------
-- Some important instances
--------------------------------------------------------------------------------

instance Show (Idx ds) where
  show Z = "Idx Ø"
  show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (idxToList xs)

instance Dimensions'' ds => Show (Dim ds) where
  show D = "Dim Ø"
  show xs = "Dim" ++ foldr (\i s -> " " ++ show i ++ s) ""
    (idxToList $ dimMax `inSpaceOf` xs)

instance Show (SomeDim xns) where
  show (SomeDim p) = show p

instance Show (Dim (xds :: [XNat])) where
  show d = case someDimVal d of
    Nothing -> "Unknown dim"
    Just sd -> show sd

instance Functor (Dimensional xns) where
  fmap f d = Dimensional (f . _runDimensional d)
  {-# INLINE fmap #-}
instance Applicative (Dimensional xns) where
  pure x = Dimensional $ const x
  {-# INLINE pure #-}
  f <*> v = Dimensional $ \d -> _runDimensional f d (_runDimensional v d)
  {-# INLINE (<*>) #-}
instance Monad (Dimensional xns) where
  return  = pure
  {-# INLINE return #-}
  m >>= k = Dimensional $ \d -> _runDimensional (k $ _runDimensional m d) d
  {-# INLINE (>>=) #-}


idxToList :: Idx ds -> [Int]
idxToList Z = []
idxToList (x :! xs) = x : idxToList xs

idxFromList :: [Int] -> Idx ds
idxFromList [] = unsafeCoerce Z
idxFromList (x:xs) = unsafeCoerce $ x :! unsafeCoerce (idxFromList xs)

instance Eq (Idx ds) where
  Z == Z = True
  (a:!as) == (b:!bs) = a == b && as == bs
  Z /= Z = False
  (a:!as) /= (b:!bs) = a /= b || as /= bs

instance Eq (Dim ds) where
  D == D = True
  (_:*as) == (_:*bs) = as == bs
  (a:?as) == (b:?bs) = a == b && as == bs
  (a:*as) == (b:?bs) = SomeNat a == b && as == bs
  (a:?as) == (b:*bs) = a == SomeNat b && as == bs

-- | With this instance we can slightly reduce indexing expressions
--   e.g. x ! (1 :! 2 :! 4) == x ! (1 :! 2 :! 4 :! Z)
instance Num (Idx '[n]) where
  (a:!Z) + (b:!Z) = (a+b) :! Z
  (a:!Z) - (b:!Z) = (a-b) :! Z
  (a:!Z) * (b:!Z) = (a*b) :! Z
  signum (a:!Z)   = signum a :! Z
  abs (a:!Z)      = abs a :! Z
  fromInteger i   = fromInteger i :! Z

-- Disable this because it causes Nat ambiguity when being used
-- instance Num (Slice n 1) where
--   Get a + Get b = Get (a+b)
--   _ + _ = Get 1
--   Get a - Get b = Get (a-b)
--   _ - _ = Get 1
--   Get a * Get b = Get (a*b)
--   _ * _ = Get 1
--   signum (Get a) = Get $ signum a
--   signum x       = x
--   abs (Get a)    = Get $ abs a
--   abs x          = x
--   fromInteger i  = Get $ fromInteger i


instance Ord (Idx ds) where
  compare Z Z = EQ
  compare (a:!as) (b:!bs) = compare as bs `mappend` compare a b

instance Ord (Dim ds) where
  compare D D = EQ
  compare (_:*as) (_:*bs) = compare as bs
  compare (a:?as) (b:?bs) = compare as bs `mappend` compare a b
  compare (a:?as) (b:*bs) = compare as bs `mappend` compare a (SomeNat b)
  compare (a:*as) (b:?bs) = compare as bs `mappend` compare (SomeNat a) b

instance Dimensions' ds => Bounded (Dim ds) where
  maxBound = dim
  {-# INLINE maxBound #-}
  minBound = dim
  {-# INLINE minBound #-}

instance Dimensions'' ds => Bounded (Idx ds) where
  maxBound = dimMax
  {-# INLINE maxBound #-}
  minBound = dimMin
  {-# INLINE minBound #-}

instance Dimensions'' ds => Enum (Idx ds) where
  succ = succIdx
  {-# INLINE succ #-}
  pred = predIdx
  {-# INLINE pred #-}
  toEnum = toIdx
  {-# INLINE toEnum #-}
  fromEnum = fromIdx
  {-# INLINE fromEnum #-}
  enumFrom x = take (diffIdx maxBound x + 1) $ iterate succ x
  {-# INLINE enumFrom #-}
  enumFromTo x y | x >= y    = take (diffIdx x y + 1) $ iterate pred x
                 | otherwise = take (diffIdx y x + 1) $ iterate succ x
  {-# INLINE enumFromTo #-}
  enumFromThen x x' = take n $ iterate (stepIdx dn) x
    where
      dn = diffIdx x' x
      n  = 1 + if dn == 0 then 0
                          else if dn > 0 then diffIdx maxBound x `div` dn
                                         else diffIdx x minBound `div` negate dn
  {-# INLINE enumFromThen #-}
  enumFromThenTo x x' y = take n $ iterate (stepIdx dn) x
    where
      dn = diffIdx x' x
      n  = 1 + if dn == 0 then 0
                          else diffIdx y x `div` dn
  {-# INLINE enumFromThenTo #-}


instance IsList (Idx ds) where
  type Item (Idx ds) = Int
  fromList = idxFromList
  toList = idxToList

-- | Get the first dimension
headDim :: t (d ': ds :: [k]) -> Proxy d
headDim _ = Proxy


instance Dimensions' ('[] :: [Nat]) where
  dim = D
  {-# INLINE dim #-}

instance ( KnownOrder (d ': ds)
         , KnownDims (d ': ds)
         , Dimensions' ds
         )  => Dimensions' ((d ': ds) :: [Nat]) where
  dim = Proxy :* dim
  {-# INLINE dim #-}

instance XDimensions ns '[] where
  xdim _ = D
  {-# INLINE xdim #-}

instance ( XDimensions ns xs
         , KnownNat n
         ) => XDimensions (n ': ns) (XN ': xs) where
  xdim _ = case someNatVal (natVal $ Proxy @n) of
    Just sv -> sv :? xdim (Proxy @ns)
    Nothing -> error "Impossible happend: someNatVal (natVal n) == Nothing!"
  {-# INLINE xdim #-}

instance ( XDimensions ns xs
         , KnownNat n
         ) => XDimensions (n ': ns) (N n ': xs) where
  xdim _ = Proxy @n :* xdim (Proxy @ns)
  {-# INLINE xdim #-}


instance Dimensions'' ('[] :: [Nat]) where
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
  dropDims _ Z = unsafeCoerce Z
  {-# INLINE dropDims #-}
  takeDims _ Z = unsafeCoerce Z
  {-# INLINE takeDims #-}
  dimMax = Z
  {-# INLINE dimMax #-}
  dimMin = Z
  {-# INLINE dimMin #-}
  succIdx = id
  {-# INLINE succIdx #-}
  predIdx = id
  {-# INLINE predIdx #-}
  fromIdx _ = 0
  {-# INLINE fromIdx #-}
  toIdx _ = Z
  {-# INLINE toIdx #-}
  stepIdx _ = id
  {-# INLINE stepIdx #-}
  diffIdx _ _ = 0
  {-# INLINE diffIdx #-}

instance ( Dimensions'' ds
         , KnownDims (d ': ds)
         , KnownOrder (d ': ds)
         )
          => Dimensions'' (d ': ds) where
  totalDim _ = fromIntegral (natVal (Proxy @d))
             * totalDim (Proxy @ds)
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
  succIdx ds@(i:!is) = case fromInteger (natVal' (headDim# ds)) of
                         n -> if i == n then 1 :! succIdx is
                                        else i+1 :! is
  {-# INLINE succIdx #-}
  predIdx ds@(i:!is) = if i == 1
                       then fromInteger (natVal' (headDim# ds)) :! predIdx is
                       else i-1 :! is
  {-# INLINE predIdx #-}
  fromIdx ds@(i:!is) = i-1 + fromInteger (natVal' (headDim# ds)) * fromIdx is
  {-# INLINE fromIdx #-}
  toIdx j = r
    where
      r = case divMod j $ fromInteger (natVal' (headDim# r)) of
            (j', i) -> i+1 :! toIdx j'
  {-# INLINE toIdx #-}
  stepIdx di ds@(i:!is)
        = case divMod (di + i - 1) $ fromInteger (natVal' (headDim# ds)) of
           (0  , i') -> i'+1 :! is
           (di', i') -> i'+1 :! stepIdx di' is
  {-# INLINE stepIdx #-}
  diffIdx ds@(i1:!is1) (i2:!is2) = i1 - i2
        + fromInteger (natVal' (headDim# ds)) * diffIdx is1 is2
  {-# INLINE diffIdx #-}


appendIdx :: Idx as -> Int -> Idx (as +: b)
appendIdx Z i = i :! Z
appendIdx jjs@(j :! js) i = case proofCons jjs js of
    Refl -> unsafeCoerce $ j :! appendIdx js i
  where
    proofCons :: Idx as -> Idx bs -> as :~: (b :+ bs)
    proofCons _ _ = unsafeCoerce Refl
{-# INLINE appendIdx #-}

splitIdx :: KnownOrder as => Idx (as ++ bs) -> (Idx as, Idx bs)
splitIdx idx = rez
  where
    getAs :: (Idx as, Idx bs) -> Proxy as
    getAs _ = Proxy
    rez = splitN (order $ getAs rez) idx
    splitN :: Int -> Idx (as ++ bs) -> (Idx as, Idx bs)
    splitN 0 js = unsafeCoerce (Z, js)
    splitN n (j :! js) = first (unsafeCoerce . (j :!))
                       $ splitN (n-1) (unsafeCoerce js)
    splitN _ Z  = unsafeCoerce (Z, Z)
{-# INLINE splitIdx #-}



-- | Primitive proxy for taking head dimension
headDim# :: t (d ': ds :: [k]) -> Proxy# d
headDim# _ = proxy#
{-# INLINE headDim# #-}


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



--------------------------------------------------------------------------------
-- * Type-level programming
--------------------------------------------------------------------------------

-- | It is better to know the length of a dimension list and avoid infinite types.
type KnownOrder (ns :: [k]) = KnownNat (Length ns)

-- | A constraint family that makes sure all subdimensions are known.
type family KnownDims (ns :: [Nat]) :: Constraint where
  KnownDims '[] = ()
  KnownDims (x ': xs) = ( KnownNat x
                        , KnownOrder xs
                        , Dimensions' xs
                        , Dimensions'' xs
                        , KnownDims xs)

-- | Make sure all dimensions are not degenerate
type family ValidDims (ns :: [Nat]) :: Constraint where
  ValidDims '[] = ()
  ValidDims (x ': xs) = (2 <= x, ValidDims xs)


-- | Unify usage of XNat and Nat.
--   This is useful in function and type definitions.
--   Assumes a given XNat to be known at type-level (N n constructor).
type family KnownDim (x::k) :: Nat where
  KnownDim n = n
  KnownDim (N n) = n


-- | FixedDim puts very tight constraints on what list of naturals can be.
--   This allows establishing strong relations between [XNat] and [Nat].
type family FixedDim (xns :: [XNat]) (ns :: [Nat]) :: [Nat] where
  FixedDim '[] ns = '[]
  FixedDim (N n ': xs) ns = n ': FixedDim xs (Tail ns)
  FixedDim (XN  ': xs) ns = Head ns ': FixedDim xs (Tail ns)

type family FixedXDim (xns :: [XNat]) (ns :: [Nat]) :: [XNat] where
  FixedXDim xs '[] = '[]
  FixedXDim xs (n ': ns) = WrapHead n xs ': FixedXDim (Tail xs) ns

type family WrapHead (n :: Nat) (xs :: [XNat]) :: XNat where
  WrapHead x (N _ ': _) = N x
  WrapHead _ (XN  ': _) = XN
  WrapHead x '[]         = N x

-- | Synonym for a type-level cons
--     (injective, since this is just a synonym for the list constructor)
type (a :: k) :+ (as :: [k]) = a ': as
infixr 5 :+
-- | Synonym for a type-level snoc (injective!)
type (ns :: [k]) +: (n :: k) = EvalSnoc ('Snoc ns n)
infixl 5 +:



-- | List concatenation
type (as :: [k]) ++ (bs :: [k]) = EvalConcat ('Concat as bs)
infixr 5 ++
-- | Reverse a list (injective!)
type Reverse (xs :: [k]) = EvalReverse ('Reverse xs)
-- | Drop a number of elements
type Drop (n::Nat) (xs :: [k]) = EvalDrop ('Drop n xs)
-- | Take a number of elements
type Take (n::Nat) (xs :: [k]) = EvalTake ('Take n xs)

-- | Type-level list operations
data List k
  = NoOp [k]
  | Cons k [k]
  | Snoc [k] k
  | Concat [k] [k]
  | Reverse [k]
  | Drop Nat [k]
  | Take Nat [k]

type family EvalNoOp (xs :: List k) = (ys :: [k]) | ys -> xs where
  EvalNoOp ('NoOp '[]) = '[]
  EvalNoOp ('NoOp (x ': xs)) = x ': xs

type family EvalCons (xs :: List k) = (ys :: [k]) | ys -> xs where
  EvalCons ('Cons x xs) = x ': xs

type EvalSnoc (xs :: List k) = GetList1 (Snoc1 xs)

type family EvalConcat (xs :: List k) :: [k] where
  EvalConcat ('Concat '[a] '[b]) = '[a,b]
  EvalConcat ('Concat as (b ': bs)) = EvalConcat ('Concat (as +: b) bs)
  EvalConcat ('Concat as _) = as



type instance 'Concat as  '[]       == 'Concat bs '[] = as == bs
type instance 'Concat as (a ': as1) == 'Concat bs (b ': bs1)
            = 'Concat (as +: a) as1 == 'Concat (bs +: b) bs1
type instance 'Concat _ (_ ': _)    == 'Concat _ '[] = 'False
type instance 'Concat _   '[]       == 'Concat _ (_ ': _) = 'False

type EvalReverse (xs :: List k) = Reversed (Reverse' xs)

type family EvalDrop (xs :: List k) :: [k] where
  EvalDrop ('Drop _ '[])       = '[]
  EvalDrop ('Drop 0 xs)        = xs
  EvalDrop ('Drop n (x ': xs)) = EvalDrop ('Drop (n-1) xs)

type family EvalTake (xs :: List k) :: [k] where
  EvalTake ('Take _ '[])       = '[]
  EvalTake ('Take 0 xs)        = '[]
  EvalTake ('Take n (x ': xs)) = x ': EvalTake ('Take (n-1) xs)



type family EvalList (x :: List k) :: [k] where
  -- EvalList (xs :: [k])     = xs
  EvalList ('NoOp xs)      = EvalNoOp    ('NoOp xs)
  EvalList ('Cons x xs)    = EvalCons    ('Cons x xs)
  EvalList ('Snoc xs x)    = EvalSnoc    ('Snoc xs x)
  EvalList ('Concat xs ys) = EvalConcat  ('Concat xs ys)
  EvalList ('Reverse xs)   = EvalReverse ('Reverse xs)
  EvalList ('Drop n xs)    = EvalDrop    ('Drop n xs)
  EvalList ('Take n xs)    = EvalTake    ('Take n xs)


-- type family (:++) (x :: k) (xs::l) = (ys :: l) | ys -> xs x where
--   x :++ (xs :: [k])     = x ': xs
--   x :++ ('NoOp xs)      = 'NoOp (x ': xs)
--   y :++ ('Cons x xs)    = 'Cons y (x ': xs)
--   y :++ ('Snoc xs x)    = 'Snoc (y ': xs) x
--   y :++ ('Concat xs ys) = 'Concat (y ': xs) ys
--   y :++ ('Reverse xs)   = 'Reverse (xs +: y)
--   y :++ ('Drop 0 xs)    = 'Drop 0 (y ': xs)
--   y :++ ('Take 0 xs)    = 'Take 1 (y ': xs)
--   y :++ ('Take 1 xs)    = 'Take 2 (y ': xs)
--   y :++ ('Take 2 xs)    = 'Take 3 (y ': xs)
--   y :++ ('Take 3 xs)    = 'Take 4 (y ': xs)
--   y :++ ('Take 4 xs)    = 'Take 5 (y ': xs)
-- infixr 5 :++





-- | A weird data type used to make `(+:)` operation injective.
--   `List k [k]` must have at least two elements.
data List1 k = L1Single k | L1Head k [k]
type family Snoc1 (xs :: List k) = (ys :: List1 k) | ys -> xs where
  Snoc1 ('Snoc '[] y)        = 'L1Single y
  Snoc1 ('Snoc (x ': xs) y :: List Nat)
      = ('L1Head x (GetList1Nat (SnocNat xs y)) :: List1 Nat)
  Snoc1 ('Snoc (x ': xs) y :: List XNat)
      = ('L1Head x (GetList1XNat (SnocXNat xs y)) :: List1 XNat)
  Snoc1 ('Snoc (x ': xs) y)  = 'L1Head x (xs +: y)
type family GetList1 (ts :: List1 k) = (rs :: [k]) | rs -> ts where
  GetList1 ('L1Single x) = '[x]
  GetList1 ('L1Head y (x ':xs)) = y ': x ': xs

-- | Even more weird thing - specialization to kind Nat and XNat.
--   Otherwise, example below will not typecheck.
--   The problem, I guess, is in too many layers of type families nested.
--   Though, even if I fully annotate everything with kind signature
--            it still does not work without this weird specialization.
-- ff :: Proxy k -> Proxy (as +: k) -> Proxy (k :+ bs) -> Proxy (as ++ bs)
-- ff _ _ _ = Proxy
-- yy :: Proxy ('[3,7,2] :: [Nat])
-- yy = ff (Proxy @5) (Proxy @'[3,7,5]) (Proxy @'[5,2])
type SnocNat (ns :: [Nat]) (n :: Nat) = Snoc1 ('Snoc ns n)
type family GetList1Nat (ts :: List1 Nat) = (rs :: [Nat]) | rs -> ts where
  GetList1Nat ('L1Single x) = '[x]
  GetList1Nat ('L1Head y (x ':xs)) = y ': x ': xs
type SnocXNat (ns :: [XNat]) (n :: XNat) = Snoc1 ('Snoc ns n)
type family GetList1XNat (ts :: List1 XNat) = (rs :: [XNat]) | rs -> ts where
  GetList1XNat ('L1Single x) = '[x]
  GetList1XNat ('L1Head y (x ':xs)) = y ': x ': xs


type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
  Reversed 'REmpty = '[]
  Reversed ('Reversing ('L1Single a)) = '[a]
  Reversed ('Reversing ('L1Head y (x ':xs))) = y ': x ': xs



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

data Reversing k = REmpty | Reversing (List1 k)
type family Reverse' (as :: List k) = (rs :: Reversing k) | rs -> as where
  Reverse' ('Reverse '[]) = 'REmpty
  Reverse' ('Reverse (a ': as)) = 'Reversing
    (Snoc1 ('Snoc (Reversed (Reverse' ('Reverse as))) a))

type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + Length as
