{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE GADTs, PolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FunctionalDependencies     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UnboxedTuples      #-}
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
-- Some of the type-level list operations are implemented using type families
--   and weirdly duplicated for kinds k,Nat,XNat:
--   This duplication is needed to workaround some GHC bugs (panic with "no skolem info")
-----------------------------------------------------------------------------

module Numeric.Dimensions
  ( -- * Data types
    Idx (..), Dim (..), XNat, XN, N
  , SomeDim (..), someDimVal
  , Slice (..)
  , Dimensional (..), runDimensional, withDim, withRuntimeDim
    -- * Operations
  , Dimensions, Dimensions' (..), Dimensions'' (..)
  , XDimensions (..)
  , inSpaceOf, asSpaceOf, appendIdx, splitIdx
    -- * Type-level programming
  , FixedDim, FixedXDim, KnownOrder, ValidDims
  , FiniteDims, WrapDims, WrapHead, UnwrapDims
  , type (:<), type (>:)
--  , inferSubDimensions
  , module Numeric.Dimensions.List
    -- * Type evidence
  , ConcatEvidence (..), SuffixEvidence (..), PrefixEvidence (..)
--  , unsafeEqProof, unsafeConcatProofs, unsafeConsProof, listProof
--  , ListProof (..), ConcatProofs (..), ConsProof (..)
  ) where


import Control.Arrow (first)
import GHC.TypeLits
import GHC.Prim
import GHC.Types
import GHC.Exts
import Data.Proxy
import Data.Type.Equality

import Unsafe.Coerce

import Numeric.Dimensions.List

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
    SomeDim (ps :: Dim ds) <- someDimVal xs
    return $ withSuccKnown (order ps) ps
          ( \Refl -> let pps = p :* ps
                     in case ( isFixed xxs pps
                             , isXFixed xxs pps
                             ) of
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

-- | Run a function on a dimensionality that is known only at runtime
withRuntimeDim :: [Int]
               -> (forall ns . ( Dimensions ns ) => Dim ns -> a)
               -> Either String a
withRuntimeDim xns f | any (< 2) xns = Left "All dimensions must be at least of size 2."
                     | otherwise     = case someNatVal p of
    Just (SomeNat _) -> withDim (constructXDims p xns) f
    Nothing          -> Left "Cannot get length of a dimension list."
  where
    p = fromIntegral $ length xns
    constructXDims :: Integer -> [Int] -> Dim (xns :: [XNat])
    constructXDims 0 _  = unsafeCoerce D
    constructXDims _ [] = unsafeCoerce D
    constructXDims i (x:xs) = case someNatVal (fromIntegral x) of
      Nothing -> constructXDims i xs
      Just n  -> unsafeCoerce (n :? constructXDims (i+1) xs)

withSuccKnown :: Int
              -> p xs
              -> ( forall n . KnownNat n => (1 + Length xs) :~: n -> a)
              -> a
withSuccKnown n p g = case someNatVal (fromIntegral n) of
    Just (SomeNat m) -> g (evidence p m)
    Nothing          -> error "Something is terribly wrong. Is the length negative?"
  where
    evidence:: p xs -> q m -> (1 + Length xs) :~: m
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

-- | The main constraint type.
--   With this we are sure that all dimension values are known at compile time,
--   plus we have all handy functions for `Dim` and `Idx` types.
type Dimensions xs = ( KnownDims xs
                     , FiniteDims xs
                     , KnownList xs
                     , Dimensions' xs
                     , Dimensions'' xs)




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
  -- | Get various evidence that `as ++ bs ~ asbs` given as and bs
  concatEvidence :: (Dimensions bs)
                 => p (ds :: [Nat]) -> q (bs :: [Nat])   -> ConcatEvidence (ds :: [Nat]) (bs :: [Nat])
  -- | Get various evidence that `as ++ bs ~ asbs` given bs and asbs
  prefixEvidence :: (Dimensions bs, IsSuffix bs ds ~ 'True)
                 => p (bs :: [Nat]) -> q (ds :: [Nat]) -> PrefixEvidence (bs :: [Nat]) (ds :: [Nat])
  -- | Get various evidence that `as ++ bs ~ asbs` given as and asbs
  suffixEvidence :: (Dimensions as, FiniteDims ds, IsPrefix as ds ~ 'True)
                 => p (as :: [Nat]) -> q (ds :: [Nat]) -> SuffixEvidence (as :: [Nat]) (ds :: [Nat])

-- | Similar to `const` or `asProxyTypeOf`;
--   to be used on such implicit functions as `dim`, `dimMax`, etc.
inSpaceOf :: a ds -> b ds -> a ds
inSpaceOf x _ = x
{-# INLINE inSpaceOf #-}

asSpaceOf :: a ds -> (b ds -> c) -> (b ds -> c)
asSpaceOf _ = id
{-# INLINE asSpaceOf #-}

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

instance ( KnownDims (d ': ds)
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
  concatEvidence _ bs = ConcatEvidence (dim `inSpaceOf` bs)
  {-# INLINE concatEvidence #-}
  prefixEvidence (_ :: q bs) _ = case (unsafeEqProof :: bs :~: '[]) of
    Refl -> PrefixEvidence D
  {-# INLINE prefixEvidence #-}
  suffixEvidence (_ :: q as) _ = case (unsafeEqProof :: as :~: '[]) of
    Refl -> SuffixEvidence D
  {-# INLINE suffixEvidence #-}

instance ( Dimensions'' ds
         , KnownList ds
         , KnownDims (d ': ds)
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
  concatEvidence _ (bs :: q bs) = case concatEvidence (Proxy @ds) bs of
    ConcatEvidence (asbs :: Dim asbs) -> case unsafeConcatProofs @(d ': ds) @bs @(d ': asbs) of
      ConcatProofs -> ConcatEvidence (Proxy @d :* asbs)
  {-# INLINE concatEvidence #-}
  prefixEvidence (bs :: q bs) dds =
    if (order dds - order bs) > 0
    then case (unsafeEqProof :: IsSuffix bs ds :~: 'True) of
      Refl -> case prefixEvidence bs (Proxy @ds) of
        PrefixEvidence (as :: Dim as) -> case ( unsafeConcatProofs @(d ': as) @bs @(d ': ds) ) of
          ConcatProofs -> PrefixEvidence (unsafeCoerce $ (Proxy @d) :* as)
    else case (# unsafeEqProof :: (d ': ds) :~: bs
               , unsafeConcatProofs @'[] @(d ': ds) @(d ': ds)
               #) of
      (# Refl, ConcatProofs #) -> PrefixEvidence D
  {-# INLINE prefixEvidence #-}
  suffixEvidence (das :: q das) _ = case tList das of
    TLEmpty -> case ( unsafeConcatProofs @'[] @(d ': ds) @(d ': ds) ) of
      ConcatProofs -> SuffixEvidence (dim @(d ': ds))
    TLCons _ (as :: TypeList as) -> case ( unsafeEqProof :: IsPrefix as ds :~: 'True ) of
      Refl -> unsafeCoerce (suffixEvidence as (Proxy @ds))
  {-# INLINE suffixEvidence #-}



appendIdx :: Idx as -> Int -> Idx (as +: b)
appendIdx Z i = i :! Z
appendIdx jjs@(j :! js) i = case proofCons jjs js of
    Refl -> unsafeCoerce $ j :! appendIdx js i
  where
    proofCons :: Idx as -> Idx bs -> as :~: (b :+ bs)
    proofCons _ _ = unsafeCoerce Refl
{-# INLINE appendIdx #-}

splitIdx :: KnownList as => Idx (as ++ bs) -> (Idx as, Idx bs)
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

-- type family KnownOrders (ns :: [k]) :: Constraint where
--   KnownOrders '[] = ()
--   KnownOrders (x ': xs) = ( KnownOrder (x ': xs)
--                           , KnownOrders xs
--                           )

-- | A constraint family that makes sure all subdimensions are known.
type family KnownDims (ns :: [Nat]) :: Constraint where
  KnownDims '[] = ()
  KnownDims (x ': xs) = ( KnownNat x
                        , Dimensions' xs
                        , Dimensions'' xs
                        , KnownDims xs)
  KnownDims xs = ( Dimensions' xs
                 , Dimensions'' xs
                 )

type family FiniteDims (ns :: [Nat]) :: Constraint where
  FiniteDims '[] = ()
  FiniteDims (x ': xs) = ( KnownList xs, FiniteDims xs)
  FiniteDims xs = KnownList xs



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

type family WrapDims (x::[k]) :: [XNat] where
  WrapDims ('[] :: [Nat])     = '[]
  WrapDims (n ': ns :: [Nat]) = N n ': WrapDims ns
  WrapDims (xns :: [XNat])    = xns

type family UnwrapDims (xns::[XNat]) = (ns :: [Nat]) | ns -> xns where
  UnwrapDims '[] = '[]
  UnwrapDims (N x ': xs) = x ': UnwrapDims xs

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

-- | Use this strange thing to give various proofs that (as ++ bs ~ asbs)
data ConcatProofs (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
  = ConcatList as bs asbs => ConcatProofs


-- | Fool typechecker by saying that `as ++ bs ~ asbs`
unsafeConcatProofs :: ConcatProofs (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
unsafeConcatProofs = unsafeCoerce (ConcatProofs @'[] @'[] @'[])

---- | Help to do contraction of two lists
--contractEvidence :: p (as +: m) -> q (m :+ bs) -> ConcatProofs as bs (as ++ bs)
--contractEvidence _ _ = unsafeConcatProofs



data (Dimensions as, Dimensions bs)
  => ConcatEvidence (as :: [Nat]) (bs :: [Nat])
  = forall (asbs :: [Nat])
  . ( ConcatList as bs asbs
    , Dimensions asbs
    ) => ConcatEvidence (Dim (asbs :: [Nat]))

data (Dimensions bs, Dimensions asbs)
  => PrefixEvidence (bs :: [Nat]) (asbs :: [Nat])
  = forall (as :: [Nat])
  . ( ConcatList as bs asbs
    , Dimensions as
    ) => PrefixEvidence (Dim (as :: [Nat]))

data (Dimensions as, Dimensions asbs)
  => SuffixEvidence (as :: [Nat]) (asbs :: [Nat])
  = forall (bs :: [Nat])
  . ( ConcatList as bs asbs
    , Dimensions bs
    ) => SuffixEvidence (Dim (bs :: [Nat]))

---- | Fool typechecker by saying that list `xs` has Cons constructor
--unsafeConsProof :: ConsProof (as :: [Nat])
--unsafeConsProof = unsafeCoerce (ConsProof (Proxy @1) (Proxy @'[]))
--
-- | Fool typechecker by saying that a ~ b
unsafeEqProof :: a :~: b
unsafeEqProof = unsafeCoerce Refl




---- | Get evidence about various relations between haskell list type [Nat]
----   and its list op counterpart List Nat.
--listProof :: p (xs :: [Nat]) -> ListProof xs
--listProof _ = unsafeCoerce $ ListProof (Proxy @'Empty)

--
---- | If we know (1) Dimensions ds and (2) Size of prefix list,
----   we can derive Dimensions instances for both, prefix and suffix of the list
--inferSubDimensions :: forall (x :: Type) (p :: [Nat] -> Type) (q :: [Nat] -> Type)
--                             (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
--                    . ( ConcatList as bs asbs
--                      , Dimensions asbs
--                      , FiniteDims as
--                      , KnownList as
--                      )
--                   => p as
--                   -> q bs
--                   -> ( ( Dimensions as
--                        , Dimensions bs
--                        ) => Dim as -> Dim bs -> x
--                      )
--                   -> x
--inferSubDimensions aas bs f
--  = case tList aas of
--      TLEmpty -> case (# unsafeConcatProofs :: ConcatProofs '[] bs bs
--                       , unsafeEqProof :: bs :~: asbs
--                       #) of
--        (# ConcatProofs, Refl #) -> f D (dim `inSpaceOf` bs)
--      TLCons (p :: Proxy a) ps ->
--                 case (# unsafeConcatProofs :: ConcatProofs (Tail as) bs (Tail asbs)
--                       , unsafeConcatProofs :: ConcatProofs as bs asbs
--                       , unsafeEqProof :: a :~: Head asbs
--                       #) of
--        (# ConcatProofs, ConcatProofs, Refl #) ->
--            ( inferSubDimensions @x @Proxy @q @(Tail as) @bs @(Tail asbs) ) (Proxy @(Tail as)) bs (f . (p :*))
----      Nothing -> case ( unsafeConsProof ::  ConsProof as
----                      , unsafeConsProof ::  ConsProof asbs
----                      , unsafeEqProof :: Head as :~: Head asbs
----                      , unsafeConcatProofs :: ConcatProofs (Tail as) bs (Tail asbs)
----                      , unsafeConcatProofs :: ConcatProofs as bs asbs
----                      ) of
----        (ConsProof p ps, ConsProof _ _, Refl, ConcatProofs _ _ _, ConcatProofs _ _ _) ->
----            ( inferSubDimensions @x @Proxy @q @(Tail as) @bs @(Tail asbs) ) ps bs (f . (p :*))
