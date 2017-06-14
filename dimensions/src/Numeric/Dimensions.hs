{-# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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
  , XDim (..), SomeDim (..), xDimVal, someDimVal, sameDim, compareDim
  , Slice (..)
  , ValidDim (..), inferLastValidDim
  , Proxy (..)
    -- * Operations
  , Dimensions, Dimensions' (..), Dimensions'' (..)
  , DimensionsEvidence (..)
  , XDimensions (), xdim
  , inSpaceOf, asSpaceOf, appendIdx, splitIdx
    -- * Type families for Dim-XDim manipulations
  , FixedDim, FixedXDim, ValidDims
  , WrapDims, UnwrapDims, AsXDims, AsDims
  , WrapHead, ConsDim
  , type (:<), type (>:)
    -- * Generic type-level list operations
  , module Numeric.Dimensions.List
    -- * Re-export of GHC.TypeLits
  , Nat, KnownNat, natVal, SomeNat (..), someNatVal
  , type (+), type (-), type (<=), type (<=?)
  ) where


import           Control.Arrow           (first)
import           Data.Maybe              (isJust)
import           Data.Proxy              (Proxy (..))
import           Data.Type.Equality      ((:~:) (..))
import           GHC.Exts                (Constraint, IsList (..), Proxy#,
                                          State#, proxy#)
import           GHC.TypeLits            (type (+), type (-), type (<=),
                                          type (<=?), ErrorMessage (..),
                                          KnownNat, Nat, SomeNat (..),
                                          TypeError, natVal, natVal', sameNat,
                                          someNatVal)

import           Unsafe.Coerce           (unsafeCoerce)

import           Numeric.Dimensions.List

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
  (:*) :: KnownNat d
       => !(Proxy d) -> !(Dim ds) -> Dim (ConsDim d ds)
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
--   In contrast to SomeDim, it preserves the order of dimensions,
--   and it can keep some of the dimensions in the list static
--   while making other dimensions known only at runtime.
data XDim (xns :: [XNat])
  = forall ns . ( Dimensions ns
                , FixedDim xns ns ~ ns
                , FixedXDim xns ns ~ xns
                ) => XDim (Dim ns)

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDim = forall ns . Dimensions ns => SomeDim (Dim ns)

-- | A singleton type used to prove that the given type-level list
--   is indeed a correct list of Dimensions
data DimensionsEvidence (ns :: [Nat])
  = Dimensions ns => DimensionsEvidence

-- | A singleton type used to proof that a certain dimension is valid
data ValidDim (n :: Nat)
  = (KnownNat n, 2 <= n) => ValidDim

-- | Construct dimensionality at runtime
xDimVal :: Dim (xns :: [XNat]) -> Maybe (XDim xns)
xDimVal D = Just $ XDim D
xDimVal xxs@(p :* xs) = do
    Refl <- isValidDim p
    XDim (ps :: Dim ds) <- xDimVal xs
    let pps = p :* ps
    case ( isFixed xxs pps, isXFixed xxs pps ) of
      (Refl, Refl) -> return $ XDim pps
  where
    -- I know for sure that the constraint (FixedDim xns ns ~ ns) holds,
    --   but I need to convince the compiler that this is the case
    isFixed :: Dim xns -> Dim ns -> FixedDim xns ns :~: ns
    isFixed _ _ = unsafeCoerce Refl
    isXFixed :: Dim xns -> Dim ns -> FixedXDim xns ns :~: xns
    isXFixed _ _ = unsafeCoerce Refl
xDimVal (SomeNat p :? xs) = do
  Refl <- isValidDim p
  XDim ps <- xDimVal xs
  return $ XDim (p :* ps)


-- | Convert a list of ints into unknown type-level Dimensions list
someDimVal :: [Int] -> Maybe SomeDim
someDimVal [] = Just $ SomeDim D
someDimVal (x:xs) = do
  SomeNat p <- someNatVal (fromIntegral x)
  Refl <- isValidDim p
  SomeDim ps <- someDimVal xs
  return $ SomeDim (p :* ps)

-- | Check if the dimension size is not less than 2
isValidDim :: KnownNat d => p d -> Maybe ((2 <=? d) :~: 'True)
isValidDim p = if 2 <= natVal p then unsafeCoerce (Just Refl)
                               else unsafeCoerce Nothing

-- | We either get evidence that this function was instantiated with the
-- same type-level Dimensions, or 'Nothing'.
sameDim :: Dim as -> Dim bs -> Maybe (as :~: bs)
sameDim D D                 = Just (unsafeCoerce Refl)
sameDim (a :* as) (b :* bs) = sameNat a b >> unsafeCoerce <$> sameDim as bs
sameDim (a :? as) (b :? bs) | a == b = unsafeCoerce <$> sameDim as bs
sameDim (a :* as) (b :? bs) | SomeNat a == b = unsafeCoerce <$> sameDim as bs
sameDim (a :? as) (b :* bs) | a == SomeNat b = unsafeCoerce <$> sameDim as bs
sameDim _ _                 = Nothing

-- | Compare dimensions by their size in reversed lexicorgaphic order
--   (the biggest dimension is the last one).
compareDim :: Dim as -> Dim bs -> Ordering
compareDim D D = EQ
compareDim _ D = GT
compareDim D _ = LT
compareDim (a :* as) (b :* bs) = compareDim as bs `mappend` compare (SomeNat a) (SomeNat b)
compareDim (a :? as) (b :? bs) = compareDim as bs `mappend` compare a b
compareDim (a :* as) (b :? bs) = compareDim as bs `mappend` compare (SomeNat a) b
compareDim (a :? as) (b :* bs) = compareDim as bs `mappend` compare a (SomeNat b)

-- | Proof that the last dimension in the list is, indeed, KnownNat and >= 2.
inferLastValidDim :: forall n ns . Dimensions (n ': ns) => ValidDim (Last (n ': ns))
inferLastValidDim = case tList (Proxy @(n ': ns)) of
    TLCons _ TLEmpty -> ValidDim
    TLCons _ (TLCons (_ :: Proxy x) (_ :: TypeList xs)) -> case inferTailDimensions (Proxy @(n ': ns)) of
      DimensionsEvidence -> inferLastValidDim @x @xs

--------------------------------------------------------------------------------
-- * Dimension-enabled operations
--------------------------------------------------------------------------------

-- | The main constraint type.
--   With this we are sure that all dimension values are known at compile time,
--   plus we have all handy functions for `Dim` and `Idx` types.
type Dimensions xs = ( KnownDims xs
                     , FiniteList xs
                     , ValidDims xs
                     , Dimensions' xs
                     , Dimensions'' xs)


class Dimensions' (ds :: [Nat]) where
  -- | Dimensionality of our space
  dim :: Dim ds

class XDimensions (xds :: [XNat]) where
  wrapDim :: ( Dimensions ds
             , FixedXDim xds ds ~ xds) => Dim ds -> Dim xds

-- | Loose compile-time information about dimensionalities
xdim :: forall (ds :: [Nat]) (xds :: [XNat]) (p :: [Nat] -> *)
      . ( Dimensions ds
        , XDimensions xds
        , FixedXDim xds ds ~ xds) => p ds -> Dim xds
xdim p = wrapDim (dim `inSpaceOf` p)
{-# INLINE xdim #-}

-- | Support for Idx GADT
class Dimensions' ds => Dimensions'' (ds :: [Nat]) where
    -- | Total number of elements - product of all dimension sizes (unboxed)
    totalDim :: t ds -> Int
    -- | Run a primitive loop over all dimensions (1..n)
    loopS  :: Dim  ds -> (Idx  ds -> State# s -> State# s) -> State# s -> State# s
    -- | Run a loop over all dimensions keeping a boxed accumulator (1..n)
    loopA  :: Dim  ds -> (Idx  ds -> a -> a) -> a -> a
    -- | Run a loop in a reverse order n..1
    loopReverse :: Dim ds -> (Idx  ds -> a -> a) -> a -> a
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
    -- | Infer that concatenation is also Dimensions
    inferConcatDimensions :: forall (bs :: [Nat]) (p :: [Nat] -> *) (q :: [Nat] -> *)
                           . Dimensions bs
                          => p ds
                          -> q bs
                          -> DimensionsEvidence (ds ++ bs)
    -- | Infer that prefix is also Dimensions
    inferPrefixDimensions :: (IsSuffix bs ds ~ 'True, FiniteList bs, Dimensions ds)
                        => p bs
                        -> q ds
                        -> DimensionsEvidence (Prefix bs ds)
    -- | Infer that suffix is also Dimensions
    inferSuffixDimensions :: (IsPrefix as ds ~ 'True, FiniteList as, Dimensions ds)
                        => p as
                        -> q ds
                        -> DimensionsEvidence (Suffix as ds)
    -- | Make snoc almost as good as cons
    inferSnocDimensions :: (KnownNat z, 2 <= z) => p ds -> q z -> DimensionsEvidence (ds +: z)
    -- | Init of the list is also Dimensions
    inferInitDimensions :: Dimensions ds => p ds -> DimensionsEvidence (Init ds)
    -- | Tail of the list is also Dimensions
    inferTailDimensions :: Dimensions ds => p ds -> DimensionsEvidence (Tail ds)
    -- | Take KnownNat of the list is also Dimensions
    inferTakeNDimensions :: (KnownNat n, Dimensions ds) => p n -> q ds -> DimensionsEvidence (Take n ds)
    -- | Drop KnownNat of the list is also Dimensions
    inferDropNDimensions :: (KnownNat n, Dimensions ds) => p n -> q ds -> DimensionsEvidence (Drop n ds)
    -- | Reverse of the list is also Dimensions
    inferReverseDimensions :: Dimensions ds => q ds -> DimensionsEvidence (Reverse ds)

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
    show Z  = "Idx Ø"
    show xs = "Idx" ++ foldr (\i s -> " " ++ show i ++ s) "" (idxToList xs)

instance Dimensions'' ds => Show (Dim ds) where
    show D = "Dim Ø"
    show xs = "Dim" ++ foldr (\i s -> " " ++ show i ++ s) ""
      (idxToList $ dimMax `inSpaceOf` xs)

instance Show (Dim (xds :: [XNat])) where
    show d = case xDimVal d of
      Nothing -> "Unknown dim"
      Just sd -> show sd

instance Show (XDim xns) where
    show (XDim p) = 'X' : show p

instance Show SomeDim where
    show (SomeDim p) = "Some" ++ show p


idxToList :: Idx ds -> [Int]
idxToList Z         = []
idxToList (x :! xs) = x : idxToList xs

idxFromList :: [Int] -> Idx ds
idxFromList []     = unsafeCoerce Z
idxFromList (x:xs) = unsafeCoerce $ x :! unsafeCoerce (idxFromList xs)

instance Eq (Idx ds) where
    Z == Z = True
    (a:!as) == (b:!bs) = a == b && as == bs
    Z /= Z = False
    (a:!as) /= (b:!bs) = a /= b || as /= bs

instance Eq (Dim ds) where
    a == b = isJust $ sameDim a b

instance Eq (XDim xds) where
    XDim as == XDim bs = isJust $ sameDim as bs

instance Eq SomeDim where
    SomeDim as == SomeDim bs = isJust $ sameDim as bs

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
    compare Z Z             = EQ
    compare (a:!as) (b:!bs) = compare as bs `mappend` compare a b

instance Ord (Dim ds) where
    compare = compareDim

instance Ord (XDim xds) where
    compare (XDim as) (XDim bs) = compareDim as bs

instance Ord SomeDim where
    compare (SomeDim as) (SomeDim bs) = compareDim as bs

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



instance Dimensions' ('[] :: [Nat]) where
    dim = D
    {-# INLINE dim #-}

instance ( KnownDims (d ': ds)
         , Dimensions' ds
         )  => Dimensions' ((d ': ds) :: [Nat]) where
    dim = Proxy :* dim
    {-# INLINE dim #-}

instance XDimensions '[] where
    wrapDim _ = D
    {-# INLINE wrapDim #-}

instance ( XDimensions xs
         ) => XDimensions (XN ': xs) where
    wrapDim nns@(n :* ns)
            | Just sv <- someNatVal (natVal n)
            , DimensionsEvidence <- inferTailDimensions nns = sv :? wrapDim ns
            | otherwise = error "Impossible happend: someNatVal (natVal n) == Nothing!"
    {-# INLINE wrapDim #-}

instance ( XDimensions xs
         , KnownNat n
         ) => XDimensions (N n ': xs) where
    wrapDim nns@(n :* ns)
      | DimensionsEvidence <- inferTailDimensions nns = n :* wrapDim ns
    {-# INLINE wrapDim #-}


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
    inferConcatDimensions _ (_ :: q bs) = DimensionsEvidence :: DimensionsEvidence bs
    {-# INLINE inferConcatDimensions #-}
    inferPrefixDimensions (_ :: p bs) _
      | Refl <- unsafeCoerce Refl :: bs :~: '[] = DimensionsEvidence
    {-# INLINE inferPrefixDimensions #-}
    inferSuffixDimensions (_ :: p as) _
      | Refl <- unsafeCoerce Refl :: as :~: '[] = DimensionsEvidence
    {-# INLINE inferSuffixDimensions #-}
    inferSnocDimensions _ _ = DimensionsEvidence
    {-# INLINE inferSnocDimensions #-}
    inferInitDimensions _ = error "Init -- empty type-level list"
    {-# INLINE inferInitDimensions #-}
    inferTailDimensions _ = error "Tail -- empty type-level list"
    {-# INLINE inferTailDimensions #-}
    inferTakeNDimensions _ _ = DimensionsEvidence
    {-# INLINE inferTakeNDimensions #-}
    inferDropNDimensions _ _ = DimensionsEvidence
    {-# INLINE inferDropNDimensions #-}
    inferReverseDimensions _ = DimensionsEvidence
    {-# INLINE inferReverseDimensions #-}

instance ( Dimensions'' ds
         , FiniteList ds
         , KnownDims (d ': ds)
         , 2 <= d
         )
          => Dimensions'' (d ': ds) where
    totalDim _ = fromIntegral (natVal (Proxy @d))
               * totalDim (Proxy @ds)
    {-# INLINE totalDim #-}
    loopS (n:*D) f = loop1 (fromInteger $ natVal n) (\i -> f (i:!Z))
    loopS (n:*ns) f = loopS ns (\js -> loop1 (fromInteger $ natVal n) (\i -> f (i:!js)))
    {-# INLINE loopS #-}
    loopA (n:*D) f = loopA1 (fromInteger $ natVal n) (f . (:!Z))
    loopA (n:*ns) f = loopA ns (\js -> loopA1 (fromInteger $ natVal n) (f . (:!js)))
    {-# INLINE loopA #-}
    loopReverse (n:*D) f = loopReverse1 (fromInteger $ natVal n) (f . (:!Z))
    loopReverse (n:*ns) f = loopReverse ns (\js -> loopReverse1 (fromInteger $ natVal n) (f . (:!js)))
    {-# INLINE loopReverse #-}
    ioffset (i:!Z) = i
    ioffset iis@(i:!is) = i + fromIntegral (natVal' (headDim# iis)) * ioffset is
    {-# INLINE ioffset #-}
    dropDims p ds = case (fromInteger (natVal p), order ds) of
            (0, _) -> unsafeCoerce ds
            (n, k) -> if n >= k then unsafeCoerce Z
                                else f n ds
      where
        f 0 ds'      = unsafeCoerce ds'
        f i (_:!ds') = unsafeCoerce (f (i-1) $ unsafeCoerce ds')
        f _ Z        = unsafeCoerce Z
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
    inferConcatDimensions _ (pbs :: p bs)
      | DimensionsEvidence  <- inferConcatDimensions (Proxy @ds) pbs
      , Refl <- unsafeCoerce Refl :: d :+ (ds ++ bs) :~: ((d :+ ds) ++ bs)
      = DimensionsEvidence :: DimensionsEvidence (d :+ (ds ++ bs))
    {-# INLINE inferConcatDimensions #-}
    inferPrefixDimensions (_ :: p bs) ds
      | Refl <- unsafeCoerce Refl :: Prefix bs (d :+ ds) :~: Take (Length (d :+ ds) - Length bs) (d :+ ds)
      = inferTakeNDimensions (Proxy @(Length (d :+ ds) - Length bs)) ds
    {-# INLINE inferPrefixDimensions #-}
    inferSuffixDimensions (pas :: p as) _
      | TLEmpty <- tList pas
        = DimensionsEvidence
      | TLCons _ (pas' :: TypeList as') <- tList pas
      , Refl <- unsafeCoerce Refl :: IsPrefix as' ds :~: 'True
      , Refl <- unsafeCoerce Refl :: Suffix as' ds :~: Suffix as (d :+ ds)
      , DimensionsEvidence <- inferSuffixDimensions pas' (Proxy @ds)
        = DimensionsEvidence :: DimensionsEvidence (Suffix as' ds)
      | otherwise = error "inferSuffixFiniteList: TypeList failed to pattern match"
    {-# INLINE inferSuffixDimensions #-}
    inferSnocDimensions _ (q :: q z)
      | DimensionsEvidence <- inferSnocDimensions (Proxy @ds) q
      , Refl <- unsafeCoerce Refl :: (d :+ (ds +: z)) :~: ((d :+ ds) +: z)
      = DimensionsEvidence :: DimensionsEvidence (d :+ (ds +: z))
    {-# INLINE inferSnocDimensions #-}
    inferInitDimensions _ = case tList (Proxy @ds) of
        TLEmpty -> DimensionsEvidence
        TLCons _ _ -> case inferInitDimensions (Proxy @ds) of
          DimensionsEvidence -> DimensionsEvidence :: DimensionsEvidence (d :+ Init ds)
    {-# INLINE inferInitDimensions #-}
    inferTailDimensions _ = DimensionsEvidence
    {-# INLINE inferTailDimensions #-}
    inferTakeNDimensions (pn :: p n) _
      | 0 <- natVal pn
      , Refl <- unsafeCoerce Refl :: Take n (d :+ ds) :~: '[]
        = DimensionsEvidence :: DimensionsEvidence '[]
      | otherwise
      , Refl <- unsafeCoerce Refl :: Take n (d :+ ds) :~: (d :+ Take (n-1) ds)
      , DimensionsEvidence <- inferTakeNDimensions (Proxy @(n-1)) (Proxy @ds)
        = DimensionsEvidence :: DimensionsEvidence (d :+ Take (n-1) ds)
    {-# INLINE inferTakeNDimensions #-}
    inferDropNDimensions (pn :: p n) _
      | 0 <- natVal pn
      , Refl <- unsafeCoerce Refl :: Drop n (d :+ ds) :~: (d :+ ds)
        = DimensionsEvidence :: DimensionsEvidence (d :+ ds)
      | otherwise
      , Refl <- unsafeCoerce Refl :: Drop n (d :+ ds) :~: Drop (n-1) ds
      , DimensionsEvidence <- inferDropNDimensions (Proxy @(n-1)) (Proxy @ds)
        = DimensionsEvidence :: DimensionsEvidence (Drop (n-1) ds)
    {-# INLINE inferDropNDimensions #-}
    inferReverseDimensions _
      | DimensionsEvidence <- inferReverseDimensions (Proxy @ds)
      , DimensionsEvidence <- inferSnocDimensions (Proxy @(Reverse ds)) (Proxy @d)
      , Refl <- unsafeCoerce Refl :: Reverse (d :+ ds) :~: (Reverse ds +: d)
       = DimensionsEvidence :: DimensionsEvidence (Reverse ds +: d)
    {-# INLINE inferReverseDimensions #-}


appendIdx :: Idx as -> Int -> Idx (as +: b)
appendIdx Z i = i :! Z
appendIdx jjs@(j :! js) i = case proofCons jjs js of
    Refl -> unsafeCoerce $ j :! appendIdx js i
  where
    proofCons :: Idx as -> Idx bs -> as :~: (b :+ bs)
    proofCons _ _ = unsafeCoerce Refl
{-# INLINE appendIdx #-}

splitIdx :: FiniteList as => Idx (as ++ bs) -> (Idx as, Idx bs)
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


-- | Get the first dimension
headDim :: t (d ': ds :: [k]) -> Proxy d
headDim _ = Proxy

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


-- | A constraint family that makes sure all subdimensions are known.
type family KnownDims (ns :: [Nat]) :: Constraint where
    KnownDims '[] = ()
    KnownDims (x ': xs) = ( KnownNat x, KnownDims xs )

-- | Make sure all dimensions are not degenerate
type family ValidDims (ns :: [Nat]) :: Constraint where
    ValidDims '[] = ()
    ValidDims (x ': xs) = (2 <= x, ValidDims xs)

-- | Map Dims onto XDims (injective)
type family AsXDims (ns :: [Nat]) = (xns :: [XNat]) | xns -> ns where
    AsXDims '[] = '[]
    AsXDims (n ': ns) = N n ': AsXDims ns

-- | Map XDims onto Dims (injective)
type family AsDims (xns::[XNat]) = (ns :: [Nat]) | ns -> xns where
    AsDims '[] = '[]
    AsDims (N x ': xs) = x ': AsDims xs

-- | Treat Dims or XDims uniformly as XDims
type family WrapDims (x::[k]) :: [XNat] where
    WrapDims ('[] :: [Nat])     = '[]
    WrapDims ('[] :: [XNat])    = '[]
    WrapDims (n ': ns :: [Nat]) = N n ': WrapDims ns
    WrapDims (xns :: [XNat])    = xns

-- | Treat Dims or XDims uniformly as Dims
type family UnwrapDims (xns::[k]) :: [Nat] where
    UnwrapDims ('[] :: [Nat])  = '[]
    UnwrapDims ('[] :: [XNat]) = '[]
    UnwrapDims (N x ': xs)     = x ': UnwrapDims xs
    UnwrapDims (XN ': _)       = TypeError (
           'Text "Cannot unwrap dimension XN into Nat"
     ':$$: 'Text "(dimension is not known at compile time)"
     )


-- | Unify usage of XNat and Nat.
--   This is useful in function and type definitions.
--   Mainly used in the definition of Dim.
type family ConsDim (x :: Nat) (xs :: [k]) = (ys :: [k]) | ys -> x xs where
    ConsDim x (xs :: [Nat]) = x ': xs
    ConsDim x (xs :: [XNat]) = N x ': xs


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
    0 :< _  = '[]
    1 :< ns = ns
    n :< ns = n :+ ns
infixr 6 :<

-- | Synonym for (+:) that ignores Nat values 0 and 1
type family (ns :: [Nat]) >: (n :: Nat) :: [Nat] where
    _  >: 0 = '[]
    ns >: 1 = ns
    ns >: n = ns +: n
infixl 6 >:
