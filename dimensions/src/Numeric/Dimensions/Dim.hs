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
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Dim
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Provides a data type `Dim ds` to keep dimension sizes
-- for multiple-dimensional data.
-- Lower indices go first, i.e. assumed enumeration
--          is i = i1 + i2*n1 + i3*n1*n2 + ... + ik*n1*n2*...*n(k-1).
--
-----------------------------------------------------------------------------

module Numeric.Dimensions.Dim
  ( -- * Dimension data types
    Nat, XNat, XN, N, Dim (..), dimVal, totalDim, fromInt
  , SomeDims (..), SomeDim (..), someDimVal, someDimsVal, sameDim, compareDim
  , inSpaceOf, asSpaceOf
    -- * Dimension constraints
  , Dimensions (..), KnownDim (..), KnownDims
    -- * Type-level programming
    --   Provide type families to work with lists of dimensions (`[Nat]` or `[XNat]`)
  , AsXDims, AsDims, WrapDims, UnwrapDims
  , ConsDim, NatKind
  , FixedDim, FixedXDim, WrapNat, type (:<), type (>:)
    -- * Inference of dimension evidence
  , inferDimensions, inferDimKnownDims, inferDimFiniteList
  , inferTailDimensions, inferConcatDimensions
  , inferPrefixDimensions, inferSuffixDimensions
  , inferSnocDimensions, inferInitDimensions
  , inferTakeNDimensions, inferDropNDimensions
  , inferReverseDimensions, reifyDimensions
    -- * Cons and Snoc inference
    --   Very useful functions when you need some evidence for contraction ops.
  , inferUnSnocDimensions, SnocDimensions
  , inferUnConsDimensions, ConsDimensions
  ) where

import           Data.Maybe              (isJust)
import           GHC.Exts                (Constraint, unsafeCoerce#)
import           Data.Type.Equality      ((:~:)(..))

import           Numeric.Dimensions.List
import           Numeric.TypeLits


-- | Type-level dimensionality
data Dim (ns :: k) where
  -- | Zero-rank dimensionality - scalar
  D   :: Dim '[]
  -- | List-like concatenation of dimensionality.
  --   NatKind constraint is needed here to infer that
  (:*) :: forall (n::l) (ns::[k]) . NatKind [k] l
       => !(Dim n) -> !(Dim ns) -> Dim (ConsDim n ns)
  -- | Proxy-like constructor
  Dn :: forall (n :: Nat) . KnownDim n => Dim (n :: Nat)
  -- | Nat known at runtime packed into existential constructor
  Dx :: forall (n :: Nat) (m :: Nat) . n <= m
     => !(Dim m) -> Dim (XN n)
infixr 5 :*

-- | Get runtime-known dim and make sure it is not smaller than the given Nat.
fromInt :: forall m . KnownDim m => Int -> Maybe (Dim (XN m))
fromInt i | i < dimVal' @m = Nothing
          | otherwise      = do
  SomeDim (dn :: Dim n) <- someDimVal i
  return $ case unsafeEqEvidence @(m <=? n) @'True of
      Evidence -> Dx dn



-- | Same as SomeNat, but for Dimension:
--   Hide all information about Dimension inside
data SomeDim = forall (n :: Nat) . SomeDim (Dim n)

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDims = forall (ns :: [Nat]) . SomeDims (Dim ns)

-- | Get value of type-level dim at runtime.
--   Gives a product of all dimensions if is a list.
dimVal :: Dim x -> Int
dimVal D                  = 1
dimVal (d :* ds)          = dimVal d * dimVal ds
dimVal (Dn :: Dim m)      = dimVal' @m
dimVal (Dx (Dn :: Dim m)) = dimVal' @m
{-# INLINE dimVal #-}

-- | Product of all dimension sizes.
totalDim :: forall ds proxy . Dimensions ds => proxy ds -> Int
totalDim _ = dimVal (dim @ds)
{-# INLINE totalDim #-}

-- | Similar to `someNatVal`, but for a single dimension
someDimVal :: Int -> Maybe SomeDim
someDimVal x | 0 > x     = Nothing
             | otherwise = Just (reifyDim x f)
  where
    f :: forall (n :: Nat) . KnownDim n => Proxy# n -> SomeDim
    f _ = SomeDim (Dn @n)
{-# INLINE someDimVal #-}

-- | Convert a list of ints into unknown type-level Dimensions list
someDimsVal :: [Int] -> Maybe SomeDims
someDimsVal []             = Just $ SomeDims D
someDimsVal (x:xs) | 0 > x = Nothing
                   | otherwise = do
  SomeDim p <- someDimVal x
  SomeDims ps <- someDimsVal xs
  return $ SomeDims (p :* ps)

dimList :: Dim ds -> String
dimList  D        = ""
dimList d@Dn      = show (dimVal d)
dimList (Dx d@Dn) = show (dimVal d)
dimList (d :* D)  = show (dimVal d)
dimList (d :* ds) = show (dimVal d) ++ ' ':dimList ds

-- | We either get evidence that this function was instantiated with the
-- same type-level Dimensions, or 'Nothing'.
sameDim :: Dim as -> Dim bs -> Maybe (Evidence (as ~ bs))
sameDim D D                 = Just Evidence
sameDim (a :* as) (b :* bs) | dimVal a == dimVal b = (unsafeCoerce# (Evidence @())) <$ sameDim as bs
                            | otherwise            = Nothing
sameDim _ _ = Nothing


-- | Compare dimensions by their size in lexicorgaphic order
--   from the last dimension to the first dimension
--   (the last dimension is the most significant one).
compareDim :: Dim as -> Dim bs -> Ordering
compareDim D D = EQ
compareDim _ D = GT
compareDim D _ = LT
compareDim (a :* as) (b :* bs) = compareDim as bs `mappend` compare (dimVal a) (dimVal b)
compareDim a@Dn b@Dn = compare (dimVal a) (dimVal b)
compareDim (Dx a) (Dx b) = compare (dimVal a) (dimVal b)
compareDim a@Dn (Dx b) = compare (dimVal a) (dimVal b)
compareDim (Dx a) b@Dn = compare (dimVal a) (dimVal b)
compareDim a@Dn (b :* bs) = compareDim D bs `mappend` compare (dimVal a) (dimVal b)
compareDim (Dx a) (b :* bs) = compareDim D bs `mappend` compare (dimVal a) (dimVal b)
compareDim (a :* as) b@Dn = compareDim as D `mappend` compare (dimVal a) (dimVal b)
compareDim (a :* as) (Dx b) = compareDim as D `mappend` compare (dimVal a) (dimVal b)


-- | Similar to `const` or `asProxyTypeOf`;
--   to be used on such implicit functions as `dim`, `dimMax`, etc.
inSpaceOf :: a ds -> b ds -> a ds
inSpaceOf x _ = x
{-# INLINE inSpaceOf #-}

-- | Similar to `asProxyTypeOf`,
--   Give a hint to type checker to fix the type of a function argument.
asSpaceOf :: a ds -> (b ds -> c) -> (b ds -> c)
asSpaceOf _ = id
{-# INLINE asSpaceOf #-}


instance Show (Dim ds) where
    show D  = "Dim Ø"
    show ds = "Dim " ++ dimList ds

instance Show SomeDims where
    show (SomeDims p) = "Some" ++ show p

instance Eq (Dim ds) where
    a == b = isJust $ sameDim a b

instance Eq SomeDims where
    SomeDims as == SomeDims bs = isJust $ sameDim as bs

instance Ord (Dim ds) where
    compare = compareDim

instance Ord SomeDims where
    compare (SomeDims as) (SomeDims bs) = compareDim as bs

class Dimensions (ds :: [Nat]) where
    -- | Dimensionality of our space
    dim :: Dim ds

instance Dimensions '[] where
    dim = D
    {-# INLINE dim #-}

instance (KnownDim d, Dimensions ds) => Dimensions (d ': ds) where
    dim = Dn :* dim
    {-# INLINE dim #-}

instance Dimensions ds => Bounded (Dim ds) where
    maxBound = dim
    {-# INLINE maxBound #-}
    minBound = dim
    {-# INLINE minBound #-}


--------------------------------------------------------------------------------
-- * Type-level programming
--------------------------------------------------------------------------------


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
    UnwrapDims (XN _ ': _)       = TypeError (
           'Text "Cannot unwrap dimension XN into Nat"
     ':$$: 'Text "(dimension is not known at compile time)"
     )

-- | Unify usage of XNat and Nat.
--   This is useful in function and type definitions.
--   Mainly used in the definition of Dim.
type family ConsDim (x :: l) (xs :: [k]) = (ys :: [k]) | ys -> x xs l where
    ConsDim (x :: Nat) (xs :: [Nat])  = x    ': xs
    ConsDim (x :: Nat) (xs :: [XNat]) = N x  ': xs
    ConsDim (XN m)     (xs :: [XNat]) = XN m ': xs

-- | Constraint on kinds;
--   makes sure that the second argument kind is Nat if the first is a list of Nats.
type family NatKind ks k :: Constraint where
    NatKind [Nat]  l    = l ~ Nat
    NatKind [XNat] Nat  = ()
    NatKind [XNat] XNat = ()
    NatKind  ks    k    = ks ~ [k]


-- | FixedDim tries not to inspect content of `ns` and construct it
--   based only on `xns` when it is possible.
--   This means it does not check if `XN m <= n`.
type family FixedDim (xns :: [XNat]) (ns :: [Nat]) :: [Nat] where
    FixedDim '[]          _  = '[]
    FixedDim (N n  ': xs) ns = n ': FixedDim xs (Tail ns)
    FixedDim (XN _ ': xs) ns = Head ns ': FixedDim xs (Tail ns)

-- | FixedXDim tries not to inspect content of `xns` and construct it
--   based only on `ns` when it is possible.
--   This means it does not check if `XN m <= n`.
type family FixedXDim (xns :: [XNat]) (ns :: [Nat]) :: [XNat] where
    FixedXDim _  '[]       = '[]
    FixedXDim xs (n ': ns) = WrapNat (Head xs) n ': FixedXDim (Tail xs) ns

-- | WrapNat tries not to inspect content of `xn` and construct it
--   based only on `n` when it is possible.
--   This means it does not check if `XN m <= n`.
type family WrapNat (xn :: XNat) (n :: Nat) :: XNat where
    WrapNat (XN m) n = XN m
    WrapNat  _     n = N n



-- | Synonym for (:+) that treats Nat values 0 and 1 in a special way:
--   it preserves the property that all dimensions is greater than 1.
type family (n :: Nat) :< (ns :: [Nat]) :: [Nat] where
    0 :< _  = '[]
    1 :< ns = ns
    n :< ns = n :+ ns
infixr 6 :<

-- | Synonym for (+:) that treats Nat values 0 and 1 in a special way:
--   it preserves the property that all dimensions is greater than 1.
type family (ns :: [Nat]) >: (n :: Nat) :: [Nat] where
    _  >: 0 = '[]
    ns >: 1 = ns
    ns >: n = ns +: n
infixl 6 >:




--------------------------------------------------------------------------------
-- * Inference of evidence
--------------------------------------------------------------------------------

-- | Infer `Dimensions` given that the list is KnownDims and finite
inferDimensions :: forall (ds :: [Nat])
                 . (KnownDims ds, FiniteList ds)
                => Evidence (Dimensions ds)
inferDimensions = case tList @Nat @ds of
  TLEmpty -> Evidence
  TLCons _ (_ :: TypeList ds') -> case inferDimensions @ds' of
    Evidence -> Evidence
{-# INLINE inferDimensions #-}

-- | `Dimensions` implies `KnownDims`
inferDimKnownDims :: forall (ds :: [Nat])
                   . Dimensions ds
                  => Evidence (KnownDims ds)
inferDimKnownDims = inferDimKnownDims' (dim @ds)
  where
    inferDimKnownDims' :: forall (ns :: [Nat]) . Dim ns -> Evidence (KnownDims ns)
    inferDimKnownDims' D = Evidence
    inferDimKnownDims' (Dn :* ds) = case inferDimKnownDims' ds of Evidence -> Evidence
{-# INLINE inferDimKnownDims #-}


-- | `Dimensions` implies `FiniteList`
inferDimFiniteList :: forall (ds :: [Nat])
                    . Dimensions ds
                   => Evidence (FiniteList ds)
inferDimFiniteList = inferDimFiniteList' (dim @ds)
  where
    inferDimFiniteList' :: forall (ns :: [Nat]) . Dim ns -> Evidence (FiniteList ns)
    inferDimFiniteList' D = Evidence
    inferDimFiniteList' (Dn :* ds) = case inferDimFiniteList' ds of Evidence -> Evidence
{-# INLINE inferDimFiniteList #-}


-- | Infer that tail list is also Dimensions
inferTailDimensions :: forall (ds :: [Nat])
                    . Dimensions ds
                    => Maybe (Evidence (Dimensions (Tail ds)))
inferTailDimensions = case dim @ds of
    D         -> Nothing
    Dn :* ds' -> Just $ reifyDimensions ds'


-- | Infer that concatenation is also Dimensions
inferConcatDimensions :: forall as bs
                       . (Dimensions as, Dimensions bs)
                      => Evidence (Dimensions (as ++ bs))
inferConcatDimensions = reifyDimensions $ magic (dim @as) (unsafeCoerce# $ dim @bs)
  where
    magic :: forall (xs :: [Nat]) (ys :: [Nat]) . Dim xs -> Dim ys -> Dim ys
    magic D ys         = ys
    magic xs D         = unsafeCoerce# xs
    magic (x :* xs) ys = unsafeCoerce# $ x :* magic xs ys
{-# INLINE inferConcatDimensions #-}


-- | Infer that prefix is also Dimensions
inferPrefixDimensions :: forall bs asbs
                       . (IsSuffix bs asbs ~ 'True, Dimensions bs, Dimensions asbs)
                      => Evidence (Dimensions (Prefix bs asbs))
inferPrefixDimensions = reifyDimensions $ magic (len dasbs - len (dim @bs)) (unsafeCoerce# dasbs)
  where
    dasbs = dim @asbs
    len :: forall (ns :: [Nat]) . Dim ns -> Int
    len D         = 0
    len (_ :* ds) = 1 + len ds
    magic :: forall (ns :: [Nat]) . Int -> Dim ns -> Dim ns
    magic _ D         = D
    magic 0 _         = unsafeCoerce# D
    magic n (d :* ds) = d :* magic (n-1) ds
{-# INLINE inferPrefixDimensions #-}

-- | Infer that suffix is also Dimensions
inferSuffixDimensions :: forall as asbs
                       . (IsPrefix as asbs ~ 'True, Dimensions as, Dimensions asbs)
                      => Evidence (Dimensions (Suffix as asbs))
inferSuffixDimensions = reifyDimensions $ magic (dim @as) (unsafeCoerce# $ dim @asbs)
  where
    magic :: forall (xs :: [Nat]) (ys :: [Nat]) . Dim xs -> Dim ys -> Dim ys
    magic D ys                = ys
    magic _ D                 = D
    magic (_ :* xs) (_ :* ys) = unsafeCoerce# $ magic xs ys
{-# INLINE inferSuffixDimensions #-}

-- | Make snoc almost as good as cons
inferSnocDimensions :: forall xs z
                     . (KnownDim z, Dimensions xs)
                    => Evidence (Dimensions (xs +: z))
inferSnocDimensions = reifyDimensions $ magic (dim @xs)
  where
    magic :: forall (ns :: [Nat]) . Dim ns -> Dim (ns +: z)
    magic D         = Dn :* D
    magic (d :* ds) = unsafeCoerce# (d :* magic ds)
{-# INLINE inferSnocDimensions #-}

-- | Init of the dimension list is also Dimensions,
--   and the last dimension is KnownDim.
inferUnSnocDimensions :: forall ds
                       . Dimensions ds
                      => Maybe (Evidence (SnocDimensions ds))
inferUnSnocDimensions = case dim @ds of
      D  -> Nothing
      ds -> Just $ case magic ds of
          (ys, Dn) -> case unsafeSnocDims' @ds of
            Evidence -> case reifyDimensions  @(Init ds) (unsafeCoerce# ys) of
              Evidence -> Evidence
    where
      magic :: forall (ns :: [Nat]) . Dim ns -> (Dim ns, Dim (Last ns))
      magic D         = (D, undefined)
      magic (d :* D)  = (unsafeCoerce# D, d)
      magic (d :* ds) = case magic ds of
          (ds', z) -> (d :* ds', unsafeCoerce# z)
{-# INLINE inferUnSnocDimensions #-}


-- | Tail of the dimension list is also Dimensions,
--   and the head dimension is KnownDim.
inferUnConsDimensions :: forall ds
                       . Dimensions ds
                      => Maybe (Evidence (ConsDimensions ds))
inferUnConsDimensions = case dim @ds of
      D  -> Nothing
      Dn :* ds' -> Just $ case reifyDimensions ds' +!+ unsafeConsDims' @ds of
            Evidence -> Evidence
{-# INLINE inferUnConsDimensions #-}

-- | Various evidence for the Snoc operation.
type SnocDimensions (xs :: [Nat]) =
    ( xs ~ (Init xs +: Last xs)
    , xs ~ (Init xs ++ '[Last xs])
    , IsPrefix  (Init xs) xs ~ 'True
    , IsSuffix '[Last xs] xs ~ 'True
    , Suffix    (Init xs) xs ~ '[Last xs]
    , Prefix   '[Last xs] xs ~   Init xs
    , Dimensions (Init xs)
    , KnownDim   (Last xs)
    )

-- | Various evidence for the Snoc operation.
type ConsDimensions (xs :: [Nat]) =
    ( xs ~ (  Head xs  :+ Tail xs)
    , xs ~ ('[Head xs] ++ Tail xs)
    , IsPrefix '[Head xs] xs ~ 'True
    , IsSuffix  (Tail xs) xs ~ 'True
    , Suffix   '[Head xs] xs ~   Tail xs
    , Prefix    (Tail xs) xs ~ '[Head xs]
    , Dimensions (Tail xs)
    , KnownDim   (Head xs)
    )


unsafeSnocDims' :: forall (xs :: [Nat]) . Evidence
    ( xs ~ (Init xs +: Last xs)
    , xs ~ (Init xs ++ '[Last xs])
    , IsPrefix  (Init xs) xs ~ 'True
    , IsSuffix '[Last xs] xs ~ 'True
    , Suffix    (Init xs) xs ~ '[Last xs]
    , Prefix   '[Last xs] xs ~   Init xs
    )
unsafeSnocDims' = case  unsafeEqEvidence @xs @(Init xs +: Last xs)
                    +!+ unsafeEqEvidence @xs @(Init xs ++ '[Last xs])
                    +!+ unsafeEqEvidence @(IsPrefix  (Init xs) xs) @'True
                    +!+ unsafeEqEvidence @(IsSuffix '[Last xs] xs) @'True
                    +!+ unsafeEqEvidence @(Suffix    (Init xs) xs) @'[Last xs]
                    +!+ unsafeEqEvidence @(Prefix   '[Last xs] xs) @(Init xs) of
    Evidence -> Evidence

unsafeConsDims' :: forall (xs :: [Nat]) . Evidence
    ( xs ~ (  Head xs  :+ Tail xs)
    , xs ~ ('[Head xs] ++ Tail xs)
    , IsPrefix '[Head xs] xs ~ 'True
    , IsSuffix  (Tail xs) xs ~ 'True
    , Suffix   '[Head xs] xs ~   Tail xs
    , Prefix    (Tail xs) xs ~ '[Head xs]
    )
unsafeConsDims' = case  unsafeEqEvidence @xs @(  Head xs  :+ Tail xs)
                    +!+ unsafeEqEvidence @xs @('[Head xs] ++ Tail xs)
                    +!+ unsafeEqEvidence @(IsPrefix '[Head xs] xs) @'True
                    +!+ unsafeEqEvidence @(IsSuffix  (Tail xs) xs) @'True
                    +!+ unsafeEqEvidence @(Suffix   '[Head xs] xs) @(Tail xs)
                    +!+ unsafeEqEvidence @(Prefix    (Tail xs) xs) @'[Head xs] of
    Evidence -> Evidence


-- | Init of the list is also Dimensions
inferInitDimensions :: forall xs
                     . Dimensions xs
                    => Maybe (Evidence (Dimensions (Init xs)))
inferInitDimensions = case dim @xs of
      D  -> Nothing
      ds -> Just . reifyDimensions $ magic (unsafeCoerce# ds)
    where
      magic :: forall (ns :: [Nat]) . Dim ns -> Dim ns
      magic D         = D
      magic (_ :* D)  = unsafeCoerce# D
      magic (d :* ds) = d :* magic ds
{-# INLINE inferInitDimensions #-}

-- | Take KnownDim of the list is also Dimensions
inferTakeNDimensions :: forall n xs
                      . (KnownDim n, Dimensions xs)
                     => Evidence (Dimensions (Take n xs))
inferTakeNDimensions = reifyDimensions $ magic (dimVal' @n) (dim @xs)
    where
      magic :: forall (ns :: [Nat]) . Int -> Dim ns -> Dim (Take n ns)
      magic _ D = D
      magic 0 _ = unsafeCoerce# D
      magic n (d :* ds) = unsafeCoerce# $ d :* (unsafeCoerce# $ magic (n-1) ds :: Dim (Tail ns))
{-# INLINE inferTakeNDimensions #-}

-- | Drop KnownDim of the list is also Dimensions
inferDropNDimensions :: forall n xs
                      . (KnownDim n, Dimensions xs)
                     => Evidence (Dimensions (Drop n xs))
inferDropNDimensions = reifyDimensions $ magic (dimVal' @n) (dim @xs)
    where
      magic :: forall ns . Int -> Dim ns -> Dim (Drop n ns)
      magic _ D         = D
      magic 0 ds        = unsafeCoerce# ds
      magic n (_ :* ds) = unsafeCoerce# $ magic (n-1) ds
{-# INLINE inferDropNDimensions #-}

-- | Reverse of the list is also Dimensions
inferReverseDimensions :: forall xs . Dimensions xs => Evidence (Dimensions (Reverse xs))
inferReverseDimensions = reifyDimensions $ magic (dim @xs) (unsafeCoerce# D)
    where
      magic :: forall ns . Dim ns -> Dim (Reverse ns) -> Dim (Reverse ns)
      magic D xs = xs
      magic (p:*sx) xs = magic (unsafeCoerce# sx :: Dim ns)
                               (unsafeCoerce# (p:*xs) :: Dim (Reverse ns))
{-# INLINE inferReverseDimensions #-}





--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------



-- | Use the given `Dim ds` to create an instance of `Dimensions ds` dynamically.
reifyDimensions :: forall (ds :: [Nat]) . Dim ds -> Evidence (Dimensions ds)
reifyDimensions ds = reifyDims ds Evidence
{-# INLINE reifyDimensions #-}


-- | This function does GHC's magic to convert user-supplied `dimVal'` function
--   to create an instance of KnownDim typeclass at runtime.
--   The trick is taken from Edward Kmett's reflection library explained
--   in https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
reifyDims :: forall r (ds :: [Nat]) . Dim ds -> ( Dimensions ds => r) -> r
reifyDims ds k = unsafeCoerce# (MagicDims k :: MagicDims ds r) ds
{-# INLINE reifyDims #-}
newtype MagicDims ds r = MagicDims (Dimensions ds => r)


unsafeEqEvidence :: forall x y . Evidence (x ~ y)
unsafeEqEvidence = case (unsafeCoerce# Refl :: x :~: y) of Refl -> Evidence
{-# INLINE unsafeEqEvidence #-}
