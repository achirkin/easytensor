{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame
  ( DataFrame
  , withShape
  , unboundShape
  , (%*), (<::>), (<+:>), (<:>)
  , Vector, Matrix, Scalar
  , Vec2f, Vec3f, Vec4f
  , Mat22f, Mat23f, Mat24f
  , Mat32f, Mat33f, Mat34f
  , Mat42f, Mat43f, Mat44f
  , scalar, unScalar
  , vec2, vec3, vec4
  , mat22, mat33, mat44
  , (.*.), dot, (·)
  , cross, (×), det2
  , normL1, normL2, normLPInf, normLNInf, normLP
  , inverse, det, trace, eye, diag, transpose
  ) where


import           Data.Proxy
import           Data.Type.Equality
import           GHC.Base (runRW#)
import           GHC.Prim
import           GHC.TypeLits       (Nat, natVal, type (+), KnownNat)
import           GHC.Types          (Type)
import           Numeric.Array
import qualified Numeric.Array.Family as AFam (Scalar (..))
import           Numeric.Commons
import           Numeric.Dimensions
import qualified Numeric.Matrix.Class as M
import           Unsafe.Coerce

-- | Keep data in a primitive data frame
--    and maintain information about dimensions in the type-system
data family DataFrame (t :: Type) (xs :: [k])

-- | Completely fixed at compile time
newtype instance Dimensions ns => DataFrame t (ns :: [Nat])
  = KnownDataFrame { _getDF :: Array t ns }

-- | Partially known at compile time
data instance DataFrame t (xns :: [XNat])
  = forall (ns :: [Nat])
  . ( Dimensions ns
    , FixedDim xns ns ~ ns
    , FixedXDim xns ns ~ xns
    , Show (Array t ns)
    , Eq (Array t ns)
    )
  => SomeDataFrame (Dim xns) (Array t ns)

instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame t ds) where
  show (KnownDataFrame arr) = "DataFrame:"
                         ++ "\n\tShape: " ++ show (dim `inSpaceOf` arr)
                         ++ "\n\tContent:\n" ++ show arr

deriving instance Bounded (Array t ds) => Bounded (DataFrame t ds)
deriving instance Enum (Array t ds) => Enum (DataFrame t ds)
deriving instance Eq (Array t ds) => Eq (DataFrame t ds)
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
deriving instance PrimBytes (Array t ds)
               => PrimBytes (DataFrame t ds)
deriving instance FloatBytes (Array t ds)
               => FloatBytes (DataFrame t ds)
deriving instance DoubleBytes (Array t ds)
               => DoubleBytes (DataFrame t ds)
deriving instance IntBytes (Array t ds)
               => IntBytes (DataFrame t ds)
deriving instance WordBytes (Array t ds)
               => WordBytes (DataFrame t ds)
instance ( Dimensions ds
         , ElementWise (Idx ds) t (Array Float ds)
         ) => ElementWise (Idx ds) t (DataFrame Float ds) where
  (!) = (!) . _getDF
  {-# INLINE (!) #-}
  ewmap f = KnownDataFrame . ewmap f . _getDF
  {-# INLINE ewmap #-}
  ewgen = KnownDataFrame . ewgen
  {-# INLINE ewgen #-}
  ewfold f x0 = ewfold f x0 . _getDF
  {-# INLINE ewfold #-}
  elementWise f = fmap KnownDataFrame . elementWise f . _getDF
  {-# INLINE elementWise #-}
  indexWise f = fmap KnownDataFrame . indexWise f . _getDF
  {-# INLINE indexWise #-}
  broadcast = KnownDataFrame . broadcast
  {-# INLINE broadcast #-}


instance Show (Dim ds)
      => Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame d arr) = "DataFrame:"
                         ++ "\n\tShape: " ++ show d
                         ++ "\n\tContent:\n" ++ show arr

instance Eq (DataFrame t (ds :: [XNat])) where
  SomeDataFrame d1 a1 == SomeDataFrame d2 a2
      = case check d1 d2 a1 a2 of
          Just Refl -> a1 == a2
          Nothing   -> False
    where
      check :: Dim ds -> Dim ds
            -> p ns1 -> q ns2
            -> Maybe (ns1 :~: ns2)
      check a b _ _ | a == b  = Just (unsafeCoerce Refl)
                    | otherwise = Nothing

-- | Do something with
withShape :: DataFrame t xns
          -> (forall ns . ( Dimensions ns
                          , FixedDim xns ns ~ ns
                          , FixedXDim xns ns ~ xns
                          ) => DataFrame t ns -> b)
          -> b
withShape (SomeDataFrame _ a) f = f (KnownDataFrame a)

-- | Put some of dimensions into existential data type
unboundShape :: ( FixedXDim xns ns ~ xns
                , FixedDim xns ns ~ ns
                , XDimensions ns xns
                , Dimensions ns
                , Show (Array t ns)
                , Eq (Array t ns)
                ) => DataFrame t ns -> DataFrame t xns
unboundShape (KnownDataFrame a)
    = SomeDataFrame (xdim $ dim `inSpaceOf` a) a


_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined


--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------

-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.
(%*) :: forall (t :: Type) (k :: Nat) (as :: [Nat]) (bs :: [Nat])
      . ( Dimensions (k :+ bs)
        , KnownOrder as
        , ElementWise (Idx (as +: k)) t (DataFrame t (as +: k))
        , ElementWise (Idx (k :+ bs)) t (DataFrame t (k :+ bs))
        , ElementWise (Idx (as ++ bs)) t (DataFrame t (as ++ bs))
        , Num t
        )
     => DataFrame t (as +: k) -> DataFrame t (k :+ bs) -> DataFrame t (as ++ bs)
a %* b = ewgen $ \i -> uncurry f (splitIdx i)
  where
    -- get proxy for the contracted dimension
    kp :: DataFrame t (as +: k) -> DataFrame t (k :+ bs) -> Proxy k
    kp _ _ = Proxy
    k :: Int
    k = fromInteger . natVal $ kp a b
    -- contraction function
    f i j = foldr (\l x -> x + a ! appendIdx i l
                             * b ! (l :! j)
                      ) 0 [1..k]
{-# INLINE (%*) #-}
infixl 7 %*

-- | Append one DataFrame to another, adding up their last dimensionality
(<:>) :: ( PrimBytes (DataFrame t (ds +: n))
         , PrimBytes (DataFrame t (ds +: m))
         , PrimBytes (DataFrame t (ds +: (n + m)))
         )
        => DataFrame t (ds +: n)
        -> DataFrame t (ds +: m)
        -> DataFrame t (ds +: (n + m))
a <:> b = case (# toBytes a, toBytes b
                , byteSize a
                , byteSize b
                , elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <:>

-- | Append one DataFrame to another, adding up their last dimensionality
(<::>) :: ( PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t (ds +: 2))
          )
        => DataFrame t ds
        -> DataFrame t ds
        -> DataFrame t (ds +: 2)
a <::> b = case (# toBytes a, toBytes b
                , byteSize a
                , byteSize b
                , elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <::>

-- | Append one DataFrame to another, adding up their last dimensionality
(<+:>) :: ( PrimBytes (DataFrame t (ds +: n))
          , PrimBytes (DataFrame t ds)
          , PrimBytes (DataFrame t (ds +: (n + 1)))
          )
        => DataFrame t (ds +: n)
        -> DataFrame t ds
        -> DataFrame t (ds +: (n + 1))
a <+:> b = case (# toBytes a, toBytes b
                , byteSize a
                , byteSize b
                , elementByteSize a #) of
  (# (# off1, n1, arr1 #), (# off2, n2, arr2 #), bs1, bs2, ebs #) -> case runRW#
     ( \s0 -> case newByteArray# (bs1 +# bs2) s0 of
         (# s1, mr #) -> case copyByteArray# arr1 (off1 *# ebs) mr 0# bs1 s1 of
           s2 -> case copyByteArray# arr2 (off2 *# ebs) mr bs1 bs2 s2 of
             s3 -> unsafeFreezeByteArray# mr s3
     ) of (# _, r #) -> fromBytes (# 0#, n1 +# n2, r #)
infixl 5 <+:>


--------------------------------------------------------------------------------
-- * Scalar type
--------------------------------------------------------------------------------

type Scalar t = DataFrame t ('[] :: [Nat])

-- | Convert scalar back to ordinary type
unScalar :: Scalar t -> t
unScalar = AFam._unScalar . _unArray . _getDF

-- | Convert any type to scalar wrapper
scalar :: t -> Scalar t
scalar = KnownDataFrame . Array . AFam.Scalar

--------------------------------------------------------------------------------
-- * Vector type
--------------------------------------------------------------------------------

type Vector t (n :: Nat) = DataFrame t '[n]

type Vec2f = Vector Float 2
type Vec3f = Vector Float 3
type Vec4f = Vector Float 4


-- | Scalar product -- sum of Vecs' components products,
--                     propagated into whole Vec
(.*.) :: ( Num t
         , Num (Vector t n)
         , ElementWise (Idx '[n]) t (Vector t n)
         )
      => Vector t n -> Vector t n -> Vector t n
(.*.) a b = broadcast . ewfold (const (+)) 0 $ a * b
infixl 7 .*.

-- | Scalar product -- sum of Vecs' components products -- a scalar
dot :: ( Num t
       , Num (Vector t n)
       , ElementWise (Idx '[]) t (Scalar t)
       , ElementWise (Idx '[n]) t (Vector t n)
       )
    => Vector t n -> Vector t n -> Scalar t
dot a b = broadcast . ewfold (const (+)) 0 $ a * b

-- | Dot product of two vectors
infixl 7 ·
(·) :: ( Num t
       , Num (Vector t n)
       , ElementWise (Idx '[]) t (Scalar t)
       , ElementWise (Idx '[n]) t (Vector t n)
       )
    => Vector t n -> Vector t n -> Scalar t
(·) = dot
{-# INLINE (·) #-}


-- | Sum of absolute values
normL1 :: ( Num t
          , ElementWise (Idx '[]) t (Scalar t)
          , ElementWise (Idx '[n]) t (Vector t n)
          )
       => Vector t n -> Scalar t
normL1 = broadcast . ewfold (const (\a -> (abs a +))) 0

-- | hypot function (square root of squares)
normL2 :: ( Floating t
          , ElementWise (Idx '[]) t (Scalar t)
          , ElementWise (Idx '[n]) t (Vector t n)
          )
       => Vector t n -> Scalar t
normL2 = broadcast . sqrt . ewfold (const (\a -> (a*a +))) 0

-- | Maximum of absolute values
normLPInf :: ( Ord t, Num t
             , ElementWise (Idx '[]) t (Scalar t)
             , ElementWise (Idx '[n]) t (Vector t n)
             )
          => Vector t n -> Scalar t
normLPInf = broadcast . ewfold (const (max . abs)) 0

-- | Minimum of absolute values
normLNInf :: ( Ord t, Num t
             , ElementWise (Idx '[]) t (Scalar t)
             , ElementWise (Idx '[n]) t (Vector t n)
             )
          => Vector t n -> Scalar t
normLNInf x = broadcast $ ewfold (const (min . abs))
                                 (abs $ x ! (1:!Z)) x

-- | Norm in Lp space
normLP :: ( Floating t
          , ElementWise (Idx '[]) t (Scalar t)
          , ElementWise (Idx '[n]) t (Vector t n)
          )
       => Int -> Vector t n -> Scalar t
normLP i' = broadcast . (**ri) . ewfold (const (\a -> (a**i +))) 0
  where
    i  = fromIntegral i'
    ri = recip i
{-# INLINE [2] normLP #-}
{-# RULES
"normLP/L1" normLP 1 = normL1
"normLP/L2" normLP 2 = normL2
  #-}

-- | Compose a 2D vector
vec2 :: ElementWise (Idx '[2]) t (Vector t 2) => t -> t -> Vector t 2
vec2 a b = ewgen f
  where
    f (1:!Z) = a
    f _ = b

-- | Take a determinant of a matrix composed from two 2D vectors.
--   Like a cross product in 2D.
det2 :: ( ElementWise (Idx '[2]) t (Vector t 2)
        , ElementWise (Idx '[]) t (Scalar t)
        , Num t
        ) => Vector t 2 -> Vector t 2 -> Scalar t
det2 a b = broadcast $ a ! (1 :! Z) * b ! (2 :! Z)
                     - a ! (2 :! Z) * b ! (1 :! Z)

-- | Compose a 3D vector
vec3 :: ElementWise (Idx '[3]) t (Vector t 3) => t -> t -> t -> Vector t 3
vec3 a b c = ewgen f
  where
    f (1:!Z) = a
    f (2:!Z) = b
    f _ = c

-- | Cross product
cross :: ( ElementWise (Idx '[3]) t (Vector t 3)
         , ElementWise (Idx '[]) t (Scalar t)
         , Num t
         ) => Vector t 3 -> Vector t 3 -> Vector t 3
cross a b = vec3 ( a ! (2 :! Z) * b ! (3 :! Z)
                 - a ! (3 :! Z) * b ! (2 :! Z) )
                 ( a ! (3 :! Z) * b ! (1 :! Z)
                 - a ! (1 :! Z) * b ! (3 :! Z) )
                 ( a ! (1 :! Z) * b ! (2 :! Z)
                 - a ! (2 :! Z) * b ! (1 :! Z) )


-- | Cross product for two vectors in 3D
infixl 7 ×
(×) :: ( ElementWise (Idx '[3]) t (Vector t 3)
       , ElementWise (Idx '[]) t (Scalar t)
       , Num t
        ) => Vector t 3 -> Vector t 3 -> Vector t 3
(×) = cross
{-# INLINE (×) #-}


-- | Compose a 3D vector
vec4 :: ElementWise (Idx '[4]) t (Vector t 4)
     => t -> t -> t -> t -> Vector t 4
vec4 a b c d = ewgen f
  where
    f (1:!Z) = a
    f (2:!Z) = b
    f (3:!Z) = c
    f _ = d


--------------------------------------------------------------------------------
-- * Matrix type
--------------------------------------------------------------------------------

type Matrix t (n :: Nat) (m :: Nat) = DataFrame t '[n,m]

-- Type abbreviations

type Mat22f = Matrix Float 2 2
type Mat32f = Matrix Float 3 2
type Mat42f = Matrix Float 4 2
type Mat23f = Matrix Float 2 3
type Mat33f = Matrix Float 3 3
type Mat43f = Matrix Float 4 3
type Mat24f = Matrix Float 2 4
type Mat34f = Matrix Float 3 4
type Mat44f = Matrix Float 4 4


-- | Compose a 2x2D matrix
mat22 :: ( PrimBytes (Vector t 2)
         , PrimBytes (Matrix t 2 2)
         )
      => Vector t 2 -> Vector t 2 -> Matrix t 2 2
mat22 = (<::>)

-- | Compose a 3x3D matrix
mat33 :: ( PrimBytes (Vector t 3)
         , PrimBytes (Matrix t 3 2)
         , PrimBytes (Matrix t 3 3)
         )
      => Vector t 3 -> Vector t 3 -> Vector t 3 -> Matrix t 3 3
mat33 a b c = a <::> b <+:> c

-- | Compose a 4x4D matrix
mat44 :: ( PrimBytes (Vector t 4)
         , PrimBytes (Matrix t 4 2)
         , PrimBytes (Matrix t 4 4)
         )
      => Vector t 4 -> Vector t 4 -> Vector t 4 -> Vector t 4 -> Matrix t 4 4
mat44 a b c d = (a <::>) b <:> (c <::> d)


-- | Matrix transpose
transpose :: ( KnownNat n
             , KnownNat m
             , M.MatrixCalculus t n m (Array t '[n,m])
             , M.MatrixCalculus t m n (Array t '[m,n])
             , PrimBytes (Array t '[m,n])
             )
          => Matrix t n m -> Matrix t m n
transpose = KnownDataFrame . M.transpose . _getDF

-- | One on a diagonal, zero everywhere else
eye :: ( KnownNat n
       , M.SquareMatrixCalculus t n (Array t '[n,n])
       ) => Matrix t n n
eye = KnownDataFrame M.eye

-- | Single element on a diagonal, zero everywhere else
diag :: ( KnownNat n
        , M.SquareMatrixCalculus t n (Array t '[n,n])
        ) => Scalar t -> Matrix t n n
diag = KnownDataFrame . M.diag . unScalar

-- | Determinant of  Mat
det :: ( KnownNat n
       , M.SquareMatrixCalculus t n (Array t '[n,n])
       ) => Matrix t n n -> Scalar t
det = scalar . M.det . _getDF

-- | Sum of diagonal elements
trace :: ( KnownNat n
         , M.SquareMatrixCalculus t n (Array t '[n,n])
         ) => Matrix t n n -> Scalar t
trace = scalar . M.trace . _getDF


-- | Sum of diagonal elements
inverse :: ( KnownNat n
           , M.MatrixInverse (Array t '[n,n])
           ) => Matrix t n n -> Matrix t n n
inverse = KnownDataFrame . M.inverse . _getDF
