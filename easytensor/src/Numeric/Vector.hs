{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
-- | Vector is an alias to a DataFrame with order 1.
module Numeric.Vector
    ( -- * Type aliases
      Vector
    , Vec2f, Vec3f, Vec4f, Vec2d, Vec3d, Vec4d
    , Vec2i, Vec3i, Vec4i, Vec2w, Vec3w, Vec4w
      -- * Common operations
    , (.*.), dot, (·)
    , normL1, normL2, normLPInf, normLNInf, normLP
    , normalized
    , vec2, vec3, vec4
    , det2, cross, (×)
    , unpackV2, unpackV3, unpackV4
    ) where

import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Type
import           Numeric.Scalar

--------------------------------------------------------------------------------
-- * Vector type
--------------------------------------------------------------------------------

type Vector (t :: l) (n :: k) = DataFrame t '[n]

type Vec2f = Vector Float 2
type Vec3f = Vector Float 3
type Vec4f = Vector Float 4
type Vec2d = Vector Double 2
type Vec3d = Vector Double 3
type Vec4d = Vector Double 4
type Vec2i = Vector Int 2
type Vec3i = Vector Int 3
type Vec4i = Vector Int 4
type Vec2w = Vector Word 2
type Vec3w = Vector Word 3
type Vec4w = Vector Word 4


-- | Scalar product -- sum of Vecs' components products,
--                     propagated into whole Vec
(.*.) :: ( Num t
         , Num (Vector t n)
         , SubSpace t '[n] '[] '[n]
         )
      => Vector t n -> Vector t n -> Vector t n
(.*.) a b = fromScalar . ewfoldl (+) 0 $ a * b
infixl 7 .*.

-- | Scalar product -- sum of Vecs' components products -- a scalar
dot :: ( Num t
       , Num (Vector t n)
       , SubSpace t '[n] '[] '[n]
       )
    => Vector t n -> Vector t n -> Scalar t
dot a b = ewfoldl (+) 0 $ a * b

-- | Dot product of two vectors
infixl 7 ·
(·) :: ( Num t
       , Num (Vector t n)
       , SubSpace t '[n] '[] '[n]
       )
    => Vector t n -> Vector t n -> Scalar t
(·) = dot
{-# INLINE (·) #-}


-- | Sum of absolute values
normL1 :: ( Num t, SubSpace t '[n] '[] '[n] )
       => Vector t n -> Scalar t
normL1 = ewfoldr (\a -> (abs a +)) 0

-- | hypot function (square root of squares)
normL2 :: ( Floating t , SubSpace t '[n] '[] '[n] )
       => Vector t n -> Scalar t
normL2 = sqrt . ewfoldr (\a -> (a*a +)) 0

-- | Normalize vector w.r.t. Euclidean metric (L2).
normalized :: ( Floating t , Fractional (Vector t n), SubSpace t '[n] '[] '[n] )
           => Vector t n -> Vector t n
normalized v = v / n
  where
    n = fromScalar . sqrt $ ewfoldr (\a -> (a*a +)) 0 v

-- | Maximum of absolute values
normLPInf :: ( Ord t, Num t , SubSpace t '[n] '[] '[n] )
          => Vector t n -> Scalar t
normLPInf = ewfoldr (max . abs) 0

-- | Minimum of absolute values
normLNInf :: ( Ord t, Num t , SubSpace t '[n] '[] '[n] )
          => Vector t n -> Scalar t
normLNInf x = ewfoldr (min . abs) (abs $ x ! Idx 0 :* U) x

-- | Norm in Lp space
normLP :: ( Floating t , SubSpace t '[n] '[] '[n] )
       => Int -> Vector t n -> Scalar t
normLP i' = (**ri) . ewfoldr (\a -> (a**i +)) 0
  where
    i  = fromIntegral i'
    ri = recip i
{-# INLINE [2] normLP #-}
{-# RULES
"normLP/L1" normLP 1 = normL1
"normLP/L2" normLP 2 = normL2
  #-}

-- | Compose a 2D vector
vec2 :: SubSpace t '[2] '[] '[2] => t -> t -> Vector t 2
vec2 a b = iwgen f
  where
    f (0 :* U) = scalar a
    f _        = scalar b

-- | Take a determinant of a matrix composed from two 2D vectors.
--   Like a cross product in 2D.
det2 :: ( Num t, SubSpace t '[2] '[] '[2] )
     => Vector t 2 -> Vector t 2 -> Scalar t
det2 a b = (a ! 0 :* U) * (b ! 1 :* U)
         - (a ! 1 :* U) * (b ! 0 :* U)

-- | Compose a 3D vector
vec3 :: SubSpace t '[3] '[] '[3] => t -> t -> t -> Vector t 3
vec3 a b c = iwgen f
  where
    f (0 :* U) = scalar a
    f (1 :* U) = scalar b
    f _        = scalar c

-- | Cross product
cross :: ( Num t, SubSpace t '[3] '[] '[3] )
      => Vector t 3 -> Vector t 3 -> Vector t 3
cross a b = vec3 ( unScalar
                 $ (a ! 1 :* U) * (b ! 2 :* U)
                 - (a ! 2 :* U) * (b ! 1 :* U) )
                 ( unScalar
                 $ (a ! 2 :* U) * (b ! 0 :* U)
                 - (a ! 0 :* U) * (b ! 2 :* U) )
                 ( unScalar
                 $ (a ! 0 :* U) * (b ! 1 :* U)
                 - (a ! 1 :* U) * (b ! 0 :* U) )


-- | Cross product for two vectors in 3D
infixl 7 ×
(×) :: ( Num t, SubSpace t '[3] '[] '[3] )
    => Vector t 3 -> Vector t 3 -> Vector t 3
(×) = cross
{-# INLINE (×) #-}


-- | Compose a 4D vector
vec4 :: SubSpace t '[4] '[] '[4]
     => t -> t -> t -> t -> Vector t 4
vec4 a b c d = iwgen f
  where
    f (0 :* U) = scalar a
    f (1 :* U) = scalar b
    f (2 :* U) = scalar c
    f _        = scalar d


unpackV2 :: SubSpace t '[2] '[] '[2]
         => Vector t 2 -> (t, t)
unpackV2 v = (unScalar $ v ! 0, unScalar $ v ! 1)
{-# INLINE unpackV2 #-}


unpackV3 :: SubSpace t '[3] '[] '[3]
         => Vector t 3 -> (t, t, t)
unpackV3 v = (unScalar $ v ! 0, unScalar $ v ! 1, unScalar $ v ! 2)
{-# INLINE unpackV3 #-}


unpackV4 :: SubSpace t '[4] '[] '[4]
         => Vector t 4 -> (t, t, t, t)
unpackV4 v = (unScalar $ v ! 0, unScalar $ v ! 1, unScalar $ v ! 2, unScalar $ v ! 3)
{-# INLINE unpackV4 #-}
