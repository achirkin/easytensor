{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PolyKinds         #-}
-- | Vector is an alias to a DataFrame with order 1.
module Numeric.Vector
    (  -- * Type aliases
      Vector
    , Vec2f, Vec3f, Vec4f, Vec2d, Vec3d, Vec4d
    , Vec2i, Vec3i, Vec4i, Vec2w, Vec3w, Vec4w
      -- * Vector constructors
    , Vector2 (..), Vector3 (..), Vector4 (..)
    , DataFrame(Vec2, Vec3, Vec4)
      -- * Common operations
    , (.*.), dot, (·)
    , normL1, normL2, normLPInf, normLNInf, normLP
    , normalized
    , det2, cross, (×)
    ) where

import           Numeric.DataFrame.SubSpace
import           Numeric.DataFrame.Type
import           Numeric.Scalar
import           Numeric.Vector.Internal



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

-- | Take a determinant of a matrix composed from two 2D vectors.
--   Like a cross product in 2D.
det2 :: ( Num t, SubSpace t '[2] '[] '[2] )
     => Vector t 2 -> Vector t 2 -> Scalar t
det2 a b = (a ! 0 :* U) * (b ! 1 :* U)
         - (a ! 1 :* U) * (b ! 0 :* U)

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
