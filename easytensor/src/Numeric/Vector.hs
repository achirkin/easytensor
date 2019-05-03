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

import Numeric.DataFrame.Internal.Backend ()
import Numeric.Vector.Internal
