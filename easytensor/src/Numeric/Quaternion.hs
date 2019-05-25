-----------------------------------------------------------------------------
-- |
--
-- Quaternion operations implemented for Floats and Doubles.
--
-- The types `QDouble` and `QFloat` have the same representation as corresponding @Vector t 4@.
-- This means, you can do a cheap conversion between the types.
--
-- However, arithmetic instances, such as Num and Floating are implemented in substentially different ways.
-- For example, fromInteger fills a vector fully but sets only real part to a quaternion:
--
-- >>> 1 = vec4 1 1 1 1
-- >>> 1 = packQ 0 0 0 1
--
-- All other numeric operations for vectors are element-wise, but for quaternions I have implemented
-- the actual quaternion math.
-- In most of the cases, you can think of these as being operations on complex numbers,
-- where the role of imaginary @i@ is played by a 3D vector.
-- Some of the operations are ambiguous for quaternions;
-- for example \( \sqrt{-1} = \pm i \), but also \( \sqrt{-1} = \pm j \),
-- and \( \sqrt{-1} = \pm k \), and any other unit quaterion with zero real part.
-- In cases like this, I stick to the @i@-th axis:  \( \sqrt{-1} := i \)
-----------------------------------------------------------------------------
module Numeric.Quaternion
    ( Quaternion (..)
    , Quater(Quater)
    , QDouble, QFloat
    ) where

import Numeric.DataFrame.Internal.Backend  ()
import Numeric.Quaternion.Internal
import Numeric.Quaternion.Internal.QDouble
import Numeric.Quaternion.Internal.QFloat
