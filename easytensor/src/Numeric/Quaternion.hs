-----------------------------------------------------------------------------
-- |
--
-- Quaternion operations implemented for Floats and Doubles.
--
-- The types `QDouble` and `QFloat` have the same representation as corresponding `Vector t 4`.
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
-- Some of the operations (such as trigonometric operations) are ambiguous for quaternions;
-- the general rules I follow:
--
--  1. Preserve imaginary vector axis same if possible;
--  2. If both @+q@ and @-q@ are possible, prefer real value positive (@re q >= 0@).
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
