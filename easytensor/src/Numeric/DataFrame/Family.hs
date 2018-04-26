{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
-- | The very core of DataFrame: definition of the data family.
module Numeric.DataFrame.Family ( DataFrame ) where

-- | Keep data in a primitive data frame
--    and maintain information about Dimensions in the type system
data family DataFrame (t :: l) (xs :: [k])
