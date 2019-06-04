module Numeric.DataFrame.Internal.BackendI (I) where

{- | The instance keeper for the `Backend` type.

  Using this data as a tag to the `Backend` type allows to define `Backend` instances
  in two different modules without any orphans.

 -}
data I
