{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}



module Numeric.Dimensions.DimsTest (runTests) where

import           Test.QuickCheck         (quickCheckAll)

import           Numeric.Dimensions.Dims
import           Numeric.Type.Evidence



-- | Matching against @Reverse@ pattern lets GHC know the reversion relation
--   at the type level.
--   That means the type system knows that reverse of reverse is the same list!
prop_reverseDims :: [Word] -> Bool
prop_reverseDims xs
  | SomeDims ds <- someDimsVal xs
  = case ds of
      Reverse rds -> case rds of
        Reverse rrds -> ds == rrds




return []
runTests :: IO Bool
runTests = $quickCheckAll
