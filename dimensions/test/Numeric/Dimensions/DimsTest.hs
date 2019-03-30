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

import           Numeric.Dim
import           Numeric.Dimensions.Dims
import qualified Numeric.TypedList       as TL


-- | Matching against @Reverse@ pattern lets GHC know the reversion relation
--   at the type level.
--   That means the type system knows that reverse of reverse is the same list!
prop_reverseDims :: [Word] -> Bool
prop_reverseDims xs
  | SomeDims ds <- someDimsVal xs
    = case ds of
        Reverse rds -> case rds of
          Reverse rrds -> ds == rrds


prop_concatDims :: [Word] -> [Word] -> Bool
prop_concatDims xs ys
  | SomeDims dxs <- someDimsVal xs
  , SomeDims dys <- someDimsVal ys
    = case TL.concat dxs dys of
        dxsys -> listDims dxsys == xs ++ ys


-- | TODO: bring more evidence about list equality
prop_splitDims :: Word -> [Word] -> Bool
prop_splitDims n xsys
  | SomeDims dxsys <- someDimsVal xsys
  , Dx dn <- someDimVal n
     -- TODO: hmm, this is an incomplete pattern even though
     --       Dn dn would not type check :(.
  , (xs, ys) <- splitAt (fromIntegral n) xsys
    = case TL.splitAt dn dxsys of
      (dxs, dys) -> and
        [ listDims dxs == xs
        , listDims dys == ys
        -- , dxsys == TL.concat dxs dys
        ]
  | otherwise = False -- impossible pattern-match







return []
runTests :: IO Bool
runTests = $quickCheckAll
