{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

-- | Some GHC versions show incorrect warnings here:
--
--   GHC 8.2 says "Pattern match has inaccessible right hand side"
--    if our GADT-like patterns are matched nested:
--    https://ghc.haskell.org/trac/ghc/ticket/14253
--
--   GHC 8.0 says "Pattern match(es) are non-exhaustive"
--    because it does not support COMPLETE pragmas yet.
--
module Numeric.DimTest (runTests) where

import Data.Constraint (Dict (..))
import Test.QuickCheck (quickCheckAll)

import Numeric.Dim


-- | Try inference of type-level natural values via term-level binary functions.
testBinaryOp :: forall (a :: Nat) (b :: Nat) (c :: Nat)
              . (Word -> Word -> Word)
             -> (Dim a -> Dim b -> Dim c)
             -> Dim a -> Dim b -> Bool
testBinaryOp fTerm fType da db
  | a <- dimVal da
  , b <- dimVal db
  , Dx dr <- someDimVal (fTerm a b)
      -- pattern-match against @SomeDim@ to extract a type-level natural Dim.
  , True  <- fTerm a b == dimVal (fType da db)
      -- compare the term-level function and the type-level function results
      -- as regular word values.
  , Just Dict <- sameDim dr (fType da db)
      -- now the type system knows that @c ~ fType a b@ and
      -- we can use ordinary equality function (which is @const True@).
  = dr == fType da db
testBinaryOp _ _ _ _ = False



prop_plusDim :: Word -> Word -> Bool
prop_plusDim a b = case (someDimVal a, someDimVal b) of
  (Dx da, Dx db) -> testBinaryOp (+) plusDim da db


prop_timesDim :: Word -> Word -> Bool
prop_timesDim a b = case (someDimVal a, someDimVal b) of
  (Dx da, Dx db) -> testBinaryOp (*) timesDim da db


prop_powerDim :: Word -> Word -> Bool
prop_powerDim a b = case ( someDimVal a
                         , someDimVal b
                         ) of
  (Dx da, Dx db) -> testBinaryOp (^) powerDim da db

prop_minusDim :: Word -> Word -> Bool
prop_minusDim a' b'
  | a <- max a' b'
  , b <- min a' b'
  , xda <- someDimVal a -- this is an unknown (Dim (XN 0))
  , Dx (db :: Dim b) <- someDimVal b
  , Just (Dx da) <- constrain @_ @(XN b) xda -- here da >= db
  = a - b == dimVal (minusDim da db)
prop_minusDim _ _ = False






return []
runTests :: IO Bool
runTests = $quickCheckAll
