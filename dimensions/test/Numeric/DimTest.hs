{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE UndecidableInstances      #-}



module Numeric.DimTest (runTests) where

import           Test.QuickCheck    (quickCheckAll)

import           Numeric.Type.Evidence
import           Numeric.Dim



testBinaryOp :: forall (a :: Nat) (b :: Nat) (c :: Nat)
              . (Word -> Word -> Word)
             -> (Dim a -> Dim b -> Dim c)
             -> Dim a -> Dim b -> Bool
testBinaryOp fTerm fType da db
  | a <- dimVal da
  , b <- dimVal db
  , Dx dr <- someDimVal (fTerm a b)
  , True  <- fTerm a b == dimVal (fType da db) 
  , Just E <- sameDim dr (fType da db)
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


-- prop_minusDim :: Word -> Word -> Bool
-- prop_minusDim a' b'
--   | a <- max a' b'
--   , b <- min a' b'
--   , xda <- someDimVal a -- this is an unknown (Dim (XN 0))
--   , Dx db <- someDimVal b
--   , Just (Dx da) <- constrainBy db xda -- here da >= db
--   = a - b == dimVal (minusDim da db)
-- prop_minusDim _ _ = False
--
-- constrainBy :: forall m x . Dim m -> Dim x -> Maybe (Dim (XN m))
-- constrainBy D = constrain @m





return []
runTests :: IO Bool
runTests = $quickCheckAll
