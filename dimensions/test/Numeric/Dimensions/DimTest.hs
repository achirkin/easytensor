{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}



module Numeric.Dimensions.DimTest (runTests) where

import Data.Constraint
import Data.Typeable
import Test.QuickCheck

import           Numeric.Dimensions
import qualified Numeric.TypedList  as TL


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
  , Just (Dx da) <- constrainDim @(XN b) xda -- here da >= db
  = a - b == dimVal (minusDim da db)
prop_minusDim _ _ = False


-- | Matching against @Reverse@ pattern lets GHC know the reversion relation
--   at the type level.
--   That means the type system knows that reverse of reverse is the same list!
prop_reverseDims :: [Word] -> Property
prop_reverseDims xs
  | SomeDims ds <- someDimsVal xs
    = case ds of
        Reverse rds -> case rds of
          Reverse rrds -> ds === rrds


prop_concatDims :: [Word] -> [Word] -> Property
prop_concatDims xs ys
  | SomeDims dxs <- someDimsVal xs
  , SomeDims dys <- someDimsVal ys
    = case TL.concat dxs dys of
        dxsys -> listDims dxsys === xs ++ ys


-- | TODO: bring more evidence about list equality
prop_splitDims :: Word -> [Word] -> Property
prop_splitDims n xsys
  | SomeDims dxsys <- someDimsVal xsys
  , Dx dn <- someDimVal n
     -- TODO: hmm, this is an incomplete pattern even though
     --       Dn dn would not type check :(.
  , (xs, ys) <- splitAt (fromIntegral n) xsys
    = case TL.splitAt dn dxsys of
      (dxs, dys) -> listDims dxs === xs
               .&&. listDims dys === ys
               .&&. case stripPrefixDims dxs dxsys >>= sameDims dys of
                      Nothing   -> counterexample
                                    ("stripPrefixDims with " ++ show (dxs, dys, dxsys)) False
                      Just Dict -> property True
               .&&. case stripSuffixDims dys dxsys >>= sameDims dxs of
                      Nothing   -> counterexample
                                    ("stripSuffixDims with " ++ show (dxs, dys, dxsys)) False
                      Just Dict -> property True
                -- .&&. dxsys === TL.concat dxs dys
  | otherwise = property False


prop_stripPrefixDims :: Int -> [Word] -> Property
prop_stripPrefixDims n l
  | SomeDims xsys@Dims <- someDimsVal l
  , SomeDims xs@Dims <- someDimsVal $ take (n `mod` (length l + 2)) l
  , Just ys@Dims  <- stripPrefixDims xs xsys
  , Dict <- dimsAllEq xs
  , Dict <- dimsAllTypeable xs
  , Dict <- dimsAllTypeable xsys
  , Just ys'@Dims <- TL.stripPrefix xs xsys
    = case sameDims ys ys' of
        Nothing -> counterexample
                      ("stripPrefix[Dims] discrepancy " ++ show (ys, ys')) False
        Just Dict -> property True
  | otherwise = counterexample "stripPrefix of take does not work!" False

prop_stripSuffixDims :: Int -> [Word] -> Property
prop_stripSuffixDims n l
  | SomeDims xsys@Dims <- someDimsVal l
  , SomeDims ys@Dims <- someDimsVal $ drop (n `mod` (length l + 2)) l
  , Just xs@Dims  <- stripSuffixDims ys xsys
  , Dict <- dimsAllEq ys
  , Dict <- dimsAllTypeable ys
  , Dict <- dimsAllTypeable xsys
  , Just xs'@Dims <- TL.stripSuffix ys xsys
    = case sameDims xs xs' of
        Nothing -> counterexample
                      ("stripSuffixDims[Dims] discrepancy " ++ show (xs, xs')) False
        Just Dict -> property True
  | otherwise = counterexample "stripSuffixDims of take does not work!" False


dimsAllEq :: Dims (ns :: [Nat]) -> Dict (All Eq (Map Dim ns))
dimsAllEq U         = Dict
dimsAllEq (D :* ds) | Dict <- dimsAllEq ds = Dict

dimsAllTypeable :: Dims (ns :: [Nat]) -> Dict (All Typeable ns)
dimsAllTypeable U           = Dict
dimsAllTypeable ((D :: Dim n) :* ds)
  | Dict <- mapDict cls (Dict @(KnownDim n))
  , Dict <- dimsAllTypeable ds = Dict


return []
runTests :: IO Bool
runTests = $quickCheckAll
