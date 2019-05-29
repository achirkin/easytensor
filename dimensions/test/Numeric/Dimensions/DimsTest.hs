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



module Numeric.Dimensions.DimsTest (runTests) where

import Data.Constraint
import Data.Typeable
import Test.QuickCheck

import           Numeric.Dimensions
import qualified Numeric.TypedList  as TL


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
