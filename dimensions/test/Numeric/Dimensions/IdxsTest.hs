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


module Numeric.Dimensions.IdxsTest (runTests) where

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           Test.QuickCheck         (quickCheckAll)

import           Numeric.Dimensions.Idxs

minMaxSeq :: [(Word, Word)] -> ([Word], [Word])
minMaxSeq []          = ([], [])
minMaxSeq ((x, y):xs) = (min x y :) *** (max x y :) $ minMaxSeq xs

twoIdxsSeq :: [(Word, Word, Word)] -> ([Word], [Word], SomeDims)
twoIdxsSeq inputs = (take n is1, take n is2, someDimsVal (take n ds))
  where
    n = fst $ foldl findN (0, 1) ds
    findN (k, dd) d
      = let dd' = dd * d
        in if d <= niceLimit && dd' <= niceLimit
           then (k+1,dd') else (k, niceLimit+1)
    (is1, is2, ds) = unzip3 $ map reform inputs
    niceLimit = 100000
    reform :: (Word, Word, Word) -> (Word, Word, Word)
    reform (x, y, z)
      | z >= x && z >= y = (x, y, z + 1)
      | x >= z && x >= y = (y, z, x + 1)
      | otherwise        = (x, z, y + 1)


-- | Check successful cases
prop_idxsFromWords1 :: [(Word, Word)] -> Bool
prop_idxsFromWords1 ins
  | (xs, ys) <- minMaxSeq ins
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal ys
  , mIs <- idxsFromWords @ds xs
    = or (zipWith (==) xs ys) || isJust mIs
  | otherwise
    = error "Impossible arguments"

-- | Check failing cases
prop_idxsFromWords2 :: [(Word, Word)] -> Bool
prop_idxsFromWords2 ins
  | (xs, ys) <- minMaxSeq ins
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal xs
  , mIs <- idxsFromWords @ds ys
    = null xs || isNothing mIs
  | otherwise
    = error "Impossible arguments"

-- | Check if results of idxsFromWords are consistent with idxFromWord
prop_idxsFromWords3 :: [(Word, Word)] -> Bool
prop_idxsFromWords3 ins
  | (xs, ys) <- minMaxSeq ins
  , SomeDims (ds@KnownDims :: Dims ds) <- someDimsVal ys
  , mIs <- idxsFromWords @ds xs
    = Just False /= (go xs ds <$> mIs)
  | otherwise
    = error "Impossible arguments"
  where
    go :: forall ns . [Word] -> Dims ns -> Idxs ns -> Bool
    go [] U U = True
    go (w:ws) (Dim :* ds) (i :* is)
              = Just i == idxFromWord w && go ws ds is
    go _ _ _  = False

multLimit :: Word
multLimit = floor $ sqrt (fromIntegral (maxBound :: Int) :: Double)

-- check for Word overflow
wouldNotOverflow :: [Word] -> Bool
wouldNotOverflow
  = and . snd . mapAccumR (\a e -> (a*e, a*e >= a && multLimit > a)) 1

prop_idxsFromEnum :: [(Word, Word)] -> Bool
prop_idxsFromEnum ins
  | (xs, ys) <- minMaxSeq ins
  , wouldNotOverflow ys
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal ys
  , Just ids <- idxsFromWords @ds xs
    = ids == toEnum (fromEnum ids)
  | otherwise = True

prop_idxsSucc :: [(Word, Word)] -> Bool
prop_idxsSucc ins
  | (xs, ys) <- minMaxSeq ins
  , wouldNotOverflow ys
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal ys
  , Just ids <- idxsFromWords @ds xs
    = ids == maxBound || fromEnum (succ ids) == succ (fromEnum ids)
  | otherwise = True

prop_idxsPred :: [(Word, Word)] -> Bool
prop_idxsPred ins
  | (xs, ys) <- minMaxSeq ins
  , wouldNotOverflow ys
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal ys
  , Just ids <- idxsFromWords @ds xs
    = ids == minBound || fromEnum (pred ids) == pred (fromEnum ids)
  | otherwise = True

prop_idxsPredSucc :: [(Word, Word)] -> Bool
prop_idxsPredSucc ins
  | (xs, ys) <- minMaxSeq ins
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal ys
  , Just ids <- idxsFromWords @ds xs
    =  ids == minBound || ids == maxBound
    || ( succ (pred ids) == ids && pred (succ ids) == ids )
  | otherwise = True

prop_idxsEnumFrom :: [(Word, Word)] -> Bool
prop_idxsEnumFrom ins
  | (xs, ys) <- minMaxSeq ins
  , wouldNotOverflow ys
  , product ys < 100000
  , SomeDims (KnownDims :: Dims ds) <- someDimsVal ys
  , Just ids <- idxsFromWords @ds xs
    = [ids..] == map toEnum [fromEnum ids .. fromEnum (maxBound @(Idxs ds))]
  | otherwise = True

prop_idxsEnumFromTo :: [(Word, Word, Word)] -> Bool
prop_idxsEnumFromTo ins
  | (xs, ys, SomeDims (KnownDims :: Dims ds)) <- twoIdxsSeq ins
  , Just ids <- idxsFromWords @ds xs
  , Just jds <- idxsFromWords @ds ys
    = [ids..jds] == map toEnum [fromEnum ids .. fromEnum jds]
  | otherwise = True

prop_idxsEnumFromThen :: [(Word, Word, Word)] -> Bool
prop_idxsEnumFromThen ins
  | (xs, ys, SomeDims (KnownDims :: Dims ds)) <- twoIdxsSeq ins
  , Just ids <- idxsFromWords @ds xs
  , Just jds <- idxsFromWords @ds ys
  , lim <- if jds >= ids then maxBound else minBound :: Idxs ds
    = take 1000 [ids, jds ..]
      ==
      take 1000 (map toEnum [fromEnum ids, fromEnum jds .. fromEnum lim])
  | otherwise = True

prop_idxsEnumFromThenTo :: Bool -> [(Word, Word, Word)] -> Bool
prop_idxsEnumFromThenTo up ins
  | (xs, ys, SomeDims (KnownDims :: Dims ds)) <- twoIdxsSeq ins
  , Just ids <- idxsFromWords @ds xs
  , Just jds <- idxsFromWords @ds ys
  , lim <- if up then maxBound else minBound :: Idxs ds
    = take 1000 [ids, jds .. lim]
      ==
      take 1000 (map toEnum [fromEnum ids, fromEnum jds .. fromEnum lim])
  | otherwise = True

return []
runTests :: IO Bool
runTests = $quickCheckAll
