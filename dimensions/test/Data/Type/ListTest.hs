{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Type.ListTest where

import Data.Constraint
import Data.Kind          (Type)
import Data.Type.Equality
import Data.Type.List
import Data.Type.Lits
import Numeric.Natural
import Unsafe.Coerce

data P (a :: k) = P Natural
  deriving (Eq, Show, Read)

p :: KnownNat n => P n
p = let x = P (natVal x) in x

cmpP :: P n -> P m -> SEq n m
cmpP (P a) (P b)
  | a == b = unsafeCoerce (SEq @() @())
  | otherwise = unsafeCoerce (SNe @1 @2)

data L :: [k] -> Type where
  N :: L '[]
  (:^) :: P n -> L ns -> L (n ': ns)
infixr 5 :^

data SEq :: k -> k -> Type where
  SEq :: (a ~ b, (a == b) ~ 'True) => SEq a b
  SNe :: ((a == b) ~ 'False) => SEq a b

cmpL :: L as -> L bs -> SEq as bs
cmpL N N = SEq
cmpL (a :^ as) (b :^ bs) = case (cmpP a b, cmpL as bs) of
    (SEq, SEq) -> SEq
    (SNe, _)   -> SNe
    (_, SNe)   -> SNe
cmpL N (_ :^ _) = SNe
cmpL (_ :^ _) N = SNe

instance Show (L as) where
  show N           = ""
  show (P n :^ xs) = show n ++ "." ++ show xs


data SomeL (k :: Type) = forall (as :: [k]) . SomeL (L as)


x00 :: P 0
x00 = p

x01 :: P 1
x01 = p

xs00 :: L ('[] :: [Nat])
xs00 = N

xs01 :: L '[3]
xs01 = p :^ N

xs02 :: L '[4,8]
xs02 = p :^ p :^ N

xs03 :: L '[5,7,9,2]
xs03 = p :^ p :^ p :^ p :^ N

snoc :: forall (k :: Type) (xs :: [k]) (s :: k) (xss :: [k])
      . SnocList xs s xss => L xs -> P s -> L xss
snoc N x         = x :^ N
snoc (x :^ xs) y = x :^ (snoc xs y)

someXs00 :: SomeL Nat
someXs00 = SomeL xs00
someXs01 :: SomeL Nat
someXs01 = SomeL xs01
someXs02 :: SomeL Nat
someXs02 = SomeL xs02
someXs03 :: SomeL Nat
someXs03 = SomeL xs03

tailL :: forall (k :: Type) (xs :: [k]) (s :: k) (xss :: [k])
       . SnocList xs s xss => L xss -> L xs
tailL (_ :^ N)         = N
tailL (x :^ (y :^ ys)) = x :^ tailL (y :^ ys)
tailL N                = undefined

revL1 :: forall (k :: Type) (xs :: [k]) (sx :: [k])
     . ReverseList xs sx => L xs -> L sx
revL1 N         = N
revL1 (m :^ ms) = snoc (revL1 ms) m

revL2 :: forall (k :: Type) (xs :: [k]) (sx :: [k])
      . ReverseList xs sx => L sx -> L xs
revL2 N         = N
revL2 (m :^ ms) = snoc (revL2 ms) m

revL3 :: forall (k :: Type) (xs :: [k])
      . L xs -> L (Reverse xs)
revL3 xs = go xs N
  where
    go :: forall (ys :: [k]) (sy :: [k]) (zs :: [k])
        . ReverseList ys sy => L ys -> L zs -> L (sy ++ zs)
    go N ms         = ms
    go ((n :: P n) :^ (ns :: L ns)) ms
      | Dict <- inferConcat @k @sy @zs
      = go ns (n :^ ms)


stripPrefL :: ConcatList as bs asbs => L as -> L asbs -> L bs
stripPrefL N asbs                = asbs
stripPrefL (_ :^ as) (_ :^ asbs) = stripPrefL as asbs


stripSufL :: ConcatList as bs asbs => L bs -> L asbs -> L as
stripSufL N asbs               = asbs
stripSufL bs aasbs@(a :^ asbs) = case cmpL bs aasbs of
  SEq -> N
  SNe -> a :^ stripSufL bs asbs

concatL :: ConcatList as bs asbs => L as -> L bs -> L asbs
concatL N bs         = bs
concatL (a :^ as) bs = a :^ concatL as bs



goList :: ConcatList as bs asbs => L as -> L bs -> L asbs -> IO ()
goList as bs asbs = do
  print (as, stripSufL bs asbs, concatL N as, concatL as N, stripSufL bs (concatL N asbs))
  print (bs, stripPrefL as asbs, concatL N bs, concatL bs N, stripPrefL as (concatL N asbs))
  print (asbs, concatL as bs, concatL N asbs, concatL asbs N, concatL as (concatL N bs))
  print (revL1 as, revL1 bs, revL1 asbs)


splitN :: Int -> SomeL k -> (SomeL k, SomeL k)
splitN 0 xs = (SomeL N, xs)
splitN _ (SomeL N) = (SomeL N, SomeL N)
splitN n (SomeL (x :^ xs))
  | (SomeL as, SomeL bs) <- splitN (n-1) (SomeL xs)
    = (SomeL (snoc as x), SomeL bs)

goStripN :: Int -> SomeL k -> IO ()
goStripN n xs = case splitN n xs of
  (SomeL (as :: L as), SomeL (bs :: L bs))
    | Dict <- inferConcat @_ @as @bs @(Concat as bs) -> goList as bs $ concatL as bs


anotherPrint :: forall (k :: Type) (ds :: [k])
             . (ConcatList '[Head ds] (Tail ds) ds)
             => L ds -> IO ()
anotherPrint ds = print
  ( stripPrefL as ds
  , stripSufL bs ds
  , ds
  )
  where
    (as, bs) = (case ds of x :^ xs -> (x :^ N, xs)) :: (L '[Head ds], L (Tail ds))

runTests :: IO Bool
runTests = do
  putStr "Reverse Empty: "
  print (xs00, revL1 xs00)
  putStr "Orig:    "
  print xs03
  putStr "UnSnoc:  "
  print $ tailL xs03
  putStr "Reverse: "
  print $ revL1 xs03
  let as = tailL $ tailL xs03
      bs = stripPrefL as xs03
      asbs = concatL as bs
  putStr "as:   "
  print as
  putStr "bs:   "
  print bs
  putStr "asbs: "
  print asbs
  putStr "suf:  "
  print $ stripPrefL as asbs
  putStr "pref: "
  print $ stripSufL bs asbs
  print (stripSufL xs00 asbs, stripPrefL xs00 asbs, concatL asbs asbs
        , concatL xs00 xs00, stripSufL xs00 xs00, stripPrefL xs00 xs00)
  goList as bs asbs
  goStripN 0 someXs03
  goStripN 1 someXs03
  goStripN 2 someXs03
  goStripN 3 someXs03
  goStripN 4 someXs03
  goStripN 5 someXs03
  anotherPrint xs01
  anotherPrint xs02
  anotherPrint xs03
  print (revL1 xs00, revL2 xs00, revL3 xs00)
  print (revL1 xs01, revL2 xs01, revL3 xs01)
  print (revL1 xs02, revL2 xs02, revL3 xs02)
  print (revL1 xs03, revL2 xs03, revL3 xs03)
  return True
