{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-monomorphism-restriction   #-}

module Data.Type.ListTest where

import           Data.Constraint
import           Data.Kind                      ( Type )
import           Data.Type.Equality
import           Data.Type.List
import           Data.Type.Lits
import           Numeric.Natural
import           Test.QuickCheck
import           Unsafe.Coerce

data P (a :: k) = P Natural
  deriving (Eq, Show, Read)

p :: KnownNat n => P n
p = let x = P (natVal x) in x

cmpP :: P n -> P m -> SEq n m
cmpP (P a) (P b) | a == b    = unsafeCoerce (SEq @() @())
                 | otherwise = unsafeCoerce (SNe @1 @2)

data L :: [k] -> Type where
  N :: L '[]
  (:^) :: P n -> L ns -> L (n ': ns)
infixr 5 :^

data SEq :: k -> k -> Type where
  SEq :: (a ~ b, (a == b) ~ 'True) => SEq a b
  SNe :: ((a == b) ~ 'False) => SEq a b

cmpL :: L as -> L bs -> SEq as bs
cmpL N         N         = SEq
cmpL (a :^ as) (b :^ bs) = case (cmpP a b, cmpL as bs) of
  (SEq, SEq) -> SEq
  (SNe, _  ) -> SNe
  (_  , SNe) -> SNe
cmpL N        (_ :^ _) = SNe
cmpL (_ :^ _) N        = SNe

instance Show (L as) where
  show N           = ""
  show (P n :^ N)  = show n
  show (P n :^ xs) = show n ++ "." ++ show xs


data SomeL (k :: Type) = forall (as :: [k]) . SomeL (L as)

instance Show (SomeL k) where
  show (SomeL a) = show a

x00 :: P 0
x00 = p

x01 :: P 1
x01 = p

xs00 :: L ('[] :: [Nat])
xs00 = N

xs01 :: L '[3]
xs01 = p :^ N

xs02 :: L '[4, 8]
xs02 = p :^ p :^ N

xs03 :: L '[5, 7, 9, 2]
xs03 = p :^ p :^ p :^ p :^ N

snoc
  :: forall (k :: Type) (xs :: [k]) (s :: k) (xss :: [k])
   . SnocList xs s xss
  => L xs
  -> P s
  -> L xss
snoc N         x = x :^ N
snoc (x :^ xs) y = x :^ (snoc xs y)

someXs00 :: SomeL Nat
someXs00 = SomeL xs00
someXs01 :: SomeL Nat
someXs01 = SomeL xs01
someXs02 :: SomeL Nat
someXs02 = SomeL xs02
someXs03 :: SomeL Nat
someXs03 = SomeL xs03

initL
  :: forall (k :: Type) (xs :: [k]) (s :: k) (xss :: [k])
   . SnocList xs s xss
  => L xss
  -> L xs
initL (_ :^ N        ) = N
initL (x :^ (y :^ ys)) = x :^ initL (y :^ ys)

revL1
  :: forall (k :: Type) (xs :: [k]) (sx :: [k])
   . ReverseList xs sx
  => L xs
  -> L sx
revL1 N         = N
revL1 (m :^ ms) = snoc (revL1 ms) m

revL2
  :: forall (k :: Type) (xs :: [k]) (sx :: [k])
   . ReverseList xs sx
  => L sx
  -> L xs
revL2 N         = N
revL2 (m :^ ms) = snoc (revL2 ms) m

revL3 :: forall (k :: Type) (xs :: [k]) . L xs -> L (Reverse xs)
revL3 xs = go xs N
  where
    go
      :: forall (ys :: [k]) (sy :: [k]) (zs :: [k])
       . ReverseList ys sy
      => L ys
      -> L zs
      -> L (sy ++ zs)
    go N ms = ms
    go ((n :: P n) :^ (ns :: L ns)) ms | Dict <- inferConcat @sy @zs =
      go ns (n :^ ms)


stripPrefL :: ConcatList as bs asbs => L as -> L asbs -> L bs
stripPrefL N         asbs        = asbs
stripPrefL (_ :^ as) (_ :^ asbs) = stripPrefL as asbs


stripSufL :: ConcatList as bs asbs => L bs -> L asbs -> L as
stripSufL N  asbs              = asbs
stripSufL bs aasbs@(a :^ asbs) = case cmpL bs aasbs of
  SEq -> N
  SNe -> a :^ stripSufL bs asbs

concatL :: ConcatList as bs asbs => L as -> L bs -> L asbs
concatL N         bs = bs
concatL (a :^ as) bs = a :^ concatL as bs



goList
  :: forall as bs asbs
   . ConcatList as bs asbs
  => L as
  -> L bs
  -> L asbs
  -> Property
goList as bs asbs = conjoin
  [ conjoin $ map
    ((show as ===) . show)
    [ as
    , stripSufL bs asbs
    , concatL N  as
    , concatL as N
    , stripSufL bs (concatL N asbs)
    ]
  , conjoin $ map
    ((show bs ===) . show)
    [ bs
    , stripPrefL as asbs
    , concatL N  bs
    , concatL bs N
    , stripPrefL as (concatL N asbs)
    ]
  , conjoin $ map
    ((show asbs ===) . show)
    [ asbs
    , concatL as   bs
    , concatL N    asbs
    , concatL asbs N
    , concatL as   (concatL N bs)
    ]
  , -- sadly, we have to use inferConcat here, because GHC cannot voluntarily
    -- try to infer ConcatList from the appearence of Concat TF alone.
    --   A plugin would solve this.
    case inferConcat @(Reverse bs) @(Reverse as) @_ of
    Dict -> show (revL1 asbs) === show (concatL (revL1 bs) (revL1 as))
  ]


splitN :: Int -> SomeL k -> (SomeL k, SomeL k)
splitN 0 xs        = (SomeL N, xs)
splitN _ (SomeL N) = (SomeL N, SomeL N)
splitN n (SomeL (x :^ xs)) | (SomeL as, SomeL bs) <- splitN (n - 1) (SomeL xs) =
  (SomeL (snoc as x), SomeL bs)


dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse


{-# ANN runTests "HLint: ignore Drop on a non-positive" #-}

runTests :: IO Bool
runTests = isGood <$> quickCheckResult (conjoin tests)
  where
    isGood :: Result -> Bool
    isGood Success{} = True
    isGood _         = False
    tests :: [Property]
    tests =
      [ show xs00 === show (revL1 xs00)
      , dropEnd 2 (show xs03) === show (initL xs03)
      , show xs03 === reverse (show (revL1 xs03))
      , show as === show (stripSufL bs xs03)
      , show (snd $ splitN 0 $ SomeL asbs) === drop 0 (show asbs)
      , show (snd $ splitN 1 $ SomeL asbs) === drop 2 (show asbs)
      , show (snd $ splitN 2 $ SomeL asbs) === drop 4 (show asbs)
      , show (snd $ splitN 3 $ SomeL asbs) === drop 6 (show asbs)
      , show (snd $ splitN 4 $ SomeL asbs) === drop 8 (show asbs)
      , show (snd $ splitN 5 $ SomeL asbs) === drop 10 (show asbs)
      , goList as bs asbs
      ]
    as   = initL $ initL xs03
    bs   = stripPrefL as xs03
    asbs = concatL as bs
