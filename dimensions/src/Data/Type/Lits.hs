{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType           #-}
#endif


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.List
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- A Mixture of `GHC.TypeLits` and  `GHC.TypeNats` with @Nat@s represented as
-- @Natural@ at runtime, plus some helper functions of our own.
--
--------------------------------------------------------------------------------

module Data.Type.Lits
  ( -- * Kinds
    TN.Nat, TL.Symbol
    -- * Linking type and value level
  , TN.KnownNat, TN.natVal, TN.natVal'
  , TL.KnownSymbol, TL.symbolVal, TL.symbolVal'
  , TN.SomeNat(..), TL.SomeSymbol(..)
  , TN.someNatVal, TL.someSymbolVal
  , TN.sameNat, TL.sameSymbol
    -- * Functions on type literals
  , type (+), type (*), type (^), type (-)
  , type TN.Div, type TN.Mod, type TN.Log2
  , type Min, type Max
  , TL.AppendSymbol, ShowNat
  , TN.CmpNat, TL.CmpSymbol, type (<=)
  , SOrdering (..), cmpNat, cmpSymbol
    -- * User-defined type errors
  , TL.TypeError
  , TL.ErrorMessage(..)
  ) where


import           Data.Kind     (Constraint, Type)
import qualified GHC.TypeLits  as TL
import           GHC.TypeNats  (type (*), type (+), type (-), type (^))
import qualified GHC.TypeNats  as TN
import           Unsafe.Coerce (unsafeCoerce)




-- | Convert a type-level @Nat@ into a type-level @Symbol@.
type family ShowNat (n :: TN.Nat) :: TL.Symbol where
    -- lol
    ShowNat 0 = "0"
    ShowNat 1 = "1"
    ShowNat 2 = "2"
    ShowNat 3 = "3"
    ShowNat 4 = "4"
    ShowNat 5 = "5"
    ShowNat 6 = "6"
    ShowNat 7 = "7"
    ShowNat 8 = "8"
    ShowNat 9 = "9"
    ShowNat d = TL.AppendSymbol (ShowNat (TN.Div d 10)) (ShowNat (TN.Mod d 10))


-- | Singleton-style version of `Ordering`.
--   Pattern-match againts its constructor to witness the result of
--   type-level comparison.
data SOrdering :: Ordering -> Type where
    SLT :: SOrdering 'LT
    SEQ :: SOrdering 'EQ
    SGT :: SOrdering 'GT

-- | Pattern-match against the result of this function to get the evidence
--   of comparing type-level Nats.
cmpNat :: forall (a :: TN.Nat) (b :: TN.Nat) (proxy :: TN.Nat -> Type)
        . (TN.KnownNat a, TN.KnownNat b)
       => proxy a -> proxy b -> SOrdering (TN.CmpNat a b)
cmpNat a b
  = case compare (TN.natVal a) (TN.natVal b) of
    LT -> unsafeCoerce SLT
    EQ -> unsafeCoerce SEQ
    GT -> unsafeCoerce SGT
{-# INLINE cmpNat #-}

-- | Pattern-match against the result of this function to get the evidence
--   of comparing type-level Symbols.
cmpSymbol :: forall (a :: TL.Symbol) (b :: TL.Symbol) (proxy :: TL.Symbol -> Type)
           . (TL.KnownSymbol a, TL.KnownSymbol b)
          => proxy a -> proxy b -> SOrdering (TL.CmpSymbol a b)
cmpSymbol a b
  = case compare (TL.symbolVal a) (TL.symbolVal b) of
    LT -> unsafeCoerce SLT
    EQ -> unsafeCoerce SEQ
    GT -> unsafeCoerce SGT
{-# INLINE cmpSymbol #-}

-- | Miminum among two type-level naturals.
type Min (a :: TN.Nat) (b :: TN.Nat) = Min' a b (TN.CmpNat a b)

-- | Maximum among two type-level naturals.
type Max (a :: TN.Nat) (b :: TN.Nat) = Min' a b (TN.CmpNat a b)

type family Min' (a :: TN.Nat) (b :: TN.Nat) (r :: Ordering) :: TN.Nat where
    Min' a _ 'LT = a
    Min' a _ 'EQ = a
    Min' _ b 'GT = b

type family Max' (a :: TN.Nat) (b :: TN.Nat) (r :: Ordering) :: TN.Nat where
    Max' _ b 'LT = b
    Max' _ b 'EQ = b
    Max' a _ 'GT = a

-- | Comparison of type-level naturals, as a constraint.
type (<=) (a :: TN.Nat) (b :: TN.Nat) = LE a b (TN.CmpNat a b)

type family LE (a :: TN.Nat) (b :: TN.Nat) (r :: Ordering)
                                         = (y :: Constraint) | y -> r where
    LE a b 'LT = ('LT ~ TN.CmpNat a b)
    LE a b 'EQ = ('EQ ~ TN.CmpNat a b)
    LE a b 'GT = ('GT ~ TL.TypeError
      ('TL.Text "Cannot deduce type-level Nat relation: "
          'TL.:<>: 'TL.ShowType a
          'TL.:<>: 'TL.Text " <= "
          'TL.:<>: 'TL.ShowType b
      ))
