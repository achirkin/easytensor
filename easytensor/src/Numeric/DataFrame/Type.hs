-- {-# LANGUAGE CPP                        #-}
-- {-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Type
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Type
  ( -- * Data types
    DataFrame (..)
  , SomeDataFrame (..), DataFrame'
  , pattern (:*:), pattern Z

  --   -- * Bring type classes into scope
  -- , NumericFrame
  --   -- * Utility type families and constraints
  -- , FPFRame, IntegralFrame, NumericVariantFrame, CommonOpFrame
  ) where

-- #include "MachDeps.h"
import           Data.Int                                (Int16, Int32, Int64,
                                                          Int8)
import           Data.Word                               (Word16, Word32,
                                                          Word64, Word8)
import           Foreign.Storable                        (Storable (..))
import           GHC.Base


import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Family
import           Numeric.Dimensions
import           Numeric.PrimBytes

-- | Single frame
newtype instance DataFrame (t :: Type) (ns :: [Nat])
  = SingleFrame { _getDF :: Array t ns }

-- | Multiple "columns" of data frames of the same shape
newtype instance DataFrame (ts :: [Type]) (ns :: [Nat])
  = MultiFrame { _getDFS ::  TypedList (DataFrame' ns) ts }

-- | Data frame with some dimensions missing at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame.
data instance DataFrame (ts :: l) (xs :: [XNat])
  = forall (ns :: [Nat]) . Dimensions ns
  => XFrame (DataFrame ts ns)

-- | Data frame that has an unknown dimensionality at compile time.
--   Pattern-match against its constructor to get a Nat-indexed data frame
data SomeDataFrame (t :: l)
  = forall (ns :: [Nat]) . Dimensions ns => SomeDataFrame (DataFrame t ns)

-- | DataFrame with its type arguments swapped.
newtype DataFrame' (xs :: [k]) (t :: l) = DataFrame' (DataFrame t xs)

{-# COMPLETE Z, (:*:) #-}


-- | Constructing a @MultiFrame@ using DataFrame columns
pattern (:*:) :: forall (xs :: [Type]) (ns :: [Nat])
              . ()
              => forall (y :: Type) (ys :: [Type])
              . (xs ~ (y ': ys))
                => DataFrame y ns
                -> DataFrame ys ns -> DataFrame xs ns
pattern (:*:) x xs <- (MultiFrame (DataFrame' x :* (MultiFrame -> xs)))
  where
    (:*:) x (MultiFrame xs) = MultiFrame (DataFrame' x :* xs)
infixr 6 :*:

-- | Empty MultiFrame
pattern Z :: forall (xs :: [Type]) (ns :: [Nat])
           . () => (xs ~ '[]) => DataFrame xs ns
pattern Z = MultiFrame U


instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame (t :: Type) ds) where
  show (SingleFrame arr) = unlines
                            [ "DF " ++ drop 5 (show $ dims @_ @ds) ++ ":"
                            , show arr
                            ]
instance ( Dimensions ds
         , ImplAllows Show ts ds
         ) => Show (DataFrame (ts :: [Type]) ds) where
  show dfs = unlines $
      ("DF " ++ show (order dds) ++ " x "  ++ drop 5 (show dds) ++ ":")
      : showAll 1 dfs
    where
      dds = dims @_ @ds
      showAll :: ImplAllows Show xs ds => Word -> DataFrame (xs :: [Type]) ds -> [String]
      showAll _ Z = []
      showAll n (SingleFrame arr :*: fs) = ("Var " ++ show n) : show arr : showAll (n+1) fs

type family ImplAllows (f :: Type -> Constraint) (ts :: l) (ds :: [Nat])
                                                             :: Constraint where
    ImplAllows f (t :: Type) ds = f (Array t ds)
    ImplAllows _ ('[] :: [Type]) _ = ()
    ImplAllows f (t ': ts :: [Type]) ds = (f (Array t ds), ImplAllows f ts ds)

-- TODO: shall I add all these instances to MultiFrame?
deriving instance Bounded (Array t ds) => Bounded (DataFrame t ds)
deriving instance Enum (Array t ds) => Enum (DataFrame t ds)
deriving instance Eq (Array t ds) => Eq (DataFrame t ds)
deriving instance Integral (Array t ds)
               => Integral (DataFrame t ds)
deriving instance Num (Array t ds)
               => Num (DataFrame t ds)
deriving instance Fractional (Array t ds)
               => Fractional (DataFrame t ds)
deriving instance Floating (Array t ds)
               => Floating (DataFrame t ds)
deriving instance Ord (Array t ds)
               => Ord (DataFrame t ds)
deriving instance ( Read (Array t ds), Dimensions ds )
               => Read (DataFrame t ds)
deriving instance Real (Array t ds)
               => Real (DataFrame t ds)
deriving instance RealFrac (Array t ds)
               => RealFrac (DataFrame t ds)
deriving instance RealFloat (Array t ds)
               => RealFloat (DataFrame t ds)
deriving instance (PrimArray t (Array t ds), PrimBytes t)
               => PrimArray t (DataFrame t ds)
deriving instance PrimBytes (Array t ds)
               => PrimBytes (DataFrame t ds)



-- instance ( Dimensions ds
--          , ElementWise (Idx ds) t (Array t ds)
--          ) => ElementWise (Idx ds) t (DataFrame t ds) where
--   indexOffset#  = indexOffset# . _getDF
--   {-# INLINE indexOffset# #-}
--   (!) = (!) . _getDF
--   {-# INLINE (!) #-}
--   ewmap f = KnownDataFrame . ewmap f . _getDF
--   {-# INLINE ewmap #-}
--   ewgen = KnownDataFrame . ewgen
--   {-# INLINE ewgen #-}
--   ewgenA = fmap KnownDataFrame . ewgenA
--   {-# INLINE ewgenA #-}
--   ewfoldl f x0 = ewfoldl f x0 . _getDF
--   {-# INLINE ewfoldl #-}
--   ewfoldr f x0 = ewfoldr f x0 . _getDF
--   {-# INLINE ewfoldr #-}
--   elementWise f = fmap KnownDataFrame . elementWise f . _getDF
--   {-# INLINE elementWise #-}
--   indexWise f = fmap KnownDataFrame . indexWise f . _getDF
--   {-# INLINE indexWise #-}
--   broadcast = KnownDataFrame . broadcast
--   {-# INLINE broadcast #-}
--   update i x = KnownDataFrame . update i x . _getDF
--   {-# INLINE update #-}


-- instance PrimBytes (DataFrame t ds) => Storable (DataFrame t ds) where
--   sizeOf x = I# (byteSize x)
--   alignment x = I# (byteAlign x)
--   peekElemOff ptr (I# offset) =
--     peekByteOff ptr (I# (offset *# byteSize (undefined :: DataFrame t ds)))
--   pokeElemOff ptr (I# offset) =
--     pokeByteOff ptr (I# (offset *# byteSize (undefined :: DataFrame t ds)))
--   peekByteOff (Ptr addr) (I# offset) = IO $ \s0 -> case newByteArray# bsize s0 of
--     (# s1, marr #) -> case copyAddrToByteArray# (addr `plusAddr#` offset)
--                                                  marr 0# bsize s1 of
--       s2 -> case unsafeFreezeByteArray# marr s2 of
--         (# s3, arr #) -> (# s3, fromBytes (# 0#, bsize `quotInt#` ebsize, arr #) #)
--     where
--       bsize = byteSize (undefined :: DataFrame t ds)
--       ebsize = elementByteSize (undefined :: DataFrame t ds)
--   pokeByteOff (Ptr addr) (I# offset) x = IO
--           $ \s0 -> case copyByteArrayToAddr# xbytes xboff
--                                              (addr `plusAddr#` offset)
--                                               bsize s0 of
--        s2 -> (# s2, () #)
--     where
--       !(# elOff, elNum, xbytes #) = toBytes x
--       bsize = elementByteSize x *# elNum
--       xboff  = elementByteSize x *# elOff
--   peek ptr = peekByteOff ptr 0
--   poke ptr = pokeByteOff ptr 0
--


--
-- instance Eq (DataFrame t (ds :: [XNat])) where
--   SomeDataFrame (a :: DataFrame t nsa) == SomeDataFrame (b :: DataFrame t nsb)
--       = case sameDim (dim @nsa) (dim @nsb) of
--           Just Evidence -> a == b
--           Nothing   -> False
--
-- instance Show (DataFrame t (ds :: [XNat])) where
--   show (SomeDataFrame arr) = show arr




--
-- instance (RepresentableList xs, All Bounded xs) => Bounded (Tuple xs) where
--     minBound = go (tList @Type @xs)
--       where
--         go :: forall (ys :: [Type])
--             . All Bounded ys => TypeList ys -> Tuple ys
--         go U         = U
--         go (_ :* xs) = minBound *! go xs
--     maxBound = go (tList @Type @xs)
--       where
--         go :: forall (ys :: [Type])
--             . All Bounded ys => TypeList ys -> Tuple ys
--         go U         = U
--         go (_ :* xs) = maxBound *! go xs
--
-- instance All Eq xs => Eq (Tuple xs) where
--     (==) U U                 = True
--     (==) (x :* tx) (y :* ty) = x == y && tx == ty
--     (/=) U U                 = False
--     (/=) (x :* tx) (y :* ty) = x /= y || tx /= ty
--
-- -- | Ord instance of the Tuple implements inverse lexicorgaphic ordering.
-- --   That is, the last element in the tuple is the most significant one.
-- --
-- --   Note, this will never work on infinite-dimensional tuples!
-- instance (All Eq xs, All Ord xs) => Ord (Tuple xs) where
--     compare U U                 = EQ
--     compare (x :* tx) (y :* ty) = compare tx ty <> compare x y
--
-- instance All Show xs => Show (Tuple xs) where
--     show U         = "U"
--     show (x :* xs) = show x ++ " :* " ++ show xs
--     showsPrec _ U = showString "U"
--     showsPrec p (x :* xs) = showParen (p >= 5)
--                           $ showsPrec 5 x
--                           . showString " :* "
--                           . showsPrec 5 xs
--
--
-- instance (RepresentableList xs, All Read xs) => Read (Tuple xs) where
--     readPrec = go (tList @Type @xs)
--       where
--         go :: forall (ys :: [Type])
--             . All Read ys => TypeList ys -> Read.ReadPrec (Tuple ys)
--         go U = U <$ Read.expectP (Read.Symbol "U")
--         go (_ :* ts) = Read.parens $ Read.prec 5 $ do
--           x <- Read.step Read.readPrec
--           Read.expectP (Read.Symbol ":*")
--           xs <- Read.step $ go ts
--           return (x :* xs)
