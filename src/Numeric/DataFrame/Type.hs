{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Type
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Type
  ( DataFrame (..), withShape, unboundShape
  , ElementDataType (..), EDTRefl (..)
  ) where

import           Data.Proxy
import           Data.Type.Equality
import           Data.Type.Equality
import           GHC.Base             (runRW#)
import           GHC.Exts             (IsList (..))
import           GHC.Prim
import           GHC.TypeLits         (KnownNat, Nat, SomeNat (..), natVal,
                                       someNatVal)
import           GHC.Types
import           Numeric.Array
import qualified Numeric.Array.Family as AFam (Scalar (..))
import qualified Numeric.Commons      as NCommons
import           Numeric.Dimensions
import qualified Numeric.Matrix.Class as M
import           Unsafe.Coerce

-- | Keep data in a primitive data frame
--    and maintain information about Dimensions in the type-system
data family DataFrame (t :: Type) (xs :: [k])

-- | Completely fixed at compile time
newtype instance Dimensions ns => DataFrame t (ns :: [Nat])
  = KnownDataFrame { _getDF :: Array t ns }

-- | Partially known at compile time
data instance DataFrame t (xns :: [XNat])
  = forall (ns :: [Nat])
  . ( Dimensions ns
    , FixedDim xns ns ~ ns
    , FixedXDim xns ns ~ xns
    , Show (Array t ns)
    , Eq (Array t ns)
    , NCommons.PrimBytes (Array t ns)
    )
  => SomeDataFrame (Dim xns) (Array t ns)


-- fromFoldableN :: forall (t :: Type) (f :: Type -> Type)
--                        (as :: [k]) (xas :: [XNat])
--               . ( xas ~ WrapDims as
--                 , Foldable f
--                 )
--              => Int
--              -> f (DataFrame t as)
--              -> DataFrame t (xas +: XN)
-- fromFoldableN n xs = case runRW#
--        ( \s0 -> case newByteArray# (len# *# elS) s0 of
--            (# s1, marr #) -> case go 0# len# tobytesX marr s1 of
--                s2 -> unsafeFreezeByteArray# marr s2
--        ) of (# _, r #) -> NCommons.fromBytes (# 0#, len#, r #)
--   where
--     elS = NCommons.elementByteSize (undefined :: DataFrame t as)
--     go pos lim tobytesX@(# offX, step, arrX #) marr s
--       | isTrue# (pos >=# lim) = s
--       | otherwise = go (pos +# step) lim tobytesX marr
--            (copyByteArray# arrX (offX *# elS) marr (pos *# elS) (step *# elS) s)


-- instance IsList (DataFrame Float ('[XN] :: [XNat])) where
--   type Item (DataFrame Float '[XN]) = DataFrame Float ('[] :: [Nat])
--   fromList xs = fromListN (length xs) xs
--   fromListN _ [] = error "DataFrame fromList: the list must have at least two elements"
--   fromListN _ [_] = error "DataFrame fromList: the list must have at least two elements"
--   fromListN n@(I# n#) xs  | Just d1@(SomeNat (pn :: Proxy n)) <- someNatVal (fromIntegral n)
--                   = SomeDataFrame (d1 :? D) (df pn)
--     where
--       elSize# = NCommons.byteSize (0 :: Float)
--       df :: KnownNat n => Proxy n -> Array Float '[n]
--       df _ = case runRW#
--         ( \s0 -> let !(# s1, marr #) = newByteArray# (n# *# elSize#) s0
--                      go s _ [] = s
--                      go s pos (KnownDataFrame (Array (AFam.Scalar (F# a))) : as)
--                                  = go (writeFloatArray# marr pos a s) (pos +# 1#) as
--                      s2 = go s1 0# xs
--                  in unsafeFreezeByteArray# marr s2
--         ) of (# _, r #) -> NCommons.fromBytes (# 0#, n#, r #)
--   fromListN n _ = error $ "DataFrame fromList: not a proper list length: " ++ show n
--   toList (SomeDataFrame _ (df :: Array Float ns))
--     = NCommons.ewfold @(Idx '[Head ns])
--       (\_ x a -> KnownDataFrame (Array $ AFam.Scalar x) : a) [] df

-- instance ( xnsm ~ (x ': xns')
--          , xns ~ Init xnsm
--          , Last xnsm ~ XN
--          , ns ~ UnwrapDims xns
--          , NCommons.PrimBytes (Array t ns)
--          , Dimensions ns
--          , Show t
--          , Eq t
--          , NCommons.PrimBytes t
--          )
--       => IsList (DataFrame t ((x ': xns') :: [XNat])) where
--   type Item (DataFrame t (x ': xns')) = DataFrame t (UnwrapDims (Init (x ': xns')))
--   fromList xs = fromListN (length xs) xs
--   fromListN _ []  = error "DataFrame fromList: the list must have at least two elements"
--   fromListN _ [_] = error "DataFrame fromList: the list must have at least two elements"
--   fromListN n@(I# n#) xs  | Just dLast@(SomeNat (pm :: Proxy m)) <- someNatVal (fromIntegral n)
--                           , (pnsm :: Proxy nsm, Refl, Refl, Refl) <- makeEvs pm
--                           , I# len# <- totalDim (Proxy @ns)
--                           , xd <- xdim @nsm @xnsm Proxy
--                   = unsafeCoerce ( SomeDataFrame
--                                    (unsafeCoerce xd)
--                                    (unsafeCoerce (df pnsm len# :: Array t nsm)
--                                  ) :: DataFrame t '[])
--     where
--       elSize# = NCommons.byteSize (head xs)
--       df :: NCommons.PrimBytes (Array t nsm) => Proxy nsm -> Int# -> Array t nsm
--       df _ len# = case runRW#
--         ( \s0 -> let !(# s1, marr #) = newByteArray# (n# *# elSize# *# len#) s0
--                      go s _ [] = s
--                      go s pos (KnownDataFrame earr : as) = case NCommons.toBytes earr of
--                        (# eoff#, _, ea #) -> go
--                          (copyByteArray# ea (eoff# *# elSize#) marr (pos *# elSize#) (elSize# *# len#) s)
--                          (pos +# len#)
--                          as
--                      s2 = go s1 0# xs
--                  in unsafeFreezeByteArray# marr s2
--         ) of (# _, r #) -> NCommons.fromBytes (# 0#, n# *# len#, r #)
--       makeEvs :: Proxy m
--               -> ( Proxy nsm
--                  , nsm :~: (ns +: m)
--                  , FixedDim  xnsm (ns +: m) :~: (ns +: m)
--                  , FixedXDim xnsm (ns +: m) :~: xnsm
--                  )
--       makeEvs _ = (Proxy, unsafeCoerce Refl, unsafeCoerce Refl, unsafeCoerce Refl)
--   fromListN n _ = error $ "DataFrame fromList: not a proper list length: " ++ show n
--   toList (SomeDataFrame _ df) = go offset
--     where
--       !(I# step) = totalDim (Proxy @ns)
--       !(# offset, lenN, arr #) = NCommons.toBytes df
--       lim = offset +# lenN
--       go pos | isTrue# (pos >=# lim)  = []
--              | otherwise = NCommons.fromBytes (# pos, step , arr #) : go (pos +# step)


  -- toList (SomeDataFrame (SomeNat (pn :: Proxy n) :? D) (df :: Array Float ns))
  --   | Refl <- (unsafeCoerce Refl :: n :~: Head ns) = NCommons.ewfold @(Idx '[n])
  --     (\_ x a -> KnownDataFrame (Array $ AFam.Scalar x) : a) [] df
  -- toList (SomeDataFrame (SomeNat (pn :: Proxy n) :? D) (df :: Array Float ns))
  --   | ConcatEvidence dimAsbs <- concatEvidence df (Proxy @'[n])
  --   , Refl <- (unsafeCoerce Refl :: n :~: Head ns) = NCommons.ewfold @(Idx '[n])
  --     (\_ x a -> KnownDataFrame (Array $ AFam.Scalar x) : a) [] df

instance ( xnsm ~ (N n ': xns')
         , xns ~ Init xnsm
         , Last xnsm ~ XN
         , ns ~ UnwrapDims xns
         , NCommons.PrimBytes (Array Float ns)
         , Dimensions ns
         , XDimensions xns
         )
      => IsList (DataFrame Float ((N n ': xns') :: [XNat])) where
  type Item (DataFrame Float (N n ': xns')) = DataFrame Float (UnwrapDims (Init (N n ': xns')))
  fromList xs = fromListN (length xs) xs
  fromListN _ []  = error "DataFrame fromList: the list must have at least two elements"
  fromListN _ [_] = error "DataFrame fromList: the list must have at least two elements"
  fromListN n@(I# n#) xs  | Just dLast@(SomeNat (pm :: Proxy m)) <- someNatVal (fromIntegral n)
                          , (pnsm :: Proxy nsm, SnocEvidence dnsm, Refl, Refl, Refl) <- makeEvs pm
                          , I# len# <- totalDim (Proxy @ns)
                          , xd <- xdim @nsm @xnsm Proxy
                  = SomeDataFrame xd (df pnsm len#)
    where
      elSize# = NCommons.byteSize (head xs)
      df :: NCommons.PrimBytes (Array Float nsm) => Proxy nsm -> Int# -> Array Float nsm
      df _ len# = case runRW#
        ( \s0 -> let !(# s1, marr #) = newByteArray# (n# *# elSize# *# len#) s0
                     go s _ [] = s
                     go s pos (KnownDataFrame earr : as) = case NCommons.toBytes earr of
                       (# eoff#, _, ea #) -> go
                         (copyByteArray# ea (eoff# *# elSize#) marr (pos *# elSize#) (elSize# *# len#) s)
                         (pos +# len#)
                         as
                     s2 = go s1 0# xs
                 in unsafeFreezeByteArray# marr s2
        ) of (# _, r #) -> NCommons.fromBytes (# 0#, n# *# len#, r #)
      makeEvs :: KnownNat m
              => Proxy m
              -> ( Proxy nsm
                 , SnocEvidence ns m
                 , nsm :~: (ns +: m)
                 , FixedDim  xnsm (ns +: m) :~: (ns +: m)
                 , FixedXDim xnsm (ns +: m) :~: xnsm
                 )
      makeEvs p = (Proxy, snocEvidence (Proxy @ns) p, unsafeCoerce Refl, unsafeCoerce Refl, unsafeCoerce Refl)
  fromListN n _ = error $ "DataFrame fromList: not a proper list length: " ++ show n
  toList (SomeDataFrame _ df) = go offset
    where
      !(I# step) = totalDim (Proxy @ns)
      !(# offset, lenN, arr #) = NCommons.toBytes df
      lim = offset +# lenN
      go pos | isTrue# (pos >=# lim)  = []
             | otherwise = NCommons.fromBytes (# pos, step , arr #) : go (pos +# step)


-- | This class is used to pattern match against available data types
--   represented by EDTRefl
class ElementDataType t where
  -- | Get corresponding singleton constructor for a given element data type
  edtRefl :: proxy t -> EDTRefl t

-- | Represent available element data types
data EDTRefl :: (Type -> Type) where
  EDTFloat :: EDTRefl Float

instance ElementDataType Float where
  edtRefl _ = EDTFloat




-- | Do something with
withShape :: DataFrame t xns
          -> (forall ns . ( Dimensions ns
                          , FixedDim xns ns ~ ns
                          , FixedXDim xns ns ~ xns
                          ) => DataFrame t ns -> b)
          -> b
withShape (SomeDataFrame _ a) f = f (KnownDataFrame a)

-- | Put some of Dimensions into existential data type
unboundShape :: ( FixedXDim xns ns ~ xns
                , FixedDim xns ns ~ ns
                , XDimensions xns
                , Dimensions ns
                , NCommons.PrimBytes (Array t ns)
                , Show (Array t ns)
                , Eq (Array t ns)
                ) => DataFrame t ns -> DataFrame t xns
unboundShape (KnownDataFrame a)
    = SomeDataFrame (xdim $ dim `inSpaceOf` a) a






instance ( Show (Array t ds)
         , Dimensions ds
         ) => Show (DataFrame t ds) where
  show (KnownDataFrame arr) = unlines
                            [ "DF [" ++ drop 4 (show $ dim `inSpaceOf` arr) ++ "]:"
                            , show arr
                            ]

instance Show (Dim ds)
      => Show (DataFrame t (ds :: [XNat])) where
  show (SomeDataFrame d arr) = unlines
                            [ "DF [" ++ drop 4 (show d) ++ "]:"
                            , show arr
                            ]


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
deriving instance NCommons.PrimBytes (Array t ds)
               => NCommons.PrimBytes (DataFrame t ds)
deriving instance NCommons.FloatBytes (Array t ds)
               => NCommons.FloatBytes (DataFrame t ds)
deriving instance NCommons.DoubleBytes (Array t ds)
               => NCommons.DoubleBytes (DataFrame t ds)
deriving instance NCommons.IntBytes (Array t ds)
               => NCommons.IntBytes (DataFrame t ds)
deriving instance NCommons.WordBytes (Array t ds)
               => NCommons.WordBytes (DataFrame t ds)
instance ( Dimensions ds
         , NCommons.ElementWise (Idx ds) t (Array Float ds)
         ) => NCommons.ElementWise (Idx ds) t (DataFrame Float ds) where
  (!) = (NCommons.!) . _getDF
  {-# INLINE (!) #-}
  ewmap f = KnownDataFrame . NCommons.ewmap f . _getDF
  {-# INLINE ewmap #-}
  ewgen = KnownDataFrame . NCommons.ewgen
  {-# INLINE ewgen #-}
  ewfold f x0 = NCommons.ewfold f x0 . _getDF
  {-# INLINE ewfold #-}
  elementWise f = fmap KnownDataFrame . NCommons.elementWise f . _getDF
  {-# INLINE elementWise #-}
  indexWise f = fmap KnownDataFrame . NCommons.indexWise f . _getDF
  {-# INLINE indexWise #-}
  broadcast = KnownDataFrame . NCommons.broadcast
  {-# INLINE NCommons.broadcast #-}



instance Eq (DataFrame t (ds :: [XNat])) where
  SomeDataFrame d1 a1 == SomeDataFrame d2 a2
      = case check d1 d2 a1 a2 of
          Just Refl -> a1 == a2
          Nothing   -> False
    where
      check :: Dim ds -> Dim ds
            -> p ns1 -> q ns2
            -> Maybe (ns1 :~: ns2)
      check a b _ _ | a == b  = Just (unsafeCoerce Refl)
                    | otherwise = Nothing



instance ( ConcatList as bs asbs
         , Dimensions asm
         , Dimensions (m ': bs)
         , asm ~ (as +: m)
         , Dimensions asbs
         , M.MatrixProduct (Array t (as +: m)) (Array t (m :+ bs)) (Array t asbs)
         )
       => M.MatrixProduct (DataFrame t asm)
                          (DataFrame t (m ': bs))
                          (DataFrame t asbs) where
  prod x y = KnownDataFrame $ M.prod (_getDF x) (_getDF y)





_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
