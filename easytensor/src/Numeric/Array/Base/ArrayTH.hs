-- {-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE RecordWildCards         #-}
-- {-# LANGUAGE UndecidableInstances  #-}
-- {-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications  #-}
-- {-# LANGUAGE BangPatterns  #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Array.Base.ArrayTH
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- A set of templates for all primitive array type
--
-----------------------------------------------------------------------------

module Numeric.Array.Base.ArrayTH
    ( -- * Supported data type definitions
      arrayFDef, arrayDDef, arrayIDef
      -- * Utility functions
    , broadcastArrayDec
    , loop1#, loop2#, loop1a#
    , zipVDec, mapVDec, accumV2Dec
      -- * Instances
    , instanceElementWiseDec
    , instanceShowDec
    , instanceEqDec
    , instanceOrdDec
    , instanceNumDec
    , instancePrimBytesDec
    ) where

import           Language.Haskell.TH

import           GHC.Base             (runRW#)
import           GHC.Prim
import           GHC.TypeLits
import           GHC.Types
import           Data.Proxy
import           Foreign.Storable (sizeOf, alignment)

import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.Dimensions


-- | Use this data type to keep all required info
--   about an array type being constructed
data ArrayTypeDef = ArrayTypeDef
  { elPrimType :: TypeQ
    -- ^ Primitive (unboxed, unlifted) type of an array element
  , elBoxedType :: TypeQ
    -- ^ Array element type (boxed)
  , arrType :: TypeQ
    -- ^ Type of current array instance
  , arrFromScalar :: Name
    -- ^ FromScalarX contstructor
  , arrConstr :: Name
    -- ^ regular array constructor
  , elemSize :: ExpQ
    -- ^ Array element size in bytes
  , elemAlign :: ExpQ
    -- ^ Array element alignment in bytes
  , indexArray :: ExpQ
    -- ^ index/**/TYPE/**/Array# functions
  , writeArray :: ExpQ
    -- ^ write/**/TYPE/**/Array# functions
  , arrName :: String
    -- ^ instance name, e.g. "ArrayF"
  , elConstr :: Name
    -- ^ Constructor of a boxed element type, e.g. `F#`
  , eqOp :: ExpQ
    -- ^ PrimOp `equal`, e.g. eqFloat#
  , neOp :: ExpQ
    -- ^ PrimOp `not equal`, e.g. neFloat#
  , gtOp :: ExpQ
    -- ^ PrimOp `greater than`, e.g. gtFloat#
  , geOp :: ExpQ
    -- ^ PrimOp `greater than or equal`, e.g. geFloat#
  , ltOp :: ExpQ
    -- ^ PrimOp `less than`, e.g. ltFloat#
  , leOp :: ExpQ
    -- ^ PrimOp `less than or equal`, e.g. leFloat#
  , plusOp :: ExpQ
    -- ^ PrimOp `(+)`, e.g. plusFloat#
  , minusOp :: ExpQ
    -- ^ PrimOp `(-)`, e.g. minusFloat#
  , timesOp :: ExpQ
    -- ^ PrimOp `(*)`, e.g. ltimesFloat#
  , negateOp :: ExpQ
    -- ^ PrimOp `negate`, e.g. negateFloat#
  , mkElemLit :: Integer -> ExpQ
    -- ^ Make primitive literat of required type
  }



arrayFDef :: ArrayTypeDef
arrayFDef = ArrayTypeDef
  { elPrimType = conT ''Float#
  , elBoxedType = conT ''Float
  , arrType = conT ''ArrayF
  , arrFromScalar = 'FromScalarF#
  , arrConstr = 'ArrayF#
  , elemSize = litE . IntPrimL . fromIntegral $ sizeOf (0 :: Float)
  , elemAlign = litE . IntPrimL . fromIntegral $ alignment (0 :: Float)
  , indexArray = varE 'indexFloatArray#
  , writeArray = varE 'writeFloatArray#
  , arrName = "ArrayF"
  , elConstr = 'F#
  , eqOp = varE 'eqFloat#
  , neOp = varE 'neFloat#
  , gtOp = varE 'gtFloat#
  , geOp = varE 'geFloat#
  , ltOp = varE 'ltFloat#
  , leOp = varE 'leFloat#
  , plusOp = varE 'plusFloat#
  , minusOp = varE 'minusFloat#
  , timesOp = varE 'timesFloat#
  , negateOp = varE 'negateFloat#
  , mkElemLit = litE . FloatPrimL . fromInteger
  }

arrayDDef :: ArrayTypeDef
arrayDDef = ArrayTypeDef
  { elPrimType = conT ''Double#
  , elBoxedType = conT ''Double
  , arrType = conT ''ArrayD
  , arrFromScalar = 'FromScalarD#
  , arrConstr = 'ArrayD#
  , elemSize = litE . IntPrimL . fromIntegral $ sizeOf (0 :: Double)
  , elemAlign = litE . IntPrimL . fromIntegral $ alignment (0 :: Double)
  , indexArray = varE 'indexDoubleArray#
  , writeArray = varE 'writeDoubleArray#
  , arrName = "ArrayD"
  , elConstr = 'D#
  , eqOp = varE '(==##)
  , neOp = varE '(/=##)
  , gtOp = varE '(>##)
  , geOp = varE '(>=##)
  , ltOp = varE '(<##)
  , leOp = varE '(<=##)
  , plusOp = varE '(+##)
  , minusOp = varE '(-##)
  , timesOp = varE '(*##)
  , negateOp = varE 'negateDouble#
  , mkElemLit = litE . DoublePrimL . fromInteger
  }

arrayIDef :: ArrayTypeDef
arrayIDef = ArrayTypeDef
  { elPrimType = conT ''Int#
  , elBoxedType = conT ''Int
  , arrType = conT ''ArrayI
  , arrFromScalar = 'FromScalarI#
  , arrConstr = 'ArrayI#
  , elemSize = litE . IntPrimL . fromIntegral $ sizeOf (0 :: Int)
  , elemAlign = litE . IntPrimL . fromIntegral $ alignment (0 :: Int)
  , indexArray = varE 'indexIntArray#
  , writeArray = varE 'writeIntArray#
  , arrName = "ArrayI"
  , elConstr = 'I#
  , eqOp = varE '(==#)
  , neOp = varE '(/=#)
  , gtOp = varE '(>#)
  , geOp = varE '(>=#)
  , ltOp = varE '(<#)
  , leOp = varE '(<=#)
  , plusOp = varE '(+#)
  , minusOp = varE '(-#)
  , timesOp = varE '(*#)
  , negateOp = varE 'negateInt#
  , mkElemLit = litE . IntPrimL
  }


--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | Do something in a loop for int i from 0 to n
loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop1# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1# #-}


-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s)
       -> State# s -> State# s
loop2# n m f = loop' 0# 0#
  where
    loop' i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop' 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop' (i +# 1#) j s1
{-# INLINE loop2# #-}

-- | Do something in a loop for int i from 0 to n
loop1a# :: Int# -> (Int# -> a -> a) -> a -> a
loop1a# n f = loop' 0#
  where
    loop' i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop' (i +# 1#) s1
{-# INLINE loop1a# #-}

broadcastArrayDec :: ArrayTypeDef -> Q [Dec]
broadcastArrayDec ArrayTypeDef {..} =
  [d| -- | Uses first argument to enforce type (can and should be undefined)
      broadcastArray :: $(elBoxedType) -> $(arrType) ds
      broadcastArray $(conP elConstr [[p|x|]]) = $(conE arrFromScalar) x
      {-# INLINE broadcastArray #-}
  |]

accumV2Dec :: ArrayTypeDef -> Q [Dec]
accumV2Dec ArrayTypeDef {..} =
  [d| -- | Accumulates only idempotent operations!
      --   Being applied to FromScalars, executes only once!
      accumV2 :: ($(elPrimType)-> $(elPrimType) -> a -> a)
              -> $(arrType) ds -> $(arrType) ds -> a -> a
      accumV2 f $(conP arrFromScalar [[p|a|]])
                $(conP arrFromScalar [[p|b|]]) = f a b
      accumV2 f $(conP arrConstr [[p|offset|],[p|n|],[p|a|]])
                $(conP arrFromScalar [[p|b|]]) = loop1a# n
          (\i -> f ($(indexArray) a (offset +# i)) b)
      accumV2 f $(conP arrFromScalar [[p|a|]])
                $(conP arrConstr [[p|offset|],[p|n|],[p|b|]]) = loop1a# n
          (\i -> f a ($(indexArray) b (offset +# i)))
      accumV2 f $(conP arrConstr [[p|offsetA|],[p|n|],[p|a|]])
                $(conP arrConstr [[p|offsetB|],[p|_|],[p|b|]]) = loop1a# n
          (\i -> f ($(indexArray) a (offsetA +# i))
                   ($(indexArray) b (offsetB +# i))
          )
  |]


mapVDec :: ArrayTypeDef -> Q [Dec]
mapVDec ArrayTypeDef {..} =
  [d| mapV :: ($(elPrimType) -> $(elPrimType)) -> $(arrType) ds -> $(arrType) ds
      mapV f $(conP arrFromScalar [[p|x|]]) = $(conE arrFromScalar) (f x)
      mapV f $(conP arrConstr [[p|offset|],[p|n|],[p|a|]]) = case runRW#
           ( \s0 -> case newByteArray# (n *# $(elemSize)) s0 of
               (# s1, marr #) -> case loop1# n
                     (\i s' -> case f ($(indexArray) a (offset +# i)) of
                       r -> $(writeArray) marr i r s'
                     ) s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> $(conE arrConstr) 0# n r
      {-# INLINE mapV #-}
  |]


zipVDec :: ArrayTypeDef -> Q [Dec]
zipVDec ArrayTypeDef {..} =
  [d| zipV :: ($(elPrimType) -> $(elPrimType) -> $(elPrimType))
           -> $(arrType) ds -> $(arrType) ds -> $(arrType) ds
      zipV f $(conP arrFromScalar [[p|a|]])
             $(conP arrFromScalar [[p|b|]]) = $(conE arrFromScalar) (f a b)
      zipV f x $(conP arrFromScalar [[p|b|]]) = mapV (`f` b) x
      zipV f $(conP arrFromScalar [[p|a|]]) y = mapV (f a) y
      zipV f $(conP arrConstr [[p|offsetA|],[p|n|],[p|a|]])
             $(conP arrConstr [[p|offsetB|],[p|_|],[p|b|]]) = case runRW#
           ( \s0 -> case newByteArray# (n *# $(elemSize) ) s0 of
               (# s1, marr #) -> case loop1# n
                     (\i s' -> case f ($(indexArray) a (offsetA +# i))
                                      ($(indexArray) b (offsetB +# i)) of
                       r -> $(writeArray) marr i r s'
                     ) s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> $(conE arrConstr) 0# n r
      {-# INLINE zipV #-}
  |]


--------------------------------------------------------------------------------
-- * Instances
--------------------------------------------------------------------------------



instanceElementWiseDec :: ArrayTypeDef -> Q [Dec]
instanceElementWiseDec ArrayTypeDef {..} =
  [d| wr :: $(arrType) (ds :: [Nat]) -> Int# -> Int#
         -> (MutableByteArray# RealWorld -> State# RealWorld -> State# RealWorld)
         -> $(arrType) ds
      wr _ bs n f' = case runRW#
           ( \s0 -> case newByteArray# bs s0 of
                     (# s1, marr #) ->  case f' marr s1 of
                       s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> $(conE arrConstr) 0# n r
      {-# INLINE wr #-}

      data ArrayUpdate# (f :: * -> *) s
        = AU# Int# !(f (MutableByteArray# s -> State# s -> State# s))

      instance Dimensions ds => ElementWise (Idx ds) $(elBoxedType) ($(arrType) (ds :: [Nat])) where
        (!) $(conP arrConstr [[p|off|],[p|_|],[p|a|]]) i
             = case fromEnum i of I# j -> $(conE elConstr) ($(indexArray) a (off +# j))
        (!) $(conP arrFromScalar [[p|x|]]) _ = $(conE elConstr) x
        {-# INLINE (!) #-}

        broadcast $(conP elConstr [[p|x|]]) = $(conE arrFromScalar) x
        {-# INLINE broadcast #-}

        ewmap f x@($(conP arrConstr [[p|offset|],[p|n|],[p|arr|]])) = case runRW#
           (\s0 -> case newByteArray# bs s0 of
             (# s1, marr #) -> case newMutVar# 0 s1 of
               (# s2, mi #) -> case loopS (dimMax `inSpaceOf` x)
                   (\ii s' -> case readMutVar# mi s' of
                     (# s'', I# i #) ->
                       case f ii ($(conE elConstr) ($(indexArray) arr (offset +# i))) of
                        $(conP elConstr [[p|r|]]) -> writeMutVar# mi (I# (i +# 1#))
                                                  ($(writeArray) marr i r s'')
                   ) s2 of
                 s3 -> unsafeFreezeByteArray# marr s3
           ) of (# _, r #) -> $(conE arrConstr) 0# n r
          where
            bs = n *# $(elemSize)
        ewmap f x@($(conP arrFromScalar [[p|scalVal|]])) = case runRW#
           (\s0 -> case newByteArray# bs s0 of
             (# s1, marr #) -> case newMutVar# 0 s1 of
               (# s2, mi #) -> case loopS (dimMax `inSpaceOf` x)
                   (\ii s' -> case readMutVar# mi s' of
                     (# s'', I# i #) ->
                       case f ii ($(conE elConstr) scalVal) of
                        $(conP elConstr [[p|r|]]) -> writeMutVar# mi (I# (i +# 1#))
                                                     ($(writeArray) marr i r s'')
                   ) s2 of
                 s3 -> unsafeFreezeByteArray# marr s3
           ) of (# _, r #) -> $(conE arrConstr) 0# n r
          where
            n = case totalDim x of I# d -> d
            bs = n *# $(elemSize)
        {-# INLINE ewmap #-}

        ewgen f = case runRW#
           (\s0 -> case newByteArray# bs s0 of
             (# s1, marr #) -> case newMutVar# 0 s1 of
               (# s2, mi #) -> case loopS (dimMax `inSpaceOf` x)
                   (\ii s' -> case readMutVar# mi s' of
                     (# s'', I# i #) -> case f ii of
                        $(conP elConstr [[p|r|]]) -> writeMutVar# mi (I# (i +# 1#))
                                                     ($(writeArray) marr i r s'')
                   ) s2 of
                 s3 -> unsafeFreezeByteArray# marr s3
           ) of (# _, r #) -> $(conE arrConstr) 0# n r
          where
            x = undefined :: $(arrType) ds
            n = case totalDim x of I# d -> d
            bs = n *# $(elemSize)
        {-# INLINE ewgen #-}

        ewfold f v0 x@($(conP arrConstr [[p|offset|],[p|_|],[p|arr|]])) = case runRW#
          (\s0 -> case newMutVar# (0,v0) s0 of
             (# s1, miv #) -> case loopS (dimMax `inSpaceOf` x)
                   (\ii s' -> case readMutVar# miv s' of
                     (# s'', (I# i, v) #) -> writeMutVar# miv
                            ( I# (i +# 1#)
                            , f ii ($(conE elConstr) ($(indexArray) arr (offset +# i))) v
                            ) s''
                   ) s1 of
                s2 -> readMutVar# miv s2
          ) of (# _, (_, r) #) -> r
        ewfold f v0 x@($(conP arrFromScalar [[p|scalVal|]])) = case runRW#
          (\s0 -> case newMutVar# v0 s0 of
              (# s1, miv #) -> case loopS (dimMax `inSpaceOf` x)
                    (\ii s' -> case readMutVar# miv s' of
                      (# s'', v #) -> writeMutVar# miv
                             ( f ii ($(conE elConstr) scalVal) v
                             ) s''
                    ) s1 of
                 s2 -> readMutVar# miv s2
          ) of (# _, r #) -> r
        {-# INLINE ewfold #-}

        indexWise f x@($(conP arrConstr [[p|offset|],[p|n|],[p|arr|]]))
            = case loopA (dimMax `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
              AU# _ f' -> wr x bs n <$> f'
          where
            g ds (AU# i f') = AU# ( i +# 1# )
                                $ (\($(conP elConstr [[p|z|]])) u a s -> $(writeArray) a i z (u a s))
                                 <$> f ds ($(conE elConstr) ($(indexArray) arr (offset +# i))) <*> f'
            bs = n *# $(elemSize)

        indexWise f x@($(conP arrFromScalar [[p|scalVal|]]))
            = case loopA (dimMax `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
              AU# _ f' -> wr x bs n <$> f'
          where
            n = case totalDim x of I# d -> d
            g ds (AU# i f') = AU# ( i +# 1# )
                                $ (\($(conP elConstr [[p|z|]])) u a s -> $(writeArray) a i z (u a s))
                                 <$> f ds ($(conE elConstr) scalVal) <*> f'
            bs = n *# $(elemSize)


        elementWise f x@($(conP arrConstr [[p|offset|],[p|n|],[p|arr|]])) =
            wr x bs n <$> loop1a# n g (pure (\_ s -> s))
          where
            g i f' = (\($(conP elConstr [[p|z|]])) u a s -> $(writeArray) a i z (u a s))
                            <$> f ($(conE elConstr) ($(indexArray) arr (offset +# i))) <*> f'
            bs = n *# $(elemSize)
        elementWise f x@($(conP arrFromScalar [[p|scalVal|]])) =
            wr x bs n <$> loop1a# n g (pure (\_ s -> s))
          where
            fa = f ($(conE elConstr) scalVal)
            n = case totalDim x of I# d -> d
            g i f' = (\($(conP elConstr [[p|z|]])) u a s -> $(writeArray) a i z (u a s))
                            <$> fa <*> f'
            bs = n *# $(elemSize)
  |]






instanceShowDec :: ArrayTypeDef -> Q [Dec]
instanceShowDec ArrayTypeDef {..} =
  [d| instance Dimensions ds
            => Show ($(arrType) ds) where
        show x
            | D <- dim @ds = "{ " ++ show (x ! Z) ++ " }"
            | _ :* D <- dim @ds = ('{' :) . drop 1 $
                            foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
                                    [minBound .. maxBound]
            | (_ :: Proxy n) :* (_ :: Proxy m) :* (_ :: Dim ds') <- dim @ds
            , DimensionsEvidence <- inferDropNDimensions (Proxy @2) x
              = let loopInner :: Idx ds' -> Idx '[n,m] -> String
                    loopInner ods (n:!m:!_) = ('{' :) . drop 2 $
                                    foldr (\i ss -> '\n':
                                            foldr (\j s ->
                                                     ", " ++ show (x ! (i :! j :! ods)) ++ s
                                                  ) ss [1..m]
                                          ) " }" [1..n]
                    loopOuter :: Idx ds' -> String -> String
                    loopOuter Z s  = "\n" ++ loopInner Z maxBound ++ s
                    loopOuter ds s = "\n(i j" ++ drop 3 (show ds) ++ "):\n"
                                          ++ loopInner ds maxBound ++ s
                in drop 1 $ foldr loopOuter "" [minBound..maxBound]
            | otherwise
              = error $(errorMsg)
  |]
  where
    errorMsg = litE $ StringL $ "Show " ++ arrName ++ " -- impossible pattern match on Dim!"


instanceEqDec :: ArrayTypeDef -> Q [Dec]
instanceEqDec ArrayTypeDef {..} =
  [d| instance Eq ($(arrType) ds) where
        a == b = accumV2 (\x y r -> r && isTrue# ($(eqOp) x y)) a b True
        {-# INLINE (==) #-}
        a /= b = accumV2 (\x y r -> r || isTrue# ($(neOp) x y)) a b False
        {-# INLINE (/=) #-}
  |]

instanceOrdDec :: ArrayTypeDef -> Q [Dec]
instanceOrdDec ArrayTypeDef {..} =
  [d| -- | Implement partial ordering for `>`, `<`, `>=`, `<=`
      --     and lexicographical ordering for `compare`
      instance Ord ($(arrType) ds) where
        a > b = accumV2 (\x y r -> r && isTrue# ($(gtOp) x y)) a b True
        {-# INLINE (>) #-}
        a < b = accumV2 (\x y r -> r && isTrue# ($(ltOp) x y)) a b True
        {-# INLINE (<) #-}
        a >= b = accumV2 (\x y r -> r && isTrue# ($(geOp) x y)) a b True
        {-# INLINE (>=) #-}
        a <= b = accumV2 (\x y r -> r && isTrue# ($(leOp) x y)) a b True
        {-# INLINE (<=) #-}
        -- | Compare lexicographically
        compare a b = accumV2 (\x y r -> r `mappend`
                                if isTrue# ($(gtOp) x y)
                                then GT
                                else if isTrue# ($(ltOp) x y)
                                     then LT
                                     else EQ
                              ) a b EQ
        {-# INLINE compare #-}
        -- | Element-wise minimum
        min = zipV  (\x y -> if isTrue# ($(gtOp) x y) then y else x)
        {-# INLINE min #-}
        -- | Element-wise maximum
        max = zipV  (\x y -> if isTrue# ($(gtOp) x y) then x else y)
        {-# INLINE max #-}
  |]

instanceNumDec :: ArrayTypeDef -> Q [Dec]
instanceNumDec ArrayTypeDef {..} =
  [d| instance Num ($(arrType) ds) where
        (+) = zipV $(plusOp)
        {-# INLINE (+) #-}
        (-) = zipV $(minusOp)
        {-# INLINE (-) #-}
        (*) = zipV $(timesOp)
        {-# INLINE (*) #-}
        negate = mapV $(negateOp)
        {-# INLINE negate #-}
        abs = mapV (\x -> if isTrue# ($(geOp) x $(mkElemLit 0)) then x else $(negateOp) x)
        {-# INLINE abs #-}
        signum = mapV (\x -> if isTrue# ($(gtOp) x $(mkElemLit 0))
                            then $(mkElemLit 1)
                            else if isTrue# ($(ltOp) x $(mkElemLit 0))
                                 then $(mkElemLit (-1))
                                 else $(mkElemLit 0)
                      )
        {-# INLINE signum #-}
        fromInteger = broadcastArray . fromInteger
        {-# INLINE fromInteger #-}
  |]


instancePrimBytesDec :: ArrayTypeDef -> Q [Dec]
instancePrimBytesDec ArrayTypeDef {..} =
  [d| instance Dimensions ds => PrimBytes ($(arrType) ds) where
        type ElemPrim ($(arrType) ds) = $(elPrimType)
        toBytes $(conP arrConstr [[p|off|],[p|size|],[p|a|]]) = (# off, size, a #)
        toBytes $(conP arrFromScalar [[p|x|]]) = case runRW#
           ( \s0 -> case newByteArray# bs s0 of
               (# s1, marr #) -> case loop1# n
                     (\i s' -> $(writeArray) marr i x s'
                     ) s1 of
                   s2 -> unsafeFreezeByteArray# marr s2
           ) of (# _, r #) -> (# 0#, n, r #)
          where
            n = case totalDim (undefined :: ArrayF ds) of I# d -> d
            bs = n *# $(elemSize)
        {-# INLINE toBytes #-}
        fromBytes (# off, size, a #) = $(conE arrConstr) off size a
        {-# INLINE fromBytes #-}
        byteSize x = case totalDim x of
           I# d -> $(elemSize) *# d
        {-# INLINE byteSize #-}
        byteAlign _ = $(elemAlign)
        {-# INLINE byteAlign #-}
        elementByteSize _ = $(elemSize)
        {-# INLINE elementByteSize #-}
        ix i $(conP arrConstr [[p|off|],[p|_|],[p|a|]]) = $(indexArray) a (off +# i)
        ix _ $(conP arrFromScalar [[p|x|]])  = x
        {-# INLINE ix #-}
  |]
