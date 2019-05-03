{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Quaternion.Internal.QDouble
    ( QDouble, Quater (..)
    ) where

import qualified Control.Monad.ST                     as ST
import           Data.Coerce                          (coerce)
import           GHC.Exts
import           Numeric.DataFrame.Internal.PrimArray
import qualified Numeric.DataFrame.ST                 as ST
import           Numeric.DataFrame.Type
import           Numeric.PrimBytes                    (PrimBytes)
import           Numeric.Quaternion.Internal
import           Numeric.Scalar.Internal
import           Numeric.Vector.Internal
import           Text.Read

type QDouble = Quater Double

deriving instance PrimBytes (Quater Double)
deriving instance PrimArray Double (Quater Double)

instance Quaternion Double where
    newtype Quater Double = QDouble Vec4d
    {-# INLINE packQ #-}
    packQ = coerce (vec4 :: Double -> Double -> Double -> Double -> Vec4d)
    {-# INLINE unpackQ# #-}
    unpackQ# = coerce (unpackV4# :: Vec4d -> (# Double, Double, Double, Double #))
    {-# INLINE fromVecNum #-}
    fromVecNum (unpackV3# -> (# x, y, z #))  = packQ x y z
    {-# INLINE fromVec4 #-}
    fromVec4 = coerce
    {-# INLINE toVec4 #-}
    toVec4 = coerce
    {-# INLINE square #-}
    square (unpackQ# -> (# x, y, z, w #)) = (x * x) + (y * y) + (z * z) + (w * w)
    {-# INLINE im #-}
    im (unpackQ# -> (# x, y, z, _ #)) = packQ x y z 0.0
    {-# INLINE re #-}
    re (unpackQ# -> (# _, _, _, w #)) = packQ 0.0 0.0 0.0 w
    {-# INLINE imVec #-}
    imVec (unpackQ# -> (# x, y, z, _ #)) = vec3 x y z
    {-# INLINE taker #-}
    taker (unpackQ# -> (# _, _, _, w #)) = w
    {-# INLINE takei #-}
    takei (unpackQ# -> (# x, _, _, _ #)) = x
    {-# INLINE takej #-}
    takej (unpackQ# -> (# _, y, _, _ #)) = y
    {-# INLINE takek #-}
    takek (unpackQ# -> (# _, _, z, _ #)) = z
    {-# INLINE conjugate #-}
    conjugate (unpackQ# -> (# x, y, z, w #))
      = packQ (negate x) (negate y) (negate z) w
    {-# INLINE rotScale #-}
    rotScale (unpackQ# -> (# i, j, k, t #))
             (unpackV3# -> (# x, y, z #))
      = let l = t*t - i*i - j*j - k*k
            d = 2.0 * ( i*x + j*y + k*z)
            t2 = t * 2.0
        in vec3 (l*x + d*i + t2 * (z*j - y*k))
                (l*y + d*j + t2 * (x*k - z*i))
                (l*z + d*k + t2 * (y*i - x*j))
    {-# INLINE getRotScale #-}
    getRotScale a b = case (# unpackV3# a, unpackV3# b #) of
      (# _, (# 0, 0, 0 #) #) -> packQ 0.0 0.0 0.0 0.0
      (# (# 0, 0, 0 #), _ #) -> let x = (1 / 0 :: Double) in packQ x x x x
      (# (# a1, a2, a3 #), (# b1, b2, b3 #) #) ->
        let ma = sqrt (a1*a1 + a2*a2 + a3*a3)
            mb = sqrt (b1*b1 + b2*b2 + b3*b3)
            d  = a1*b1 + a2*b2 + a3*b3
            c  = sqrt (ma*mb + d)
            ma2 = ma * sqrt 2.0
            r  = 1.0 / (ma2 * c)
        in case unpackV3# (cross a b) of
          (# 0, 0, 0 #)
            | d > 0        -> packQ 0.0 0.0 0.0 (sqrt (mb / ma))
                              -- Shall we move result from k to i component?
            | otherwise    -> packQ 0.0 0.0 (sqrt (mb / ma)) 0.0
          (# t1, t2, t3 #) -> packQ (t1 * r) (t2 * r) (t3 * r) (c / ma2)
    {-# INLINE axisRotation #-}
    axisRotation v a = case unpackV3# v of
      (# 0, 0, 0 #) -> packQ 0.0 0.0 0.0 1.0
      (# x, y, z #) ->
        let c = cos (a * 0.5)
            s = sin (a * 0.5)
                / sqrt (x*x + y*y + z*z)
        in packQ (x * s) (y * s) (z * s) c
    {-# INLINE qArg #-}
    qArg (unpackQ# -> (# x, y, z, w #)) = 2 * atan2 (sqrt (x*x + y*y + z*z)) w
    {-# INLINE fromMatrix33 #-}
    fromMatrix33 m
      = let d = ( ix 0# m * ( ix 4# m * ix 8# m - ix 5# m * ix 7# m )
                - ix 1# m * ( ix 3# m * ix 8# m - ix 5# m * ix 6# m )
                + ix 2# m * ( ix 3# m * ix 7# m - ix 4# m * ix 6# m )
                ) ** 0.33333333333333333333333333333333
        in packQ
            (sqrt (max 0.0 (d + ix 0# m - ix 4# m - ix 8# m )) * signum (ix 5# m - ix 7# m) * 0.5)
            (sqrt (max 0.0 (d - ix 0# m + ix 4# m - ix 8# m )) * signum (ix 6# m - ix 2# m) * 0.5)
            (sqrt (max 0.0 (d - ix 0# m - ix 4# m + ix 8# m )) * signum (ix 1# m - ix 3# m) * 0.5)
            (sqrt (max 0.0 (d + ix 0# m + ix 4# m + ix 8# m )) * 0.5)

    {-# INLINE fromMatrix44 #-}
    fromMatrix44 m
      = let d = ( ix 0# m * ( ix 5# m * ix 10# m - ix 6# m * ix 9# m )
                - ix 1# m * ( ix 4# m * ix 10# m - ix 6# m * ix 8# m )
                + ix 2# m * ( ix 4# m * ix  9# m - ix 5# m * ix 8# m )
                ) ** 0.33333333333333333333333333333333
            c = 0.5 / ix 15# m
        in packQ
            (sqrt (max 0.0 (d + ix 0# m - ix 5# m - ix 10# m )) * signum (ix 6# m - ix 9# m) * c)
            (sqrt (max 0.0 (d - ix 0# m + ix 5# m - ix 10# m )) * signum (ix 8# m - ix 2# m) * c)
            (sqrt (max 0.0 (d - ix 0# m - ix 5# m + ix 10# m )) * signum (ix 1# m - ix 4# m) * c)
            (sqrt (max 0.0 (d + ix 0# m + ix 5# m + ix 10# m )) * c)

    {-# INLINE toMatrix33 #-}
    toMatrix33 (unpackQ# -> (# 0.0, 0.0, 0.0, w #))
      = let x = w * w
            f 0 = (# 3 :: Int , x #)
            f k = (# k-1, 0 #)
        in case gen# (CumulDims [9,3,1]) f 0 of
            (# _, m #) -> m -- diag (scalar (w * w))
    toMatrix33 (unpackQ# -> (# x', y', z', w' #)) =
      let x = scalar x'
          y = scalar y'
          z = scalar z'
          w = scalar w'
          x2 = x * x
          y2 = y * y
          z2 = z * z
          w2 = w * w
          l2 = x2 + y2 + z2 + w2
      in ST.runST $ do
        df <- ST.newDataFrame
        ST.writeDataFrameOff df 0 $ l2 - 2*(z2 + y2)
        ST.writeDataFrameOff df 1 $ 2*(x*y + z*w)
        ST.writeDataFrameOff df 2 $ 2*(x*z - y*w)
        ST.writeDataFrameOff df 3 $ 2*(x*y - z*w)
        ST.writeDataFrameOff df 4 $ l2 - 2*(z2 + x2)
        ST.writeDataFrameOff df 5 $ 2*(y*z + x*w)
        ST.writeDataFrameOff df 6 $ 2*(x*z + y*w)
        ST.writeDataFrameOff df 7 $ 2*(y*z - x*w)
        ST.writeDataFrameOff df 8 $ l2 - 2*(y2 + x2)
        ST.unsafeFreezeDataFrame df
    {-# INLINE toMatrix44 #-}
    toMatrix44 (unpackQ# -> (# 0.0, 0.0, 0.0, w #)) = ST.runST $ do
      df <- ST.newDataFrame
      mapM_ (flip (ST.writeDataFrameOff df) 0) [0..15]
      let w2 = scalar (w * w)
      ST.writeDataFrameOff df 0 w2
      ST.writeDataFrameOff df 5 w2
      ST.writeDataFrameOff df 10 w2
      ST.writeDataFrameOff df 15 1
      ST.unsafeFreezeDataFrame df
    toMatrix44 (unpackQ# -> (# x', y', z', w' #)) =
      let x = scalar x'
          y = scalar y'
          z = scalar z'
          w = scalar w'
          x2 = x * x
          y2 = y * y
          z2 = z * z
          w2 = w * w
          l2 = x2 + y2 + z2 + w2
      in ST.runST $ do
        df <- ST.newDataFrame
        ST.writeDataFrameOff df 0 $ l2 - 2*(z2 + y2)
        ST.writeDataFrameOff df 1 $ 2*(x*y + z*w)
        ST.writeDataFrameOff df 2 $ 2*(x*z - y*w)
        ST.writeDataFrameOff df 3 0
        ST.writeDataFrameOff df 4 $ 2*(x*y - z*w)
        ST.writeDataFrameOff df 5 $ l2 - 2*(z2 + x2)
        ST.writeDataFrameOff df 6 $ 2*(y*z + x*w)
        ST.writeDataFrameOff df 7 0
        ST.writeDataFrameOff df 8 $ 2*(x*z + y*w)
        ST.writeDataFrameOff df 9 $ 2*(y*z - x*w)
        ST.writeDataFrameOff df 10 $ l2 - 2*(y2 + x2)
        ST.writeDataFrameOff df 11 0
        ST.writeDataFrameOff df 12 0
        ST.writeDataFrameOff df 13 0
        ST.writeDataFrameOff df 14 0
        ST.writeDataFrameOff df 15 0
        ST.unsafeFreezeDataFrame df


ix :: PrimArray Double a => Int# -> a -> Double
ix = ix#
{-# INLINE ix #-}


instance Num QDouble where
    QDouble a + QDouble b
      = QDouble (a + b)
    {-# INLINE (+) #-}
    QDouble a - QDouble b
      = QDouble (a - b)
    {-# INLINE (-) #-}
    (unpackQ# -> (# a1, a2, a3, a4 #)) * (unpackQ# -> (# b1, b2, b3, b4 #))
      = packQ
           ((a4 * b1) +
            (a1 * b4) +
            (a2 * b3) -
            (a3 * b2)
            )
           ((a4 * b2) -
            (a1 * b3) +
            (a2 * b4) +
            (a3 * b1)
            )
           ((a4 * b3) +
            (a1 * b2) -
            (a2 * b1) +
            (a3 * b4)
            )
           ((a4 * b4) -
            (a1 * b1) -
            (a2 * b2) -
            (a3 * b3)
            )
    {-# INLINE (*) #-}
    negate (QDouble a) = QDouble (negate a)
    {-# INLINE negate #-}
    abs = packQ 0.0 0.0 0.0 . sqrt . square
    {-# INLINE abs #-}
    signum q@(unpackQ# -> (# x, y, z, w #))
      = case square q of
          0.0 -> packQ 0.0 0.0 0.0 0.0
          qd -> case recip (sqrt qd) of
             s -> packQ (x * s) (y * s) (z * s) (w * s)
    {-# INLINE signum #-}
    fromInteger = packQ 0.0 0.0 0.0 . fromInteger
    {-# INLINE fromInteger #-}


instance Fractional QDouble where
    {-# INLINE recip #-}
    recip q@(unpackQ# -> (# x, y, z, w #)) = case negate (recip (square q)) of
      c -> packQ (x * c) (y * c) (z * c) (negate (w * c))
    {-# INLINE (/) #-}
    a / b = a * recip b
    {-# INLINE fromRational #-}
    fromRational = packQ 0.0 0.0 0.0 . fromRational


instance  Floating QDouble where
    {-# INLINE pi #-}
    pi = packQ 0.0 0.0 0.0 3.141592653589793
    {-# INLINE exp #-}
    exp (unpackQ# -> (# x, y, z, w #))
      = case (# (x * x) +
                (y * y) +
                (z * z)
             , exp w
             #) of
        (# 0.0, et #) -> packQ 0.0 0.0 0.0 et
        (# mv2, et #) -> case sqrt mv2 of
          mv -> case et * sin mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (et * cos mv)
    {-# INLINE log #-}
    log (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 | w >= 0    -> packQ 0.0 0.0 0.0 (log w)
            | otherwise -> packQ 3.141592653589793 0.0 0.0 (log (negate w))
        mv2 -> case (# sqrt (mv2 + (w * w)), sqrt mv2 #) of
          (# mq, mv #) -> case atan2 mv w / mv of
            l -> packQ (x * l) (y * l) (z * l) (log mq)
    {-# INLINE sqrt #-}
    sqrt (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 | w >= 0    -> packQ 0.0 0.0 0.0 (sqrt w)
            | otherwise -> packQ (sqrt (negate w)) 0.0 0.0 0.0
        mv2 ->
          let mq = sqrt (mv2 + w * w)
              l2 = sqrt mq
              tq = w / (mq * 2.0)
              sina = sqrt (0.5 - tq) * l2 / sqrt mv2
          in packQ (x * sina) (y * sina) (z * sina) (sqrt (0.5 + tq) * l2)
    {-# INLINE sin #-}
    sin (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (sin w)
        mv2 -> case sqrt mv2 of
          mv -> case cos w * sinh mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (sin w * cosh mv)
    {-# INLINE cos #-}
    cos (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (cos w)
        mv2 -> case sqrt mv2 of
          mv -> case sin w * sinh mv / negate mv of
            l -> packQ (x * l) (y * l) (z * l) (cos w * cosh mv)
    {-# INLINE tan #-}
    tan (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (tan w)
        mv2 ->
          let mv = sqrt mv2
              chv = cosh mv
              shv = sinh mv
              ct = cos w
              st = sin w
              cq = recip ( (ct * ct * chv * chv) + (st * st * shv * shv) )
              l = chv * shv * cq / mv
          in packQ (x * l) (y * l) (z * l) (ct * st * cq)
    {-# INLINE sinh #-}
    sinh (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (sinh w)
        mv2 -> case sqrt mv2 of
          mv -> case cosh w * sin mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (sinh w * cos mv)
    {-# INLINE cosh #-}
    cosh (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (cosh w)
        mv2 -> case sqrt mv2 of
          mv -> case sinh w * sin mv / mv of
            l -> packQ (x * l) (y * l) (z * l) (cosh w * cos mv)
    {-# INLINE tanh #-}
    tanh (unpackQ# -> (# x, y, z, w #))
      = case (x * x) + (y * y) + (z * z) of
        0.0 -> packQ 0.0 0.0 0.0 (tanh w)
        mv2 ->
          let mv = sqrt mv2
              cv = cos mv
              sv = sin mv
              cht = cosh w
              sht = sinh w
              cq = recip ( (cht * cht * cv * cv) + (sht * sht * sv * sv) )
              l = cv * sv * cq / mv
          in packQ (x * l) (y * l) (z * l) (cht * sht * cq)
    {-# INLINE asin #-}
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where
          i = case signum . im $ q of
                0  -> packQ 1.0 0.0 0.0 0.0
                i' -> i'
    {-# INLINE acos #-}
    acos q = pi/2 - asin q
    {-# INLINE atan #-}
    atan q@(unpackQ# -> (# _, _, _, w #))
      = if square imq == 0
        then packQ 0.0 0.0 0.0 (atan w)
        else i / 2 * log ( (i + q) / (i - q) )
      where
        i = signum imq
        imq = im q
    {-# INLINE asinh #-}
    asinh q = log (q + sqrt (q*q + 1))
    {-# INLINE acosh #-}
    acosh q = log (q + sqrt (q*q - 1))
    {-# INLINE atanh #-}
    atanh q = 0.5 * log ((1+q)/(1-q))


instance Eq QDouble where
    {-# INLINE (==) #-}
    QDouble a == QDouble b = a == b
    {-# INLINE (/=) #-}
    QDouble a /= QDouble b = a /= b


instance Show QDouble where
    showsPrec p (unpackQ# -> (# x, y, z, w #))
        = case finS of
            SEmpty -> showChar '0'
            Simple -> finF
            SParen -> showParen (p > 6) finF
      where
        (finS, finF) = go SEmpty
          [(w, Nothing), (x, Just 'i'), (y, Just 'j'), (z, Just 'k')]
        go :: ShowState -> [(Double, Maybe Char)] -> (ShowState, ShowS)
        go s ((v,l):xs)
          | (s0, f0) <- showComponent s v l
          , (s', f') <- go s0 xs
            = (s', f0 . f')
        go s [] = (s, id)
        showLabel Nothing  = id
        showLabel (Just c) = showChar c
        showComponent :: ShowState -> Double -> Maybe Char -> (ShowState, ShowS)
        showComponent sState val mLabel = case (sState, compare val 0) of
          (_     , EQ) -> ( sState, id )
          (SEmpty, GT) -> ( Simple, shows val . showLabel mLabel )
          (SEmpty, LT) -> ( SParen, shows val . showLabel mLabel )
          (_     , GT) -> ( SParen
                          , showString " + " . shows val . showLabel mLabel )
          (_     , LT) -> ( SParen
                          , showString " - " . shows (negate val) . showLabel mLabel )

data ShowState = SEmpty | Simple | SParen
    deriving Eq

instance Read QDouble where
    readPrec     = parens $ readPrec >>= go id 0 0 0 0
      where
        go :: (Double -> Double)
           -> Double -> Double -> Double -> Double
           -> Double -> ReadPrec QDouble
        go f x y z w new =
          let def = pure (packQ x y z (f new))
              withLabel EOF         = def
              withLabel (Ident "i")
                = (lexP >>= proceed (f new) y z w) <++ pure (packQ (f new) y z w)
              withLabel (Ident "j")
                = (lexP >>= proceed x (f new) z w) <++ pure (packQ x (f new) z w)
              withLabel (Ident "k")
                = (lexP >>= proceed x y (f new) w) <++ pure (packQ x y (f new) w)
              withLabel l           = proceed x y z (f new) l
          in (lexP >>= withLabel) <++ def
        proceed :: Double -> Double -> Double -> Double
                -> Lexeme -> ReadPrec QDouble
        proceed x y z w (Symbol "+") = readPrec >>= go id x y z w
        proceed x y z w (Symbol "-") = readPrec >>= go negate x y z w
        proceed x y z w EOF          = pure (packQ x y z w)
        proceed _ _ _ _ _            = pfail

    readListPrec = readListPrecDefault
    readList     = readListDefault
