-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Inference
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This is a supplementary module that provides a GHC type-checker plugin.
-- The plugin infers some equalities that overwise would be impossible to infer
--  using GHC typechecker only.
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Numeric.Dimensions.Inference (plugin) where

--import           Control.Applicative (Alternative (..))
import           Control.Arrow
--import           Control.Monad       ((>=>))
--import           Data.Coerce
import           Data.Maybe     --      (catMaybes, fromMaybe, mapMaybe)
--import           Data.Semigroup

import           Class
import           Coercion
--import           FamInst
--import           FastString
--import           Id
--import           InstEnv             (DFunId)
import           Name
import           Outputable          (Outputable (..), showSDocUnsafe)
import           Plugins             (Plugin (..), defaultPlugin)
import           TcEvidence
import           TcPluginM
import           TcRnMonad
import           TcType
import           TyCon
--import           TyCoRep
import           Type
import Var
import           UniqFM (UniqFM)
import qualified UniqFM
import qualified Unique
--import           TysWiredIn          (listTyCon)

-- import           Numeric.Dimensions.Inference.Types


-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just inferNatsPlugin }


inferNatsPlugin :: TcPlugin
inferNatsPlugin = TcPlugin
  { tcPluginInit  = return ()
  , tcPluginSolve = const decideNatOps
  , tcPluginStop  = const (return ())
  }

decideNatOps ::  [Ct] -- ^ Givens
              -> [Ct] -- ^ Deriveds
              -> [Ct] -- ^ Wanteds
              -> TcPluginM TcPluginResult
decideNatOps _ _ []      = return (TcPluginOk [] [])
decideNatOps givens _ wanteds = do
  tcPluginIO . print . catMaybes $ map getKnownNatMaybe givens
  uncurry TcPluginOk . second concat . unzip . catMaybes <$> mapM checkNats wanteds

getKnownNatMaybe :: Ct -> Maybe (TyVar, EvVar)
getKnownNatMaybe CDictCan{ cc_ev = CtGiven {ctev_evar = v, ctev_pred = t} } =
  case classifyPredType t of
    ClassPred cl (t0:_) -> if getOccName cl == mkOccName tcClsName "KnownNat"
                          then flip (,) v <$> tcGetTyVar_maybe t0
                          else Nothing
    _ -> Nothing
getKnownNatMaybe _ = Nothing


type KnownNatMap = UniqFM EvVar

lookupKnownNatEvVar :: KnownNatMap -> TyVar -> Maybe EvVar
lookupKnownNatEvVar = UniqFM.lookupUFM

-- | Map: TyVar -> EvVar
getKnownNats :: [Ct] -> UniqFM EvVar
getKnownNats = UniqFM.listToUFM . mapMaybe getKnownNatMaybe


checkNats :: Ct -> TcPluginM (Maybe ((EvTerm, Ct), [Ct]))
checkNats ct@CDictCan{ cc_ev = CtWanted{ctev_pred = t, ctev_loc = myLoc}} = do
  tcPluginIO . putStrLn $ "Checking!! " ++ show t
  case classifyPredType t of
    ClassPred cl types ->
      case ( getOccName cl == mkOccName tcClsName "KnownNat"
           , splitTyConApp_maybe <$> types
           ) of
        (True, [Just (constr, apps)])
          | Just natOp <- getNatOpMaybe constr -> do
              tcPluginIO . putStrLn $ "Checking... " ++ show apps
              newWanteds <- traverse (fmap CNonCanonical . newWanted myLoc . mkClassPred cl . (:[])) apps
              return $ Just
                  ( ( EvLit (EvNum 17), ct)  -- (EvStr $ mkFastString "It does not work this way :("), ct )
                  , newWanteds
                  )
          | otherwise -> return Nothing
        _ -> return Nothing
    _ -> return Nothing
checkNats _ = return Nothing

getNatOpMaybe :: TcTyCon -> Maybe (Integer -> Integer -> Integer)
getNatOpMaybe c = case occNameString (getOccName c) of
  "+" -> Just (+)
  "-" -> Just (-)
  "*" -> Just (*)
  "^" -> Just (^)
  _   -> Nothing




instance Show TcPredType where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
instance Show Var where
  show = showSDocUnsafe . ppr
instance Show TcTyThing where
  show = showSDocUnsafe . ppr
instance Show Ct where
  show = showSDocUnsafe . ppr
instance Show Name where
  show = showSDocUnsafe . ppr
instance Show EvBind where
  show = showSDocUnsafe . ppr
instance Show EvTerm where
  show = showSDocUnsafe . ppr
instance Show CtEvidence where
  show = showSDocUnsafe . ppr
instance Show Class where
  show = showSDocUnsafe . ppr
instance Show Coercion where
  show = showSDocUnsafe . ppr
instance Show CtLoc where
  show = ("CtLoc " ++) . showSDocUnsafe . ppr . ctl_origin
instance Show SkolemInfo where
  show (SigSkol utc ep x) = "SigSkol {" ++ show utc ++ "} {" ++ showSDocUnsafe (ppr ep) ++
                  "} {" ++ showSDocUnsafe (ppr x) ++ "} "
  -- show (PatSynSigSkol n) = "PatSynSigSkol " ++ showSDocUnsafe (ppr n)
  show (ClsSkol n) = "ClsSkol " ++ showSDocUnsafe (ppr n)
  show (DerivSkol n) = "DerivSkol " ++ showSDocUnsafe (ppr n)
  show InstSkol = "InstSkol"
  show (InstSC n) = "InstSC " ++ showSDocUnsafe (ppr n)
  show DataSkol = "DataSkol"
  show FamInstSkol = "FamInstSkol"
  show (PatSkol a _) = "PatSkol " ++ showSDocUnsafe (ppr a) ++ " HsMatchContext"
  show ArrowSkol = "ArrowSkol"
  show (IPSkol n) = "IPSkol " ++ showSDocUnsafe (ppr n)
  show (RuleSkol n) = "RuleSkol " ++ showSDocUnsafe (ppr n)
  show (InferSkol n) = "InferSkol " ++ showSDocUnsafe (ppr n)
  show BracketSkol = "BracketSkol"
  show (UnifyForAllSkol n) = "UnifyForAllSkol " ++ showSDocUnsafe (ppr n)
  show UnkSkol = "UnkSkol"


deriving instance Show UserTypeCtxt
