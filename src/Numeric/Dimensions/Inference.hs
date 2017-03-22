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
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Numeric.Dimensions.Inference (plugin) where

import           Data.Maybe (catMaybes, fromMaybe)

import           Class      (Class)
import           ConLike    (ConLike (..))
import           Outputable (Outputable (..), showSDocUnsafe, text, ($$), (<+>))
import           Plugins    (Plugin (..), defaultPlugin)
import           TcEvidence (EvTerm (..))
import           TcPluginM
import           TcRnTypes  (Ct (..), TcPlugin (..), TcPluginResult (..),
                             ctEvPred, ctEvidence, isWanted, mkNonCanonical)
import           Type       (EqRel (..), Kind, PredTree (EqPred), PredType,
                             classifyPredType, eqType, getEqPredTys, mkTyVarTy)
import           TysWiredIn (typeNatKind)

import           Coercion   (CoercionHole, Role (..), mkForAllCos, mkHoleCo,
                             mkInstCo, mkNomReflCo, mkUnivCo)
import           DataCon    (DataCon)
import           HsBinds    (PatSynBind)
import           HsExpr     (ArithSeqInfo, HsMatchContext (..),
                             HsStmtContext (..), UnboundVar (..))
import           HsLit      (HsOverLit)
import           HsPat      (LPat)
import           HsTypes    (HsIPName (..))
import           Name       (Name)
import           OccName
import           PatSyn     (PatSyn)
import           RdrName    (GlobalRdrEnv (..), RdrName)
import           TcPluginM  (newCoercionHole, newFlexiTyVar)
import           TcRnTypes
import           TcType
import           TcTypeNats (typeNatAddTyCon, typeNatExpTyCon, typeNatMulTyCon,
                             typeNatSubTyCon)
import           TyCon
import           TyCoRep    (UnivCoProvenance (..))
import           TyCoRep    (Type (..))
import           Type       (mkPrimEqPred)

import           Module     (mkModuleName)
import           TcTypeNats (typeNatLeqTyCon)
import           Type
import           TysWiredIn (promotedFalseDataCon, promotedTrueDataCon)

-- import           Debug.Trace

-- import           Numeric.Dimensions

-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.Dimensions.Inference \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just inferListOpsPlugin }

inferListOpsPlugin :: TcPlugin
inferListOpsPlugin = TcPlugin
  { tcPluginInit  = return ()
  , tcPluginSolve = const decideListOps
  , tcPluginStop  = const (return ())
  }

decideListOps :: [Ct] -> [Ct] -> [Ct]
               -> TcPluginM TcPluginResult
decideListOps _givens _deriveds []     = return (TcPluginOk [] [])
decideListOps givens  deriveds wanteds =
  uncurry TcPluginOk . unzip . catMaybes <$> mapM eliminateIdentities wanteds


-- . traceShow (toConstr t)

eliminateIdentities :: Ct -> TcPluginM (Maybe ((EvTerm, Ct), Ct))
eliminateIdentities ct@(CNonCanonical CtWanted{ctev_pred = t, ctev_loc = myLoc})
  = case classifyPredType t of
      EqPred NomEq lhs rhs -> do
        -- tcPluginIO . putStrLn $ showSDocUnsafe $ ppr t
        frModule <- findImportedModule (mkModuleName "Numeric.Dimensions") Nothing
        case frModule of
          Found _ modNDims -> do
            tSimplifyList <- lookupOrig modNDims (mkOccName tcName "SimplifyList") >>= tcLookupTyCon
            tToList <- lookupOrig modNDims (mkOccName tcName "ToList") >>= tcLookupTyCon
            tToListNat <- lookupOrig modNDims (mkOccName tcName "ToListNat") >>= tcLookupTyCon
            tEvalConsNat <- lookupOrig modNDims (mkOccName tcName "EvalConsNat") >>= tcLookupTyCon
            tEvalCons <- lookupOrig modNDims (mkOccName tcName "EvalCons") >>= tcLookupTyCon
            let rSListToList :: Type -> Type
                rSListToList myType = fromMaybe myType $
                  splitTyConApp_maybe myType >>= \(constr, apps) ->
                    if constr == tSimplifyList
                    then case apps of
                          [_, innerType] -> splitTyConApp_maybe innerType >>= \(innerConstr, _) -> Just $
                            if innerConstr == tSimplifyList || innerConstr == tToList || innerConstr == tEvalConsNat
                            then rSListToList innerType
                            else mkTyConApp constr (rSListToList <$> apps)
                          _ -> Nothing
                    else Just $ mkTyConApp constr (rSListToList <$> apps)
            -- tcPluginIO . print $ tyConArity tSimplifyList
            -- tcPluginIO . print $ tyConArity tToList
            -- tcPluginIO . print $ tyConArity tEvalCons
            -- tcPluginIO . putStrLn $ "LHS TYPE: " ++ show lhs
            -- tcPluginIO . putStrLn $ "LHS TYPE 2: " ++ show (rSListToList lhs)
            -- tcPluginIO . putStrLn $ "RHS TYPE: " ++ show rhs
            -- tcPluginIO . putStrLn $ "RHS TYPE 2: " ++ show (rSListToList rhs)
            let newLhs = rSListToList lhs
                newRhs = rSListToList rhs
                newEqPred = mkPrimEqPredRole Nominal newLhs newRhs

            newWantedEq <- newWanted myLoc newEqPred
            tcPluginIO . putStrLn $ "WAS: " ++ show t
            tcPluginIO . putStrLn $ "NOW: " ++ show newEqPred
            -- tcPluginIO . putStrLn $ "LHS EXPANSION: " ++ show (splitTyConApp_maybe lhs)
            -- tcPluginIO . putStrLn $ "RHS EXPANSION: " ++ show (splitTyConApp_maybe rhs)
            return $ Just ((EvCoercion (mkUnivCo (PluginProv "numeric-dimensions-inference") Nominal t newEqPred),ct), CNonCanonical newWantedEq)
          _ -> return Nothing
      _ -> return Nothing
eliminateIdentities _ = return Nothing















deriving instance Show Ct
deriving instance Show CtEvidence
deriving instance Show EqRel
deriving instance Show Hole
deriving instance Show CtLoc
deriving instance Show TcEvDest
deriving instance Show UnboundVar
deriving instance Show CtOrigin
deriving instance Show TypeOrKind
deriving instance Show SkolemInfo
deriving instance Show UserTypeCtxt
deriving instance Show HsIPName
deriving instance Show ConLike
deriving instance Show (HsMatchContext Name)
deriving instance Show (HsStmtContext Name)




instance Show TcLclEnv where
  show _ = "TcLclEnv{..}"
instance Show ExpType where
  show (Check t) = "Check " ++ show t
  show Infer {}  = "Infer{..}::ExpType"
instance Show TcPredType where
  show = showSDocUnsafe . ppr
instance Show DataCon where
  show = showSDocUnsafe . ppr
instance Show Class where
  show = showSDocUnsafe . ppr
instance Show TcTyVar where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
instance Show OccName where
  show = showSDocUnsafe . ppr
instance Show Name where
  show = showSDocUnsafe . ppr
instance Show RdrName where
  show = showSDocUnsafe . ppr
instance Show SubGoalDepth where
  show = showSDocUnsafe . ppr
instance Show CoercionHole where
  show = showSDocUnsafe . ppr
instance Show GlobalRdrEnv where
  show = showSDocUnsafe . ppr
instance Show ErrorThing where
  show = showSDocUnsafe . ppr
instance Show TypeSize where
  show = showSDocUnsafe . ppr
instance Show PatSyn where
  show = showSDocUnsafe . ppr
instance Outputable (PatSynBind idL idR) => Show (PatSynBind idL idR) where
  show = showSDocUnsafe . ppr
instance Outputable (LPat n) => Show (LPat n) where
  show = showSDocUnsafe . ppr
instance Outputable (HsOverLit n) => Show (HsOverLit n) where
  show = showSDocUnsafe . ppr
instance Outputable (ArithSeqInfo n) => Show (ArithSeqInfo n) where
  show = showSDocUnsafe . ppr
