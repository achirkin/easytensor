-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Inference
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-- This module has copied some chunks of code from Christiaan Baaij's
--   ghc-typelits-knownnat-0.2.4
--   https://hackage.haskell.org/package/ghc-typelits-knownnat-0.2.4/docs/src/GHC-TypeLits-KnownNat-Solver.html
--   Module     :  GHC.TypeLits.KnownNat.Solver
--   Copyright  :  (C) 2016, University of Twente
--   License    :  BSD2 (see the file LICENSE)
--   Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards      #-}

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
import           FamInst (tcInstNewTyCon_maybe)
--import           FastString
import Id         (idType)
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
import           Var (DFunId)
import           UniqFM (UniqFM)
import qualified UniqFM
-- import qualified Unique
import PrelNames
import Module
import           TysWiredIn
import Numeric.Dimensions.Inference.KnownNatFuns ()

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
  { tcPluginInit  = initInferenceTypes
  , tcPluginSolve = decideNatOps
  , tcPluginStop  = const (return ())
  }

decideNatOps :: Maybe InferenceTypes
             -> [Ct] -- ^ Givens
             -> [Ct] -- ^ Deriveds
             -> [Ct] -- ^ Wanteds
             -> TcPluginM TcPluginResult
decideNatOps _ _ _ []      = return (TcPluginOk [] [])
decideNatOps (Just it) givens _ wanteds
  = fmap (uncurry TcPluginOk) -- wrap a list of solved constraints and a list of new Wanteds
  . runInference it givens -- runInference looks up a list of known KnownNat Constraints
                           -- and passes it in the checkNats thing;
                           -- it also manages a list of new constraints
  $ catMaybes <$> mapM checkNats wanteds
decideNatOps _ _ _ _ = return (TcPluginOk [] [])


checkNats :: Ct -> InferenceM (Maybe (EvTerm, Ct))
checkNats ct@CDictCan{ cc_ev = CtWanted{ctev_pred = t, ctev_loc = myLoc}}
  | ClassPred cl [opType] <- classifyPredType t
  , "KnownNat" <- occNameString (getOccName cl)
  , Just (constr, apps) <- splitTyConApp_maybe opType = do
      -- make an evidence term for each type argument of our operation
      argEvTerms <- traverse (getKnownNatEvTerm myLoc) apps
      -- Construct evidence
      it <- getITypes
      return $ flip (,) ct
            <$> produceEvidenceTerm it opType constr apps argEvTerms
checkNats _ = return Nothing

-- | Lookup existing or create new evidence term for a given type.
--   Make sure to call it on kind Nat!!!
getKnownNatEvTerm :: CtLoc -- current location is needed to crate a new wanted in case we need it
                  -> Type -- lookup evidence for this type or create a new one
                  -> InferenceM EvTerm
getKnownNatEvTerm loc x
    -- Evidence for a numeric literal is obvious
  | Just i <- isNumLitTy x = return $ EvLit (EvNum i)
    -- Lookup or create evidence for a variable
  | Just v <- getTyVar_maybe x = getKnownNatVarEvTerm loc v
    -- Create a new variable for other types of expressions
  | otherwise = inTcPluginM (newFlexiTyVar typeNatKind) >>= newWantedKnownNat loc


--------------------------------------------------------------------------------
-- Actual conversion happens here
--------------------------------------------------------------------------------

-- | Generate evidence that Nat operation is KnownNat
produceEvidenceTerm :: InferenceTypes  -- ^ Pre-loaded definitions for types and classes
                    -> Type            -- ^ Nat operation type (i.e. rezT = constr + argTypes)
                    -> TyCon           -- ^ Nat operation type constructor
                    -> [Type]          -- ^ List of arguments
                    -> [EvTerm]        -- ^ List of corresponding evidences being of KnownNat
                    -> Maybe EvTerm
produceEvidenceTerm InferenceTypes{..} rezT constr argTypes argEvTerms
  | -- Get id of a function used to get evidence at the term level
    Just natOpFunId <- natOpMaybe constr
    -- TcCoercion for transform: SNat' (f x y) ~ Integer
  , Just (_, coerceSNat'Integer) <- tcInstNewTyCon_maybe sNatTyCon [rezT]
    -- Evidence produced by using function application
  , funEv <- EvDFunApp natOpFunId argTypes argEvTerms
  , Just (_, coerceKnownNatSNat) <- tcInstNewTyCon_maybe (classTyCon knownNatClass) [rezT] -- KnownNat n ~ SNat n
  , [ kn_meth ] <- classMethods knownNatClass
  , Just kn_tcRep <- tyConAppTyCon_maybe -- SNat
                      $ funResultTy      -- SNat n
                      $ dropForAlls      -- KnownNat n => SNat n
                      $ idType kn_meth   -- forall n. KnownNat n => SNat n
  , Just (_, coerceSNatInteger) <- tcInstNewTyCon_maybe kn_tcRep [rezT]  -- SNat n ~ Integer
  , op_to_kn <-  coerceSNat'Integer `mkTcTransCo` mkTcSymCo (coerceKnownNatSNat `mkTcTransCo` coerceSNatInteger)
    = Just $ mkEvCast funEv op_to_kn
  | otherwise = Nothing


--------------------------------------------------------------------------------
-- Custom plugin environment definition
--------------------------------------------------------------------------------

-- | Keep useful inference information here
data InferenceTypes = InferenceTypes
  { knownNatClass :: !Class
    -- ^ Used to create KnownNat constraints
  , natOpMaybe :: TyCon -> Maybe DFunId
    -- ^ Get substitute function id by type constructor
  , sNatTyCon  :: TyCon
    -- ^ Type constructor for our SNat' substitute
  }

initInferenceTypes :: TcPluginM (Maybe InferenceTypes)
initInferenceTypes = do
    knownNatClass <- tcLookupClass knownNatClassName
    frModule <- findImportedModule (mkModuleName "Numeric.Dimensions.Inference.KnownNatFuns") Nothing
    case frModule of
      Found _ thisModule -> do
        sNatTyCon <- lookupOrig thisModule (mkOccName tcName "SNat'") >>= tcLookupTyCon
        funAddId  <- lookupOrig thisModule (mkOccName varName "funAdd") >>= tcLookupId
        funRemId  <- lookupOrig thisModule (mkOccName varName "funRem") >>= tcLookupId
        funMulId  <- lookupOrig thisModule (mkOccName varName "funMul") >>= tcLookupId
        funPowId  <- lookupOrig thisModule (mkOccName varName "funPow") >>= tcLookupId
        let natOpMaybe c = case occNameString (getOccName c) of
              "+" -> Just funAddId
              "-" -> Just funRemId
              "*" -> Just funMulId
              "^" -> Just funPowId
              _   -> Nothing
        return $ Just InferenceTypes {..}
      _ -> return Nothing

type KnownNatMap = UniqFM EvTerm

-- | State monad to keep list of KnownNat devinitions,
--   Reader monad for InferenceTypes,
--   and Writer monad for a list of new Wanted constraints
newtype InferenceM a = InferenceM
  { runInferenceM :: (KnownNatMap, InferenceTypes) -> TcPluginM (a, (KnownNatMap, [Ct])) }

-- | Get InferenceTypes from Reader environment
getITypes :: InferenceM InferenceTypes
getITypes = InferenceM $ \(x,it) -> return (it, (x,[]))

-- | Run a child monad within Inference
inTcPluginM :: TcPluginM a -> InferenceM a
inTcPluginM m = InferenceM $ \(s, _) -> flip (,) (s, []) <$> m



instance Functor InferenceM where
  fmap f x = InferenceM $ fmap (first f) . runInferenceM x

instance Applicative InferenceM where
  pure x = InferenceM $ pure . (,) x . second (const [])
  af <*> ax = InferenceM $ \(s0, it) -> do
      (x, (s1, ws1)) <- runInferenceM ax (s0, it)
      (f, (s2, ws2)) <- runInferenceM af (s1, it)
      return (f x, (s2,ws1 ++ ws2))

instance Monad InferenceM where
  return = pure
  mx >>= f = InferenceM $ \(s0, it) -> do
      (x, (s1, ws1)) <- runInferenceM mx (s0, it)
      (y, (s2, ws2)) <- runInferenceM (f x) (s1, it)
      return (y, (s2, ws1 ++ ws2))

-- | Run inference with the list of givens
--   And return a list of wanteds
runInference :: InferenceTypes
             -> [Ct] -- ^ list of givens
             -> InferenceM a  -- ^ the whole inference stuff
             -> TcPluginM (a, [Ct]) -- ^ computation result plus a list of new wanteds
runInference it givens inf = second snd <$> runInferenceM inf (getKnownNats givens, it)
  where
    -- | Extract evidence about given KnownNat var from given Ct
    getKnownNatMaybe :: Ct -> Maybe (TyVar, EvTerm)
    getKnownNatMaybe CDictCan{ cc_ev = ev@CtGiven { ctev_pred = t } }
      | ClassPred cl (t0:_) <- classifyPredType t
      , "KnownNat" <- occNameString (getOccName cl)
      = flip (,) (ctEvTerm ev) <$> tcGetTyVar_maybe t0
    getKnownNatMaybe _ = Nothing
    -- | Map: TyVar -> EvVar
    getKnownNats :: [Ct] -> KnownNatMap
    getKnownNats = UniqFM.listToUFM . mapMaybe getKnownNatMaybe


-- | Create a new evidence term for KnownNat constraint by making a new Wanted
newWantedKnownNat :: CtLoc -- ^ current location (of wanted constraint we trying to solve now)
                  -> TyVar -- ^ type variable we want to have a new wanted
                  -> InferenceM  EvTerm
newWantedKnownNat loc tv = InferenceM $ \(knownNatMap, it) -> do
    let predType = mkClassPred (knownNatClass it) [t]
    -- Create a new wanted constraint
    wantedCtEv <- newWanted loc predType
    let ev      = ctEvTerm wantedCtEv
        wanted  = mkNonCanonical wantedCtEv
    -- Set the source-location of the new wanted constraint to the source
    -- location of the [W]anted constraint we are currently trying to solve
    let ct_ls   = ctLocSpan loc
        ctl     = ctEvLoc  wantedCtEv
        wanted' = setCtLoc wanted (setCtLocSpan ctl ct_ls)
    return (ev, (UniqFM.addToUFM knownNatMap tv ev, [wanted']))
  where
    t = mkTyVarTy tv

-- | Lookup existing or create new evidence term for a given type variable
getKnownNatVarEvTerm :: CtLoc -- current location is needed to crate a new wanted in case we need it
                     -> TyVar -- lookup evidence for this type variable or create a new one
                     -> InferenceM EvTerm
getKnownNatVarEvTerm l tv = InferenceM
    (\(knownNatMap, _) -> return (UniqFM.lookupUFM knownNatMap tv, (knownNatMap,[]))) >>= m
  where
    m (Just ev) = return ev
    m Nothing   = newWantedKnownNat l tv






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
