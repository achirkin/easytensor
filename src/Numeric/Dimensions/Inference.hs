-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Dimensions.Inference
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This is a plugin module to make possible better inference of KnownNat constraints
-- for results of Nat operations and calculation of type-level lists length.
--
--
-- This module is made following Christiaan Baaij's example `ghc-typelits-knownnat-0.2.4`
--   https://hackage.haskell.org/package/ghc-typelits-knownnat-0.2.4/docs/src/GHC-TypeLits-KnownNat-Solver.html
--
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Numeric.Dimensions.Inference
  ( -- | Plugin is required by GHC to use Module as a typechecker plugin
    plugin
    -- * Term-level evidence
    -- `SNat'` type serves as a substitute for real SNat type hidden in `GHC.TypeLits`.
    -- We also need to export term-level evidence funtions:
    --  they do simple Integer arithmetics to support type-level Nat operations.
  , SNat' (..), funAdd, funRem, funMul, funPow, funSucc, funPred
  ) where

import           Control.Arrow (first, second)
import           Data.List     (find)
import           Data.Maybe    (catMaybes, mapMaybe)
import           Data.Proxy    (Proxy (..))
import           GHC.TypeLits

import           Class         (Class, classMethods, classTyCon)
import           FamInst       (tcInstNewTyCon_maybe)
import           Id            (idType)
import           Module        (mkModuleName)
import           Name          (getOccName, mkOccName, occNameString, tcName,
                                varName)
import           Plugins       (Plugin (..), defaultPlugin)
import           PrelNames     (knownNatClassName)
import           TcEvidence    (EvLit (EvNum),
                                EvTerm (EvDFunApp, EvLit, EvSuperClass),
                                mkEvCast, mkTcSymCo, mkTcTransCo)
import           TcPluginM     (FindResult (Found), TcPluginM,
                                findImportedModule, lookupOrig, newWanted,
                                tcLookupClass, tcLookupId, tcLookupTyCon)
import           TcRnMonad     (Ct (..), CtEvidence (..), CtLoc, TcPlugin (..),
                                TcPluginResult (..), ctEvLoc, ctEvTerm,
                                ctLocSpan, mkNonCanonical, setCtLoc,
                                setCtLocSpan)
import           TcType        (tcEqKind, tcGetTyVar_maybe)
import           TyCon         (TyCon)
import           Type          (PredTree (..), TyVar, Type, classifyPredType,
                                dropForAlls, eqType, funResultTy,
                                getTyVar_maybe, isNumLitTy, mkClassPred,
                                mkNumLitTy, mkTyVarTy, splitTyConApp_maybe,
                                tyConAppTyCon_maybe, typeKind)
import           TysWiredIn    (promotedConsDataCon, typeNatKind)
import           UniqFM        (UniqFM)
import qualified UniqFM        (addToUFM, listToUFM, lookupUFM)
import           Var           (DFunId)


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
decideNatOps _ _ _ [] = return (TcPluginOk [] [])
decideNatOps (Just it) givens _ wanteds =
    fmap (uncurry TcPluginOk)  -- wrap a list of solved constraints and a list of new Wanteds
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
      argEvTerms <- catMaybes <$> traverse (getKnownNatEvTerm myLoc) apps
      it <- getITypes
      return $ if length argEvTerms == length apps
               -- Construct evidence
               then flip (,) ct
                      <$> produceEvidenceTerm it opType
                                              (occNameString $ getOccName constr)
                                              apps argEvTerms
               else Nothing
checkNats _ = return Nothing

-- | Lookup existing or create new evidence term for a given type.
--   Make sure to call it on kind Nat!!!
getKnownNatEvTerm :: CtLoc -- current location is needed to crate a new wanted in case we need it
                  -> Type -- lookup evidence for this type or create a new one
                  -> InferenceM (Maybe EvTerm)
getKnownNatEvTerm myLoc x
    -- Evidence for a numeric literal is obvious
  | Just i <- isNumLitTy x = return . Just $ EvLit (EvNum i)
    -- Lookup or create evidence for a variable
  | Just v <- getTyVar_maybe x = Just <$> getKnownNatVarEvTerm myLoc v
    -- Solve Length xs constucts
  | Just (constr, [_,xs]) <- splitTyConApp_maybe x
  , "Length" <- occNameString (getOccName constr)
  , (ys, shift) <- unwrapConsType xs = do
    it@InferenceTypes {..} <- getITypes
    case (shift, find (\(t,_,_) -> eqType t ys) lengthKnownNats) of
      (0, Nothing) -> return Nothing
      (_, Just (_,shift2,ev)) ->
        return $ case compare shift shift2 of
          EQ -> Just ev
          LT -> produceEvidenceTerm it x "-" [ys, mkNumLitTy (shift2 - shift)]
                                             [ev, EvLit (EvNum (shift2 - shift))]
          GT -> produceEvidenceTerm it x "+" [ys, mkNumLitTy (shift - shift2)]
                                             [ev, EvLit (EvNum (shift - shift2))]
      (_, Nothing) -> do
        ev <- mkWantedKnownNat myLoc ys
        return $
          produceEvidenceTerm it x "+" [ys, mkNumLitTy shift] [ev, EvLit (EvNum shift)]
    -- go deeper recursively
  | Just (constr, apps) <- splitTyConApp_maybe x = do
      -- make an evidence term for each type argument of our operation
      -- we also check all arguments for being of kind Nat -- this is required for our recursion assumption
      argEvTerms <- catMaybes <$> traverse (getKnownNatEvTerm myLoc)
                                           (filter (tcEqKind typeNatKind . typeKind) apps)
      it <- getITypes
      return $ if length argEvTerms == length apps
               -- Construct evidence
               then produceEvidenceTerm it x (occNameString (getOccName constr)) apps argEvTerms
               else Nothing
    -- Honestly, I don't know what can be here
  | otherwise = return Nothing
     -- Create a new variable for other types of expressions
     -- fmap Just $ inTcPluginM (newFlexiTyVar typeNatKind) >>= newWantedKnownNat loc


--------------------------------------------------------------------------------
-- Actual conversion happens here
--------------------------------------------------------------------------------

-- | Generate evidence that Nat operation is KnownNat
produceEvidenceTerm :: InferenceTypes  -- ^ Pre-loaded definitions for types and classes
                    -> Type            -- ^ Nat operation type (i.e. rezT = constr + argTypes)
                    -> String          -- ^ Nat operation type constructor (eg "+")
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
  , Just (_, coerceKnownNatSNat) <- tcInstNewTyCon_maybe (classTyCon knownNatClass) [rezT]
  , [ natSingFun ] <- classMethods knownNatClass
  , Just sNatOrigTyCon <- tyConAppTyCon_maybe
                       . funResultTy $ dropForAlls (idType natSingFun)
    -- TcCoercion for transform: SNat (f x y) ~ Integer
  , Just (_, coerceSNatInteger) <- tcInstNewTyCon_maybe sNatOrigTyCon [rezT]
  , fullCoercion <- coerceSNat'Integer
                    `mkTcTransCo`
                    mkTcSymCo (coerceKnownNatSNat `mkTcTransCo` coerceSNatInteger)
    = Just $ mkEvCast funEv fullCoercion
  | otherwise = Nothing


--------------------------------------------------------------------------------
-- Custom plugin environment definition
--------------------------------------------------------------------------------

-- | Keep useful inference information here
data InferenceTypes = InferenceTypes
  { knownNatClass   :: !Class
    -- ^ Used to create KnownNat constraints
  , natOpMaybe      :: String -> Maybe DFunId
    -- ^ Get substitute function id by type constructor
  , sNatTyCon       :: TyCon
    -- ^ Type constructor for our SNat' substitute
  , lengthKnownNats :: [(Type, Integer, EvTerm)]
    -- ^ List of available KnownNat (Length xs)
  }

initInferenceTypes :: TcPluginM (Maybe InferenceTypes)
initInferenceTypes = do
    knownNatClass <- tcLookupClass knownNatClassName
    frModule <- findImportedModule (mkModuleName "Numeric.Dimensions.Inference") Nothing
    case frModule of
      Found _ thisModule -> do
        sNatTyCon <- lookupOrig thisModule (mkOccName tcName "SNat'") >>= tcLookupTyCon
        funAddId  <- lookupOrig thisModule (mkOccName varName "funAdd") >>= tcLookupId
        funRemId  <- lookupOrig thisModule (mkOccName varName "funRem") >>= tcLookupId
        funMulId  <- lookupOrig thisModule (mkOccName varName "funMul") >>= tcLookupId
        funPowId  <- lookupOrig thisModule (mkOccName varName "funPow") >>= tcLookupId
        let natOpMaybe c = case c of
              "+" -> Just funAddId
              "-" -> Just funRemId
              "*" -> Just funMulId
              "^" -> Just funPowId
              _   -> Nothing
            lengthKnownNats = []
        return $ Just InferenceTypes {..}
      _ -> return Nothing

-- | TyVar -> EvTerm map
type KnownNatMap = UniqFM EvTerm

-- | State monad to keep list of KnownNat devinitions,
--   Reader monad for InferenceTypes,
--   and Writer monad for a list of new Wanted constraints
newtype InferenceM a = InferenceM
  { runInferenceM :: (KnownNatMap, InferenceTypes) -> TcPluginM (a, (KnownNatMap, [Ct])) }

-- | Get InferenceTypes from Reader environment
getITypes :: InferenceM InferenceTypes
getITypes = InferenceM $ \(x,it) -> return (it, (x,[]))

-- -- | Run a child monad within Inference
-- inTcPluginM :: TcPluginM a -> InferenceM a
-- inTcPluginM m = InferenceM $ \(s, _) -> flip (,) (s, []) <$> m



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
runInference it givens inf = second snd
                          <$> runInferenceM inf ( knownNats
                                                , it{ lengthKnownNats = knownLNats }
                                                )
  where
    -- | Extract evidence about given KnownNat var from given Ct
    getKnownNatMaybe :: Ct -> Maybe (TyVar, EvTerm)
    getKnownNatMaybe CDictCan{ cc_ev = ev@CtGiven { ctev_pred = t } }
      | ClassPred cl (t0:_) <- classifyPredType t
      , "KnownNat" <- occNameString (getOccName cl)
      = flip (,) (ctEvTerm ev) <$> tcGetTyVar_maybe t0
    getKnownNatMaybe _ = Nothing
    -- | Extract evidence about given KnownNat var from given Ct
    getLKnownNatMaybe :: Ct -> Maybe (Type, Integer, EvTerm)
    getLKnownNatMaybe CDictCan{ cc_ev = ev@CtGiven { ctev_pred = t } }
        -- Find KnownNat (Length xs) constructs
        -- Length xs has 2 params: k, xs
      | ClassPred cl (t0:_) <- classifyPredType t
      , "KnownNat" <- occNameString (getOccName cl)
      , Just (constr, [_,xs]) <- splitTyConApp_maybe t0
      , "Length" <- occNameString (getOccName constr)
      , (listType, lenDiff) <- unwrapConsType xs
        = Just (listType, lenDiff, ctEvTerm ev)
         -- FiniteList has two parameters: k, (xs :: [k])
         -- We want to get its KnownNat (Length xs) superclass
         -- and use it as an evidence later
      | ClassPred cl [_,xs] <- classifyPredType t
      , "FiniteList" <- occNameString (getOccName cl)
      , (listType, lenDiff) <- unwrapConsType xs
        = Just (listType, lenDiff, EvSuperClass (ctEvTerm ev) 0)
    getLKnownNatMaybe _ = Nothing
    -- | Map: TyVar -> EvVar
    knownNats :: KnownNatMap
    knownNats = UniqFM.listToUFM $ mapMaybe getKnownNatMaybe givens
    knownLNats :: [(Type, Integer, EvTerm)]
    knownLNats = mapMaybe getLKnownNatMaybe givens

-- | Get a type of kind [k], and unwrap all Cons applications;
--   count number of Conses.
--   Used to calculate length shift.
unwrapConsType :: Type -> (Type, Integer)
unwrapConsType xs
    -- TODO FIXME assume Cons hase three params: k, y, ys
  | Just (constr, [_,_,ys]) <- splitTyConApp_maybe xs
  , constr == promotedConsDataCon  = second (+1) $ unwrapConsType ys
  | otherwise                      = (xs, 0)


--------------------------------------------------------------------------------
-- Getting EvTerms and creating new wanteds if necessary
--------------------------------------------------------------------------------

-- | Create a new evidence term for KnownNat constraint by making a new Wanted
newWantedKnownNat :: CtLoc -- ^ current location (of wanted constraint we trying to solve now)
                  -> TyVar -- ^ type variable we want to have a new wanted
                  -> InferenceM  EvTerm
newWantedKnownNat loc tv = do
  ev <- mkWantedKnownNat loc (mkTyVarTy tv)
  InferenceM $ \(knownNatMap, _) ->
    return (ev, (UniqFM.addToUFM knownNatMap tv ev, []))

-- | Just make a new wanted KnownNat for a type, without registering it in knownNatMap
mkWantedKnownNat :: CtLoc -- ^ current location (of wanted constraint we trying to solve now)
                 -> Type -- ^ type we want to have a new wanted
                 -> InferenceM EvTerm
mkWantedKnownNat loc t = InferenceM $ \(knownNatMap, it) -> do
    let predType = mkClassPred (knownNatClass it) [t]
    -- Create a new wanted constraint
    wantedCtEv <- newWanted loc predType
    let ev      = ctEvTerm wantedCtEv
        wanted  = mkNonCanonical wantedCtEv
        wanted' = setCtLoc wanted (setCtLocSpan (ctEvLoc wantedCtEv) (ctLocSpan loc))
    return (ev, (knownNatMap, [wanted']))

-- | Lookup existing or create new evidence term for a given type variable
getKnownNatVarEvTerm :: CtLoc -- current location is needed to crate a new wanted in case we need it
                     -> TyVar -- lookup evidence for this type variable or create a new one
                     -> InferenceM EvTerm
getKnownNatVarEvTerm l tv = InferenceM
    (\(knownNatMap, _) -> return (UniqFM.lookupUFM knownNatMap tv, (knownNatMap,[]))) >>= m
  where
    m (Just ev) = return ev
    m Nothing   = newWantedKnownNat l tv


--------------------------------------------------------------------------------
-- Evidence construction
--------------------------------------------------------------------------------


-- | This newtype declaration has the same representation
--   as Integer type and as hidden SNat type from GHC.TypeLits
newtype SNat' (n :: Nat) = SNat' Integer


funAdd :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' ((+) a b)
funAdd = SNat' (natVal (Proxy @a) + natVal (Proxy @b))

funRem :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' ((-) a b)
funRem = SNat' (natVal (Proxy @a) - natVal (Proxy @b))

funMul :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' ((*) a b)
funMul = SNat' (natVal (Proxy @a) * natVal (Proxy @b))

funPow :: forall (a :: Nat) (b :: Nat) . (KnownNat a, KnownNat b) => SNat' ((^) a b)
funPow = SNat' (natVal (Proxy @a) ^ natVal (Proxy @b))

funSucc :: forall (x :: Nat) . KnownNat x => SNat' (x + 1)
funSucc = SNat' (natVal (Proxy @x) + 1)

funPred :: forall (x :: Nat) . KnownNat x => SNat' (x - 1)
funPred = SNat' (natVal (Proxy @x) - 1)
