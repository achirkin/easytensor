{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase   #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Numeric.DataFrame.TcPlugin (plugin) where

-- import Control.Monad (join)
import           Bag
import           Class
import           Control.Arrow       (first, (***))
import           Data.Function       ((&))
import           Data.Maybe          (catMaybes, mapMaybe, maybeToList, fromMaybe)
import           Data.Monoid         (First (..), Any (..))
import           GhcPlugins
import           InstEnv
import           Panic               (panicDoc)
import           TcEvidence          (EvTerm (..))
import           TcPluginM
import           TcRnTypes

import CoAxiom
import qualified TcRnMonad
import qualified IfaceEnv
import qualified Finder
import qualified TcType

-- import           TcSMonad (emitNewDerivedEq, runTcSDeriveds)
import           IOEnv
-- import           ErrUtils


-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.DataFrame.TcPlugin \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just TcPlugin
      { tcPluginInit  = initEtTcState
      , tcPluginSolve = runPluginSolve
      , tcPluginStop  = const (return ())
      }
  -- , installCoreToDos = install
#if MIN_VERSION_ghc(8,6,0)
  , pluginRecompile = purePlugin
#endif
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  bf <- lookupBackendFamily
  return (CoreDoPluginPass "Numeric.DataFrame.CorePlugin" (pass bf) : todo)

{-
  The plan for CoreToDo in DataFrame.Type

  I want to make all specific instances for DFBackend and different backend types

  Since I guarantee that DFBackend is a newtype wrapper with exactly the same
  behavior as the backends, I don't even want to derive the instance using
  the usual machinery. Just declare all instances with existing DFunIds.

  Steps V1:

  1. Lookup type family instances (branches of CoAxiom) of Backend t n

  1. Check out all KnownBackend-based instances of DFBackend

  2. Run the following for every class:

    * Run the following for every Backend type family instance:

      I.  Lookup specific class instance with found paramaters t n b (lookupInstEnv)
      II. Use mkLocalInstance with parameters of found instance and replaced RHS types
      III.  Add new instance to (mg_insts :: ![ClsInst]) of ModGuts

  Steps V2:

  1. Lookup type family instances (branches of CoAxiom) of Backend t n

  2. For every type instance:

     2.1 Lookup all class instances

     2.2 For every class instance:

         * Use mkLocalInstance with parameters of found instance
           and replaced RHS types
         * Add new instance to (mg_insts :: ![ClsInst]) of ModGuts

 -}
pass :: CoAxiom Branched -> ModGuts -> CoreM ModGuts
pass backendFamily guts = do

    matchedInstances <- matchInstances <$> getUniquesM
    -- mapM_ (putMsg . ppr) typeMaps
    -- mapM_ (putMsg . ppr) matchedInstances
    --
    -- mapM_ (\b -> putMsg (ppr b) >> putMsg "------") $ mg_binds guts

    return guts
      { mg_insts = map snd matchedInstances ++ mg_insts guts
      , mg_inst_env = extendInstEnvList (mg_inst_env guts)
                    $ map snd matchedInstances
      , mg_binds = map fst matchedInstances ++ mg_binds guts
      }
  where

    -- backends for which I derive class instances
    backendInstances = fromBranches $ coAxiomBranches backendFamily

    -- list of backends with overlapping mods:
    --  just substitute class instances and we are done.
    typeMaps :: [(OverlapMode, [TyVar], Type, Type)]
    typeMaps = map mapBackend backendInstances
      where
        mapBackend coaxb = ( overlap coaxb
                           , coAxBranchTyVars coaxb
                           , coAxBranchRHS coaxb
                           , mkTyConApp dfBackendTyCon
                               $ coAxBranchLHS coaxb ++ [coAxBranchRHS coaxb]
                           )
        overlap coaxb = if null $ coAxBranchIncomps coaxb
                        then Overlapping NoSourceText
                        else Incoherent NoSourceText

    -- lookup for class instances here
    instances = InstEnv.instEnvElts $ mg_inst_env guts

    -- DFbackend type constructor is supposed to be defined in this module
    dfBackendTyCon
      = let checkDfBackendTyCon tc
                = if occName (tyConName tc) == mkTcOcc "DFBackend"
                  then First $ Just tc
                  else First Nothing
         in fromMaybe (
               panicDoc "Numeric.DataFrame.TcPlugin"
                        "Could not find DFBackend type constructor"
            ) . getFirst $ foldMap checkDfBackendTyCon $ mg_tcs guts

    matchInstances uniques = catMaybes $ zipWith ($)
      [ \u -> let refId = instanceDFunId ci
                  f i = (mkBind refId i, i)
               in f <$> matchInstance u tm refId (instanceHead ci)
      | tm <- typeMaps
      , ci <- instances
      ] uniques

    matchInstance :: Unique
                  -> (OverlapMode, [TyVar], Type, Type)
                  -> DFunId
                  -> ([TyVar], Class, [Type])
                  -> Maybe ClsInst
    matchInstance uniq
                  (overlapMode, bTyVars, bOrigT, bNewT)
                  iDFunId
                  (iTyVars, iclass, iTyPams)
      | (Any True, newTyPams) <- matchTys iTyPams
      , (_, newDFunTy) <- matchTy (idType iDFunId)
      , newDFunId <- mkExportedLocalId
          (DFunId isNewType) newName newDFunTy
        = Just $ mkLocalInstance
                    newDFunId
                    (OverlapFlag overlapMode False)
                    iTyVars iclass newTyPams
      | otherwise
        = Nothing
      where
        matchTy = maybeReplaceTypeOccurrences bTyVars bOrigT bNewT
        matchTys = mapM matchTy
        isNewType = isNewTyCon (classTyCon iclass)
        newName = mkExternalName uniq (mg_module guts)
                   newOccName
                   (mg_loc guts)
        newOccName
          = let oname = occName (idName $ iDFunId)
             in mkOccName (occNameSpace oname)
                          (occNameString oname ++ "DFBackend")

    mkBind :: DFunId -> ClsInst -> CoreBind
    mkBind oldId newInst
        = NonRec newId (Var oldId) -- TODO: make a proper cast expression.
      where
        newId = instanceDFunId newInst


-- mkOccName :: NameSpace -> String -> OccName
-- mkOccNameFS :: NameSpace -> FastString -> OccName
-- UniqSupply.hs
--   getUniqueM :: m Unique
--   mkExternalName :: Unique -> Module -> OccName -> SrcSpan -> Name

-- | Look through the type recursively;
--   If the type occurrence found, replace it with the new type.
--   While the type is checked, the function also tracks how type variables
--   are renamed.
--   So the result is the changed type and an indication if it has been changed.
maybeReplaceTypeOccurrences :: [TyVar] -> Type -> Type -> Type -> (Any, Type)
maybeReplaceTypeOccurrences tv told tnew = replace
  where
    mkSubsts xs = mkSubsts' tv $ map mkTyVarTy xs
    mkSubsts' [] [] = Just emptyTCvSubst
    mkSubsts' (x:xs) (t:ts) = (\s -> extendTCvSubst s x t)
                           <$> mkSubsts' xs ts
    mkSubsts' _ _ = Nothing
    replace :: Type -> (Any, Type)
    replace t
      | tvars <- tyCoVarsOfTypeWellScoped t
      , Just sub <- mkSubsts tvars
      , told' <- substTyAddInScope sub told
      , eqType t told'
        = (Any True, substTyAddInScope sub tnew)
        -- split type constructors
      | Just (tyCon, tys) <- splitTyConApp_maybe t
        = mkTyConApp tyCon <$> mapM replace tys
        -- split foralls
      | (bndrs@(_:_), t') <- splitPiTys t
        = mkPiTys bndrs <$> replace t'
        -- split arrow types
      | Just (at, rt) <- splitFunTy_maybe t
        = mkFunTy <$> replace at <*> replace rt
      | otherwise
        = (Any False, t)




lookupBackendFamily :: CoreM (CoAxiom Branched)
lookupBackendFamily = do
    hscEnv <- getHscEnv
    md <- liftIO $ lookupModule hscEnv mdName pkgNameFS
    backendName <- liftIO
        $ TcRnMonad.initTcRnIf '?' hscEnv () ()
        $ IfaceEnv.lookupOrig md (mkTcOcc "Backend")
    (eps, hpt) <- liftIO $
        TcRnMonad.initTcRnIf '?' hscEnv () () TcRnMonad.getEpsAndHpt
    backendTyCon <- lookupTyCon backendName

    let getArrayAxiom ca@CoAxiom {..}
          | co_ax_tc == backendTyCon = Just ca
          | otherwise                = Nothing
        cas =  mapMaybe getArrayAxiom $ (do
          hmi <- maybeToList $ lookupHpt hpt (moduleName md)
          typeEnvCoAxioms . md_types $ hm_details hmi
          ) ++ typeEnvCoAxioms (eps_PTE eps)

    return $ case cas of
      []   -> panicDoc "Numeric.DataFrame.TcPlugin" $
        "Could not find instances of the closed type family" <> ppr backendTyCon
      ca:_ -> ca
  where
    mdName = mkModuleName "Numeric.DataFrame.Internal.Array.Family"
    pkgNameFS = fsLit "easytensor"




lookupModule :: HscEnv
             -> ModuleName
             -> FastString
             -> IO Module
lookupModule hscEnv mdName pkg = go [Just pkg, Just (fsLit "this")]
  where
    go [] = panicDoc "Numeric.DataFrame.TcPlugin" $
      "Could not find module " <> ppr mdName
    go (x:xs) = findIt x >>= \case
      Nothing -> go xs
      Just md -> return md

    findIt = fmap getIt . Finder.findImportedModule hscEnv mdName
    getIt (Found _ md) = Just md
    getIt (FoundMultiple ((md, _):_)) = Just md
    getIt _ = Nothing




-- data TcPluginResult
--   = TcPluginContradiction [Ct]
--     -- ^ The plugin found a contradiction.
--     -- The returned constraints are removed from the inert set,
--     -- and recorded as insoluble.
--
--   | TcPluginOk [(EvTerm,Ct)] [Ct]
--     -- ^ The first field is for constraints that were solved.
--     -- These are removed from the inert set,
--     -- and the evidence for them is recorded.
--     -- The second field contains new work, that should be processed by
--     -- the constraint solver.

runPluginSolve ::
     EtTcState
  -> [Ct] -- ^ given
  -> [Ct] -- ^ derived
  -> [Ct] -- ^ wanteds
  -> TcPluginM TcPluginResult
-- there is really nothing to do if wanteds are empty
runPluginSolve _ _ _ [] = return (TcPluginOk [] [])
-- actual solving is done here
runPluginSolve ets@EtTcState {..} givens _deriveds wanteds =
    case getFirst $ foldMap (First . checkGivenKnownBackend ets) givens of
      Nothing -> return (TcPluginOk [] [])
      Just dfGiven -> do
        -- mapM_ (printIt . ppr) $ replicate 100 ()
        backendWanteds <- catMaybes <$> mapM (checkWantedBackendInstance ets) wanteds
        if null backendWanteds
        then return (TcPluginOk [] [])
        else do
          minferBackendInstance <- lookupInferBackendInstance ets
          case minferBackendInstance of
            Nothing -> return (TcPluginOk [] [])
            Just inferBackendInstance ->
              firstChecked
                (solveBackendWanted ets inferBackendInstance dfGiven) backendWanteds
  where
    firstChecked _ [] = pure $ TcPluginOk [] []
    firstChecked f (x:xs) = do
      mr <- f x
      case mr of
        Nothing -> firstChecked f xs
        Just r  -> pure r


{- Background materials:

Lookup example usage of EvDFunApp in
https://github.com/isovector/constraints-emerge/blob/master/Data/Constraint/Emerge/Plugin.hs

Related Dict type is exactly the same as my Evidence type
https://www.stackage.org/haddock/lts-12.19/constraints-0.10.1/Data-Constraint.html

Blog post explanation:
https://qbaylogic.com/blog/2016/08/10/solving-knownnat-constraints-plugin.html

The first super simple example:
http://christiaanb.github.io/posts/type-checker-plugin/

Summary:

  EvTerm is a "constraint description"

  EvDFunApp DFunId [Type] [EvTerm] is an application of "constraint function" to
    a list of "constraint descriptions"

    * DFunId can be taken from class instance ClsInst using  instanceDFunId

    * [Type] is the list of type parameters of the class

    * [EvTerm] obvisouly is the list of resolved constraints of the class

To find a proper DFunId, I use a novel trick:
  Extract a class dictionary from the Evidence type (BareConstraint type).
  Then,  'InferBackendInstance t n c' class does the rest:
    * A InferBackendInstance is a dictionary (function)
    * but it is also just a newtype wrapper on top of its single function inferBackendInstance
    * inferBackendInstance is a no-parameter function - a BareConstraint value
    * BareConstraint is just an unsafeCoerced dictionary of 'c'
    * thus, 'InferBackendInstance t ds c' is the same as 'c'

-}



data EtTcState = EtTcState
  { knownBackendClass         :: Class
    -- ^ KnownBackend class keeps a singleton type for backend inference.
    --   At the same time, it serves as a beacon for the constraint plugin.
  , inferBackendInstanceClass :: Class
    -- ^ Our magic class that is used to find other instances
  , dataFrameTyCon            :: TyCon
    -- ^[Ty]pe [Con]structor for the DataFrame type.
  }

instance Outputable EtTcState where
  ppr EtTcState {..} = vcat
    [ "EtTcState"
    , "{ knownBackendClass         = " <> ppr knownBackendClass
    , ", inferBackendInstanceClass = " <> ppr inferBackendInstanceClass
    , ", dataFrameTyCon            = " <> ppr dataFrameTyCon
    , "}"
    ]


-- | Lookup necessary definitions
initEtTcState :: TcPluginM EtTcState
initEtTcState = do
    hscEnv <- getTopEnv
    md <- tcPluginIO $ lookupModule hscEnv afModule (fsLit "easytensor")
    inferBackendInstanceClass <- lookupClass md "InferBackendInstance"
    knownBackendClass <- lookupClass md "KnownBackend"
    dataFrameTyCon <- lookupDataFrameTyCon
    return EtTcState {..}
  where
    afModule  = mkModuleName "Numeric.DataFrame.Internal.Array.Family"


lookupClass :: Module -> String -> TcPluginM Class
lookupClass md n
  = lookupOrig md (mkTcOcc n) >>= tcLookupClass


lookupInferBackendInstance :: EtTcState -> TcPluginM (Maybe ClsInst)
lookupInferBackendInstance EtTcState {..} = do
    ie <- getInstEnvs
    return $ case classInstances ie inferBackendInstanceClass of
      [x] -> Just x
      _   -> Nothing


lookupDataFrameTyCon :: TcPluginM TyCon
lookupDataFrameTyCon = do
    hscEnv <- getTopEnv
    md <- tcPluginIO $ lookupModule hscEnv dfm (fsLit "easytensor")
    n  <- lookupOrig md (mkTcOcc "DataFrame")
    tcLookupTyCon n
  where
    dfm = mkModuleName "Numeric.DataFrame.Internal.Array.Family"



-- | Expanded description of a constraint like `SomeClass a1 .. an (Array t ds)`
data WantedBackendInstance = WantedBackendInstance
  { origWanted          :: Ct
    -- ^ Original wanted constraint
  , wantedClass         :: Class
    -- ^ The class I want to derive
  , wantedClassArgsInit :: [Type]
    -- ^ Arguments of the wanted class, as appeared in the constraint.
    --   Note, the last argument of the class must be `Array t1 t2`,
    --   but this list does not include it.
  , wantedClassLastArg  :: Type
    -- ^ The last argument of the wanted class. Must be `Array t1 t2`.
  }


instance Outputable WantedBackendInstance where
  ppr WantedBackendInstance {..} = vcat
    [ "WantedBackendInstance"
    , "{ origWanted          = " <> ppr origWanted
    , ", wantedClass         = " <> ppr wantedClass
    , ", wantedClassArgsInit = " <> ppr wantedClassArgsInit
    , ", wantedClassLastArg  = " <> ppr wantedClassLastArg
    , "}"
    ]

data GivenKnownBackend = GivenKnownBackend
  { origGiven     :: Ct
    -- ^ Original given constraint
  , dataFrameType :: Type
    -- ^ `DataFrame t ds`  in  `KnownBackend (DataFrame t ds)`
  , dataElemType  :: Type
    -- ^ The first argument of the DataFrame
  , dataDims      :: Type
    -- ^ The second argument of the DataFrame
  }

instance Outputable GivenKnownBackend where
  ppr GivenKnownBackend {..} = vcat
    [ "GivenKnownBackend"
    , "{ origGiven     = " <> ppr origGiven
    , ", dataFrameType = " <> ppr dataFrameType
    , ", dataElemType  = " <> ppr dataElemType
    , ", dataDims      = " <> ppr dataDims
    , "}"
    ]

checkGivenKnownBackend :: EtTcState -> Ct -> Maybe GivenKnownBackend
checkGivenKnownBackend EtTcState {..} origGiven =
  case classifyPredType $ ctEvPred $ ctEvidence origGiven of
    ClassPred givenClass [dataFrameType]
      | givenClass == knownBackendClass -> do
      (possiblyDFTyCon, possiblyDFArgs) <- tcSplitTyConApp_maybe dataFrameType
      case (possiblyDFTyCon == dataFrameTyCon, possiblyDFArgs) of
          (True, [possiblyTKind, possiblyNatKind, dataElemType, dataDims])
             | eqTypes [possiblyTKind, possiblyNatKind]
                       [liftedTypeKind, typeNatKind]
            -> Just GivenKnownBackend {..}
          _ -> Nothing
    _ -> Nothing


-- | Check if constraint is a Class [Pred]icate, such that
--   its last argument type is an unknown DataFrame backend
--   occured as a type variable.
checkWantedBackendInstance :: EtTcState -> Ct -> TcPluginM (Maybe WantedBackendInstance)
checkWantedBackendInstance EtTcState {..} origWanted =
  case classifyPredType $ ctEvPred $ ctEvidence origWanted of
    ClassPred wantedClass wcArgs
      | wantedClass /= knownBackendClass
      , Just (wantedClassArgsInit, ty') <- unSnoc wcArgs -> do
      wantedClassLastArg <- zonkTcType ty' -- classTyVars
      -- printIt $ "Testing inner type: "  <> ppr wantedClassLastArg
      -- printIt $ "Class: "  <> ppr (classBigSig wantedClass)
      -- printIt $ "CtLoc: "  <> ppr (ctLoc origWanted)
      return $
        case   not (isRuntimeRepKindedTy wantedClassLastArg)
            && not (isTypeLevPoly wantedClassLastArg)
            && isFamFreeTy wantedClassLastArg of
          True
            | Just True <- isLiftedType_maybe wantedClassLastArg
            -> Just WantedBackendInstance {..}
          _ -> Nothing
    _ -> pure Nothing

instance Outputable CtLoc where
  ppr CtLoc {..} = vcat
    [ "CtLoc"
    , "{ ctl_origin = " <> ppr ctl_origin
    , ", ctl_env    = " <> ppr ctl_env
    , ", ctl_t_or_k = " <> ppr ctl_t_or_k
    , ", ctl_depth  = " <> ppr ctl_depth
    , "}"
    ]

instance Outputable TcLclEnv where
  ppr TcLclEnv {..} = vcat
    [ "TcLclEnv"
    , "{ tcl_loc        = " <> ppr tcl_loc
    -- , ", tcl_ctxt       = " <> ppr (map (($ emptyTidyEnv) . snd)$ map snd tcl_ctxt)
    , ", tcl_tclvl      = " <> ppr tcl_tclvl
    , ", tcl_th_ctxt    = " <> ppr tcl_th_ctxt
    , ", tcl_th_bndrs   = " <> ppr tcl_th_bndrs
    -- , ", tcl_arrow_ctxt = " <> ppr tcl_arrow_ctxt
    , ", tcl_rdr        = " <> ppr tcl_rdr
    , ", tcl_env        = " <> vcat (map pprTcTyThing $ nameEnvElts tcl_env)
    , ", tcl_bndrs      = " <> vcat (map pprTcBinder tcl_bndrs)
    -- , ", tcl_tyvars     = " <> ppr tcl_tyvars
    -- , ", tcl_lie        = " <> ppr tcl_lie
    -- , ", tcl_errs       = " <> ppr tcl_errs
    , "}"
    ]

pprTcBinder :: TcIdBinder -> SDoc
pprTcBinder (TcIdBndr i f)
  = "TcIdBndr " <> ppr i <> " " <> ppr f
pprTcBinder (TcIdBndr_ExpType n t f)
  = "TcIdBndr_ExpType " <> ppr n <> " " <> ppr t <> " " <> ppr f

-- pprTcBinder :: TcBinder -> SDoc
-- pprTcBinder (TcIdBndr i f)
--   = "TcIdBndr " <> ppr i <> " " <> ppr f
-- pprTcBinder (TcIdBndr_ExpType n t f)
--   = "TcIdBndr_ExpType " <> ppr n <> " " <> ppr t <> " " <> ppr f
-- pprTcBinder (TcTvBndr i f)
--   = "TcTvBndr " <> ppr n <> " " <> ppr v



pprTcTyThing :: TcTyThing -> SDoc
pprTcTyThing (AGlobal tf)       = "AGlobal " <> ppr tf
pprTcTyThing (ATcId a b)        = "ATcId " <> ppr a <> " " <> ppr b
pprTcTyThing (ATyVar n v)       = "ATyVar " <> ppr n <> " " <> ppr v
pprTcTyThing (ATcTyCon tc)      = "ATcTyCon " <> ppr tc
pprTcTyThing (APromotionErr pe) = "APromotionErr " <> ppr pe
-- ATcId
-- tct_id :: TcId
-- tct_info :: IdBindingInfo
-- ATyVar Name TcTyVar
-- ATcTyCon TyCon
-- APromotionErr PromotionErr

unSnoc :: [a] -> Maybe ([a], a)
unSnoc []     = Nothing
unSnoc [x]    = Just ([], x)
unSnoc (x:xs) = first (x:) <$> unSnoc xs

-- | Occurrence of `DFBackend t n backend` allows establishing connection
--     Backend t n ~ backend
data DFBackendBinding = DFBackendBinding
  { dfBackendTyCon :: TyCon
    -- ^ Disassembled constructor
  , dfBackendType  :: Type
    -- ^ The whole occurrence
  , dfbElemType    :: Type
    -- ^ element type argument
  , dfbDimsType    :: Type
    -- ^ dimension type argument
  , dfbBackType    :: Type
    -- ^ last type argument
  }

instance Outputable DFBackendBinding where
  ppr DFBackendBinding {..} = vcat
    [ "DFBackendBinding"
    , "{ dfBackendTyCon = " <> ppr dfBackendTyCon
    , ", dfBackendType  = " <> ppr dfBackendType
    , ", dfbElemType    = " <> ppr dfbElemType
    , ", dfbDimsType    = " <> ppr dfbDimsType
    , ", dfbBackType    = " <> ppr dfbBackType
    , "}"
    ]

lookupDFBackendBinding :: Ct -> Maybe DFBackendBinding
lookupDFBackendBinding ct = lookupB $ ctPred ct
  where
    lookupB dfBackendType = case splitTyConApp_maybe dfBackendType of
      Nothing
        -> Nothing
      Just (dfBackendTyCon, tys)
        -> case (occName (tyConName dfBackendTyCon) == mkTcOcc "DFBackend", tys) of
          (True, [dfbElemType, dfbDimsType, dfbBackType])
            -> Just DFBackendBinding {..}
          _ -> getFirst $ foldMap (First . lookupB) tys


-- | Replace all occurrences of one type in another.
replaceTypeOccurrences :: Type -> Type -> Type -> Type
replaceTypeOccurrences told tnew = replace
  where
    replace :: Type -> Type
    replace t
        -- found occurrence
      | eqType t told
        = tnew
        -- split type constructors
      | Just (tyCon, tys) <- splitTyConApp_maybe t
        = mkTyConApp tyCon $ map replace tys
        -- split foralls
      | (bndrs@(_:_), t') <- splitPiTys t
        = mkPiTys bndrs $ replace t'
        -- split arrow types
      | Just (at, rt) <- splitFunTy_maybe t
        = mkFunTy (replace at) (replace rt)
        -- could not find anything
      | otherwise
        = t


lookForDFBackendBinding :: GivenKnownBackend
                        -> WantedBackendInstance
                        -> TcPluginM (Maybe DFBackendBinding)
lookForDFBackendBinding
  GivenKnownBackend {..}
  WantedBackendInstance {..}
    = lookupEveryWhere [] [origWanted, origGiven]
  where
    lookupEveryWhere _ [] = pure Nothing
    lookupEveryWhere pastRefs (ct:cts) = case lookupDFBackendBinding ct of
      Just b -> return (Just b)
      Nothing -> do
        let newRef = tcl_lie . ctl_env $ ctLoc ct
        if newRef `elem` pastRefs
        then lookupEveryWhere pastRefs cts
        else do
          moreWanteds <- unsafeTcPluginTcM $ readMutVar newRef
          let f :: WantedConstraints -> [Ct]
              f wcs = ctsElts (wc_simple wcs)
                    ++ (bagToList (wc_impl wcs) >>= f . ic_wanted)
              ncts = f moreWanteds
          lookupEveryWhere (newRef:pastRefs) $ cts ++ ncts


-- So far, this works, but tracing of the instance lookup function shows
-- that is is invoked on every function call, which is not so good.
-- I need to figure out the way to reduce number of lookups.
solveBackendWanted :: EtTcState
                   -> ClsInst
                   -> GivenKnownBackend
                   -> WantedBackendInstance
                   -> TcPluginM (Maybe TcPluginResult)
solveBackendWanted
  EtTcState {..}
  inferBIInst
  ctg@GivenKnownBackend {..}
  ctw@WantedBackendInstance {..} = do
    mdfbb <- lookForDFBackendBinding ctg ctw
    case mdfbb of
      Nothing -> return Nothing
      Just dfbb@DFBackendBinding {..} -> do
        wConstraints <- traverse (substSpecific dfbb) $ substTheta subst wConstraints'
        let replDFunType
               = -- mkSpecForAllTys tyVars
                                 -- [ getTyVar "oops t" dfbElemType
                                 -- , getTyVar "oops d" dfbDimsType
                                 -- , getTyVar "oops b" dfbBackType]
                 mkFunTys wConstraints
               $ mkTyConApp (classTyCon wantedClass) (wantedClassArgsInit ++ [wantedClassLastArg])
            replDFunType2 = case splitForAllTys origDFunType of
              (tv, funt) -> mkSpecForAllTys tv $
                 mkCastTy funt
                     (mkUnsafeCo Nominal funt replDFunType)
            replDFunId = setIdType origDFunId
              $ mkSpecForAllTys tyVars replDFunType
            replDFunIdC = setIdType origDFunId replDFunType2
        printIt $ "Orig type:" <> ppr origDFunType
        printIt $ "Repl type:" <> ppr replDFunType
        (ev, newWanteds) <- createInstanceEvTerm tyVarFun (ctLoc origWanted) replDFunId
        printIt $ "Created evidence:" <> ppr (ev, origWanted)
        return . Just $ TcPluginOk
          [ (ev, origWanted) ]
          newWanteds
  where
    origDFunId = instanceDFunId inferBIInst
    origDFunType = idType origDFunId
  --(dFunTyVars, dFunTyBody) = TcType.tcSplitForAllTys origDFunType
    -- replDFunType = origDFunType
    --              & replaceTypeOccurrences _ _
    --              & replaceTypeOccurrences _ _


    (tyVars, wConstraints', _, _) = instanceSig inferBIInst
    -- create a map of substitions {type vars -> required types}
    subst = case tyVars of
      [b, c] -> let add var ty s = extendTCvSubst s var ty
                   in emptyTCvSubst
                      & add c unaryClass
                      & add b wantedClassLastArg
      xs -> panicDoc "Numeric.DataFrame.TcPlugin" $
                     "Unexpected type variables: " <> ppr xs
    -- .. and substitute type variables in the instance declaration with real types.
    -- icTyArgs = substTys subst icTyArgs'
    -- addArgs =
    tyVarFun [_b, _c] =
          [ wantedClassLastArg
          , unaryClass
          ]
    tyVarFun xs = panicDoc "Numeric.DataFrame.TcPlugin" $
                       "Unexpected type variables: " <> ppr xs

    -- Create a type of kind (Type -> Constraint),
    -- so that mutli-parameter type classes can be derived
    --  if DF backend is their last type argument.
    unaryClass :: Type
    unaryClass = mkTyConApp (classTyCon wantedClass) wantedClassArgsInit

    mkType n args = do
      tcon <- lookupTCon n
      return $  mkTyConApp tcon args


    lookupTCon s = do
        hscEnv <- getTopEnv
        md <- tcPluginIO $ lookupModule hscEnv dfm (fsLit "easytensor")
        n  <- lookupOrig md (mkTcOcc s)
        tcLookupTyCon n
      where
        dfm = mkModuleName "Numeric.DataFrame.Internal.Array.Family"

    substSpecific :: DFBackendBinding -> PredType -> TcPluginM PredType
    substSpecific DFBackendBinding {..} pt = case classifyPredType pt of
      ClassPred cl args
        | True    <- cl == knownBackendClass
        , [dfc]   <- args
        , True    <- eqTypes [dfc, dataElemType, dataDims]
                             [dfbBackType, dfbElemType, dfbDimsType]
        , clTyCon <- classTyCon cl
          -> pure $ mkTyConApp clTyCon [dataFrameType]
        | otherwise -> do
          oldEltT <- mkType "DataElemType" [dfbBackType]
          oldDimT <- mkType "DataDims" [dfbBackType]
          let repl = replaceTypeOccurrences oldEltT dataElemType
                   . replaceTypeOccurrences oldDimT dataDims
          return $ repl pt
      _ -> pure pt


    -- NB: mkCastTy :: Type -> Coercion -> Type


-- So far, this works, but tracing of the instance lookup function shows
-- that is is invoked on every function call, which is not so good.
-- I need to figure out the way to reduce number of lookups.
solveBackendWanted' :: EtTcState
                   -> ClsInst
                   -> GivenKnownBackend
                   -> WantedBackendInstance
                   -> TcPluginM (Maybe TcPluginResult)
solveBackendWanted'
  EtTcState {..}
  inferBIInst
  ctg@GivenKnownBackend {..}
  ctw@WantedBackendInstance {..} = do
    mdfbb <- lookForDFBackendBinding ctg ctw
    -- printIt $ ppr $ instanceSig inferBIInst
    -- printIt $ ppr mdfbb
    case mdfbb of
      Nothing -> return Nothing
      Just dfbb -> do
        mevs <- parseConstraints dfbb wConstraints
        -- printIt $ ppr mevs
        return $ case mevs of
          Nothing -> Nothing
          Just (evTerms, newWanteds) -> Just $ TcPluginOk
              [ ( EvDFunApp (instanceDFunId inferBIInst) icTyArgs evTerms
                , origWanted)
              ]
              newWanteds
        -- mnewWanteds <- sequence <$> mapM (newWantedConstraint origLoc) wConstraints
        -- return $ flip fmap mnewWanteds $ \newWanteds ->
        --   TcPluginOk
        --     [ ( EvDFunApp (instanceDFunId inferBIInst) icTyArgs (map getCtEvTerm newWanteds)
        --       , origWanted)
        --     ]
        --     newWanteds
  where
    {-
     A very useful function:
       instanceSig :: ClsInst -> ([TyVar], [Type], Class, [Type])
                                 (tvs, theta, clas, tys)

     Its content:
       1. [TyVar] used type variables
       2. [Type] -- looks like predicate types -- constraints.
                    However, notes in TcType.hs suggest it may be a non-Pred type.
       3. Class  -- the class itself
       4. [Type] -- type paramaters of the instance

     From TcType.hs:

        -- Split the type of a dictionary function
        -- We don't use tcSplitSigmaTy,  because a DFun may (with NDP)
        -- have non-Pred arguments, such as
        --     df :: forall m. (forall b. Eq b => Eq (m b)) -> C m
        --
        -- Also NB splitFunTys, not tcSplitFunTys;
        -- the latter  specifically stops at PredTy arguments,
        -- and we don't want to do that here

    -}
    (tyVars, wConstraints', _, icTyArgs') = instanceSig inferBIInst
    -- create a map of substitions {type vars -> required types}
    subst = case tyVars of
      [b, c] -> let add var ty s = extendTCvSubst s var ty
                   in emptyTCvSubst
                      & add c unaryClass
                      & add b wantedClassLastArg
      xs -> panicDoc "Numeric.DataFrame.TcPlugin" $
                     "Unexpected type variables: " <> ppr xs
    -- .. and substitute type variables in the instance declaration with real types.
    wConstraints = substTheta subst wConstraints'
    icTyArgs = substTys subst icTyArgs'


    {- TODO: If I pass this CtLoc unmodified, the reported error location is incorrect.
       Here is an example:

          src/Numeric/DataFrame/Type.hs:1:1: error:
          Could not deduce (Eq t0) arising from a use of ‘/=’
          from the context: ArraySingleton t0 n
            bound by the instance declaration
            at src/Numeric/DataFrame/Type.hs:30:1-63
          Possible fix:
            add (Eq t0) to the context of the instance declaration
     -}
    origLoc = ctLoc origWanted

    -- Create a type of kind (Type -> Constraint),
    -- so that mutli-parameter type classes can be derived
    --  if DF backend is their last type argument.
    unaryClass :: Type
    unaryClass = mkTyConApp (classTyCon wantedClass) wantedClassArgsInit

    parseConstraints :: DFBackendBinding -> [PredType] -> TcPluginM (Maybe ([EvTerm], [Ct]))
    parseConstraints _ [] = return (Just ([],[]))
    parseConstraints dfbb@DFBackendBinding {..} (pt:pts) = case classifyPredType pt of
      ClassPred cl args
        | True    <- cl == knownBackendClass
        , [dfc]   <- args
        , True    <- eqTypes [dfc, dataElemType, dataDims]
                             [dfbBackType, dfbElemType, dfbDimsType]
        , clTyCon <- classTyCon cl
        , newEv   <- EvCast (getCtEvTerm origGiven) $
                       mkUnsafeCo Representational
                                  (mkTyConApp clTyCon [dataFrameType])
                                  (mkTyConApp clTyCon [dfbBackType])
          -> fmap (first (newEv:)) <$> parseConstraints dfbb pts
        | otherwise -> do
          oldEltT <- mkType "DataElemType" [dfbBackType]
          oldDimT <- mkType "DataDims" [dfbBackType]
          let repl = replaceTypeOccurrences oldEltT dataElemType
                   . replaceTypeOccurrences oldDimT dataDims
          newCt <- newWantedInstance origLoc (repl pt) cl (map repl args)
          fmap (( (EvCast (getCtEvTerm newCt) $
                     mkUnsafeCo Representational
                                (repl pt)
                                pt
                  )
                  :) *** (newCt :))
            <$> parseConstraints dfbb pts
      -- TODO: probably, I can extend this to other constraint types
      _ -> pure Nothing

    mkType n args = do
      tcon <- lookupTCon n
      return $  mkTyConApp tcon args


    lookupTCon s = do
        hscEnv <- getTopEnv
        md <- tcPluginIO $ lookupModule hscEnv dfm (fsLit "easytensor")
        n  <- lookupOrig md (mkTcOcc s)
        tcLookupTyCon n
      where
        dfm = mkModuleName "Numeric.DataFrame.Internal.Array.Family"


-- newWantedConstraint :: CtLoc -> PredType -> TcPluginM (Maybe Ct)
-- newWantedConstraint l pt =
--   case classifyPredType pt of
--     ClassPred cl args -> Just <$> newWantedInstance l pt cl args
--          -- TODO: probably, I can extend this to other constraint types
--     _ -> pure Nothing


-- | The InferBackendInstance instance for resolving the instance of the wanted
--   class pulls a few more instances;
--   this function creates a new wanted constraint for a required instance.
--   Feed the class constraint and its argument types here and get back
--   an evidence term for DFunId and a new wanted Ct.
newWantedInstance ::
     CtLoc -- ^ Location (where the error message pops up).
           --   The simplest option is to get location of the original wanted Ct.
  -> PredType -- ^ Wanted class (with type variables already substituted)
  -> Class -- ^ Wanted class itself
  -> [Xi]
     -- ^ Type arguments of the class.
     --
     -- From TcRnTypes.hs:
     -- cc_tyargs are function-free, hence Xi
     -- The syntax of xi (ξ) types:
     -- xi ::= a | T xis | xis -> xis | ... | forall a. tau
     -- Two important notes:
     --      (i) No type families, unless we are under a ForAll
     --      (ii) Note that xi types can contain unexpanded type synonyms;
     --           however, the (transitive) expansions of those type synonyms
     --           will not contain any type functions, unless we are under a ForAll.
     -- We enforce the structure of Xi types when we flatten (TcCanonical)
  -> TcPluginM Ct
newWantedInstance loc predTy cls tyArgs = do
    w <- newWanted loc predTy
    return $ CDictCan w cls tyArgs False
        -- Not sure about the last argument.
        -- From TcRnTypes.hs:
        -- See Note [The superclass story] in TcCanonical
        -- True <=> (a) cc_class has superclasses
        --          (b) we have not (yet) added those
        --              superclasses as Givens


-- | Ct always contains an EvTerm.
--   Thus, if I create a new wanted Ct then I always can get the corresponding
--   EvTerm (e.g. to put it into some EvDFunApp)
getCtEvTerm :: Ct -> EvTerm
getCtEvTerm = ctEvTerm . ctEvidence
{-# INLINE getCtEvTerm #-}


-- | Create EvTerm from a dictionary function by asking for all its arguments
--   via newWanteds.
createInstanceEvTerm :: ([TyVar] -> [Type]) -> CtLoc -> DFunId -> TcPluginM (EvTerm, [Ct])
createInstanceEvTerm tyArgF loc dFunId = do
    newWanteds <- traverse (newWantedConstraint loc) wConstraints
    return ( EvDFunApp dFunId (tyArgF tyVars) $ map getCtEvTerm newWanteds
           , newWanteds
           )
  where
    (tyVars, wConstraints, _, _) = TcType.tcSplitDFunTy $ idType dFunId

createInstanceEvTerm' :: ([TyVar] -> [Type]) -> CtLoc -> DFunId -> DFunId -> TcPluginM (EvTerm, [Ct])
createInstanceEvTerm' tyArgF loc dFunId dFunIdCasted = do
    newWanteds <- traverse (newWantedConstraint loc) wConstraints
    return ( EvDFunApp dFunIdCasted (tyArgF tyVars) $ map getCtEvTerm newWanteds
           , newWanteds
           )
  where
    (tyVars, wConstraints, _, _) = TcType.tcSplitDFunTy $ idType dFunId




newWantedConstraint :: CtLoc -> PredType -> TcPluginM Ct
newWantedConstraint loc = fmap mkNonCanonical . newWanted loc
  -- w <- newWanted loc predTy
  -- return $ case classifyPredType predTy of
  --   ClassPred cl args -> CDictCan w cl args False
  --   _ -> mkNonCanonical w

--------------------------------------------------------------------------------
-- DEBUG things

printIt :: SDoc -> TcPluginM ()
printIt = tcPluginIO . putStrLn . showSDocUnsafe
