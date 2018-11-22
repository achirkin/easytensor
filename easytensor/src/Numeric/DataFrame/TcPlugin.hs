{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Numeric.DataFrame.TcPlugin (plugin) where

import           GHC.TcPluginM.Extra
import           GhcPlugins
import           TcPluginM
import           TcRnTypes
import           Class (Class)
import           Data.Maybe (catMaybes, mapMaybe, maybeToList)
import           FamInstEnv (familyInstances, FamInst)
import           TcRnMonad (getEpsAndHpt)
import           IfaceSyn (IfaceDecl (..))
import           CoAxiom (CoAxiom (..), fromBranches, Branches (..), Branched)
import           Panic (panicDoc)
import           IfaceType (ifaceTyConName)

-- | To use the plugin, add
--
-- @
-- {\-\# OPTIONS_GHC -fplugin Numeric.DataFrame.TcPlugin \#-\}
-- @
--
-- To the header of your file.
plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just
                     $ tracePlugin "Numeric.DataFrame.TcPlugin"
                       TcPlugin
      { tcPluginInit  = initEtTcState
      , tcPluginSolve = runPluginSolve
      , tcPluginStop  = const (return ())
      }
#if MIN_VERSION_ghc(8,6,0)
  , pluginRecompile = purePlugin
#endif
  }


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
runPluginSolve ets@EtTcState {..} givens deriveds wanteds = do
    arrayWanteds <- catMaybes <$> mapM (checkArrayClassCt ets) wanteds
    if null arrayWanteds
    then return (TcPluginOk [] [])
    else do
      printIt $ "Givens: " <> ppr givens
      printIt $ "Deriveds: " <> ppr deriveds
      printIt $ "Wanteds: " <> ppr arrayWanteds
      printIt $ ppr arrayInstances
      printIt $ ppr arraySingDecl

      return (TcPluginOk [] [])

{- Roadmap:

Lookup usage of EvDFunApp in
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

    * in Emerge, [Type] is the list of PredTypes;
      looks like it comes from splitTyConApp_maybe types - i.e. a list of types
        this class was applied to in the Wanted constraint.

    * [EvTerm] obvisouly is the list of resolved constraints of the class

What missing here is how am I supposed to create a new DFunId or a new instance?
Probably, I need to make a special class that can be used in a generalized way
like inferClassInstace :: c t => ArraySing t ds -> Evidence (c (Array t ds))

-}



data EtTcState = EtTcState
  { arraySingletonTyCon :: TyCon
    -- ^ [Ty]pe [Con]structor for the class `ArraySingleton`.
    --   I hook up special solving behavior to this, otherwise ordinary,
    --   class with a single function.
  , arrayTyCon :: TyCon
    -- ^ [Ty]pe [Con]structor for the type family `Array`.
    --   Its equations enumerate possible DataFrame backends.
  , arrayInstances :: IfaceDecl -- CoAxiom Branched
    -- ^ [IfaceAxiom] List of family instances
  , arraySingDecl :: IfaceDecl
    -- ^ [IfaceData] Declaration of the GADT used for restoring constraints
  }

-- | Lookup necessary definitions
initEtTcState :: TcPluginM EtTcState
initEtTcState = do
    md <- lookupModule afModule (fsLit "easytensor")
    iface <- lookupArrayModuleIface md

    arraySingletonTyCon <- lookupArraySingletonTyCon md
    arraySingTyCon <- lookupArraySingTyCon md
    arrayTyCon <- lookupArrayTyCon md
    let arrayInstances = lookupArrayInstances arrayTyCon iface
        arraySingDecl = lookupSingDecl arraySingTyCon iface
    printIt $ ppr arrayInstances
    printIt $ ppr arraySingDecl
    return EtTcState {..}
  where
    afModule  = mkModuleName "Numeric.DataFrame.Internal.Array.Family"


-- | Get the interface of the module where the backend type family
--   and corresponding singletons are defined.
lookupArrayModuleIface :: Module -> TcPluginM ModIface
lookupArrayModuleIface md = unsafeTcPluginTcM $ do
    dflags     <- getDynFlags
    (eps, hpt) <- getEpsAndHpt
    case lookupIfaceByModule dflags hpt (eps_PIT eps) md of
      Nothing -> panicDoc "Numeric.DataFrame.TcPlugin" $
        "Could not find the interface for module " <> ppr md
      Just iface -> return iface


-- | Lookup the class which will serve as our special constraint.
lookupArraySingletonTyCon :: Module -> TcPluginM TyCon
lookupArraySingletonTyCon md = do
    n  <- lookupName md (mkTcOcc "ArraySingleton")
    tcLookupTyCon n

-- | Lookup the evidence GADT
lookupArraySingTyCon :: Module -> TcPluginM TyCon
lookupArraySingTyCon md = do
    n  <- lookupName md (mkTcOcc "ArraySing")
    tcLookupTyCon n


-- | Lookup the class which will serve as our special constraint.
lookupArrayTyCon :: Module -> TcPluginM TyCon
lookupArrayTyCon md = do
    n  <- lookupName md (mkTcOcc "Array")
    tcLookupTyCon n


lookupArrayInstances :: TyCon -> ModIface -> IfaceDecl
lookupArrayInstances arrTyCon iface = wantOneDecl ("declaration of " <> ppr arrTyCon) axioms
  where
    axioms = mapMaybe getArrayAxiom $ mi_decls iface
    getArrayAxiom (_, ia@IfaceAxiom {..} )
      | ifaceTyConName ifTyCon == tyConName arrTyCon = Just ia
    getArrayAxiom _                                  = Nothing

lookupSingDecl :: TyCon -> ModIface -> IfaceDecl
lookupSingDecl arsTyCon iface = wantOneDecl ("declaration of " <> ppr arsTyCon) ds
  where
    ds = mapMaybe getAS $ mi_decls iface
    getAS (_, x@IfaceData {..} ) | ifName == tyConName arsTyCon = Just x
    getAS _                                                     = Nothing

wantOneDecl :: SDoc -> [a] -> a
wantOneDecl _    [x] = x
wantOneDecl thing xs = panicDoc "Numeric.DataFrame.TcPlugin" $
  "Expected to find exactly one " <> thing <> " but found " <> ppr (length xs)

-- lookupArrayInstances :: TyCon -> ModIface -> TcPluginM (CoAxiom Branched)
-- lookupArrayInstances arrTyCon iface = do
--
--     let cas =  mapMaybe getArrayAxiom $ do
--           lookupIfaceByModule dflags
--           hmi <- maybeToList $ lookupHpt hpt (moduleName md)
--           atHome <- typeEnvCoAxioms . md_types $ hm_details hmi
--           inInstalled <- typeEnvCoAxioms $ eps_PTE eps
--           [atHome, inInstalled]
--
--     printIt . ppr $ flip map cas $ \ca@CoAxiom {..} ->
--       (co_ax_unique, co_ax_name, co_ax_role, co_ax_tc, fromBranches co_ax_branches, co_ax_implicit, ca)
--
--     case cas of
--       [ca] -> return ca
--       []   -> fail "Error at the plugin initialization stage: could not find Array type family"
--       _    -> fail "Error at the plugin initialization stage: found multiple Array type family declarations."
--   where
--     getArrayAxiom ca@CoAxiom {..}
--           | co_ax_tc == arrTyCon = Just ca
--           | otherwise            = Nothing


-- | Expanded description of a constraint like `SomeClass a1 .. an (Array t ds)`
data WantedArrayInstance = WantedArrayInstance
  { origWanted :: Ct
    -- ^ Original wanted constraint
  , wantedClass :: Class
    -- ^ The class I want to derive
  , wantedClassArgs :: [Type]
    -- ^ Arguments of the wanted class, as appeared in the constraint.
    --   Note, the last argument must be `Array t1 t2` here.
  , arrElemType :: Type
    -- ^ The first argument of the type family `Array`
  , arrDims :: Type
    -- ^ The second argument of the type family `Array`
  }

instance Outputable WantedArrayInstance where
  ppr WantedArrayInstance {..} = vcat
    [ "Wanted Array Instance"
    , "{ origWanted      = " <> ppr origWanted
    , ", wantedClass     = " <> ppr wantedClass
    , ", wantedClassArgs = " <> ppr wantedClassArgs
    , ", arrElemType     = " <> ppr arrElemType
    , ", arrDims         = " <> ppr arrDims
    , "}"
    ]


-- | Check if constraint is a Class [Pred]icate, such that
--   its last argument type is Array (unresolved DataFrame backend).
checkArrayClassCt :: EtTcState -> Ct -> TcPluginM (Maybe WantedArrayInstance)
checkArrayClassCt EtTcState {..} origWanted =
  -- first, filter the class predicates with a single
  case classifyPredType $ ctEvPred $ ctEvidence origWanted of
    ClassPred wantedClass wantedClassArgs@(_:_) -> do
      ty <- zonkTcType (last wantedClassArgs)
      return $ do
        (possiblyArrayTyCon, possiblyArrayArgs) <- tcSplitTyConApp_maybe ty
        case (possiblyArrayTyCon == arrayTyCon, possiblyArrayArgs) of
            (True, [arrElemType, arrDims])
              -> return WantedArrayInstance {..}
            _ -> Nothing
    _ -> pure Nothing








--------------------------------------------------------------------------------
-- DEBUG things, delete it later.


printIt :: SDoc -> TcPluginM ()
printIt = tcPluginIO . putStrLn . showSDocUnsafe
