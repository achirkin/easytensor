{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Numeric.DataFrame.TcPlugin (plugin) where

import           GHC.TcPluginM.Extra
                    (lookupName, lookupModule, tracePlugin
                    )
import           GhcPlugins
import           TcPluginM
import           TcRnTypes
import           Class (Class, classTyCon)
import           Data.Maybe (catMaybes, mapMaybe, maybeToList)
import           TcRnMonad (getEpsAndHpt)
import           CoAxiom (CoAxiom (..), fromBranches, Branched)
import           Panic (panicDoc)
import           InstEnv
import           TcEvidence

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
      minferBackendInstance <- lookupInferBackendInstance ets
      case minferBackendInstance of
        Nothing -> return (TcPluginOk [] [])
        Just inferBackendInstance -> do

          printIt $ "Givens: " <> ppr givens
          printIt $ "Deriveds: " <> ppr deriveds
          printIt $ "Wanteds: " <> ppr arrayWanteds
          printIt $ ppr arrayInstances
          printIt $ ppr inferBackendInstance

          solveArrayWanted ets inferBackendInstance (head arrayWanteds)

          -- return (TcPluginOk [] [])

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
  , arraySingletonClass :: Class
    -- ^ The corresponding class
  , arrayTyCon :: TyCon
    -- ^ [Ty]pe [Con]structor for the type family `Array`.
    --   Its equations enumerate possible DataFrame backends.
  , arrayInstances :: CoAxiom Branched
    -- ^ List of family instances
  , inferBackendClass :: Class
    -- ^ Our magic class that is used to find other instances
  }



-- | Lookup necessary definitions
initEtTcState :: TcPluginM EtTcState
initEtTcState = do
    md <- lookupModule afModule (fsLit "easytensor")

    arraySingletonTyCon <- lookupArraySingletonTyCon md
    arraySingletonClass <- lookupArraySingletonClass md
    arrayTyCon <- lookupArrayTyCon md
    inferBackendClass <- lookupInferBackendClass md
    arrayInstances <- lookupArrayFamily md arrayTyCon

    return EtTcState {..}
  where
    afModule  = mkModuleName "Numeric.DataFrame.Internal.Array.Family"


lookupInferBackendClass :: Module -> TcPluginM Class
lookupInferBackendClass md = do
    n  <- lookupName md (mkTcOcc "InferBackendInstance")
    tcLookupClass n

lookupInferBackendInstance :: EtTcState -> TcPluginM (Maybe ClsInst)
lookupInferBackendInstance EtTcState {..} = do
    ie <- getInstEnvs
    return $ case classInstances ie inferBackendClass of
      [x] -> Just x
      _ -> Nothing


-- | Lookup the class which will serve as our special constraint.
lookupArraySingletonTyCon :: Module -> TcPluginM TyCon
lookupArraySingletonTyCon md = do
    n  <- lookupName md (mkTcOcc "ArraySingleton")
    tcLookupTyCon n

-- | Lookup the class which will serve as our special constraint.
lookupArraySingletonClass :: Module -> TcPluginM Class
lookupArraySingletonClass md = do
    n  <- lookupName md (mkTcOcc "ArraySingleton")
    tcLookupClass n


-- | Lookup the class which will serve as our special constraint.
lookupArrayTyCon :: Module -> TcPluginM TyCon
lookupArrayTyCon md = do
    n  <- lookupName md (mkTcOcc "Array")
    tcLookupTyCon n



lookupArrayFamily :: Module -> TyCon -> TcPluginM (CoAxiom Branched)
lookupArrayFamily md arrTyCon = do
    (eps, hpt) <- unsafeTcPluginTcM getEpsAndHpt

    let cas =  mapMaybe getArrayAxiom $ (do
          hmi <- maybeToList $ lookupHpt hpt (moduleName md)
          typeEnvCoAxioms . md_types $ hm_details hmi
          ) ++ typeEnvCoAxioms (eps_PTE eps)


    printIt . ppr $ flip map cas $ \ca@CoAxiom {..} ->
      (co_ax_unique, co_ax_name, co_ax_role, co_ax_tc, fromBranches co_ax_branches, co_ax_implicit, ca)

    return $ case cas of
      []   -> panicDoc "Numeric.DataFrame.TcPlugin" $
        "Could not find instances of the closed type family" <> ppr arrTyCon
      ca:_ -> ca
  where
    getArrayAxiom ca@CoAxiom {..}
          | co_ax_tc == arrTyCon = Just ca
          | otherwise            = Nothing


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




-- So far, this works, but tracing of the instance lookup function shows
-- that is is invoked on every function call, which is not so good.
-- I need to figure out the way to reduce number of lookups.
solveArrayWanted :: EtTcState -- ^ plugin state
                 -> ClsInst -- ^ InferBackendInstance
                 -> WantedArrayInstance -- ^ Single constraint that involves Array
                 -> TcPluginM TcPluginResult
solveArrayWanted
  EtTcState {..}
  inferBIInst
  WantedArrayInstance {..} = do

    printIt $ ppr $ instanceHead inferBIInst
    printIt $ ppr [arrElemType, arrDims, unaryClass]

    (a0b, c0) <- lookupConstraint "UnitBase"   [arrElemType]
    (a1b, c1) <- lookupConstraint "ScalarBase" [arrElemType]
    (a2b, c2) <- lookupConstraint "Vec2Base"   [arrElemType]
    (anb, cn) <- lookupConstraint "ListBase"   [arrElemType, arrDims]


    wArraySingletonTyCon
        <- newWanted origLoc
                        $ mkTyConApp arraySingletonTyCon [arrElemType, arrDims]
    w0b <- newWanted origLoc c0
    w1b <- newWanted origLoc c1
    w2b <- newWanted origLoc c2
    wnb <- newWanted origLoc cn

    -- TODO: tedious: need to lookup all instances manually?..
    return (TcPluginOk
              [(EvDFunApp (is_dfun inferBIInst)
                [arrElemType, arrDims, unaryClass]
                [ toEvTerm wArraySingletonTyCon
                , toEvTerm w0b
                , toEvTerm w1b
                , toEvTerm w2b
                , toEvTerm wnb
                ], origWanted)]
              [ CDictCan wArraySingletonTyCon arraySingletonClass [arrElemType, arrDims] False
              , CDictCan w0b wantedClass a0b False
              , CDictCan w1b wantedClass a1b False
              , CDictCan w2b wantedClass a2b False
              , CDictCan wnb wantedClass anb False
              ])
  where
    toEvTerm CtWanted {ctev_dest = EvVarDest ev} = EvId ev
    toEvTerm CtGiven {ctev_evar = ev } = EvId ev
    toEvTerm _ = undefined
    origLoc = ctev_loc $ cc_ev origWanted
    lookupConstraint n xs = do
      t <- lookupATyCon n
      let args = staticArgs ++ [mkTyConApp t xs]
      printIt $ ppr (staticArgs, xs, args, mkTyConApp (classTyCon wantedClass) args)
      return (args, mkTyConApp (classTyCon wantedClass) args)
    staticArgs = take (length wantedClassArgs - 1) wantedClassArgs
    unaryClass = mkTyConApp (classTyCon wantedClass) staticArgs

    lookupATyCon n = do
        let afModule = mkModuleName "Numeric.DataFrame.Internal.Array.Family"
        md <- lookupModule afModule (fsLit "easytensor")
        na  <- lookupName md (mkTcOcc n)
        tcLookupTyCon na


--------------------------------------------------------------------------------
-- DEBUG things, delete it later.


printIt :: SDoc -> TcPluginM ()
printIt = tcPluginIO . putStrLn . showSDocUnsafe
