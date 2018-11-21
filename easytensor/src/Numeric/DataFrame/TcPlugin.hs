{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Numeric.DataFrame.TcPlugin (plugin) where

import           GHC.TcPluginM.Extra
import           GhcPlugins
import           TcPluginM
import           TcRnTypes
import           Class (Class)
import           Data.Maybe (catMaybes)

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
      return (TcPluginOk [] [])



data EtTcState = EtTcState
  { arraySingletonTyCon :: TyCon
    -- ^ [Ty]pe [Con]structor for the class `ArraySingleton`.
    --   I hook up special solving behavior to this, otherwise ordinary,
    --   class with a single function.
  , arrayTyCon :: TyCon
    -- ^ [Ty]pe [Con]structor for the type family `Array`.
    --   Its equations enumerate possible DataFrame backends.
  }

-- | Lookup necessary definitions
initEtTcState :: TcPluginM EtTcState
initEtTcState = do
  arraySingletonTyCon <- lookupArraySingletonTyCon
  arrayTyCon <- lookupArrayTyCon
  return EtTcState {..}

-- | Lookup the class which will serve as our special constraint.
lookupArraySingletonTyCon :: TcPluginM TyCon
lookupArraySingletonTyCon = do
    md      <- lookupModule asModule (fsLit "easytensor")
    gcdTcNm <- lookupName md (mkTcOcc "ArraySingleton")
    tcLookupTyCon gcdTcNm
  where
    asModule  = mkModuleName "Numeric.DataFrame.Internal.Array.Family"

-- | Lookup the class which will serve as our special constraint.
lookupArrayTyCon :: TcPluginM TyCon
lookupArrayTyCon = do
    md      <- lookupModule asModule (fsLit "easytensor")
    gcdTcNm <- lookupName md (mkTcOcc "Array")
    tcLookupTyCon gcdTcNm
  where
    asModule  = mkModuleName "Numeric.DataFrame.Internal.Array.Family"


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
