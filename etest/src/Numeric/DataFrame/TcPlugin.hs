{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Numeric.DataFrame.TcPlugin (plugin) where

import           Class               (Class, classTyCon)
import           Control.Arrow       (first)
import           Data.Function       ((&))
import           Data.Maybe          (catMaybes)
import           GHC.TcPluginM.Extra (lookupModule, lookupName, tracePlugin)
import           GhcPlugins
import           InstEnv
import           Panic               (panicDoc)
import           TcEvidence
import           TcPluginM
import           TcRnTypes

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
runPluginSolve ets@EtTcState {..} _givens _deriveds wanteds = do
    arrayWanteds <- catMaybes <$> mapM (checkArrayClassCt ets) wanteds
    if null arrayWanteds
    then return (TcPluginOk [] [])
    else do
      minferBackendInstance <- lookupInferBackendInstance ets
      case minferBackendInstance of
        Nothing -> return (TcPluginOk [] [])
        Just inferBackendInstance ->
          solveArrayWanted inferBackendInstance (head arrayWanteds)


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
  { arrayTyCon          :: TyCon
    -- ^ [Ty]pe [Con]structor for the type family `Array`.
    --   Its equations enumerate possible DataFrame backends.
  -- , arrayInstances      :: CoAxiom Branched
  --   -- ^ List of family instances
  , inferBackendClass   :: Class
    -- ^ Our magic class that is used to find other instances
  }



-- | Lookup necessary definitions
initEtTcState :: TcPluginM EtTcState
initEtTcState = do
    md <- lookupModule afModule (fsLit "easytensor")

    arrayTyCon <- lookupArrayTyCon md
    inferBackendClass <- lookupInferBackendClass md
    -- arrayInstances <- lookupArrayFamily md arrayTyCon

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
      _   -> Nothing


-- | Lookup the class which will serve as our special constraint.
lookupArrayTyCon :: Module -> TcPluginM TyCon
lookupArrayTyCon md = do
    n  <- lookupName md (mkTcOcc "Array")
    tcLookupTyCon n

-- Not needed right now.
-- lookupArrayFamily :: Module -> TyCon -> TcPluginM (CoAxiom Branched)
-- lookupArrayFamily md arrTyCon = do
--     (eps, hpt) <- unsafeTcPluginTcM getEpsAndHpt
--
--     let cas =  mapMaybe getArrayAxiom $ (do
--           hmi <- maybeToList $ lookupHpt hpt (moduleName md)
--           typeEnvCoAxioms . md_types $ hm_details hmi
--           ) ++ typeEnvCoAxioms (eps_PTE eps)
--
--     return $ case cas of
--       []   -> panicDoc "Numeric.DataFrame.TcPlugin" $
--         "Could not find instances of the closed type family" <> ppr arrTyCon
--       ca:_ -> ca
--   where
--     getArrayAxiom ca@CoAxiom {..}
--           | co_ax_tc == arrTyCon = Just ca
--           | otherwise            = Nothing


-- | Expanded description of a constraint like `SomeClass a1 .. an (Array t ds)`
data WantedArrayInstance = WantedArrayInstance
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
  , arrElemType         :: Type
    -- ^ The first argument of the type family `Array`
  , arrDims             :: Type
    -- ^ The second argument of the type family `Array`
  }

instance Outputable WantedArrayInstance where
  ppr WantedArrayInstance {..} = vcat
    [ "Wanted Array Instance"
    , "{ origWanted          = " <> ppr origWanted
    , ", wantedClass         = " <> ppr wantedClass
    , ", wantedClassArgsInit = " <> ppr wantedClassArgsInit
    , ", wantedClassLastArg  = " <> ppr wantedClassLastArg
    , ", arrElemType         = " <> ppr arrElemType
    , ", arrDims             = " <> ppr arrDims
    , "}"
    ]


-- | Check if constraint is a Class [Pred]icate, such that
--   its last argument type is Array (unresolved DataFrame backend).
checkArrayClassCt :: EtTcState -> Ct -> TcPluginM (Maybe WantedArrayInstance)
checkArrayClassCt EtTcState {..} origWanted =
  -- first, filter the class predicates with a single
  case classifyPredType $ ctEvPred $ ctEvidence origWanted of
    ClassPred wantedClass wcArgs
      | Just (wantedClassArgsInit, ty') <- unSnoc wcArgs -> do
      wantedClassLastArg <- zonkTcType ty'
      return $ do
        (possiblyArrayTyCon, possiblyArrayArgs) <- tcSplitTyConApp_maybe wantedClassLastArg
        case (possiblyArrayTyCon == arrayTyCon, possiblyArrayArgs) of
            (True, [arrElemType, arrDims])
              -> return WantedArrayInstance {..}
            _ -> Nothing
    _ -> pure Nothing
  where
    unSnoc []     = Nothing
    unSnoc [x]    = Just ([], x)
    unSnoc (x:xs) = first (x:) <$> unSnoc xs



-- So far, this works, but tracing of the instance lookup function shows
-- that is is invoked on every function call, which is not so good.
-- I need to figure out the way to reduce number of lookups.
solveArrayWanted :: ClsInst -- ^ InferBackendInstance
                 -> WantedArrayInstance -- ^ Single constraint that involves Array
                 -> TcPluginM TcPluginResult
solveArrayWanted
  inferBIInst
  WantedArrayInstance {..} = do
    newWanteds <- mapM (newWantedConstraint origLoc) wConstraints
    return $
      TcPluginOk
        [ ( EvDFunApp (instanceDFunId inferBIInst) icTyArgs (map getCtEvTerm newWanteds)
          , origWanted)
        ]
        newWanteds
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
      [t, n, c] -> let add var ty s = extendTCvSubst s var ty
                   in emptyTCvSubst
                      & add t arrElemType
                      & add n arrDims
                      & add c unaryClass
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


newWantedConstraint :: CtLoc -> PredType -> TcPluginM Ct
newWantedConstraint l pt =
  -- first, filter the class predicates with a single
  case classifyPredType pt of
    ClassPred cl args -> newWantedInstance l pt cl args
         -- TODO: probably, I can extend this to other constraint types
    _ -> panicDoc "Numeric.DataFrame.TcPlugin" $
                   "Expected class constraint, but got: " <> ppr pt


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

--------------------------------------------------------------------------------
-- DEBUG things

-- printIt :: SDoc -> TcPluginM ()
-- printIt = tcPluginIO . putStrLn . showSDocUnsafe
