-- {-# OPTIONS_GHC -fobject-code   #-}
{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.List.InjectiveSnoc
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- A small core plugin to make GHC think that @Snoc@ is injective
--
--
--------------------------------------------------------------------------------
module Data.Type.List.InjectiveSnoc ( plugin ) where


import           Avail
import           BooleanFormula (mkTrue)
import           BuildTyCl      (buildClass)
import           Class
import           CoAxiom
import           Data.IORef
import           FamInstEnv     (mkBranchedCoAxiom, mkCoAxBranch)
import           GhcPlugins
import           InstEnv
import           IOEnv          (fixM, getEnv, readMutVar, runIOEnv, updMutVar)
import           MkId           (mkDictFunId)
import qualified OccName        as OccName
import           TcMType        (freshenTyVarBndrs)
import           TcRnTypes      (Env (..), TcRnIf)
import           TysPrim
import           TcRnMonad (getTopEnv, initTcInteractive)

-- | A small core plugin to make GHC think that @Snoc@ is injective
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos =
      const $ pure . (CoreDoPluginPass "InjectiveSnoc" injectiveSnocPass :)
  }

injectiveSnocPass :: ModGuts -> CoreM ModGuts
injectiveSnocPass guts = do
    cSnocList <- mkClassSnocList
    (iSnocList, eSnocList) <- mkInstSnocList cSnocList
    let [ATI tcSnoc _, ATI tcInit _, ATI tcLast _] = classATItems cSnocList
        snocClsTyCon = classTyCon cSnocList

    cReverseList <- mkClassReverseList
    (iReverseList, eReverseList) <- mkInstReverseList cReverseList
    let [ATI tcReverse _, ATI tcReverse' _] = classATItems cReverseList
        tcReverseList = classTyCon cReverseList
    
    return guts
          { mg_tcs
              = snocClsTyCon : tcSnoc : tcInit : tcLast
              : tcReverseList : tcReverse : tcReverse'
              : mg_tcs guts
          , mg_exports
              = AvailTC (getName cSnocList)
                [getName cSnocList, getName tcSnoc, getName tcInit, getName tcLast] []
              : AvailTC (getName cReverseList)
                [getName cReverseList, getName tcReverse] []
              : mg_exports guts
          , mg_insts
              = iSnocList
              : iReverseList
              : mg_insts guts
          , mg_binds
              = eSnocList
              : eReverseList
              : mg_binds guts
          }



mkClassSnocList :: CoreM Class
mkClassSnocList = liftThinCoreM $ do
    k  <- genTyVar "k" liftedTypeKind
    let kKind = mkTyVarTy k
        ksKind = mkListTy kKind
    xs <- genTyVar "xs" ksKind
    x  <- genTyVar "x"  kKind
    ys <- genTyVar "ys" ksKind

    let xsT = mkTyVarTy xs
        ysT = mkTyVarTy ys
        xT = mkTyVarTy x
        funDeps = -- [FunDep TyVar]
          [ ([xs, x], [ys]), ([ys], [xs, x])
          , ([xs],[k]), ([x],[k]), ([ys],[k])]
        constraints = WithTypeFamilies $ \(~[tcSnoc, tcInit, tcLast]) ->
          [ -- ys ~ Snoc xs x :: [PredType]
            mkHeqType
              ksKind ksKind ysT (mkTyConApp tcSnoc [kKind, xsT, xT])
            -- xs ~ Init ys
          , mkHeqType
              ksKind ksKind xsT (mkTyConApp tcInit [kKind, ysT])
            -- x ~ Last ys
          , mkHeqType
              kKind kKind xT (mkTyConApp tcLast [kKind, ysT])
          ]
        tyVarBndrs =
          [ Bndr k (NamedTCB Specified)
          , Bndr xs AnonTCB
          , Bndr x  AnonTCB
          , Bndr ys AnonTCB]
    makeClassAndFamilies "SnocList"
      tyVarBndrs funDeps constraints $ WithTypeFamilies $ \(~[tcSnoc, tcInit, tcLast]) ->
      [ ( "Snoc"
        , ksKind
        , Just (getName ys)
        , [ Bndr k  (NamedTCB Specified)
          , Bndr xs AnonTCB
          , Bndr x  AnonTCB]
        , Injective [True,True,True]
        , [ do ki <- new "k" liftedTypeKind
               a  <- new "a" ki
               as <- new "as" $ mkListTy ki
               b  <- new "b" ki
               [ki, cons ki a as, b] ~> cons ki a (mkTyConApp tcSnoc [ki, as, b])
          , do ki <- new "k" liftedTypeKind
               b  <- new "b" ki
               [ki, nil ki, b] ~> list ki [b]
          ]
        )
      , ( "Init"
        , ksKind
        , Just (getName xs)
        , [ Bndr k  (NamedTCB Specified)
          , Bndr ys AnonTCB]
        , Injective [True,False]
        , [ do ki <- new "k" liftedTypeKind
               a  <- new "a" ki
               [ki, list ki [a]] ~> nil ki
          , do ki <- new "k" liftedTypeKind
               a  <- new "a" ki
               as <- new "as" $ mkListTy ki
               [ki, cons ki a as] ~> cons ki a (mkTyConApp tcInit [ki, as])
          ]
        )
      , ( "Last"
        , kKind
        , Just (getName x)
        , [ Bndr k  (NamedTCB Specified)
          , Bndr ys AnonTCB]
        , Injective [True,False]
        , [ do ki <- new "k" liftedTypeKind
               a  <- new "a" ki
               [ki, list ki [a]] ~> a
          , do ki <- new "k" liftedTypeKind
               a  <- new "a" ki
               as <- new "as" $ mkListTy ki
               [ki, cons ki a as] ~> mkTyConApp tcLast [ki, as]
          ]
        )
      ]

mkInstSnocList :: Class -> CoreM (ClsInst, CoreBind)
mkInstSnocList snocListClass
  | [ATI snocTf _, ATI initTf _, ATI lastTf _] <- classATItems snocListClass
    = liftThinCoreM $ do
      k  <- genTyVar "k" liftedTypeKind
      let kKind = mkTyVarTy k
          ksKind = mkListTy kKind
      xs <- genTyVar "xs" ksKind
      x  <- genTyVar "x"  kKind
      ys <- genTyVar "ys" ksKind

      dfunName <- genExternalName OccName.varName "inst$SnocList"

      let xsT = mkTyVarTy xs
          ysT = mkTyVarTy ys
          xT = mkTyVarTy x
          constraints =
            [ -- ys ~ Snoc xs x :: [PredType]
              mkHeqType
               ksKind ksKind ysT (mkTyConApp snocTf [kKind, xsT, xT])
            ]
          genConstraints = \[snocEq] ->
            [ Var snocEq
            , mkHeqExpr ksKind ksKind xsT (mkTyConApp initTf [kKind, ysT])
            , mkHeqExpr kKind kKind xT (mkTyConApp lastTf [kKind, ysT])
            ]

      makeClsInst dfunName [k, xs, x, ys]
        constraints genConstraints snocListClass [kKind, xsT, xT, ysT]
  | otherwise
    = error "mkInstSnocList: Unexpected class"


mkClassReverseList :: CoreM Class
mkClassReverseList = liftThinCoreM $ do
    k  <- genTyVar "k" liftedTypeKind
    let kKind = mkTyVarTy k
        ksKind = mkListTy kKind
    xs <- genTyVar "xs" ksKind
    ts <- genTyVar "ts" ksKind
    ys <- genTyVar "ys" ksKind

    let xsT = mkTyVarTy xs
        ysT = mkTyVarTy ys
        funDeps = -- [FunDep TyVar]
          [ ([xs], [ys]), ([ys], [xs])
          , ([xs],[k]), ([ys],[k])]
        constraints = WithTypeFamilies $ \(~[tcReverse,_tcReverse']) ->
          [ -- ys ~ Reverse xs :: [PredType]
            mkHeqType
              ksKind ksKind ysT (mkTyConApp tcReverse [kKind, xsT])
            -- xs ~ Reverse ys
          , mkHeqType
              ksKind ksKind xsT (mkTyConApp tcReverse [kKind, ysT])
          ]
        tyVarBndrs =
          [ Bndr k (NamedTCB Specified)
          , Bndr xs AnonTCB
          , Bndr ys AnonTCB]
    makeClassAndFamilies "ReverseList"
      tyVarBndrs funDeps constraints $ WithTypeFamilies $ \(~[_tcReverse, tcReverse']) ->
      [ ( "Reverse"
        , ksKind
        , Just (getName ys)
        , [ Bndr k (NamedTCB Specified)
          , Bndr xs AnonTCB
          ]
        , Injective [True, True]
        , [ do ki <- new "k" liftedTypeKind
               as <- new "as" $ mkListTy ki
               [ki, as] ~> mkTyConApp tcReverse' [ki, as, nil ki]
          ]
        )
      , ( "Reverse'"
        , ksKind
        , Just (getName ys)
        , [ Bndr k  (NamedTCB Specified)
          , Bndr xs AnonTCB
          , Bndr ts AnonTCB
          ]
        , Injective [True, True, False]
        , [ do ki <- new "k" liftedTypeKind
               a  <- new "a" ki
               as <- new "as" $ mkListTy ki
               bs <- new "bs" $ mkListTy ki
               [ki, cons ki a as, bs] ~> mkTyConApp tcReverse' [ki, as, cons ki a bs]
          , do ki <- new "k" liftedTypeKind
               as <- new "as" $ mkListTy ki
               [ki, nil ki, as] ~> as
          ]
        )
      ]

mkInstReverseList :: Class -> CoreM (ClsInst, CoreBind)
mkInstReverseList reverseListClass
  | [ATI tcReverse _, ATI _tcReverse' _] <- classATItems reverseListClass
    = liftThinCoreM $ do
      k  <- genTyVar "k" liftedTypeKind
      let kKind = mkTyVarTy k
          ksKind = mkListTy kKind
      xs <- genTyVar "xs" ksKind
      ys <- genTyVar "ys" ksKind

      dfunName <- genExternalName OccName.varName "inst$ReverseList"

      let xsT = mkTyVarTy xs
          ysT = mkTyVarTy ys
          constraints =
            [ -- ys ~ Reverse xs :: [PredType]
              mkHeqType
               ksKind ksKind ysT (mkTyConApp tcReverse [kKind, xsT])
            ]
          genConstraints = \[reverseEq] ->
            [ Var reverseEq
            , mkHeqExpr ksKind ksKind xsT (mkTyConApp tcReverse [kKind, ysT])
            ]

      makeClsInst dfunName [k, xs, ys]
        constraints genConstraints reverseListClass [kKind, xsT, ysT]
  | otherwise
    = error "mkInstReverseList: Unexpected class"



cons :: Kind -> Type -> Type -> Type
cons k t ts = mkTyConApp promotedConsDataCon [k, t, ts]

nil :: Kind -> Type
nil = flip mkPromotedListTy []

list :: Kind -> [Type] -> Type
list = mkPromotedListTy

-- | Construct a type equality constraint
mkHeqExpr :: Kind -> Kind -> Type -> Type -> CoreArg
mkHeqExpr ka kb a b = Var (dataConWrapId heqDataCon)
                      `mkTyApps` [ka, kb, a, b]
                      `App` primEq
  where
    eqPrimTy = mkTyConApp eqPrimTyCon [ka, kb, a, b]
    tupType  = mkTupleTy Unboxed []
    tupDaCon = tupleDataCon Unboxed 0
    primEq :: CoreArg
    primEq = Var (dataConWrapId tupDaCon)
             `Cast` mkUnsafeCo Representational tupType eqPrimTy

-- | Construct a type equality constraint
mkHeqType :: Kind -> Kind -> Type -> Type -> Type
mkHeqType ka kb a b = mkTyConApp heqTyCon [ka, kb, a, b]


-- | Run a simple TcRnIf monad with empty environment
--    (in order to use functions from BuildTyCl).
liftThinCoreM :: ThinCoreM a -> CoreM a
liftThinCoreM m = do
  hscEnv <- getHscEnv
  origModule <- getModule
  origScrSpan <- getSrcSpanM
  usRef <- getUniqueSupplyM >>= liftIO . newIORef
  liftIO $ runIOEnv (Env hscEnv usRef (ThinCoreGblEnv origModule origScrSpan) ()) m


type ThinCoreM a = TcRnIf ThinCoreGblEnv () a

data ThinCoreGblEnv = ThinCoreGblEnv
  { thinCoreModule :: Module
  , thinCoreLoc    :: SrcSpan
  }

instance ContainsModule ThinCoreGblEnv where
  extractModule = thinCoreModule

getLocM :: ThinCoreM SrcSpan
getLocM = thinCoreLoc . env_gbl <$> getEnv

makeClassAndFamilies
  :: String
     -- ^ Class name
  -> [TyConBinder]
     -- ^ Binders for all type variables
     --   (shared between the class and associated type families and constraints)
     --   e.g. [ TvBndr k (NamedTCB Specified) , TvBndr xs AnonTCB ]
  -> [FunDep TyVar]
     -- ^ Functional dependencies between type variables
  -> WithTypeFamilies ThetaType
     -- ^ [PredType] -- constraints (can access type family inside)
  -> WithTypeFamilies
       [( String -- tf name
        , Kind         -- family result kind
        , Maybe Name   -- family result type Name
        , [TyConBinder] -- argument type variable binders
        , Injectivity -- injectivity for each arg var, including kinds
        , [WithVars CoAxBranch] -- equations
        )]
     -- ^ Type family declaration
  -> ThinCoreM Class
makeClassAndFamilies classNameStr tyVarBndrs funDeps constraintsF tfsF = do
    name <- genExternalName clsName classNameStr
    fixM $ \klass -> do
      let mkFamilies :: [TyCon] -> ThinCoreM [TyCon]
          mkFamilies tfs = traverse mkFam (runWithTypeFamilies tfsF tfs)
          mkFam (n, r, ts, bs, i, eqs) =
            makeTypeFamily n (Just klass) r ts bs i eqs
      families <- fixM mkFamilies
      buildClass
        name tyVarBndrs (map (const Nominal) tyVarBndrs) funDeps $
        Just
          ( runWithTypeFamilies constraintsF families
          , map (flip ATI Nothing) families
          , []  -- :: [TcMethInfo]
          , mkTrue -- :: type ClassMinimalDef = BooleanFormula Name
          )





-- | Generate a unique name in the given namespace
genExternalName :: NameSpace -> String -> ThinCoreM Name
genExternalName ns n = do
    origModule <- getModule
    origScrSpan <- getLocM
    clsUnique <- getUniqueM
    let classOccName = mkOccName ns n
    return $ mkExternalName clsUnique origModule classOccName origScrSpan



-- | Generate a new type variable
genTyVar :: String -> Kind -> ThinCoreM TyVar
genTyVar n ki = do
    origScrSpan <- getLocM
    tvUnique <- getUniqueM
    let tvOccName = mkOccName OccName.tvName n
        tvn = mkInternalName tvUnique tvOccName origScrSpan
    return $ mkTyVar tvn ki

-- | Generate a new local variable
genLocalVar :: Type -> ThinCoreM TyVar
genLocalVar ty = do
    origScrSpan <- getLocM
    vUnique <- getUniqueM
    let vOccName = mkOccName OccName.varName "t"
        vn = mkInternalName vUnique vOccName origScrSpan
    return $ mkLocalVar VanillaId vn ty vanillaIdInfo

runWithVars :: WithVars a -> ThinCoreM a
runWithVars m = liftIO (newIORef []) >>= _runWithVars m

newtype WithVars a = WithVars {_runWithVars :: IORef [TyVar] -> ThinCoreM a }

instance Functor WithVars where
  fmap f x = WithVars $ fmap f <$> _runWithVars x

instance Applicative WithVars where
  pure = WithVars . pure . pure
  af <*> ax = WithVars $ \r -> _runWithVars af r <*> _runWithVars ax r

instance Monad WithVars where
  return = pure
  m >>= k = WithVars $ \r -> _runWithVars m r >>= ($ r) . _runWithVars . k


newtype WithTypeFamilies a = WithTypeFamilies {runWithTypeFamilies :: [TyCon] -> a }
  deriving (Functor, Applicative, Monad)

-- | Create a new type variable for an axiom
new :: String -> Kind -> WithVars Type
new n k = WithVars $ \tvsRef -> do
   tv <- genTyVar n k
   updMutVar tvsRef (tv:)
   return $ mkTyVarTy tv

-- | Write a single branch for an axiom (type family)
(~>) :: [Type] -> Type -> WithVars CoAxBranch
(~>) lhs rhs = WithVars $ \tvsRef -> do
  loc <- getLocM
  tvs <- reverse <$> readMutVar tvsRef
  return $ mkCoAxBranch tvs [] [] lhs rhs (map (const Nominal) tvs) loc
infix 4 ~>


makeTypeFamily :: String    -- name of the family
               -> Maybe Class   -- associated type class
               -> Kind          -- family result kind
               -> Maybe Name   -- family result type name
               -> [TyConBinder] -- argument type variable binders
               -> Injectivity -- injectivity for each arg var, including kinds
               -> [WithVars CoAxBranch] -- equations
               -> ThinCoreM TyCon
makeTypeFamily famNameStr maClass resName resKind tvbds inj wveqs = do
    famName <- genExternalName tcName famNameStr
    coName <- genExternalName tcName ("D:R:" ++ famNameStr)
    famEqs <- sequence $ map runWithVars wveqs
    let famCoAx = mkBranchedCoAxiom coName famTyCon famEqs
        famTyCon = mkFamilyTyCon
          famName tvbds resName resKind
          (ClosedSynFamilyTyCon (Just famCoAx)) maClass inj
    return famTyCon


makeClsInst :: Name
            -> [TyVar]
              -- ^ TyVars stay the same in DFunId, but are freshened in ClsInst
            -> ThetaType
               -- ^ given constraints (part of signature)
            -> ([CoreBndr] -> [CoreArg])
               -- ^ derived constraints (how to provide class constraints)
            -> Class -> [Type] -> ThinCoreM (ClsInst, CoreBind)
makeClsInst dFunName tvs theta genConstraints clas tys = do
  hscEnv <- getTopEnv
  (_ , Just (subst, tvs')) <- liftIO $ initTcInteractive hscEnv $ freshenTyVarBndrs tvs
  thetaVars <- traverse genLocalVar theta
  let tys' = substTys subst tys
      dfun = mkDictFunId dFunName tvs theta clas tys
      oflag = OverlapFlag (NoOverlap NoSourceText) False
      inst = mkLocalInstance dfun oflag tvs' clas tys'
      instExpr = mkCoreLams (tvs ++ thetaVars) $
        classDataCon clas `mkCoreConApps`
          (map mkTyArg tys ++ genConstraints thetaVars)
  return ( inst, NonRec dfun instExpr )
