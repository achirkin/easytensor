-- {-# OPTIONS_GHC -fobject-code   #-}
{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.Type.List.Internal.Plugin ( plugin, Inj (..) ) where


import CoAxiom
import Data.Data  (Data, typeRep)
import Data.Proxy
import GhcPlugins
import Panic      (panicDoc)

-- | A small core plugin to make GHC think that @Snoc@ is injective
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos =
      const $ pure . (CoreDoPluginPass "dimensions:tweakInjectivity" tweakInjectivityPass :)
#if MIN_VERSION_ghc(8,6,0)
  , pluginRecompile = purePlugin
#endif
  }

-- | Force arguments of a type family to be injective
newtype Inj = Inj [Bool]
  deriving (Eq, Data)

tweakInjectivityPass :: ModGuts -> CoreM ModGuts
tweakInjectivityPass guts = pure $ guts { mg_tcs = map f $ mg_tcs guts }
  where
    anns = getModuleAnns guts

    f tc = case lookupUFM anns tc of
      Just ((n,inj):_) | n == tyConName tc
        -> tweakInjectivity inj tc
      _ -> tc

-- insert injectivity annotation into a type family TyCon if possible
tweakInjectivity :: Inj -> TyCon -> TyCon
tweakInjectivity (Inj injs) origTc
  | length injs /= length (tyConTyVars origTc)
    = panicDoc "dimensions:tweakInjectivity plugin error" $ vcat
      [ text "Injectivity annotation must correspond to the list of family arguments:"
      , text "Injectivity annotation:" <+> ppr injs
      , text "Family arguments:" <+> ppr (tyConTyVars origTc)
      ]
  | Just origFlav <- famTyConFlav_maybe origTc
    = tweakInjectivity' origFlav injs origTc
  | otherwise
    = panicDoc "dimensions:tweakInjectivity plugin error" $
        text "Expected a family type constructor, but got:" <+> ppr origTc

tweakInjectivity' :: FamTyConFlav -> [Bool] -> TyCon -> TyCon
tweakInjectivity' origFlav injs origTc = newTc
  where
    newTc = mkFamilyTyCon
      (tyConName origTc)    -- Name
      (tyConBinders origTc) -- [TyConBinder]
      (tyConResKind origTc) -- Kind
      (famTcResVar origTc)  -- Maybe Name
       famFlav              -- FamTyConFlav
      (tyConAssoc_maybe origTc) -- Maybe Class
      (Injective injs)          -- Injectivity
    famFlav
      | ClosedSynFamilyTyCon (Just axiom) <- origFlav
        = ClosedSynFamilyTyCon . Just $
            replaceTyConInAxiom (tyConName origTc) newTc axiom
      | otherwise
        = origFlav


replaceTyConInAxiom :: Name -> TyCon -> CoAxiom br -> CoAxiom br
replaceTyConInAxiom n newTc coax = coax
    { co_ax_tc = if tyConName (co_ax_tc coax) == n then newTc else co_ax_tc coax
    , co_ax_branches = mapAccumBranches (const f) $ co_ax_branches coax
    }
  where
    f :: CoAxBranch -> CoAxBranch
    f b = b { cab_rhs = replaceTyConInType n newTc (cab_rhs b) }

-- Replace all occurrences of a TyCon with an equivalent TyCon
-- (correctness is not checked)
replaceTyConInType :: Name -> TyCon -> Type -> Type
replaceTyConInType myName newTc t
  | Just (origTc, ts) <- splitTyConApp_maybe t
  , tc <- if tyConName origTc == myName then newTc else origTc
    = mkTyConApp tc $ map (replaceTyConInType myName newTc) ts
  | (bndrs@(_:_), t') <- splitForAllTys t
    = mkSpecForAllTys bndrs $ replaceTyConInType myName newTc t'
  | Just (at, rt) <- splitFunTy_maybe t
    = mkFunTy (replaceTyConInType myName newTc at)
              (replaceTyConInType myName newTc rt)
  | otherwise
    = t


getModuleAnns :: ModGuts -> UniqFM [(Name, Inj)]
getModuleAnns = go . mg_anns
  where
    valTRep = typeRep (Proxy :: Proxy Inj)
    go :: [Annotation] -> UniqFM [(Name, Inj)]
    go [] = emptyUFM
    go (Annotation
         (NamedTarget n) -- ignore module targets
         (Serialized trep bytes)
        : as)
      | trep == valTRep -- match type representations
      = addToUFM_Acc (:) (:[]) (go as) n (n, deserializeWithData bytes)
    -- ignore non-matching annotations
    go (_:as) = go as



-- tweakInjectivityPass :: ModGuts -> CoreM ModGuts
-- tweakInjectivityPass guts = pure $ foldr (uncurry tweakInjectivityInGuts) guts (getModuleAnns guts)
--
--
-- tweakInjectivityInGuts :: Name -> Inj -> ModGuts -> ModGuts
-- tweakInjectivityInGuts n inj guts
--   | Just origTc <- find ((n ==) . tyConName) $ mg_tcs guts
--   , newTc <- tweakInjectivity inj origTc
--     = guts
--     { mg_tcs = map (replaceTyConInTyCon n newTc) $ mg_tcs guts
--     , mg_fam_insts = map (replaceTyConInFamInst n newTc) $ mg_fam_insts guts
--     -- at least, need to traverse also mg_binds and mg_fam_inst_env
--     }
-- tweakInjectivityInGuts _ _ guts = guts
--
-- replaceTyConInFamInst :: Name -> TyCon -> FamInst -> FamInst
-- replaceTyConInFamInst n newTc fi = fi
--   { fi_axiom = replaceTyConInAxiom n newTc $ fi_axiom fi
--   , fi_tys = map (replaceTyConByName n newTc) $ fi_tys fi
--   , fi_rhs = replaceTyConByName n newTc $ fi_rhs fi
--   }
--
-- replaceTyConInTyCon :: Name -> TyCon -> TyCon -> TyCon
-- replaceTyConInTyCon n newTc tc
--   | tyConName tc == n
--     = newTc
--   | Just t <- synTyConRhs_maybe tc
--     = mkSynonymTyCon
--         (tyConName tc)
--         (tyConBinders tc)
--         (tyConResKind tc)
--         (tyConRoles tc)
--         (replaceTyConByName n newTc t)
--         (isTauTyCon tc)
--         (isFamFreeTyCon tc)
--   | isFamilyTyCon tc
--     = let tc' = mkFamilyTyCon
--                   (tyConName tc)
--                   (tyConBinders tc)
--                   (tyConResKind tc)
--                   (famTcResVar tc)
--                   famFlav
--                   (tyConAssoc_maybe tc)
--                   (tyConInjectivityInfo tc)
--
--           famFlav = case famTyConFlav_maybe tc of
--             Just (ClosedSynFamilyTyCon (Just axiom))
--               -> ClosedSynFamilyTyCon . Just $
--                   (replaceTyConInAxiom n newTc axiom) { co_ax_tc = tc' }
--             Just f
--               -> f
--             Nothing
--               -> panicDoc "dimensions:tweakInjectivity plugin error" $
--                   text "Expected a type family, but got" <+> ppr tc
--       in tc'
-- replaceTyConInTyCon _ _ tc = tc
--
-- replaceTyConInAxiom :: Name -> TyCon -> CoAxiom br -> CoAxiom br
-- replaceTyConInAxiom n newTc coax = coax
--     { co_ax_tc = if tyConName (co_ax_tc coax) == n then newTc else co_ax_tc coax
--     , co_ax_branches = mapAccumBranches (const f) $ co_ax_branches coax
--     }
--   where
--     f :: CoAxBranch -> CoAxBranch
--     f b = b { cab_rhs = replaceTyConByName n newTc (cab_rhs b) }
--
--
-- -- insert injectivity annotation into a type family TyCon if possible
-- tweakInjectivity :: Inj -> TyCon -> TyCon
-- tweakInjectivity (Inj injs) origTc
--   | length injs /= length (tyConTyVars origTc)
--     = panicDoc "dimensions:tweakInjectivity plugin error" $ vcat
--       [ text "Injectivity annotation must correspond to the list of family arguments:"
--       , text "Injectivity annotation:" <+> ppr injs
--       , text "Family arguments:" <+> ppr (tyConTyVars origTc)
--       ]
--   | otherwise
--     = newTc
--   where
--     newTc = mkFamilyTyCon
--       (tyConName origTc)    -- Name
--       (tyConBinders origTc) -- [TyConBinder]
--       (tyConResKind origTc) -- Kind
--       (famTcResVar origTc)  -- Maybe Name
--       famFlav               -- FamTyConFlav
--       (tyConAssoc_maybe origTc) -- Maybe Class
--       (Injective injs)          -- Injectivity
--     famFlav
--       | Just axiom <- isClosedSynFamilyTyConWithAxiom_maybe origTc
--         = ClosedSynFamilyTyCon . Just $ replaceTyConInAxiom (tyConName origTc) newTc axiom
--       | isOpenTypeFamilyTyCon origTc
--         = OpenSynFamilyTyCon
--       | isAbstractTyCon origTc
--         = AbstractClosedSynFamilyTyCon
--       | otherwise
--         = panicDoc "dimensions:tweakInjectivity plugin error" $
--             text "Unexpected family flavour:" <+> ppr origTc
--
-- -- Replace all occurrences of a TyCon with an equivalent TyCon
-- -- (correctness is not checked)
-- replaceTyConByName :: Name -> TyCon -> Type -> Type
-- replaceTyConByName myName newTc t
--   | Just (origTc, ts) <- splitTyConApp_maybe t
--   , tc <- if tyConName origTc == myName then newTc else origTc
--     = mkTyConApp tc $ map (replaceTyConByName myName newTc) ts
--   | (bndrs@(_:_), t') <- splitForAllTys t
--     = mkSpecForAllTys bndrs $ replaceTyConByName myName newTc t'
--   | Just (at, rt) <- splitFunTy_maybe t
--     = mkFunTy (replaceTyConByName myName newTc at)
--               (replaceTyConByName myName newTc rt)
--   | otherwise
--     = t
--
--
-- getModuleAnns :: ModGuts -> [(Name, Inj)]
-- getModuleAnns = go . mg_anns
--   where
--     valTRep = typeRep (Proxy :: Proxy Inj)
--     go :: [Annotation] -> [(Name, Inj)]
--     go [] = []
--     go (Annotation
--          (NamedTarget n) -- ignore module targets
--          (Serialized trep bytes)
--         : as)
--       | trep == valTRep -- match type representations
--       = (n, deserializeWithData bytes) : go as
--     -- ignore non-matching annotations
--     go (_:as) = go as
