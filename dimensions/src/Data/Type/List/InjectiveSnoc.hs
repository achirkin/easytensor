{-# OPTIONS_GHC -fobject-code   #-}
{-# OPTIONS_HADDOCK hide, prune #-}
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


import           CoAxiom    (CoAxBranch (..), CoAxiom (..), mapAccumBranches)
import           Data.Maybe (fromMaybe)
import           GhcPlugins

-- NB: check out
--  https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/compiler/typecheck/TcTypeNats.hs
--  for further ideas.
--  Maybe, I can use BuiltInSynFamily to do a lot of super cool stuff!

-- | A small core plugin to make GHC think that @Snoc@ is injective
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos =
      const $ pure . (CoreDoPluginPass "InjectiveSnoc" injectiveSnocPass :)
  }

injectiveSnocPass :: ModGuts -> CoreM ModGuts
injectiveSnocPass = pure . modSnocFam updateSnocFam

modSnocFam :: (TyCon -> TyCon) -> ModGuts -> ModGuts
modSnocFam f guts = guts {mg_tcs = map g (mg_tcs guts)}
  where
    g tc
      | nameOccName (tyConName tc) == mkTcOcc "Snoc" = f tc
      | otherwise = tc

updateSnocFam :: TyCon -> TyCon
updateSnocFam tc = fromMaybe tc $ do
    axiom <- isClosedSynFamilyTyConWithAxiom_maybe tc
    let newAxiom = axiom
          { co_ax_tc = newTc
          , co_ax_branches = mapAccumBranches f (co_ax_branches axiom)
          }
        f _ b = b { cab_rhs = repTc (cab_rhs b) }
        repTc t = case splitTyConApp_maybe t of
          Just (c, ts)
            | nameOccName (tyConName c) == mkTcOcc "Snoc"
              -> mkTyConApp newTc $ map repTc ts
            | otherwise
              -> mkTyConApp c $ map repTc ts
          Nothing
              -> t
        newTc = mkFamilyTyCon
          (tyConName tc)    -- Name
          (tyConBinders tc)   -- [TyConBinder]
          (tyConResKind tc) -- Kind
          (famTcResVar tc)  -- Maybe Name
          (ClosedSynFamilyTyCon (Just newAxiom)) -- FamTyConFlav
          (tyConAssoc_maybe tc)  -- Maybe Class
          -- Injectivity copied from a dummy injective TF with the same head
          (Injective [True, True, True])     -- Injectivity
    return newTc
