{-
Disable some errors and warnings during the haddock pass
  (caused by compiler plugins and hs-boot)
 -}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.Simple
import Distribution.Simple.Setup

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \a -> confHook simpleUserHooks a . tweakFlags }

tweakFlags :: ConfigFlags -> ConfigFlags
tweakFlags flags = flags
  { configProgramArgs = addHaddockArgs (configProgramArgs flags) }

addHaddockArgs :: [(String, [String])] -> [(String, [String])]
addHaddockArgs []
  = [("haddock", newHaddockGhcArgs)]
addHaddockArgs (("haddock", args):otherProgsArgs)
  = ("haddock", args ++ newHaddockGhcArgs) : otherProgsArgs
addHaddockArgs (progArgs:otherProgsArgs)
  = progArgs : addHaddockArgs otherProgsArgs

newHaddockGhcArgs :: [String]
newHaddockGhcArgs =
  [ "--optghc=-fdefer-type-errors"
  , "--optghc=-fno-warn-deferred-type-errors"
  , "--optghc=-fno-warn-missing-home-modules"
  ]
