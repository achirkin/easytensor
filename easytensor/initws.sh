#!/bin/bash
export PATH=/usr/local/ghc-8.2.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
rm -rf dist
rm -rf dist-newstyle
cabal update
cabal sandbox init
pushd .cabal-sandbox
git clone --recursive https://github.com/achirkin/ghc-mod.git
popd
cabal sandbox add-source .cabal-sandbox/ghc-mod
cabal sandbox add-source .cabal-sandbox/ghc-mod/vendor/ghc-syb/utils
cabal sandbox add-source .cabal-sandbox/ghc-mod/vendor/cabal-helper
cabal sandbox add-source ../dimensions
cabal install ghc-mod QuickCheck dimensions --allow-newer --ghc-options="-XTypeSynonymInstances"
cabal configure
