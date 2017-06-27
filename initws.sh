#!/bin/bash
# This script sets up ghc-mod environment to work with GHC 8.2.1-RC1
#  while the official ghc-mod support is not here yet
export PATH=/usr/local/ghc-8.2.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
rm easytensor/cabal.sandbox.config
rm dimensions/cabal.sandbox.config
rm cabal.sandbox.config
rm -rf easytensor/dist
rm -rf dimensions/dist
rm -rf .cabal-sandbox
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
cabal sandbox add-source dimensions
cabal sandbox add-source easytensor
cabal install QuickCheck
cabal install cabal-doctest-1 --allow-newer
cabal install .cabal-sandbox/ghc-mod/vendor/ghc-syb/utils/
cabal install .cabal-sandbox/ghc-mod/vendor/cabal-helper/
cabal install .cabal-sandbox/ghc-mod/
pushd easytensor
cabal sandbox init --sandbox=../.cabal-sandbox
popd
pushd dimensions
cabal sandbox init --sandbox=../.cabal-sandbox
popd
cabal install dimensions/
