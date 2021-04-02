#!/bin/bash
cabal="/opt/cabal/bin/cabal"
ghcvers=("8.4.4" "8.6.5" "8.8.4" "8.10.4" "9.0.1")
#ghcvers=("9.0.1")

hpack dimensions/package.yaml
hpack easytensor/package.yaml
#${cabal} update

for ver in ${ghcvers[*]}; do
  ghcpath="/opt/ghc/${ver}/bin/ghc"
  hcpkgpath="/opt/ghc/${ver}/bin/ghc-pkg"
  pams="--enable-tests --disable-documentation --project-file=cabal.project --with-compiler=${ghcpath} --with-hc-pkg=${hcpkgpath}"
  echo "Building for ghc-${ver}..."
  ${cabal} v2-build dimensions ${pams}
  echo "Testing for ghc-${ver}..."
  ${cabal} v2-test dimensions --test-show-details=streaming ${pams}
done
