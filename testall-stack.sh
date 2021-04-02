#!/bin/sh

echo "Optimizations disabled..."
echo "GHC 8.4..."
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.4.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.4.yaml --flag *:-unsafeindices
echo "GHC 8.6..."
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.6.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.6.yaml --flag *:-unsafeindices
echo "GHC 8.8..."
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.8.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.8.yaml --flag *:-unsafeindices
echo "GHC 8.10..."
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.10.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O0 -fforce-recomp" --resolver snapshot-8.10.yaml --flag *:-unsafeindices



echo "Optimizations enabled"
echo "GHC 8.4..."
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.4.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.4.yaml --flag *:-unsafeindices
echo "GHC 8.6..."
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.6.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.6.yaml --flag *:-unsafeindices
echo "GHC 8.8..."
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.8.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.8.yaml --flag *:-unsafeindices
echo "GHC 8.10..."
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.10.yaml --flag *:unsafeindices
stack test --ghc-options="-Werror -O -fforce-recomp" --resolver snapshot-8.10.yaml --flag *:-unsafeindices

echo "All done!"
