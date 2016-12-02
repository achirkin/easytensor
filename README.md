### easytensor

Just experimenting with Haskell type system and GHC 8 features.


#### NB on using Atom

Just a reminder:
if using atom editor with `haskell-ghc-mod`, you need to build a `ghc-mod` executable
in the project folder.
There is a problem in lts-7.11 that it does not compile due to package `extra`.
To overcome this and successfully install ghc-mod, you need to run:
```
  stack build ghc-mod extra-1.4.12
```
Also, do not forget to remove folder `dist` if cabal created it,
and make sure atom plugin uses stack sandboxing.
