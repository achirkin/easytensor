### easytensor

This library is a rewrite of [fastvec](https://github.com/achirkin/fastvec).
The idea is to use the most of GHC 8 type level features to typecheck dimensionality and ranks of tensors.
Data type `Tensor` is presented in two versions:

 * `Numeric.Tensor` provides a flexible tensor type with fully type-checked list of co- and contravariant dimensions.
 * `Numeric.EasyTensor` provides a simplified tensor type, limited to rank (1,1) tensors.

Behind the scenes all data types are implemented as primitive values or primitive byte arrays, aiming at maximum performance.
Tricky layers of newtypes and closed type families (*which are not exposed to a user*) allow some kind of ad-hoc polymorphism:
for example, `Vector t n` implemented as `ByteArray#` is overloaded by a specialized `VFloatX2 Float# Float#` that can be passed directly in FP registers of a processor.

### Implemenation status and plan

  - [x] Basic implementation of generic n-dimensional vectors and matrices based on type Float
  - [ ] Basic implementation of generic n-dimensional vectors and matrices based other types (Double, Int, Word, Int8.. etc.)
  - [x] EasyTensor wrapper, limited to rank (1,1) tensors (and also (0,0),(1,0), and (0,1))
  - [ ] Overloaded fast implementation for low-dimensional vectors and matrices: only VFloatX2 is done.
  - [ ] Overloaded fast SIMD implementation based on fastvec (avx2, foreign import prim and llvm)
  - [ ] `Tensor t [Nat] [Nat]` rank (n,m) flexible tensor wrapper
  - [ ] Lens-like interfaces


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
