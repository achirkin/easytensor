### easytensor

This library aims at providing fast, simple, and useful geometry types for use in computer graphics and low-dimensional physics modelling.

All geometry types implement standard `Prelude` numerical classes, such as `Num`, `Fractional`, `Floating`,
favouring element-wise operations:
functions like `+`, `*`, `/` are all element-wise. 
Common matrix and vector operations are provided separately.

A special care should be taken when using `Ord` instances:

 * `<`, `>`, `<=`, `>=` provide Pareto dominance inequalities (partial ordering);
 * `compare` provides  lexicographical ordering;
 * `min`, `max` give element-wise minimum and maximum respectively.


This library is a rewrite of [fastvec](https://github.com/achirkin/fastvec).
The idea is to use the most of GHC 8 type level features to typecheck dimensionality and ranks of tensors.
Data type `Tensor` is presented in two versions:

 * `Numeric.Tensor` provides a flexible tensor type parametrized by a fully type-checked list of co- and contravariant dimensions.
 * `Numeric.EasyTensor` provides a simplified tensor type, limited to rank (1,1) and lower tensors.

Behind the scenes all data types are implemented as primitive values or primitive byte arrays, aiming at maximum performance.
Tricky layers of newtypes and closed type families (*which are not exposed to a user*) allow some kind of ad-hoc polymorphism:
for example, `Vector t n` implemented as `ByteArray#` is overloaded by a specialized `VFloatX2 Float# Float#` that can be passed directly in FP registers of a processor.

### Implemenation status and plan

  - [x] Basic implementation of generic n-dimensional vectors and matrices based on type Float
  - [ ] Basic implementation of generic n-dimensional vectors and matrices based other types (Double, Int, Word, Int8.. etc.)
  - [x] `DataFrame t [Nat]` type for multi-dimensional arrays.
  - [x] EasyTensor wrapper, limited to rank (1,1) tensors (and also (0,0),(1,0), and (0,1))
  - [ ] Overloaded fast implementation for low-dimensional vectors and matrices: only VFloatX2 is done.
  - [ ] Overloaded fast SIMD implementation based on fastvec (avx2, foreign import prim and llvm)
  - [ ] `Tensor t [Nat] [Nat]` rank (n,m) flexible tensor wrapper
  - [ ] Lens-like interfaces - partially
  - [ ] ~~Release last dimension from requiring KnownNat in order to flexibly read data at runtime.~~
  - [x] Flexible inference between statically known and runtime known dimensions `Dim [Nat]` and `Dim [XNat]`
  - [ ] Smart MATLAB- or R-like indexing of rows and columns.


#### NB on using Atom

Just as a reminder:
if using atom editor with `haskell-ghc-mod`, you need to build a `ghc-mod` executable
in the project folder.
Therefore, to install ghc-mod, type following:
```
  stack setup
  stack build happy
  stack build ghc-mod
```
Also, do not forget to remove folder `dist` if cabal created it,
and make sure atom plugin uses stack sandboxing.
