Easytensor: many-dimensional type-safe numeric ops
==================================
[![Build Status](https://secure.travis-ci.org/achirkin/easytensor.svg)](http://travis-ci.org/achirkin/easytensor)

The project consists of two parts:
 * `dimensions` is a library to support type-level operations on lists of dimensions;
 * `easytensor` wraps low-level operations on primitive byte arrays in a type-safe data type indexed over an element type and a list of dimensions.

### dimensions [![Hackage](https://img.shields.io/hackage/v/dimensions.svg)](https://hackage.haskell.org/package/dimensions)

`Numeric.Type.List` provides type-level operations on lists.
`Numeric.TypedList` is the core module providing a typelist-indexed type that is, in fact, just a newtype wrapper on a plain haskell list.
`Numeric.Dimensions` provides:
  * promoted type `XNat = N Nat | XN` similar to type `Maybe`; kind `XNat` is used for type variables when some of dimensions in a type-level list are not known at compile time;
  * data type `Dims (ds :: [k])`, where `k` is either `Nat` or `XNat`, together with class `Dimensions ds` it allows lots of type-level operations on type-level dimensionality;
  * data type `Idxs (ds :: [k])` is used to index over many-dimensional space defined by `Dims ds`.

### easytensor [![Hackage](https://img.shields.io/hackage/v/easytensor.svg)](https://hackage.haskell.org/package/easytensor)

This library aims at providing fast, simple, and useful geometry types for use in computer graphics and low-dimensional physics modelling.

All geometry types implement standard `Prelude` numerical classes, such as `Num`, `Fractional`, `Floating`,
favouring element-wise operations:
functions like `+`, `*`, `/` are all element-wise.
Common matrix and vector operations are provided separately.

A special care should be taken when using `Ord` controversial instances (never use a DataFrame as a polymorphic `Ord a`!):

 * `<`, `>`, `<=`, `>=` provide Pareto dominance inequalities (partial ordering);
 * `compare` provides  lexicographical ordering;
 * `min`, `max` give element-wise minimum and maximum respectively.


This library is a rewrite of [fastvec](https://github.com/achirkin/fastvec).
The idea is to use the most of GHC 8.2+ type level features to typecheck dimensionality and ranks of tensors.

Data type `DataFrame` is presented in two versions:

 * `DataFrame t (ds :: [Nat])` - dimensionality of a dataframe is totally known at compile time.
 * `DataFrame t (ds :: [XNat]` - some dimensions may be known at runtime only.

Parameter `t` of a DataFrame can be arbitrary type that has an instance of `PrimBytes`.
`PrimBytes` typeclass can be automatically derived using `Generics`.
This mechanics allows creating interleaved arrays, e.g. `DataFrame (Double, Float, Float) ds`.

Parameter `t` of a `DataFrame` can also be a list of `PrimBytes` types:
this way, `DataFrame` consists of several "columns" or "variables" of different type and same dimensionality.

Behind the scenes all data types are implemented as primitive values or primitive byte arrays, aiming at maximum performance.
Tricky layers of newtypes and closed type families (*which are not exposed to a user*) allow some kind of ad-hoc polymorphism:
for example, `Vector t n` implemented as `ByteArray#` is overloaded by a specialized `FloatX2 Float# Float#` in case of `Vector Float 2`.

### v2 pre-release TODO

Version 2 of easytensor is a total overhaul of the original easytensor library.
I changed memory layout and indexing style for arrays and much more.
The primary goal of all these changes to align the library API to fit better
the expectations of developers from a conventional Haskell library.

Following is the list of what I want to do before v2 becomes the master branch:

  - [ ] Normalise `Ord` instance for dataframes.
  - [ ] Subspace: strict and lazy versions of folds (and add tests), maybe add cumuldims
  - [ ] Readable errors via overlappable class instances (Failed).
  - [x] Introduce a DataFrame `Backend` class and depend less on `PrimBytes`.
  - [ ] Add optional field offset functions for `PrimBytes` derivable via Generic.
  - [x] Use `DeriveAll` for better inference of common type classes.
  - [ ] Maybe split README for easytensor and dimensions (but anyway write a better README).
  - [ ] Better test coverage.
  - [ ] A more conventional Show and Read instance for DataFrame.
  - [ ] Move current show instances into `easytensor-pretty` and improve them.
  - [ ] Try `Typeable`, `Data`, `Generic` for data frames.
  - [x] Refactor module dependency tree (try not to depend on implementation anywhere).
  - [x] Polymorphic vector constructors.

### Feature TODO

There is a bunch of things I have implemented earlier but have not re-implemented in the new easytensor version or planned to implement for the use-cases I already have.

  - [x] Basic implementation of generic ns-dimensional dataframes
  - [x] Quaternions
  - [ ] Matrix-specific operations
    - [x] [Transpose, eye, etc.](https://github.com/achirkin/easytensor/blob/master/easytensor/src/Numeric/Matrix/Class.hs)
      - [ ] Check if it makes sense to generalize these for higher ranks
      - [ ] Investigate if more typcial matrix functions are needed
    - [x] LU decomposition, determinant and inverse
    - [ ] SVD decomposition
    - [ ] Homogenous coordinate operations
      - [x] Class [`HomTransform`](https://github.com/achirkin/easytensor/blob/master/easytensor/src/Numeric/Matrix/Class.hs#L80) (borrowed from the previous version of the lib)
      - [x] Implementation of the class
  - [x] Lens-like interfaces - `Numeric.DataFrame.SubSpace`
  - [ ] `Tensor t [Nat] [Nat]` rank (n,m) flexible tensor wrapper
  - [ ] Smart MATLAB- or R-like indexing of rows and columns: investigate options.
  - [ ] (`easytensor-tc`) Type-checker plugin to reduce necessary `E <- inferXXX` patterns number and simplify the user interface:
    - [ ] `KnownDim` inference for basic operations; look at [ghc-typelits-natnormalise](https://github.com/clash-lang/ghc-typelits-natnormalise)
    - [ ] `Dimensions ds ==> All KnownDim ds`
    - [ ] Better `FixedDims xnd ns` inference of `ns` when pattern-matching against `XNat`-indexed `DataFrame` or `Dims`
    - [ ] Better `ConcatList as bs asbs` inference
    - [ ] Shortcut for inferring injective type families `Reverse` and `Snoc`
    - [ ] Investigate other use-cases
  - [ ] `easytensor-extra` extra features and helpers:
    - [ ] Quasiquotes for constructing fixed-dimensional matrices and vectors using a MATLAB-like syntax
    - [ ] Investigate options for binary and csv serialization
    - [ ] Polygon triangulation


### Performance TODO

In the end, `easytensor` is supposed to be used in programming for computer graphics, thus it must be very fast.
The library was developed with this idea in mind to allow the room for optimization.

  - [ ] Implement fast comparison with partial help of sameMutableByteArray#
         (e.g. `Numeric.DataFrame.Internal.Array.Family.ArrayBase`)
  - [ ] Add more low-dimensional specialized Array family instances
         (`Numeric.DataFrame.Internal.Array.Family`)
    - [x] `FloatX[2|3|4]`, `DoubleX[2|3|4]` vectors
    - [ ] `FloatX[22|33|44]`, `DoubleX[22|33|44]` matrices
    - [ ] Other options? `FloatX[2|3|4][2|3|4]` matrices? `[Int|Word][8|16|32|64]` types?
  - [ ] Add lots of rewrite rules for specialized Array family instances.
  - [ ] Specialize class instances for Array family instances.
  - [ ] Implement the same Array family instances using SIMD, activated by a dedicated flag.
        (different Array implementations would stay in different folders, e.g. `src-base`, `src-avx` or `src-sse`)

### Documentation and tutorials

  - [ ] Write a proper tutirual blog post and put a link here.
  - [ ] Put more links to code examples here
        (you are welcome to put a link to your use-case repository here via a PR):
    - A few simple examples are available in [`bench/misc.hs`](https://github.com/achirkin/easytensor/blob/master/easytensor/bench/misc.hs).
    - Using `SubSpace` operations to iterate over many-dimensional single-var frames in [`bench/subspacefolds`](https://github.com/achirkin/easytensor/blob/master/easytensor/bench/subspacefolds.hs).
    - Writing highly-generic typeclass instances for generating random frames in [`tests`](https://github.com/achirkin/easytensor/blob/master/easytensor/test/Numeric/DataFrame/Arbitraries.hs).
    - Using multi-variable frames and matrices (run-time-only-known dimensionality) in [`tests`](https://github.com/achirkin/easytensor/blob/master/easytensor/test/Numeric/MatrixTest.hs).
    - Deriving `PrimBytes` to use custom element types for frames in [`vulkan-triangles`](https://github.com/achirkin/vulkan/blob/master/vulkan-triangles/src/Lib/Vulkan/Vertex.hs);
      this is an example of creating an interleaved array to be sent to the GPU rendering pipeline.
