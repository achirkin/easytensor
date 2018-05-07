Easytensor: many-dimensional type-safe numeric ops
==================================
[![Build Status](https://secure.travis-ci.org/achirkin/easytensor.svg)](http://travis-ci.org/achirkin/easytensor)

The project consists of two parts:
 * `dimensions` is a library to support type-level operations on lists of dimensions;
 * `easytensor` wraps low-level operations on primitive byte arrays in a type-safe data type indexed over an element type and a list of dimensions.

### dimensions [![Hackage](https://img.shields.io/hackage/v/dimensions.svg)](https://hackage.haskell.org/package/dimensions)

`Numeric.Dimensions.List` provides type-level operations on lists and means to infer constraints over these operations at runtime.
`Numeric.Dimension` provides:
  * promoted type `XNat = N Nat | XN` similar to type `Maybe`; kind `XNat` is used for type variables when some of dimensions in a type-level list are not known at compile time;
  * data type `Dims (ds :: [k])`, where `k` is either `Nat` or `XNat`, together with class `Dimensions ds` it allows lots of type-level operations on type-level dimensionality;
  * data type `Idxs (ds :: [k])` is used to index over many-dimensional space defined by `Dims ds`.

### easytensor [![Hackage](https://img.shields.io/hackage/v/easytensor.svg)](https://hackage.haskell.org/package/easytensor)

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

Data type `DataFrame` is presented in two versions:

 * `DataFrame t (ds :: [Nat])` - dimensionality of a dataframe is totally known at compile time.
 * `DataFrame t (ds :: [XNat]` - some dimensions may be known at runtime only.

Behind the scenes all data types are implemented as primitive values or primitive byte arrays, aiming at maximum performance.
Tricky layers of newtypes and closed type families (*which are not exposed to a user*) allow some kind of ad-hoc polymorphism:
for example, `Vector t n` implemented as `ByteArray#` is overloaded by a specialized `FloatX2 Float# Float#` in case of `Vector Float 2`.

### Implemenation status and plan

  - [ ] Improve ConcatList and FixedDims inference.
  - [x] Basic implementation of generic ns-dimensional dataframes matrices based on type Float
  - [x] Basic implementation of generic ns-dimensional dataframes based on other types (Double, Int, Word, Int8.. etc.)
  - [ ] Overloaded fast implementation for low-dimensional vectors and matrices: only FloatX2, FloatX3, FloatX4, DoubleX2, DoubleX3, DoubleX4, and Scalar is done.
  - [x] Quaternions
  - [ ] Overloaded fast SIMD implementation based on fastvec (avx2, foreign import prim and llvm)
  - [ ] `Tensor t [Nat] [Nat]` rank (n,m) flexible tensor wrapper
  - [x] Lens-like interfaces - `Numeric.DataFrame.SubSpace`
  - [x] Flexible inference between statically known and runtime known dimensions `Dim [Nat]` and `Dim [XNat]`
  - [ ] Smart MATLAB- or R-like indexing of rows and columns.
