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

A special care should be taken when using `Ord` instances:

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

### Implemenation status and plan

  - [ ] Improve ConcatList and FixedDims inference.
  - [x] Basic implementation of generic ns-dimensional dataframes
  - [x] Quaternions
  - [ ] `Tensor t [Nat] [Nat]` rank (n,m) flexible tensor wrapper
  - [x] Lens-like interfaces - `Numeric.DataFrame.SubSpace`
  - [ ] Smart MATLAB- or R-like indexing of rows and columns.

### Performance TODO

In the end, `easytensor` is supposed to be used in programming for computer graphics, thus it must be very fast.
The library was developed with this idea in mind to allow the room for optimization.

  - [ ] Implement fast comparison with partial help of sameMutableByteArray#
         (e.g. `Numeric.DataFrame.Internal.Array.Family.ArrayBase`)
  - [ ] Add more low-dimensional specialized Array family instances
         (`Numeric.DataFrame.Internal.Array.Family` a few of them are already implemented).
  - [ ] Add lots of rewrite rules for specialized Array family instances.
  - [ ] Specialize class instances for Array family instances.
  - [ ] Implement the same Array family instances using SIMD, activated by a dedicated flag.
        (different Array implementations would stay in different folders, e.g. `src-base`, `src-avx` or `src-sse`)
