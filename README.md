Easytensor: many-dimensional type-safe numeric ops
==================================
[![Build Status](https://travis-ci.org/achirkin/easytensor.svg?branch=v2)](https://travis-ci.org/achirkin/easytensor)

The project consists of two parts:
 * `dimensions` is a library to support type-level operations on lists of dimensions;
 * `easytensor` wraps low-level operations on primitive byte arrays in a type-safe data type indexed over an element type and a list of dimensions.

### dimensions [![Hackage](https://img.shields.io/hackage/v/dimensions.svg)](https://hackage.haskell.org/package/dimensions)

`Data.Type.List` and `Data.Type.Lits` provide type-level operations on lists, `Nat`s, and `Symbols`s.

`Numeric.TypedList` is the core module providing a typelist-indexed type that is, in fact, just a newtype wrapper on a plain haskell list.
The `TypedList (f :: k -> Type) (xs :: [k])` represents a lot of things; by changing type parameter `f`, I use `TypedList` as a flexible (yet typesafe) tuple, a finite dimensions list, or an index over such a list.

`Numeric.Dimensions` provides:
  * promoted type `XNat = N Nat | XN` similar to type `Maybe`; kind `XNat` is used for type variables when some of dimensions in a type-level list are not known at compile time;
  * data type `Dims (ds :: [k])`, where `k` is either `Nat` or `XNat`, together with class `Dimensions ds` it allows lots of type-level operations on type-level dimensionality;
  * data type `Idxs (ds :: [k])` is used to index over many-dimensional space defined by `Dims ds`.

### easytensor [![Hackage](https://img.shields.io/hackage/v/easytensor.svg)](https://hackage.haskell.org/package/easytensor)

This library aims at providing fast, simple, and useful geometry types for use in computer graphics and low-dimensional physics modelling.

All geometry types implement standard `Prelude` numerical classes, such as `Num`, `Fractional`, `Floating`, lexicographical `Ord`
favouring element-wise operations:
functions like `+`, `*`, `/` are all element-wise.
Common matrix and vector operations are provided separately.

Data type `DataFrame` is presented in two versions:

 * `DataFrame t (ds :: [Nat])` - dimensionality of a dataframe is totally known at compile time.
 * `DataFrame t (ds :: [XNat]` - some dimensions may be known at runtime only.

Parameter `t` of a DataFrame can be arbitrary type that has an instance of `PrimBytes`.
`PrimBytes` typeclass can be automatically derived using `Generics`.
This mechanics allows creating interleaved arrays, e.g. `DataFrame (Double, Float, Float) ds`.

Parameter `t` of a `DataFrame` can also be a list of `PrimBytes` types:
this way, `DataFrame` consists of several "columns" or "variables" of different types and same dimensionality.

Behind the scenes all data types are implemented as primitive values or primitive byte arrays, aiming at maximum performance.
Tricky layers of newtypes and closed type families (*which are not exposed to a user*) allow some kind of ad-hoc polymorphism:
for example, `Vector t n` implemented as `ByteArray#` is overloaded by a specialized `FloatX2 Float# Float#` in case of `Vector Float 2`.


### Supported GHC versions

The packages are tested on GHC 8.4+.
`dimensions` may work on GHC 8.2, but the corresponding tests were dropped.
Support of `easytensor` on GHC 8.2 was dropped due to:

  - https://gitlab.haskell.org/ghc/ghc/issues/14058
  - https://gitlab.haskell.org/ghc/ghc/issues/13188
  - Annoying `Semigroup`-`Monoid` story

### What have changed since version 1

Everything! In general, v2 of `easytensor` is meant to show an *expectable* behavior:

  * All `Show` and `Read` instances look like automatically-generated instances for algebraic data types.
  * 0-based indexing instead of 1-based indexing makes conversion between offsets, indices, and `Enum` more intuitive.
  * Order of dimensions in `Dims` list is reversed (the first dimension is "the most significant"), thus making `Ord` instances of `Dims` and `Idxs` coincide with conventional Haskell lexicographic ordering. The implication of this is the next two points.
  * `DataFrame` `Ord` instances are now proper total lexicographic ordering.
  * `DataFrame` layout now is row-first instead of column-first. Therefore, to keep low-level SIMD optimizations of 3D geometry possible, I've had to transpose all matrices in `HomTransform4` class.
  * `Nat`-indexed `Dims`, `Idxs`, and `DataFrame` now have `Generic` and `Data` instances that make these datatypes look like ordinary algebraic data types.
  * More obvious ways to construct `DataFrame`s from pieces.
  * Removed all declarations and modules that may look controversial or do not belong here.
  * Just added more tests and focused on the core functionality and correctness :)
