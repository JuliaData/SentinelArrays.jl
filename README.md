# SentinelArrays.jl

[![CI](https://github.com/JuliaData/SentinelArrays.jl/workflows/CI/badge.svg)](https://github.com/JuliaData/SentinelArrays.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/JuliaData/SentinelArrays.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaData/SentinelArrays.jl)
[![deps](https://juliahub.com/docs/SentinelArrays/deps.svg)](https://juliahub.com/ui/Packages/SentinelArrays/uMYVe?t=2)
[![version](https://juliahub.com/docs/SentinelArrays/version.svg)](https://juliahub.com/ui/Packages/SentinelArrays/uMYVe)
[![pkgeval](https://juliahub.com/docs/SentinelArrays/pkgeval.svg)](https://juliahub.com/ui/Packages/SentinelArrays/uMYVe)

*Array types that can use sentinel values of the element type for special values*

## Installation

The package is registered in the [`General`](https://github.com/JuliaRegistries/General) registry and so can be installed at the REPL with `] add SentinelArrays`.

## Usage

    SentinelArray(A::AbstractArray, sentinel, value)
    SentinelVector{T}(undef, len, sentinel, value)
    SentinelArray{T, N}(undef, dims, sentinel, value)

Construct a `SentinelArray` by either wrapping an existing `AbstractArray` or using the standard `undef` pattern, which first constructs an `Array` to be wrapped by `SentinelArray`.

A `SentinelArray{T}` wraps an `AbstractArray` of type `T`, and accepts a `sentinel` and `value` argument. If the wrapped array has an element equal to `sentinel`, `value` is returned instead.
This allows, for example, simulating a `Vector{Union{Float64, Missing}}` by doing something like `SentinelVector{Float64}(undef, len, NaN, missing)`. The resulting `SentinelArray` will have
`eltype` equal to `Union{T, typeof(value)}`.

For isbits types, a random/reasonable sentinel will be attempted if none provided. For non-isbits, `#undef` is used as sentinel. The default value, if not provided, is `missing`.

A `SentinelArray` allows setting an element to `value`, which will result in the `sentinel` being stored in the wrapped array.

For isbits element types, if a `setindex!` operation is attempted with the `SentinelArray`'s current sentinel value, another sentinel will be attempted to be chosen that doesn't already exist in the wrapped array.
If another sentinel can't be found (i.e. the values already exist in the wrapped array), an error will be thrown.

### Caveats
  * For float and date/time types, reasonable sentinels exist that don't have meaningful *real* values, so it's recommended to use the default sentinels
  * For `Integer` types, all bit patterns are valid, so a random value is chosen; if usage will involve extremely high cardinality relative to the type range, consider using `Vector{Union{T, Missing}}` instead
  * `SentinelArray`s are not allowed to have plain `Bool` elements since there isn't a possible sentinel
  * A `SentinelArray` *IS NOT* thread-safe when usage relies on `setindex!` and potential sentinel collision (due to the check to automatically cycle the sentinel to another value); this will normally only affect small `Integer` element types where the potential for sentinel collision is highest
  * Access to a `SentinelArray`'s raw data is possible via `parent(A)` or `pointer(A)`, but note that the raw data will *NOT* respect the sentinel=>value semantics; i.e. the raw data may have entries of the sentinel value.

### Use-cases
  * It is generally recommended to use `Vector{Union{T, typeof(value}}` instead of `SentinelArray` if possible
  * Currently, a non-copying operation like `convert(Vector{T}, A::Vector{Union{T, Missing}})` isn't possible, even though you may know there aren't `missing` values; a `SentinelArray` allows this operation by just "unwrapping" the storage array, like `parent(A)`
  * It's also currently not possible to use `Vector{Union{T, Missing}}` with `Mmap.mmap`; with `SentinelArray`, you can write the parent array to disk and then do `SentinelArray(Mmap.mmap(array_type, file))` (assuming usage of default sentinel/value)

### Examples

```julia
# initialize a new SentinelArray with default sentinel/value
A = SentinelVector{Float64}(undef, 10)

# wrap an existing array, manually providing sentinel/value
parent = zeros(UInt8, 10)
B = SentinelArray(parent, typemax(UInt8), nothing)
```

## Project Status

The package is tested against Julia `1.0`, current stable release, and nightly on Linux, OS X, and Windows.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.

[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-latest-url]: https://JuliaData.github.io/SentinelArrays.jl/latest

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: https://JuliaData.github.io/SentinelArrays.jl/stable

[travis-img]: https://travis-ci.org/JuliaData/SentinelArrays.jl.svg?branch=master
[travis-url]: https://travis-ci.org/JuliaData/SentinelArrays.jl

[codecov-img]: https://codecov.io/gh/JuliaData/SentinelArrays.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaData/SentinelArrays.jl

[issues-url]: https://github.com/JuliaData/SentinelArrays.jl/issues
