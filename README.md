# SentinelArrays.jl

*Array types that can use sentinel values of the element type for special values*

## Installation

The package is registered in the [`General`](https://github.com/JuliaRegistries/General) registry and so can be installed at the REPL with `] add SentinelArrays`.

## Usage
A `SentinelArray` should act/wrap a regular `Array` for any given operation.

Examples
```julia
x = SentinelVector{Float64}(undef, 10)

# use like regular Array
x[1] = 3.14

# when setting the special value, the underlying storage will store the sentinel value instead
sent = x.sentinel
x[2] = missing
@assert x.data[2] === sent

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
