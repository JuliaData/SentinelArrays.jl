module SentinelArrays

using Dates, Random

export SentinelArray, SentinelMatrix, SentinelVector, SentinelCollisionError, ChainedVector, MissingVector

"""
    SentinelArray(A::AbstractArray, sentinel, value)
    SentinelVector{T}(undef, len, sentinel, value)
    SentinelMatrix{T}(undef, dims, sentinel, value)
    SentinelArray{T, N}(undef, dims, sentinel, value)

Construct a `SentinelArray` by either wrapping an existing `AbstractArray` or using the standard `undef` pattern, which first constructs an `Array` to be wrapped by `SentinelArray`.

A `SentinelArray{T}` wraps an `AbstractArray` of type `T`, and accepts a `sentinel` and `value` argument. If the wrapped array has an element equal to `sentinel`, `value` is returned instead.
This allows, for example, simulating a `Vector{Union{Float64, Missing}}` by doing something like `SentinelVector{Float64}(undef, len, NaN, missing)`. The resulting `SentinelArray` will have
`eltype` equal to `Union{T, typeof(value)}`.

For isbits types, a random/reasonable sentinel will be attempted if none provided. For non-isbits, `#undef` is used as sentinel. The default value, if not provided, is `missing`.

A `SentinelArray` allows setting an element to `value`, which will result in the `sentinel` being stored in the wrapped array.

For isbits element types, if a `setindex!` operation is attempted with the `SentinelArray`'s current sentinel value, another sentinel will be attempted to be chosen that doesn't already exist in the wrapped array.
If another sentinel can't be found (i.e. the values already exist in the wrapped array), an error will be thrown.

**CAVEATS**:
  * For float and date/time types, reasonable sentinels exist that don't have meaningful *real* values, so it's recommended to use the default sentinels
  * For `Integer` types, all bit patterns are valid, so a random value is chosen; if usage will involve extremely high cardinality relative to the type range, consider using `Vector{Union{T, Missing}}` instead
  * `SentinelArray`s are not allowed to have plain `Bool` elements since there isn't a possible sentinel
  * A `SentinelArray` *IS NOT* thread-safe when usage relies on `setindex!` and potential sentinel collision (due to the check to automatically cycle the sentinel to another value); this will normally only affect small `Integer` element types where the potential for sentinel collision is highest
  * Access to a `SentinelArray`'s raw data is possible via `parent(A)` or `pointer(A)`, but note that the raw data will *NOT* respect the sentinel=>value semantics; i.e. the raw data may have entries of the sentinel value.

Use-cases:
  * It is generally recommended to use `Vector{Union{T, typeof(value}}` instead of `SentinelArray` if possible
  * Currently, a non-copying operation like `convert(Vector{T}, A::Vector{Union{T, Missing}})` isn't possible, even though you may know there aren't `missing` values; a `SentinelArray` allows this operation by just "unwrapping" the storage array, like `parent(A)`
  * It's also currently not possible to use `Vector{Union{T, Missing}}` with `Mmap.mmap`; with `SentinelArray`, you can write the parent array to disk and then do `SentinelArray(Mmap.mmap(array_type, file))` (assuming usage of default sentinel/value)

Examples:
```julia
# initialize a new SentinelArray with default sentinel/value
A = SentinelVector{Float64}(undef, 10)

# wrap an existing array, manually providing sentinel/value
parent = zeros(UInt8, 10)
B = SentinelArray(parent, typemax(UInt8), nothing)
```
"""
mutable struct SentinelArray{T, N, S, V, A <: AbstractArray{T, N}} <: AbstractArray{Union{T, V}, N}
    data::A
    sentinel::S
    value::V

    SentinelArray(A::AbstractArray{Bool, N}, sentinel::S, value::V) where {N, S, V} = error("SentinelArray  not allowed for `Bool` elements (no sentinel available). Try using a `Array{Union{Bool, $V}` instead.")
    function SentinelArray(A::AbstractArray{T, N}, sentinel::S=defaultsentinel(T), value::V=defaultvalue(T)) where {T, N, S, V}
        return new{T, N, S, V, typeof(A)}(A, sentinel, value)
    end
end

Base.parent(A::SentinelArray) = A.data
# users of pointer should be careful because it's the raw storage data, which doesn't respect the sentinel semantics
Base.pointer(A::SentinelArray) = pointer(parent(A))

const SentinelVector{T} = SentinelArray{T, 1}
const SentinelMatrix{T} = SentinelArray{T, 2}

defaultvalue(T) = missing

_newsentinel(T) = reinterpret(T, rand(UInt8, sizeof(T)))[1]
newsentinel(T) = !isbitstype(T) ? undef : Base.invokelatest(_newsentinel, T)
_defaultsentinel(T) = reinterpret(T, fill(0xff, sizeof(T)))[1]
defaultsentinel(T) = !isbitstype(T) ? undef : Base.issingletontype(T) ? throw(ArgumentError("singleton type $T not allowed in a SentinelArray")) : Base.invokelatest(_defaultsentinel, T)

# constructors
function SentinelArray{T, N}(::UndefInitializer, dims::Tuple{Vararg{Integer}}, s=nothing, v=defaultvalue(T)) where {T, N}
    A = Array{T, N}(undef, dims)
    # initilize w/ missing values
    if isbitstype(T)
        if s === nothing
            sent = defaultsentinel(T)
            ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), pointer(A), 0xff, sizeof(A))
        else
            sent = s
            fill!(A, sent)
        end
    else
        sent = something(s, defaultsentinel(T))
    end
    return SentinelArray(A, sent, v)
end

function SentinelArray{T}(::UndefInitializer, dims::Tuple{Vararg{Integer}}, s=nothing, v=defaultvalue(T)) where {T}
    SentinelArray{T, length(dims)}(undef, dims, s, v)
end

SentinelArray{T, N}(::UndefInitializer, dims::Vararg{Integer, N}) where {T, N} = SentinelArray{T, N}(undef, dims)
SentinelArray{T}(::UndefInitializer, dims::Integer...) where {T} = SentinelArray{T, length(dims)}(undef, dims)
SentinelVector{T}(::UndefInitializer, len::Int, s=nothing, v=defaultvalue(T)) where {T} = SentinelArray{T, 1}(undef, (len,), s, v)
SentinelArray{T, N, S, V, A}(::UndefInitializer, dims::Dims) where {T, N, S, V, A} = SentinelArray{T}(undef, dims)

function Base.convert(::Type{SentinelArray{T}}, arr::AbstractArray{T2, N}) where {T, T2, N}
    A = SentinelArray(Array{T, N}(undef, size(arr)))
    @inbounds for i in eachindex(arr)
        A[i] = arr[i]
    end
    return A
end

Base.convert(::Type{SentinelArray}, arr::AbstractArray{T}) where {T} = convert(SentinelArray{T}, arr)
Base.convert(::Type{SentinelVector{T}}, arr::AbstractArray) where {T} = convert(SentinelArray{T}, arr)

Base.empty(A::SentinelVector{T}, ::Type{U}=T) where {T, U} = SentinelVector{U}(undef, 0)
# Base.emptymutable(A::SentinelVector{T}, ::Type{U}=T) where {T, U} = SentinelVector{U}(undef, 0)

# conversion between SentinelArrays
function recode!(A::SentinelArray{T, N, S, V}, newsentinel::S) where {T, N, S, V}
    oldsentinel = A.sentinel
    P = parent(A)
    @simd for i in eachindex(P)
        @inbounds x = P[i]
        @inbounds P[i] = ifelse(eq(x, oldsentinel), newsentinel, x)
    end
    A.sentinel = newsentinel
    return
end

struct SentinelCollisionError <: Exception
    msg
end

function _newsentinel!(arrays::SentinelArray{T, N, S, V}...; newsent, force::Bool=true) where {T, N, S, V}
    if S === UndefInitializer
        # undef can't be recoded
        return
    end
    A = arrays[1]
    oldsent = A.sentinel
    if !force
        if all(x->x.sentinel == oldsent, arrays)
            # all arrays have the same sentinel & we're not forcing a new one
            return
        end
    end
    attempts = 0
    # find a new sentinel that doesn't already exist in parent
    while true
        foundnewsent = eq(oldsent, newsent)
        if !foundnewsent
            for A in arrays
                p = parent(A)
                for i in eachindex(p)
                    @inbounds z = p[i]
                    if eq(z, newsent)
                        foundnewsent = true
                        break
                    end
                end
            end
        end
        !foundnewsent && break
        newsent = newsentinel(T)
        attempts += 1
        attempts > 5 && throw(SentinelCollisionError("error trying to automatically find a new sentinel for SentinelArray; consider using `Vector{Union{$T, $V}}` instead"))
    end
    for A in arrays
        recode!(A, newsent)
    end
    return
end
newsentinel!(arrays::SentinelArray{T, N, S, V}...; kwargs...) where {T, N, S, V} =
    Base.invokelatest(_newsentinel!, arrays...; newsent=newsentinel(T), kwargs...)

# Basic AbstractArray interface definitions
Base.size(A::SentinelArray) = size(parent(A))
Base.size(A::SentinelArray, i) = size(parent(A), i)
Base.length(A::SentinelArray) = length(parent(A))
Base.axes(A::SentinelArray) = axes(parent(A))
Base.axes(A::SentinelArray, i) = axes(parent(A), i)
Base.stride(A::SentinelArray, k::Integer) = stride(parent(A), k)
Base.strides(A::SentinelArray) = strides(parent(A))

Base.IndexStyle(::Type{SentinelArray{T, N, S, V, A}}) where {T, N, S, V, A} = Base.IndexStyle(A)

# we define our own `eq` here because, for example, for NaN, we want ===
eq(x::T, y::T) where {T <: Real} = x === y
# but if they use `missing` as sentinel, we want isequal
eq(x, y) = isequal(x, y)

Base.@propagate_inbounds function Base.getindex(A::SentinelArray{T, N, S, V}, i::Int) where {T, N, S, V}
    @boundscheck checkbounds(A, i)
    if S === UndefInitializer
        @inbounds x = isassigned(parent(A), i) ? parent(A)[i] : A.value
        return x
    elseif Base.issingletontype(S) && Base.issingletontype(V) && S === V
        @inbounds x = parent(A)[i]
        return x
    else
        @inbounds x = parent(A)[i]
        return ifelse(eq(x, A.sentinel), A.value, x)
    end
end

unset!(A, i) = isassigned(A, i) && ccall(:jl_arrayunset, Cvoid, (Any, Csize_t), A, i - 1)

Base.@propagate_inbounds function Base.setindex!(A::SentinelArray{T, N, S, V}, val, i::Int) where {T, N, S, V}
    @boundscheck checkbounds(A, i)
    if S === UndefInitializer
        if eq(val, A.value)
            unset!(parent(A), i)
        else
            parent(A)[i] = val
        end
    elseif Base.issingletontype(S) && Base.issingletontype(V) && S === V
        parent(A)[i] = val
    else
        if eq(val, A.sentinel)
            # trying to set value of our sentinel
            # need to pick a new sentinel
            newsentinel!(A)
        end
        parent(A)[i] = ifelse(eq(val, A.value), A.sentinel, val)
    end
    return val
end

# other AbstractArray functions
function Base.reverse(A::SentinelVector, s::Integer=first(LinearIndices(A)), n::Integer=last(LinearIndices(A)))
    return SentinelArray(reverse(parent(A), s, n), A.sentinel, A.value)
end

function Base.empty!(A::SentinelVector)
    empty!(parent(A))
    return A
end

function Base.resize!(A::SentinelVector{T}, len) where {T}
    oldlen = length(A)
    resize!(parent(A), len)
    if isbitstype(T) && len > oldlen
        sent = A.sentinel
        for i = (oldlen + 1:len)
            @inbounds A.data[i] = sent
        end
    end
    return A
end

function _growat!(A::SentinelVector, i, len)
    Base._growat!(parent(A), i, len)
    return
end

function _deleteat!(A::SentinelVector, i, len)
    Base._deleteat!(parent(A), i, len)
    return
end

function Base.push!(A::SentinelVector, val)
    _growat!(A, length(A) + 1, 1)
    @inbounds A[end] = val
    return A
end

function Base.deleteat!(A::SentinelVector{T, S, V, AT}, inds) where {T, S, V, AT}
    if inds isa Integer || inds isa UnitRange || S !== UndefInitializer
        deleteat!(parent(A), inds)
    elseif inds isa AbstractVector{Bool}
        n = length(A)
        length(inds) == n || throw(BoundsError(A, inds))
        p = 1
        for (q, i) in enumerate(inds)
            if isassigned(A, q)
                @inbounds A[p] = A[q]
            else
                unset!(A, p)
            end
            p += !i
        end
        deleteat!(parent(A), p:n)
    else
        _deleteat!(A, inds)
    end
    return A
end

unset!(A::Vector, i) = ccall(:jl_arrayunset, Cvoid, (Any, Csize_t), A, i - 1)

function _deleteat!(a::SentinelVector, inds)
    n = length(a)
    y = iterate(inds)
    y === nothing && return a
    p, s = y
    checkbounds(a, p)
    q = p + 1
    while true
        y = iterate(inds, s)
        y === nothing && break
        i, s = y
        if !(q <= i <= n)
            if i < q
                throw(ArgumentError("indices must be unique and sorted"))
            else
                throw(BoundsError())
            end
        end
        while q < i
            if isassigned(a, q)
                @inbounds a[p] = a[q]
            else
                unset!(a, p)
            end
            p += 1; q += 1
        end
        q = i + 1
    end
    while q <= n
        if isassigned(a, q)
            @inbounds a[p] = a[q]
        else
            unset!(a, p)
        end
        p += 1; q += 1
    end
    deleteat!(parent(a), p:n)
    return a
end

function Base.insert!(A::SentinelVector, idx::Integer, item)
    _growat!(A, idx, 1)
    @inbounds A[idx] = item
    return A
end

function Base.append!(A::SentinelVector{T, S, V}, B::SentinelVector{T, S, V}) where {T, S, V}
    newsentinel!(A, B; force=false)
    append!(parent(A), parent(B))
    return A
end

function Base.append!(A::SentinelVector{T}, b::AbstractVector) where T
    for x in b
        push!(A, x)
    end
    return A
end

const _default_splice = []

function Base.splice!(A::SentinelVector, i::Integer, ins=_default_splice)
    v = A[i]
    m = length(ins)
    if m == 0
        deleteat!(A, i)
    elseif m == 1
        A[i] = ins[1]
    else
        _growat!(A, i, m-1)
        k = 1
        for x in ins
            A[i+k-1] = x
            k += 1
        end
    end
    return v
end

function Base.splice!(A::SentinelVector, r::UnitRange{<:Integer}, ins=_default_splice)
    v = A[r]
    m = length(ins)
    if m == 0
        deleteat!(A, r)
        return v
    end

    n = length(A)
    f = first(r)
    l = last(r)
    d = length(r)

    if m < d
        delta = d - m
        _deleteat!(A, (f - 1 < n - l) ? f : (l - delta + 1), delta)
    elseif m > d
        _growat!(A, (f - 1 < n - l) ? f : (l + 1), m - d)
    end

    k = 1
    for x in ins
        A[f+k-1] = x
        k += 1
    end
    return v
end

function _growbeg!(A::SentinelVector, n)
    Base._growbeg!(parent(A), n)
    return
end

Base.prepend!(A::SentinelVector, items::AbstractVector) = _prepend!(A, Base.IteratorSize(items), items)

Base.prepend!(A::SentinelVector, iter) = _prepend!(A, Base.IteratorSize(iter), iter)
Base.pushfirst!(A::SentinelVector, iter...) = prepend!(A, iter)

function _prepend!(A, ::Union{Base.HasLength,Base.HasShape}, iter)
    n = length(iter)
    _growbeg!(A, n)
    i = 0
    for item in iter
        @inbounds A[i += 1] = item
    end
    return A
end

function _prepend!(A, ::Base.IteratorSize, iter)
    n = 0
    for item in iter
        n += 1
        pushfirst!(A, item)
    end
    reverse!(A, 1, n)
    return A
end

function Base.pop!(A::SentinelVector)
    if isempty(A)
        throw(ArgumentError("array must be non-empty"))
    end
    item = A[end]
    deleteat!(A, length(A))
    return item
end

function Base.pushfirst!(A::SentinelVector, item)
    _growbeg!(A, 1)
    A[1] = item
    return A
end

function Base.popfirst!(A::SentinelVector)
    if isempty(A)
        throw(ArgumentError("array must be non-empty"))
    end
    item = A[1]
    deleteat!(A, 1)
    return item
end

include("chainedvector.jl")
include("missingvector.jl")

include("precompile.jl")
_precompile_()

end # module
