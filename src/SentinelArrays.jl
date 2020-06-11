module SentinelArrays

using Dates

export SentinelArray, SentinelVector

"""

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

defaultvalue(T) = missing

function defaultsentinel(T)
    !isbitstype(T) && return undef
    return reinterpret(T, rand(UInt8, sizeof(T)))[1]
end
defaultsentinel(::Type{Date}) = Date(Dates.UTD(typemin(Int64)))
defaultsentinel(::Type{DateTime}) = DateTime(Dates.UTM(typemin(Int64)))
defaultsentinel(::Type{Time}) = Time(Nanosecond(typemin(Int64)))
defaultsentinel(::Type{Int64}) = -8899831978349840752

# our own custom NaN bit pattern
sentinelbit(::Type{T}) where {T <: Unsigned} = Core.bitcast(T, T(1) << (8 * sizeof(T) - 1))
function defaultsentinel(::Type{T}) where {T <: Union{Float16, Float32, Float64}}
    nan = Core.bitcast(Base.uinttype(T), T(0) / T(0))
    return Core.bitcast(T, nan | sentinelbit(Base.uinttype(T)))
end

# constructors
function SentinelArray{T, N}(::UndefInitializer, dims::Tuple{Vararg{Integer}}, s=defaultsentinel(T), v=defaultvalue(T)) where {T, N}
    A = Array{T, N}(undef, dims)
    # initilize w/ missing values
    isbitstype(T) && fill!(A, s)
    return SentinelArray(A, s, v)
end

function SentinelArray{T}(::UndefInitializer, dims::Tuple{Vararg{Integer}}, s=defaultsentinel(T), v=defaultvalue(T)) where {T}
    SentinelArray{T, length(dims)}(undef, dims, s, v)
end

SentinelArray{T, N}(::UndefInitializer, dims::Vararg{Integer, N}) where {T, N} = SentinelArray{T, N}(undef, dims)
SentinelArray{T}(::UndefInitializer, dims::Integer...) where {T} = SentinelArray{T, length(dims)}(undef, dims)
SentinelVector{T}(::UndefInitializer, len::Int, s=defaultsentinel(T), v=defaultvalue(T)) where {T} = SentinelArray{T, 1}(undef, (len,), s, v)

function Base.convert(::Type{SentinelArray{T}}, arr::AbstractArray{T2, N}) where {T, T2, N}
    A = SentinelArray(Array{T, N}(undef, size(arr)))
    @inbounds for i in eachindex(arr)
        A[i] = arr[i]
    end
    return A
end

Base.convert(::Type{SentinelArray}, arr::AbstractArray{T}) where {T} = convert(SentinelArray{T}, arr)
Base.convert(::Type{SentinelVector{T}}, arr::AbstractArray) where {T} = convert(SentinelArray{T}, arr)

function Base.similar(A::SentinelArray{T, N, S, V}, ::Type{T2}, dims::Dims{N2}) where {T, N, S, V, T2, N2}
    SentinelArray(similar(parent(A), T2, dims), A.sentinel, A.value)
end

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

function newsentinel!(arrays::SentinelArray{T, N, S, V}...; force::Bool=true) where {T, N, S, V}
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
    newsent = rand(T)
    # find a new sentinel that doesn't already exist in parent
    while true
        foundnewsent = false
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
        !foundnewsent && break
        newsent = rand(T)
        attempts += 1
        attempts > 5 && error("error trying to automatically find a new sentinel for SentinelArray")
    end
    for A in arrays
        recode!(A, newsent)
    end
    return
end

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
    checkbounds(A, i)
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

unset!(A, i) = isassigned(A, i) && ccall(:jl_arrayunset, Cvoid, (Array, Csize_t), A, i - 1)

Base.@propagate_inbounds function Base.setindex!(A::SentinelArray{T, N, S, V}, val, i::Int) where {T, N, S, V}
    checkbounds(A, i)
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
function Base.empty!(A::SentinelVector)
    empty!(parent(A))
    return A
end

Base.copy(A::SentinelArray{T, N}) where {T, N} = SentinelArray(copy(parent(A)), A.sentinel, A.value)

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

function Base.deleteat!(A::SentinelVector, idx)
    deleteat!(parent(A), idx)
    return A
end

function Base.insert!(A::SentinelVector, idx::Integer, item)
    _growat!(A, idx, 1)
    @inbounds A[idx] = item
    return A
end

function Base.vcat(A::SentinelVector{T, S, V}, B::SentinelVector{T, S, V}) where {T, S, V}
    newsentinel!(A, B; force=false)
    return SentinelArray(vcat(parent(A), parent(B)), A.sentinel, A.value)
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

end # module
