module SentinelArrays

export SentinelArray, SentinelVector

struct SentinelArray{T, N, S, V} <: AbstractArray{T, N}
    data::Array{T, N}
end

const SentinelVector{T} = SentinelArray{T, 1}

defaultvalue(x) = missing

sentinelbit(::Type{T}) where {T <: Unsigned} = Core.bitcast(T, T(1) << (8 * sizeof(T) - 1))

defaultsentinel(::Type{String}) = undef
defaultsentinel(::Type{Int64}) = -8899831978349840752

function defaultsentinel(::Type{T}) where {T <: Union{Float16, Float32, Float64}}
    nan = Core.bitcast(Base.uinttype(T), T(0) / T(0))
    return Core.bitcast(T, nan | sentinelbit(Base.uinttype(T)))
end

function SentinelArray{T, N}(A::Array{T, N}) where {T, N}
    S = defaultsentinel(T)
    V = defaultvalue(T)
    return SentinelArray{T, N, S, V}(A)
end

function SentinelArray{T, N}(A::Array{T, N}, sentinel, v) where {T, N}
    return SentinelArray{T, N, sentinel, v}(A)
end

Base.size(A::SentinelArray) = size(A.data)
Base.IndexStyle(::Type{<:SentinelArray}) = IndexLinear()
Base.@propagate_inbounds function Base.getindex(A::SentinelArray{T, N, S, V}, i::Int) where {T, N, S, V}
    checkbounds(A, i)
    if isbitstype(T)
        @inbounds x = A.data[i]
        return ifelse(x === S, V, x)
    else
        return isassigned(A.data, i) ? A.data[i] : V
    end
end

unset!(A, i) = isassigned(A, i) && ccall(:jl_arrayunset, Cvoid, (Array, Csize_t), A, i - 1)

Base.@propagate_inbounds function Base.setindex!(A::SentinelArray{T, N, S, V}, val, i::Int) where {T, N, S, V}
    checkbounds(A, i)
    if isbitstype(T)
        A.data[i] = ifelse(val === V, S, val)
    else
        if val === V
            unset!(A.data, i)
        else
            A.data[i] = val
        end
    end
    val
end

function Base.fill!(A::SentinelArray{T, N, S, V}, val) where {T, N, S, V}
    if isbitstype(T)
        fill!(A.data, ifelse(val === V, S, val))
    else
        if val === V
            foreach(i->unset!(A.data, i), eachindex(A.data))
        else
            fill!(A.data, val)
        end
    end
    A
end

function SentinelArray{T, N}(::UndefInitializer, dims::Tuple{Vararg{Integer}}) where {T, N}
    SentinelArray{T, N}(Array{T, N}(undef, dims))
end

function SentinelArray{T}(::UndefInitializer, dims::Tuple{Vararg{Integer}}) where {T}
    SentinelArray{T, length(dims)}(undef, dims)
end

SentinelArray{T, N}(::UndefInitializer, dims::Vararg{Integer, N}) where {T, N} = SentinelArray{T, N}(undef, dims)
SentinelArray{T}(::UndefInitializer, dims::Integer...) where {T} = SentinelArray{T, length(dims)}(undef, dims)

function Base.convert(::Type{SentinelArray{T}}, arr::AbstractArray{T, N}) where {T, N}
    s = SentinelArray{T, N}(undef, size(arr))
    @inbounds for i in eachindex(arr)
        s.data[i] = arr[i]
    end
    s
end

Base.convert(::Type{SentinelArray}, arr::AbstractArray{T}) where {T} = convert(SentinelArray{T}, arr)
SentinelVector{T}(::UndefInitializer, len::Int) where {T} = SentinelArray{T}(undef, len)

function Base.similar(a::SentinelArray, T, dims::Tuple{Vararg{Int64, N}}) where N
    SentinelArray{T, N}(undef, dims)
end

function Base.empty!(a::SentinelVector)
    empty!(a.data)
    a
end

Base.copy(a::SentinelArray{T, N}) where {T, N} = SentinelArray{T, N}(copy(a.data))

function Base.resize!(arr::SentinelVector, len)
    resize!(arr.data, len)
    arr
end

function Base.push!(arr::SentinelVector, val)
    push!(arr.data, val)
    arr
end

function Base.deleteat!(arr::SentinelVector, idx)
    deleteat!(arr.data, idx)
    arr
end

function Base.insert!(arr::SentinelVector, idx::Integer, item)
    insert!(arr.data, idx, item)
    arr
end

function Base.permute!(arr::SentinelArray, p::AbstractVector)
    permute!(arr.data, p)
    arr
end

function Base.sortperm(arr::SentinelArray)
    sortperm(arr.data)
end

function Base.sort!(arr::SentinelArray)
    permute!(arr, sortperm(arr))
    arr
end

function Base.vcat(a::SentinelVector{T}, b::SentinelVector{T}) where T
    SentinelVector{T}(vcat(a.data, b.data))
end

function Base.append!(a::SentinelVector{T}, b::SentinelVector) where T
    append!(a.data, b.data)
    a
end

function Base.append!(a::SentinelVector{T}, b::AbstractVector) where T
    for x in b
        push!(a, x)
    end
    a
end

function _growat!(a::SentinelVector, i, len)
    Base._growat!(a.data, i, len)
    return
end

function _deleteat!(a::SentinelVector, i, len)
    Base._deleteat!(a.data, i, len)
    return
end

const _default_splice = []

function Base.splice!(a::SentinelVector, i::Integer, ins=_default_splice)
    v = a[i]
    m = length(ins)
    if m == 0
        deleteat!(a, i)
    elseif m == 1
        a[i] = ins[1]
    else
        _growat!(a, i, m-1)
        k = 1
        for x in ins
            a[i+k-1] = x
            k += 1
        end
    end
    return v
end

function Base.splice!(a::SentinelVector, r::UnitRange{<:Integer}, ins=_default_splice)
    v = a[r]
    m = length(ins)
    if m == 0
        deleteat!(a, r)
        return v
    end

    n = length(a)
    f = first(r)
    l = last(r)
    d = length(r)

    if m < d
        delta = d - m
        _deleteat!(a, (f - 1 < n - l) ? f : (l - delta + 1), delta)
    elseif m > d
        _growat!(a, (f - 1 < n - l) ? f : (l + 1), m - d)
    end

    k = 1
    for x in ins
        a[f+k-1] = x
        k += 1
    end
    return v
end

function _growbeg!(a::SentinelVector, n)
    Base._growbeg!(a.data, n)
    return
end

function Base.prepend!(a::SentinelVector, items::AbstractVector)
    itemindices = eachindex(items)
    n = length(itemindices)
    _growbeg!(a, n)
    if a === items
        copyto!(a, 1, items, n+1, n)
    else
        copyto!(a, 1, items, first(itemindices), n)
    end
    return a
end

Base.prepend!(a::SentinelVector, iter) = _prepend!(a, Base.IteratorSize(iter), iter)
Base.pushfirst!(a::SentinelVector, iter...) = prepend!(a, iter)

function _prepend!(a, ::Union{Base.HasLength,Base.HasShape}, iter)
    n = length(iter)
    _growbeg!(a, n)
    i = 0
    for item in iter
        @inbounds a[i += 1] = item
    end
    a
end

function _prepend!(a, ::Base.IteratorSize, iter)
    n = 0
    for item in iter
        n += 1
        pushfirst!(a, item)
    end
    reverse!(a, 1, n)
    a
end

function Base.pop!(a::SentinelVector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[end]
    deleteat!(a, length(a))
    return item
end

function Base.pushfirst!(a::SentinelVector, item)
    _growbeg!(a, 1)
    a[1] = item
    return a
end

function Base.popfirst!(a::SentinelVector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[1]
    deleteat!(a, 1)
    return item
end


end # module
