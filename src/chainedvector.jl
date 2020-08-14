"""
    ChainedVector(arrays::Vector{<:AbstractVector})

Create a `ChainedVector` of a `Vector` of homogenously-typed `AbstractVector`.
The "chain" of input vectors will be treated as a single, long vector.
A full set of typical mutable operations are supported (e.g. `push!`, `append!`, etc.).

As implementation details, mutable operations on single elements (e.g. `setindex!`, `push!`)
operate in-place or mutate an existing chained array, while `append!`/`prepend!` are optimized
to "chain" the incoming array to the existing chained arrays.
"""
struct ChainedVector{T, A <: AbstractVector{T}} <: AbstractVector{T}
    arrays::Vector{A}
    inds::Vector{Int}
end

function ChainedVector(arrays::Vector{A}) where {A <: AbstractVector{T}} where {T}
    n = length(arrays)
    inds = Vector{Int}(undef, n)
    x = 0
    @inbounds for i = 1:n
        # note that arrays[i] can have zero length
        x += length(arrays[i])
        inds[i] = x
    end
    return ChainedVector{T, A}(arrays, inds)
end

Base.IndexStyle(::Type{<:ChainedVector}) = Base.IndexLinear()
Base.size(x::ChainedVector) = (length(x.inds) == 0 ? 0 : x.inds[end],)

@inline function index(A::ChainedVector, i::Integer)
    chunk = searchsortedfirst(A.inds, i)
    return chunk, i - (chunk == 1 ? 0 : @inbounds A.inds[chunk - 1])
end

Base.@propagate_inbounds function Base.getindex(A::ChainedVector, i::Integer)
    @boundscheck checkbounds(A, i)
    chunk, ix = index(A, i)
    @inbounds x = A.arrays[chunk][ix]
    return x
end

Base.@propagate_inbounds function Base.setindex!(A::ChainedVector, v, i::Integer)
    @boundscheck checkbounds(A, i)
    chunk, ix = index(A, i)
    @inbounds A.arrays[chunk][ix] = v
    return v
end

# efficient iteration
@inline function Base.iterate(A::ChainedVector)
    length(A) == 0 && return nothing
    i = 1
    chunk = 1
    chunk_i = 1
    chunk_len = A.inds[1]
    while i > chunk_len
        chunk += 1
        @inbounds chunk_len = A.inds[chunk]
    end
    x = A.arrays[chunk][1]
    # find next valid index
    i += 1
    if i > chunk_len
        while true
            chunk += 1
            chunk > length(A.inds) && break
            @inbounds chunk_len = A.inds[chunk]
            i <= chunk_len && break
        end
    else
        chunk_i += 1
    end
    return x, (i, chunk, chunk_i, chunk_len, length(A))
end

@inline function Base.iterate(A::ChainedVector, (i, chunk, chunk_i, chunk_len, len))
    i > len && return nothing
    @inbounds x = A.arrays[chunk][chunk_i]
    i += 1
    if i > chunk_len
        chunk_i = 1
        while true
            chunk += 1
            chunk > length(A.inds) && break
            @inbounds chunk_len = A.inds[chunk]
            i <= chunk_len && break
        end
    else
        chunk_i += 1
    end
    return x, (i, chunk, chunk_i, chunk_len, len)
end

# other AbstractArray functions
function Base.empty!(A::ChainedVector)
    empty!(A.arrays)
    empty!(A.inds)
    return A
end

function Base.copy(A::ChainedVector{T, AT}) where {T, AT}
    B = similar(AT, length(A))
    off = 1
    for arr in A.arrays
        n = length(arr)
        copyto!(B, off, arr, 1, n)
        off += n
    end
    return B
end

function Base.resize!(A::ChainedVector{T, AT}, len) where {T, AT}
    len >= 0 || throw(ArgumentError("`len` must be >= 0 when resizing ChainedVector"))
    len′ = length(A)
    if len′ < len
        # growing
        push!(A.arrays, similar(AT, len - len′))
        push!(A.inds, len)
    else
        # shrinking
        chunk = searchsortedfirst(A.inds, len)
        # get rid of any excess chunks
        resize!(A.arrays, chunk)
        resize!(A.inds, chunk)
        # resize individual chunk
        resize!(A.arrays[chunk], A.inds[chunk] - len)
        A.inds[chunk] -= A.inds[chunk] - len
    end
    return A
end

function Base.push!(A::ChainedVector{T, AT}, val) where {T, AT}
    if length(A.arrays) == 0
        push!(A.arrays, similar(AT, 0))
        push!(A.inds, 0)
    end
    @inbounds push!(A.arrays[end], val)
    @inbounds A.inds[end] += 1
    return A
end

function Base.pushfirst!(A::ChainedVector{T, AT}, val) where {T, AT}
    if length(A.arrays) == 0
        push!(A.arrays, similar(AT, 0))
        push!(A.inds, 0)
    end
    @inbounds pushfirst!(A.arrays[1], val)
    for i = 1:length(A.inds)
        @inbounds A.inds[i] += 1
    end
    return A
end

Base.@propagate_inbounds function Base.deleteat!(A::ChainedVector, i::Integer)
    @boundscheck checkbounds(A, i)
    chunk, ix = index(A, i)
    deleteat!(A.arrays[chunk], ix)
    for j = chunk:length(A.inds)
        @inbounds A.inds[j] -= 1
    end
    # check if we should remove an empty chunk
    if length(A.arrays[chunk]) == 0
        deleteat!(A.arrays, chunk)
        deleteat!(A.inds, chunk)
    end
    return A
end

Base.@propagate_inbounds function Base.deleteat!(A::ChainedVector, inds)
    y = iterate(inds)
    y === nothing && return A
    i, s = y
    chunk = 1
    N = length(A.inds)
    prevind = 0
    ind = chunk > N ? 0 : A.inds[chunk]
    todelete = Int[]
    deleted = 0
    while y !== nothing
        # find chunk where deleting starts
        while ind < i
            chunk += 1
            chunk > N && throw(BoundsError(A, i))
            prevind = ind
            @inbounds ind = A.inds[chunk]
        end
        # gather all indices for this chunk
        while i <= ind
            push!(todelete, i - prevind)
            y = iterate(inds, s)
            y === nothing && break
            i, s = y
        end
        # delete indices from this chunk
        deleted += length(todelete)
        @inbounds A.inds[chunk] = ind - deleted
        @inbounds deleteat!(A.arrays[chunk], todelete)
        empty!(todelete)
    end
    # update unaffected chunks
    for j = (chunk + 1):length(A.arrays)
        A.inds[j] -= deleted
    end
    # check for empty chunks and remove
    for j = length(A.inds):-1:1
        if length(A.arrays[j]) == 0
            deleteat!(A.arrays, j)
            deleteat!(A.inds, j)
        end
    end
    return A
end

Base.@propagate_inbounds function Base.deleteat!(A::ChainedVector, inds::AbstractVector{Bool})
    length(inds) == length(A) || throw(BoundsError(A, inds))
    for i = length(A):-1:1
        if inds[i]
            deleteat!(A, i)
        end
    end
    return A
end

function Base.pop!(A::ChainedVector)
    if isempty(A)
        throw(ArgumentError("array must be non-empty"))
    end
    item = A[end]
    deleteat!(A, length(A))
    return item
end

function Base.popfirst!(A::ChainedVector)
    if isempty(A)
        throw(ArgumentError("array must be non-empty"))
    end
    item = A[1]
    deleteat!(A, 1)
    return item
end

Base.@propagate_inbounds function Base.insert!(A::ChainedVector{T, AT}, i::Integer, item) where {T, AT <: AbstractVector{T}}
    if i == 1 && length(A.arrays) == 0
        push!(A.arrays, similar(AT, 0))
        push!(A.inds, 0)
        chunk, ix = 1, 1
    else
        @boundscheck checkbounds(A, i)
        chunk, ix = index(A, i)
    end
    insert!(A.arrays[chunk], ix, item)
    for j = chunk:length(A.inds)
        @inbounds A.inds[j] += 1
    end
    return A
end

function Base.vcat(A::ChainedVector{T, AT}, arrays::ChainedVector{T, AT}...) where {T, AT <: AbstractVector{T}}
    newarrays = vcat(A.arrays, map(x->x.arrays, arrays)...)
    n = length(A.inds)
    inds = Vector{Int}(undef, n + sum(x->length(x.inds), arrays))
    copyto!(inds, 1, A.inds, 1, n)
    m = n + 1
    for x in arrays
        for y in x.arrays
            @inbounds inds[m] = ((m - 1) == 0 ? 0 : inds[m - 1]) + length(y)
            m += 1
        end
    end
    return ChainedVector{T, AT}(newarrays, inds)
end

function Base.append!(A::ChainedVector{T, AT}, B::AT) where {T, AT <: AbstractVector{T}}
    push!(A.arrays, B)
    push!(A.inds, A.inds[end] + length(B))
    return A
end

function Base.append!(A::ChainedVector{T, AT}, B::ChainedVector{T, AT}) where {T, AT <: AbstractVector{T}}
    append!(A.arrays, B.arrays)
    n = length(A.inds)
    m = length(B.inds)
    resize!(A.inds, n + m)
    for i = 1:m
        @inbounds A.inds[n + i] = ((n + i - 1) == 0 ? 0 : A.inds[n + i - 1]) + length(B.arrays[i])
    end
    return A
end

function Base.append!(A::ChainedVector{T}, B) where {T}
    for x in B
        push!(A, x)
    end
    return A
end

function Base.prepend!(A::ChainedVector{T, AT}, B::AT) where {T, AT <: AbstractVector{T}}
    pushfirst!(A.arrays, B)
    n = length(B)
    pushfirst!(A.inds, n)
    for i = 2:length(A.inds)
        @inbounds A.inds[i] += n
    end
    return A
end

function Base.prepend!(A::ChainedVector{T, AT}, B::ChainedVector{T, AT}) where {T, AT <: AbstractVector{T}}
    prepend!(A.arrays, B.arrays)
    n = length(A.inds)
    m = length(B.inds)
    M = length(B)
    prepend!(A.inds, B.inds)
    for i = 1:n
        @inbounds A.inds[m + i] += M
    end
    return A
end

function Base.prepend!(A::ChainedVector{T}, B) where {T}
    for (i, x) in enumerate(B)
        insert!(A, i, x)
    end
    return A
end
