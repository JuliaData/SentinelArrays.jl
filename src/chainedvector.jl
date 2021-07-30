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
    inds = Vector{Int}(undef, length(arrays))
    setinds!(arrays, inds)
    return ChainedVector{T, A}(arrays, inds)
end

@inline function setinds!(arrays, inds)
    cleanup!(arrays, inds)
    x = 0
    @inbounds for i = 1:length(arrays)
        x += length(arrays[i])
        inds[i] = x
    end
    return
end

@noinline cleanup!(x::ChainedVector) = cleanup!(x.arrays, x.inds)

@inline function cleanup!(arrays, inds)
    @assert length(arrays) == length(inds)
    for i = length(arrays):-1:1
        if !isassigned(arrays, i) || length(arrays[i]) == 0
            deleteat!(arrays, i)
            deleteat!(inds, i)
        end
    end
    return
end

Base.IndexStyle(::Type{<:ChainedVector}) = Base.IndexLinear()
Base.size(x::ChainedVector) = (length(x.inds) == 0 ? 0 : x.inds[end],)

# interpolated search: better for very large (> 100) length(A.arrays)
# @inline function index(A::ChainedVector, i::Integer)
#     inds = A.inds
#     @inbounds interp = max(1, fld(i * length(inds), inds[end]))
#     @inbounds ind = inds[interp]
#     if i <= ind
#         interp == 1 && return (1, i)
#         @inbounds while interp > 1 && i <= inds[interp - 1]
#             interp -= 1
#         end
#         return interp == 1 ? (1, i) : (interp, i - @inbounds(inds[interp - 1]))
#     else
#         chunk = searchsortedfirst(A.inds, i)
#         return chunk, i - (chunk == 1 ? 0 : @inbounds A.inds[chunk - 1])
#     end
# end

# linear search: better for small (< 30) length(A.arrays)
# @inline function index(A::ChainedVector, i::Integer)
#     inds = A.inds
#     chunk = 1
#     @inbounds ind = inds[chunk]
#     while i > ind
#         chunk += 1
#         @inbounds ind = inds[chunk]
#     end
#     return chunk, i - (chunk == 1 ? 0 : @inbounds inds[chunk - 1])
# end

# binary search
@inline function index(A::ChainedVector, i::Integer)
    @inbounds chunk = searchsortedfirst(A.inds, i)
    return chunk, i - (chunk == 1 ? 0 : @inbounds A.inds[chunk - 1])
end

Base.@propagate_inbounds function Base.getindex(A::ChainedVector, i::Integer)
    @boundscheck checkbounds(A, i)
    chunk, ix = index(A, i)
    @inbounds x = A.arrays[chunk][ix]
    return x
end

@inline function linearindex(x::ChainedVector, chunk, chunklen, j, i, i2)
    # i2 == 0 && throw(BoundsError(x, i2))
    i == i2 && return chunk, chunklen, j
    diff = i2 - i
    if diff > 0
        # linear search forward
        # quick check if we're in the same chunk
        if j + diff <= chunklen
            # fastest path
            return chunk, chunklen, j + diff
        end
        inds = x.inds
        chunk += 1
        N = length(x.arrays)
        # chunk > N && throw(BoundsError(x, i2))
        @inbounds ind = inds[chunk]
        while i2 > ind
            chunk += 1
            # chunk > N && throw(BoundsError(x, i2))
            @inbounds ind = inds[chunk]
        end
        # chunk now points to correct chunk where i2 is found
        @inbounds chunklen = length(x.arrays[chunk])
        # convert to local chunk index
        return chunk, chunklen, chunklen - (ind - i2)
    end
    # linear search backward
    # quick check if we're in the same chunk
    if j + diff >= 1
        # fastest path
        return chunk, chunklen, j + diff
    end
    inds = x.inds
    chunk -= 1
    # chunk == 0 && throw(BoundsError(x, i2))
    @inbounds ind = inds[chunk]
    while chunk > 1 && i2 <= inds[chunk - 1]
        chunk -= 1
        @inbounds ind = inds[chunk]
    end
    # chunk now points to correct chunk where i2 is found
    @inbounds chunklen = length(x.arrays[chunk])
    # convert to local chunk index
    return chunk, chunklen, chunklen - (ind - i2)
end

function over(len, N=Threads.nthreads())
    nlen, r = divrem(len, N)
    return (((i - 1) * nlen + 1, i * nlen + ifelse(i == N, r, 0)) for i = 1:N)
end

Base.@propagate_inbounds function Base.getindex(x::ChainedVector{T, A}, inds::AbstractVector{Int}) where {T, A}
    isempty(inds) && return similar(x, 0)
    len = length(inds)
    res = similar(x.arrays[1], len)
    # N = Threads.nthreads()
    # ranges = (((j - 1) * div(len, N) + 1, j == N ? len : j * div(len, N) + 1) for j = 1:N)
    # @sync for (start, stop) in ranges
    #     Threads.@spawn begin
    #         chunk = j = ind = 1
    #         chunklen = length(x.arrays[1])
    #         arrays = x.arrays
    #         for i = start:stop
    #             @inbounds ind2 = inds[i]
    #             # @boundscheck checkbounds(x, ind2)
    #             chunk, chunklen, j = linearindex(x, chunk, chunklen, j, ind, ind2)
    #             @inbounds res[i] = arrays[chunk][j]
    #             ind = ind2
    #         end
    #     end
    # end
    chunk = j = ind = 1
    chunklen = length(x.arrays[1])
    arrays = x.arrays
    for i = 1:len
        @inbounds ind2 = inds[i]
        # @boundscheck checkbounds(x, ind2)
        chunk, chunklen, j = linearindex(x, chunk, chunklen, j, ind, ind2)
        @inbounds res[i] = arrays[chunk][j]
        ind = ind2
    end
    return res
end

Base.@propagate_inbounds function Base.isassigned(A::ChainedVector, i::Integer)
    @boundscheck checkbounds(A, i)
    chunk, ix = index(A, i)
    return @inbounds isassigned(A.arrays[chunk], ix)
end

Base.@propagate_inbounds function Base.setindex!(A::ChainedVector, v, i::Integer)
    @boundscheck checkbounds(A, i)
    chunk, ix = index(A, i)
    @inbounds A.arrays[chunk][ix] = v
    return v
end

# custom index type used in eachindex
struct ChainedVectorIndex{A} <: Integer
    array::A
    array_i::Int
    i::Int
end

import Base: +, -, *, <, >, <=, >=, ==
for f in (:+, :-, :*, :<, :>, :<=, :>=, :(==))
    @eval $f(a::ChainedVectorIndex, b::Integer) = $f(a.i, b)
    @eval $f(a::Integer, b::ChainedVectorIndex) = $f(a, b.i)
end
Base.convert(::Type{T}, x::ChainedVectorIndex) where {T <: Union{Signed, Unsigned}} = convert(T, x.i)

@inline Base.getindex(x::ChainedVectorIndex) = @inbounds x.array[x.array_i]

@inline function Base.getindex(A::ChainedVector, x::ChainedVectorIndex)
    return @inbounds x.array[x.array_i]
end

@inline function Base.setindex!(A::ChainedVector, v, x::ChainedVectorIndex)
    @inbounds x.array[x.array_i] = v
    return v
end

function Base.getindex(A::ChainedVector{T}, inds::AbstractVector{<:ChainedVectorIndex}) where {T}
    len = length(inds)
    x = Vector{T}(undef, len)
    for i = 1:len
        x[i] = inds[i][]
    end
    return x
end

# efficient iteration via eachindex
struct IndexIterator{A}
    arrays::Vector{A}
end

Base.size(x::IndexIterator) = (length(x),)
Base.length(x::IndexIterator) = sum(length, x.arrays)
Base.eltype(::Type{IndexIterator{A}}) where {A <: AbstractVector} = ChainedVectorIndex{A}

@inline function Base.eachindex(A::ChainedVector)
    # check for and remove any empty chunks
    cleanup!(A)
    return IndexIterator(A.arrays)
end

@inline function Base.iterate(x::IndexIterator)
    arrays = x.arrays
    length(arrays) == 0 && return nothing
    chunkidx = chunk_i = 1
    @inbounds chunk = arrays[chunkidx]
    # we already ran cleanup! so chunks are guaranteed non-empty
    return ChainedVectorIndex(chunk, chunk_i, 1), (arrays, chunkidx, chunk, length(chunk), chunk_i + 1, 2)
end

@inline function Base.iterate(x::IndexIterator, (arrays, chunkidx, chunk, chunklen, chunk_i, i))
    if chunk_i > chunklen
        chunkidx += 1
        chunkidx > length(arrays) && return nothing
        @inbounds chunk = arrays[chunkidx]
        chunklen = length(chunk)
        chunk_i = 1
    end
    return ChainedVectorIndex(chunk, chunk_i, i), (arrays, chunkidx, chunk, chunklen, chunk_i + 1, i + 1)
end

@inline function Base.iterate(A::ChainedVector)
    idx = eachindex(A)
    state = iterate(idx)
    state === nothing && return nothing
    ci, st = state
    return ci[], (idx, st)
end

@inline function Base.iterate(A::ChainedVector, (idx, st))
    state = iterate(idx, st)
    state === nothing && return nothing
    ci, st = state
    return ci[], (idx, st)
end

# other AbstractArray functions
Base.similar(x::ChainedVector) = similar(x, length(x))
Base.similar(x::ChainedVector{T}, len::Base.DimOrInd) where {T} = similar(x, T, len)

function Base.similar(x::ChainedVector{T}, ::Type{S}, len::Base.DimOrInd) where {T, S}
    if len == length(x)
        # return same chunks structure as x
        return ChainedVector([similar(A, S, length(A)) for A in x.arrays])
    end
    # otherwise, split the different new len over existing # of chunks in x
    N = length(x.arrays)
    if N == 0
        return ChainedVector([similar(eltype(x.arrays), len)])
    elseif len <= N
        return ChainedVector([similar(x.arrays[1], S, len)])
    end
    nlen, r = divrem(len, N)
    return ChainedVector([similar(A, S, nlen + (i == N ? r : 0)) for (i, A) in enumerate(x.arrays)])
end

Base.copyto!(dest::ChainedVector, src::AbstractVector) =
    copyto!(dest, 1, src, 1, length(src))
Base.copyto!(dest::ChainedVector, doffs::Union{Signed, Unsigned}, src::AbstractVector) =
    copyto!(dest, doffs, src, 1, length(src))
Base.copyto!(dest::ChainedVector, doffs::Union{Signed, Unsigned}, src::AbstractVector, soffs::Union{Signed, Unsigned}) =
    copyto!(dest, doffs, src, soffs, length(src) - soffs + 1)

function Base.copyto!(dest::ChainedVector{T}, doffs::Union{Signed, Unsigned},
    src::AbstractVector, soffs::Union{Signed, Unsigned}, n::Union{Signed, Unsigned}) where {T}
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    (doffs > 0 && (doffs + n - 1) <= length(dest) &&
    soffs > 0 && (soffs + n - 1) <= length(src)) || throw(ArgumentError("out of range arguments to copyto! on ChainedVector"))
    n == 0 && return dest
    N = length(dest.inds)
    # find first chunk where we'll start copying to
    aidx, _ = index(dest, doffs)
    prevind = aidx == 1 ? 0 : dest.inds[aidx - 1]
    while true
        # aidx now points to chunk where we need to copy
        A = dest.arrays[aidx]
        # now compute how many elements to copy to this chunk
        off = doffs - prevind
        chunkn = min(length(A) - off + 1, n)
        copyto!(A, off, view(src, soffs:(soffs + chunkn - 1)))
        soffs += chunkn
        n -= chunkn
        prevind = dest.inds[aidx]
        aidx += 1
        (aidx > N || n == 0) && break
        doffs = prevind + 1
    end
    return dest
end

Base.copyto!(dest::AbstractVector, src::ChainedVector) =
    copyto!(dest, 1, src, 1, length(src))
Base.copyto!(dest::AbstractVector, doffs::Union{Signed, Unsigned}, src::ChainedVector) =
    copyto!(dest, doffs, src, 1, length(src))
Base.copyto!(dest::AbstractVector, doffs::Union{Signed, Unsigned}, src::ChainedVector, soffs::Union{Signed, Unsigned}) =
    copyto!(dest, doffs, src, soffs, length(src) - soffs + 1)

function Base.copyto!(dest::AbstractVector{T}, doffs::Union{Signed, Unsigned},
    src::ChainedVector, soffs::Union{Signed, Unsigned}, n::Union{Signed, Unsigned}) where {T}
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    (doffs > 0 && (doffs + n - 1) <= length(dest) &&
    soffs > 0 && (soffs + n - 1) <= length(src)) || throw(ArgumentError("out of range arguments to copyto! on ChainedVector"))
    n == 0 && return dest
    N = length(src.inds)
    # find first chunk where we'll start copying from
    aidx, i = index(src, soffs)
    while true
        # aidx now points to chunk where we need to copy from
        A = src.arrays[aidx]
        chunkn = min(length(A) - i + 1, n)
        copyto!(dest, doffs, view(A, i:(i + chunkn - 1)))
        n -= chunkn
        aidx += 1
        (aidx > N || n == 0) && break
        doffs += chunkn
        i = 1
    end
    return dest
end

Base.copyto!(dest::ChainedVector, src::ChainedVector) =
    copyto!(dest, 1, src, 1, length(src))
Base.copyto!(dest::ChainedVector, doffs::Union{Signed, Unsigned}, src::ChainedVector) =
    copyto!(dest, doffs, src, 1, length(src))
Base.copyto!(dest::ChainedVector, doffs::Union{Signed, Unsigned}, src::ChainedVector, soffs::Union{Signed, Unsigned}) =
    copyto!(dest, doffs, src, soffs, length(src) - soffs + 1)

function Base.copyto!(dest::ChainedVector{T}, doffs::Union{Signed, Unsigned},
    src::ChainedVector, soffs::Union{Signed, Unsigned}, n::Union{Signed, Unsigned}) where {T}
    n < 0 && throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    (doffs > 0 && (doffs + n - 1) <= length(dest) &&
    soffs > 0 && (soffs + n - 1) <= length(src)) || throw(ArgumentError("out of range arguments to copyto! on ChainedVector"))
    n == 0 && return dest
    cleanup!(src)
    # find first chunk where we'll start copying to
    dN = length(dest.inds)
    didx, di = index(dest, doffs)
    dchunk = dest.arrays[didx]
    # find first chunk where we'll start copying from
    sN = length(src.inds)
    sidx, si = index(src, soffs)
    schunk = src.arrays[sidx]
    while true
        dlen = length(dchunk) - di + 1
        slen = length(schunk) - si + 1
        chunkn = min(dlen, slen, n)
        copyto!(dchunk, di, schunk, si, chunkn)
        n -= chunkn
        n == 0 && break
        if chunkn == dlen
            didx += 1
            @inbounds dchunk = dest.arrays[didx]
            di = 1
        end
        if chunkn == slen
            sidx += 1
            @inbounds schunk = src.arrays[sidx]
            si = 1
        end
    end
    return dest
end

function Base.empty!(A::ChainedVector)
    empty!(A.arrays)
    empty!(A.inds)
    return A
end

function Base.copy(A::ChainedVector{T}) where {T}
    isempty(A) && return T[]
    B = similar(A.arrays[1], length(A))
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
    cleanup!(A)
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
        @inbounds deleteat!(A.arrays[chunk], todelete)
        empty!(todelete)
    end
    # reset inds
    x = 0
    @inbounds for j = 1:N
        # note that A.arrays[j] can have zero length
        len = length(A.arrays[j])
        if len == 0
            push!(todelete, j)
        else
            x += len
            A.inds[j] = x
        end
    end
    for j in Iterators.reverse(todelete)
        deleteat!(A.arrays, j)
        deleteat!(A.inds, j)
    end
    return A
end

Base.@propagate_inbounds function Base.deleteat!(A::ChainedVector, inds::AbstractVector{Bool})
    length(inds) == length(A) || throw(BoundsError(A, inds))
    prevind = 0
    for array in A.arrays
        len = length(array)
        deleteat!(array, view(inds, (prevind + 1):(prevind + len)))
        prevind += len
    end
    setinds!(A.arrays, A.inds)
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
    lastind = length(A.arrays) == 0 ? 0 : A.inds[end]
    push!(A.arrays, B)
    push!(A.inds, lastind + length(B))
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

Base.in(x, A::ChainedVector) = any(y->x in y, A.arrays)

Base.foreach(f::F, x::ChainedVector) where {F} = foreach(x->foreach(f, x), x.arrays)
function Base.map(f::F, x::ChainedVector) where {F}
    fxs = [map(f, y) for y in x.arrays]
    T = Base.promote_typeof(fxs...)
    return ChainedVector(copyto!(Vector{T}(undef, length(x.inds)), fxs))
end

# function Base.map(f::F, x::ChainedVector) where {F}
#     tasks = map(A -> Threads.@spawn(map(f, A)), x.arrays)
#     ChainedVector([fetch(tsk) for tsk in tasks])
# end

function Base.map!(f::F, A::AbstractVector, x::ChainedVector) where {F}
    length(A) >= length(x) || throw(ArgumentError("destination must be at least as long as map! source"))
    idx = eachindex(A)
    st = iterate(idx)
    for array in x.arrays
        for y in array
            @inbounds A[st[1]] = f(y)
            st = iterate(idx, st[2])
        end
    end
    return A
end

function Base.map!(f::F, x::ChainedVector, A::AbstractVector) where {F}
    length(x) >= length(A) || throw(ArgumentError("destination must be at least as long as map! source"))
    idx = eachindex(A)
    st = iterate(idx)
    for array in x.arrays
        for j in eachindex(array)
            @inbounds array[j] = f(A[st[1]])
            st = iterate(idx, st[2])
        end
    end
    return x
end

function Base.map!(f::F, x::ChainedVector, y::ChainedVector{T}) where {F, T}
    length(x) >= length(y) || throw(ArgumentError("destination must be at least as long as map! source"))
    # check for potential fastpath
    N = length(y.arrays)
    if length(x.arrays) == N
        match = true
        for i = 1:N
            @inbounds match &= length(x.arrays[i]) == length(y.arrays[i])
        end
        if match
            # common when x was created like `similar(y, length(x))`
            for i = 1:length(x.arrays)
                @inbounds map!(f, x.arrays[i], y.arrays[i])
            end
            return x
        end
    end
    # slower path
    cleanup!(y)
    yidx = yi = 1
    @inbounds ychunk = y.arrays[yidx]
    ychunklen = length(ychunk)
    for array in x.arrays
        for j in eachindex(array)
            @inbounds array[j] = f(ychunk[yi])
            yi += 1
            if yi > ychunklen
                yi = 1
                yidx += 1
                yidx > N && @goto done
                @inbounds ychunk = y.arrays[yidx]
                ychunklen = length(ychunk)
            end
        end
    end
@label done
    return x
end

Base.any(f::Function, x::ChainedVector) = any(y -> any(f, y), x.arrays)
Base.any(x::ChainedVector) = any(y -> any(y), x.arrays)
Base.all(f::Function, x::ChainedVector) = all(y -> all(f, y), x.arrays)
Base.all(x::ChainedVector) = all(y -> all(y), x.arrays)

Base.reduce(op::OP, x::ChainedVector) where {OP} = reduce(op, (reduce(op, y) for y in x.arrays))
Base.foldl(op::OP, x::ChainedVector) where {OP} = foldl(op, (foldl(op, y) for y in x.arrays))
Base.foldr(op::OP, x::ChainedVector) where {OP} = foldr(op, (foldr(op, y) for y in x.arrays))
Base.mapreduce(f::F, op::OP, x::ChainedVector) where {F, OP} = reduce(op, (mapreduce(f, op, y) for y in x.arrays))
Base.mapfoldl(f::F, op::OP, x::ChainedVector) where {F, OP} = foldl(op, (mapfoldl(f, op, y) for y in x.arrays))
Base.mapfoldr(f::F, op::OP, x::ChainedVector) where {F, OP} = foldr(op, (mapfoldr(f, op, y) for y in x.arrays))
Base.count(f::F, x::ChainedVector) where {F} = isempty(x) ? 0 : sum(count(f, y) for y in x.arrays)
Base.count(x::ChainedVector) = isempty(x) ? 0 : sum(count(y) for y in x.arrays)

Base.extrema(x::ChainedVector) = extrema(identity, x)

function Base.extrema(f::F, x::ChainedVector) where {F}
    isempty(x) && throw(ArgumentError("collection must be non-empty"))
    mi = ma = nothing
    for A in x.arrays
        mi2, ma2 = extrema(f, A)
        if mi === nothing || mi2 < mi
            mi = mi2
        end
        if ma === nothing || ma2 > ma
            ma = ma2
        end
    end
    return mi, ma
end

function Base.findmax(f::F, x::ChainedVector) where {F}
    isempty(x) && throw(ArgumentError("collection must be non-empty"))
    cleanup!(x) # get rid of any empty arrays
    i = 1
    y = f(x.arrays[1][1])
    return findXwithfirst(!isless, f, x, y, i)
end

function Base.findmin(f::F, x::ChainedVector) where {F}
    isempty(x) && throw(ArgumentError("collection must be non-empty"))
    cleanup!(x) # get rid of any empty arrays
    i = 1
    y = f(x.arrays[1][1])
    return findXwithfirst(isless, f, x, y, i)
end

function findXwithfirst(comp, f, x, y, i)
    i′ = 1
    for A in x.arrays
        for y′ in A
            y′′ = f(y′)
            y = ifelse(comp(y′′, y), y′′, y)
            i = ifelse(comp(y′′, y), i′, i)
            i′ += 1
        end
    end
    return y, i
end

Base.findmax(x::ChainedVector) = findmax(identity, x)
Base.findmin(x::ChainedVector) = findmin(identity, x)
Base.argmax(x::ChainedVector) = findmax(identity, x)[2]
Base.argmin(x::ChainedVector) = findmin(identity, x)[2]
Base.argmax(f::F, x::ChainedVector) where {F} = findmax(f, x)[2]
Base.argmin(f::F, x::ChainedVector) where {F} = findmin(f, x)[2]

function Base.findfirst(f::Function, x::ChainedVector)
    prevind = 0
    for array in x.arrays
        res = findfirst(f, array)
        res !== nothing && return prevind + res
        prevind += length(array)
    end
    return nothing
end
Base.findfirst(x::ChainedVector{Bool}) = findfirst(identity, x)

function Base.findlast(f::Function, x::ChainedVector)
    for i = length(x.arrays):-1:1
        @inbounds array = x.arrays[i]
        res = findlast(f, array)
        res !== nothing && return (i == 1 ? 0 : x.inds[i - 1]) + res
    end
    return nothing
end
Base.findlast(x::ChainedVector{Bool}) = findlast(identity, x)

Base.@propagate_inbounds function Base.findnext(f::Function, x::ChainedVector, start)
    chunk, ix = index(x, start)
    for i = chunk:length(x.arrays)
        res = findnext(f, x.arrays[i], ix)
        res !== nothing && return (i == 1 ? 0 : x.inds[i - 1]) + res
        ix = 1
    end
    return nothing
end

Base.findnext(x::ChainedVector{Bool}, start) = findnext(identity, x, start)

Base.@propagate_inbounds function Base.findprev(f::Function, x::ChainedVector, start)
    isempty(x) && return nothing
    chunk, ix = index(x, start)
    for i = chunk:-1:1
        res = findprev(f, x.arrays[i], something(ix, length(x.arrays[i])))
        res !== nothing && return (i == 1 ? 0 : x.inds[i - 1]) + res
        ix = nothing
    end
    return nothing
end

Base.findprev(x::ChainedVector{Bool}, start) = findprev(identity, x, start)

function Base.findall(A::ChainedVector{Bool})
    n = count(A)
    I = Vector{eltype(keys(A))}(undef, n)
    cnt = i = 1
    for array in A.arrays
        for a in array
            if a
                I[cnt] = i
                cnt += 1
            end
            i += 1
        end
    end
    return I
end

Base.findall(f::Function, x::ChainedVector) = findall(map(f, x))

function Base.filter(f, a::ChainedVector{T}) where {T}
    j = 1
    b = Vector{T}(undef, length(a))
    for array in a.arrays
        for ai in array
            @inbounds b[j] = ai
            j = ifelse(f(ai), j + 1, j)
        end
    end
    resize!(b, j-1)
    sizehint!(b, length(b))
    return b
end

function Base.filter!(f, a::ChainedVector)
    foreach(A -> filter!(f, A), a.arrays)
    setinds!(a.arrays, a.inds)
    return a
end

Base.replace(f::Base.Callable, a::ChainedVector) = ChainedVector([replace(f, A) for A in a.arrays])
Base.replace!(f::Base.Callable, a::ChainedVector) = (foreach(A -> replace!(f, A), a.arrays); return a)
Base.replace(a::ChainedVector, old_new::Pair...; count::Union{Integer,Nothing}=nothing) = ChainedVector([replace(A, old_new...; count=count) for A in a.arrays])
Base.replace!(a::ChainedVector, old_new::Pair...; count::Integer=typemax(Int)) = (foreach(A -> replace!(A, old_new...; count=count), a.arrays); return a)

Base.Broadcast.broadcasted(f::F, A::ChainedVector) where {F} = map(f, A)
