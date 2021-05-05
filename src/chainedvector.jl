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

@noinline function cleanup!(A::ChainedVector)
    N = length(A.arrays)
    for i = N:-1:1
        if length(A.arrays[i]) == 0
            deleteat!(A.arrays, i)
            deleteat!(A.inds, i)
        end
    end
    return
end

Base.IndexStyle(::Type{<:ChainedVector}) = Base.IndexLinear()
Base.size(x::ChainedVector) = (length(x.inds) == 0 ? 0 : x.inds[end],)

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

# efficient iteration
struct IndexIterator{A}
    arrays::Vector{A}
end

struct ChainedVectorIteratedValue{T}
    x::T
end

@inline function Base.eachindex(A::ChainedVector)
    # check for and remove any empty chunks
    cleanup!(A)
    return IndexIterator(A.arrays)
end

@inline function Base.iterate(x::IndexIterator{A}) where {A <: AbstractVector{T}} where {T}
    arrays = x.arrays
    length(arrays) == 0 && return nothing
    chunkidx = 1
    @inbounds chunk = arrays[chunkidx]
    # we already ran cleanup! so guaranteed non-empty
    @inbounds y = chunk[1]
    return ChainedVectorIteratedValue(y), (arrays, chunkidx, chunk, length(chunk), 2)
end

@inline function Base.iterate(x::IndexIterator{A}, (arrays, chunkidx, chunk, chunklen, i)) where {A <: AbstractVector{T}} where {T}
    if i > chunklen
        chunkidx += 1
        chunkidx > length(arrays) && return nothing
        @inbounds chunk = arrays[chunkidx]
        chunklen = length(chunk)
        i = 1
    end
    @inbounds y = chunk[i]
    i += 1
    return ChainedVectorIteratedValue(y), (arrays, chunkidx, chunk, chunklen, i)
end

@inline Base.getindex(A::ChainedVector, idx::ChainedVectorIteratedValue) = idx.x

function eachind2(A)
    x = 0
    for y in A
        x += y
    end
    return x
end

function eachind3(A)
    x = 0
    for i in eachindex(A)
        @inbounds x += A[i]
    end
    return x
end

@inline function Base.iterate(A::ChainedVector)
    idx = eachindex(A)
    state = iterate(idx)
    state === nothing && return nothing
    ci, st = state
    return ci.x, (idx, st)
end

@inline function Base.iterate(A::ChainedVector, (idx, st))
    state = iterate(idx, st)
    state === nothing && return nothing
    ci, st = state
    return ci.x, (idx, st)
end

# other AbstractArray functions
Base.similar(x::ChainedVector{T}, len::Base.DimOrInd) where {T} = similar(x, T, len)
Base.similar(x::ChainedVector{T}, ::Type{S}, len::Base.DimOrInd) where {T, S} =
    ChainedVector([similar(A, S, length(A)) for A in x.arrays])

Base.copyto!(dest::ChainedVector, src::AbstractVector) =
    copyto!(dest, 1, src, 1, length(src))
Base.copyto!(dest::ChainedVector, doffs::Union{Signed, Unsigned}, src::AbstractVector) =
    copyto!(dest, doffs, src, 1, length(src))

function Base.copyto!(dest::ChainedVector{T}, doffs::Union{Signed, Unsigned},
    src::AbstractVector, soffs::Union{Signed, Unsigned}, n::Union{Signed, Unsigned}) where {T}
    # @show length(dest), doffs, length(src), soffs, n
    (doffs > 0 && (doffs + n - 1) <= length(dest) &&
    soffs > 0 && (soffs + n - 1) <= length(src)) || throw(ArgumentError("out of range arguments to copyto! on ChainedVector"))
    n == 0 && return dest
    N = length(dest.inds)
    aidx = 1
    prevind = 0
    # find first chunk where we'll start copying to
    @inbounds while dest.inds[aidx] < doffs
        prevind = dest.inds[aidx]
        aidx += 1
        aidx > N && break
    end
    while true
        # aidx now points to chunk where we need to copy
        A = dest.arrays[aidx]
        # now compute how many elements to copy to this chunk
        off = doffs - prevind
        chunkn = min(length(A) - off + 1, n)
        # @show aidx, off, chunkn, soffs, soffs:(soffs + chunkn - 1)
        copyto!(A, off, view(src, soffs:(soffs + chunkn - 1)))
        soffs += chunkn
        n -= chunkn
        prevind = dest.inds[aidx]
        aidx += 1
        aidx > N && break
        doffs = prevind + 1
    end
    return dest
end

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
    # TODO: split inds up by A.arrays and call deleteat! on views into inds
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

Base.in(x, A::ChainedVector) = any(y->x in y, A.arrays)

# function Base.first(itr::ChainedVector, n::Integer)
#     collect(Iterators.take(itr, n))
# end

# function Base.last(itr::ChainedVector, n::Integer)
#     reverse!(collect(Iterators.take(Iterators.reverse(itr), n)))
# end

Base.foreach(f::F, x::ChainedVector) where {F} = foreach(x->foreach(f, x), x.arrays)

Base.map(f::F, x::ChainedVector) where {F} = ChainedVector([map(f, y) for y in x.arrays])

# function Base.map(f::F, x::ChainedVector) where {F}
#     tasks = map(A -> Threads.@spawn(map(f, A)), x.arrays)
#     ChainedVector([fetch(tsk) for tsk in tasks])
# end

function Base.map!(f::F, A::AbstractVector, x::ChainedVector) where {F}

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
Base.count(f::F, x::ChainedVector) where {F} = sum(count(f, y) for y in x.arrays)
Base.count(x::ChainedVector) = sum(count(y) for y in x.arrays)
