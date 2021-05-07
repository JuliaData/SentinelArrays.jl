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
        if length(arrays[i]) == 0
            deleteat!(arrays, i)
            deleteat!(inds, i)
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
struct ChainedVectorIndex{A}
    array::A
    i::Int
end

@inline Base.getindex(x::ChainedVectorIndex) = @inbounds x.array[x.i]

@inline function Base.getindex(A::ChainedVector, x::ChainedVectorIndex)
    return @inbounds x.array[x.i]
end

@inline function Base.getindex(A::ChainedVector{T}, x::AbstractVector{<:ChainedVectorIndex}) where {T}
    y = Vector{T}(undef, length(x))
    j = 1
    for idx in x
        @inbounds y[j] = idx[]
        j += 1
    end
    return y
end

@inline function Base.setindex!(A::ChainedVector, v, x::ChainedVectorIndex)
    @inbounds x.array[x.i] = v
    return v
end

struct IndexIterator{A}
    arrays::Vector{A}
end

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
    chunkidx = 1
    @inbounds chunk = arrays[chunkidx]
    # we already ran cleanup! so guaranteed non-empty
    return ChainedVectorIndex(chunk, 1), (arrays, chunkidx, chunk, length(chunk), 2)
end

@inline function Base.iterate(x::IndexIterator, (arrays, chunkidx, chunk, chunklen, i))
    if i > chunklen
        chunkidx += 1
        chunkidx > length(arrays) && return nothing
        @inbounds chunk = arrays[chunkidx]
        chunklen = length(chunk)
        i = 1
    end
    return ChainedVectorIndex(chunk, i), (arrays, chunkidx, chunk, chunklen, i + 1)
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
Base.similar(x::ChainedVector{T}, len::Base.DimOrInd) where {T} = similar(x, T, len)

function Base.similar(x::ChainedVector{T}, ::Type{S}, len::Base.DimOrInd) where {T, S}
    if len == length(x)
        # return same chunks structure as x
        return ChainedVector([similar(A, S, length(A)) for A in x.arrays])
    end
    # otherwise, split the different new len over existing # of chunks in x
    N = length(x.arrays)
    if len <= N
        return ChainedVector([similar(A, S, len)])
    end
    nlen, r = divrem(len, N)
    return ChainedVector([similar(A, S, nlen + (i == N ? r : 0)) for (i, A) in enumerate(x.arrays)])
end

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
    # find first chunk where we'll start copying to
    aidx, _ = index(dest, doffs)
    prevind = aidx == 1 ? 0 : dest.inds[aidx - 1]
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

Base.foreach(f::F, x::ChainedVector) where {F} = foreach(x->foreach(f, x), x.arrays)
Base.map(f::F, x::ChainedVector) where {F} = ChainedVector([map(f, y) for y in x.arrays])

# function Base.map(f::F, x::ChainedVector) where {F}
#     tasks = map(A -> Threads.@spawn(map(f, A)), x.arrays)
#     ChainedVector([fetch(tsk) for tsk in tasks])
# end

function Base.map!(f::F, A::AbstractVector, x::ChainedVector) where {F}
    length(A) >= length(x) || throw(ArgumentError("destination must be at least as long as map! source"))
    i = 1
    for array in x.arrays
        for y in array
            @inbounds A[i] = f(y)
            i += 1
        end
    end
    return A
end

function Base.map!(f::F, x::ChainedVector, A::AbstractVector) where {F}
    length(x) >= length(A) || throw(ArgumentError("destination must be at least as long as map! source"))
    i = 1
    for array in x.arrays
        for j in eachindex(array)
            @inbounds array[j] = f(A[i])
            i += 1
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
Base.count(f::F, x::ChainedVector) where {F} = sum(count(f, y) for y in x.arrays)
Base.count(x::ChainedVector) = sum(count(y) for y in x.arrays)

Base.extrema(x::ChainedVector) = extrema(identity, x)

function Base.extrema(f::F, x::ChainedVector) where {F}
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
    return findmaxwithfirst(!isless, f, x, y, i)
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
Base.argmax(x::ChainedVector) = findmax(identity, x)[1]
Base.argmin(x::ChainedVector) = findmin(identity, x)[1]
Base.argmax(f::F, x::ChainedVector) where {F} = findmax(f, x)[1]
Base.argmin(f::F, x::ChainedVector) where {F} = findmin(f, x)[1]

for f in (:findfirst, :findlast)
    @eval function Base.$f(f::Function, x::ChainedVector)
        for array in x.arrays
            res = $f(f, array)
            res !== nothing && return res
        end
        return nothing
    end
    @eval Base.$f(x::ChainedVector{Bool}) = $f(identity, x)
end

Base.@propagate_inbounds function Base.findnext(f::Function, x::ChainedVector, start)
    @boundscheck checkbounds(x, start)
    chunk, ix = index(x, start)
    for i = chunk:length(x.arrays)
        res = findnext(f, x.arrays[i], ix)
        res !== nothing && return res
        ix = 1
    end
    return nothing
end

Base.findnext(x::ChainedVector{Bool}, start) = findnext(identity, x, start)

Base.@propagate_inbounds function Base.findprev(f::Function, x::ChainedVector, start)
    @boundscheck checkbounds(x, start)
    chunk, ix = index(x, start)
    for i = chunk:-1:1
        res = findprev(f, x.arrays[i], something(ix, length(x.arrays[i])))
        res !== nothing && return res
        ix = nothing
    end
    return nothing
end

Base.findprev(x::ChainedVector{Bool}, start) = findprev(identity, x, start)

function Base.findall(A::ChainedVector{Bool})
    n = count(A)
    I = Vector{eltype(keys(A))}(undef, n)
    cnt = 1
    for array in A.arrays
        for (i, a) in pairs(array)
            if a
                I[cnt] = i
                cnt += 1
            end
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
Base.replace!(f::Base.Callable, a::ChainedVector) = foreach(A -> replace!(f, A), a.arrays)
Base.replace(a::ChainedVector, old_new::Pair...; count::Union{Integer,Nothing}=nothing) = ChainedVector([replace(A, old_new...; count=count) for A in a.arrays])
Base.replace!(a::ChainedVector, old_new::Pair...; count::Union{Integer,Nothing}=nothing) = foreach(A -> replace!(A, old_new...; count=count), a.arrays)

import Base.Broadcast: Broadcasted
struct ChainedVectorStyle <: Broadcast.AbstractArrayStyle{1} end
Base.BroadcastStyle(::Type{<:ChainedVector}) = ChainedVectorStyle()
Base.similar(bc::Broadcasted{ChainedVectorStyle}, ::Type{T}) where {T} = similar(bc.args[1], T, length(bc))

function Base.copyto!(dest::ChainedVector, bc::Broadcasted{ChainedVectorStyle})
    for (x, y) in zip(dest.arrays, bc.args[1].arrays)
        for i in eachindex(x)
            @inbounds x[i] = bc.f(y[i])
        end
    end
    return dest
end
