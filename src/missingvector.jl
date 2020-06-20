mutable struct MissingVector <: AbstractVector{Missing}
    len::Int64
end

Base.IndexStyle(::Type{MissingVector}) = Base.IndexLinear()
Base.size(x::MissingVector) = (x.len,)

Base.@propagate_inbounds function Base.getindex(x::MissingVector, i::Int)
    @boundscheck checkbounds(x, i)
    missing
end

Base.@propagate_inbounds function Base.setindex!(x::MissingVector, ::Missing, i::Int)
    @boundscheck checkbounds(x, i)
    missing
end

Base.similar(x::MissingVector, ::Type{Missing}, dims::Dims{N}) where {N} = MissingVector(dims[1])

function Base.empty!(x::MissingVector)
    x.len = 0
    return x
end

Base.empty(m::MissingVector, T) = MissingVector(0)

function Base.resize!(x::MissingVector, len)
    len >= 0 || throw(ArgumentError("`len` must be >= 0 when resizing MissingVector"))
    x.len = len
    return x
end

function Base.push!(x::MissingVector, ::Missing)
    x.len += 1
    return x
end

function Base.pushfirst!(x::MissingVector, ::Missing)
    x.len += 1
    return x
end

Base.@propagate_inbounds function Base.deleteat!(x::MissingVector, i::Integer)
    @boundscheck checkbounds(x, i)
    x.len -= 1
    return x
end

Base.@propagate_inbounds function Base.deleteat!(x::MissingVector, inds)
    @boundscheck checkbounds(x, first(inds))
    @boundscheck checkbounds(x, last(inds))
    for i in reverse(inds)
        deleteat!(x, i)
    end
    return x
end

Base.@propagate_inbounds function Base.deleteat!(x::MissingVector, inds::AbstractVector{Bool})
    length(inds) == length(x) || throw(BoundsError(x, inds))
    for i = length(x):-1:1
        if inds[i]
            deleteat!(x, i)
        end
    end
    return x
end

function Base.pop!(x::MissingVector)
    if isempty(x)
        throw(ArgumentError("array must be non-empty"))
    end
    x.len -= 1
    return missing
end

function Base.popfirst!(x::MissingVector)
    if isempty(x)
        throw(ArgumentError("array must be non-empty"))
    end
    x.len -= 1
    return missing
end

Base.@propagate_inbounds function Base.insert!(x::MissingVector, i::Integer, ::Missing)
    if !(i == 1 && isempty(x))
        @boundscheck checkbounds(x, i)
    end
    x.len += 1
    return x
end

function Base.vcat(x::MissingVector, arrays::MissingVector...)
    A = MissingVector(x.len)
    for array in arrays
        A.len += array.len
    end
    return A
end

for f in (:append!, :prepend!)
    @eval begin
        function Base.$f(x::MissingVector, y::AbstractVector{Missing})
            x.len += length(y)
            return x
        end

        function Base.$f(x::MissingVector, itr)
            for i in itr
                push!(x, i)
            end
            return x
        end
    end
end
