module BufferedVectors

export BufferedVector, skip_element!, shiftleft!, unsafe_push!
mutable struct BufferedVector{T} <: AbstractArray{T,1}
    elements::Vector{T}
    occupied::Int
end
BufferedVector{T}() where {T} = BufferedVector(T[], 0)
BufferedVector(v::Vector{T}) where {T} = BufferedVector{T}(v, length(v))

Base.length(x::BufferedVector) = x.occupied
Base.size(x::BufferedVector) = (x.occupied,)
Base.@propagate_inbounds function Base.last(x::BufferedVector)
    return x.elements[x.occupied]
end
@inline function Base.first(x::BufferedVector)
    @boundscheck((x.occupied > 0) || Base.throw_boundserror(x, 1))
    return @inbounds(x.elements[1])
end
@inline function Base.getindex(x::BufferedVector, i::Int)
    @boundscheck((x.occupied >= i && i > 0) || Base.throw_boundserror(x, i))
    return @inbounds(x.elements[i])
end
Base.ndims(x::BufferedVector) = 1
Base.empty!(x::BufferedVector) = (x.occupied = 0; x)
Base.isempty(x::BufferedVector) = x.occupied == 0
Base.IndexStyle(::BufferedVector) = Base.IndexLinear()
Base.IteratorSize(::BufferedVector) = Base.HasLength()
Base.IteratorEltype(::BufferedVector) = Base.HasEltype()
Base.collect(x::BufferedVector{T}) where {T} = length(x) > 0 ? @inbounds(x.elements[1:length(x)]) : T[]
Base.eltype(::BufferedVector{T}) where T = T
@inline Base.push!(buffer::T, x::S) where {T,S} = push!(buffer, convert(T, x))
@inline function Base.push!(buffer::BufferedVector{T}, x::T) where {T}
    if length(buffer.elements) == buffer.occupied
        Base._growend!(buffer.elements, _grow_by(T))
    end
    buffer.occupied += 1
    @inbounds buffer.elements[buffer.occupied] = x
end
_grow_by(::Type) = 16
_grow_by(::Type{T}) where {T<:Union{Bool,UInt8}} = 64

@inline unsafe_push!(buffer::BufferedVector{T}, x::S) where {T,S} = unsafe_push!(buffer, convert(T, x))
@inline function unsafe_push!(buffer::BufferedVector{T}, x::T) where {T}
    buffer.occupied += 1
    @inbounds buffer.elements[buffer.occupied] = x
end
Base.ensureroom(x::BufferedVector, n) = ((length(x.elements) < n) && Base._growend!(x.elements, n - length(x.elements)); return nothing)
skip_element!(x::BufferedVector) = x.occupied += 1
function shiftleft!(x::BufferedVector, n)
    n < 0 && throw(ArgumentError("n must be >= 0"))
    n == 0 && return
    len = length(x)
    if n >= len
        empty!(x)
        return nothing
    end
    unsafe_copyto!(x.elements, 1, x.elements, 1 + n, len - n)
    x.occupied -= n
    return nothing
end

end # module
