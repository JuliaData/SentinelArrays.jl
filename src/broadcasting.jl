using Base.Broadcast: Broadcasted, BroadcastStyle, DefaultArrayStyle, AbstractArrayStyle, Unknown

struct SentinelArrayStyle{S <: BroadcastStyle} <: AbstractArrayStyle{Any} end
SentinelArrayStyle(::S) where {S} = SentinelArrayStyle{S}()
SentinelArrayStyle(::S, ::Val{N}) where {S,N} = SentinelArrayStyle(S(Val(N)))
SentinelArrayStyle(::Val{N}) where N = SentinelArrayStyle{DefaultArrayStyle{N}}()
function SentinelArrayStyle(a::BroadcastStyle, b::BroadcastStyle)
    inner_style = BroadcastStyle(a, b)

    # if the inner_style is Unknown then so is the outer-style
    if inner_style isa Unknown
        return Unknown()
    else
        return SentinelArrayStyle(inner_style)
    end
end
function Base.BroadcastStyle(::Type{<:SentinelArray{T, N, S, V, A}}) where {T, N, S, V, A}
    inner_style = typeof(BroadcastStyle(A))
    return SentinelArrayStyle{inner_style}()
end

Base.BroadcastStyle(::SentinelArrayStyle{A}, ::SentinelArrayStyle{B}) where {A, B} = SentinelArrayStyle(A(), B())
Base.BroadcastStyle(::SentinelArrayStyle{A}, b::B) where {A, B} = SentinelArrayStyle(A(), b)
Base.BroadcastStyle(a::A, ::SentinelArrayStyle{B}) where {A, B} = SentinelArrayStyle(a, B())
Base.BroadcastStyle(::SentinelArrayStyle{A}, b::DefaultArrayStyle) where {A} = SentinelArrayStyle(A(), b)
Base.BroadcastStyle(a::AbstractArrayStyle{M}, ::SentinelArrayStyle{B}) where {B,M} = SentinelArrayStyle(a, B())

"""
    unwrap_broadcasted

Recursively unwraps `SentinelArray`s and `SentinelArrayStyle`s.
replacing the `SentinelArray`s with the wrapped array,
and `SentinelArrayStyle` with the wrapped `BroadcastStyle`.
"""
function unwrap_broadcasted(bc::Broadcasted{SentinelArrayStyle{S}}) where S
    inner_args = map(unwrap_broadcasted, bc.args)
    return Broadcasted{S}(bc.f, inner_args)
end
unwrap_broadcasted(x) = x
unwrap_broadcasted(x::SentinelArray) = parent(x)


# We need to implement copy because if the wrapper array type does not support setindex
# then the `similar` based default method will not work
function Broadcast.copy(bc::Broadcasted{SentinelArrayStyle{S}}) where S
    inner_bc = unwrap_broadcasted(bc)
    data = copy(inner_bc)

    return SentinelArray(data)
end

function Base.copyto!(dest::AbstractArray, bc::Broadcasted{SentinelArrayStyle{S}}) where S
    inner_bc = unwrap_broadcasted(bc)
    copyto!(dest, inner_bc)
    return SentinelArray(dest)
end
