using StaticArrays

#region AppendBuffer
export AppendBuffer
"""
A vector implementation that pre-allocated all its memory at creation an supports custom strides.

This can cause aliased elements!
"""
struct AppendBuffer{T, L, C, S} <: StaticVector{C, T}
    data::Vector{UInt8}

    function AppendBuffer{T, L, C, S}(::typeof(!), data::Vector{UInt8}) where {T, L, C, S}
        _checkplain(T)
        return new{T, L, C, S}(data)
    end
end
#endregion

#region constructors
"""
Creates a `AppendBuffer` with initial size.
"""
function AppendBuffer{T, L, C, S}(n::Integer=0, allocator::Allocator=malloc) where {T, L, C, S}
    type = AppendBuffer{T, L, C, S}
    data = Vector{UInt8}(allocator, sizeof(type))
    ab = type(!, data)

    resize!(ab, n)
    return ab
end
"""
Creats a `AppendBuffer` from a existing collection.
"""
function AppendBuffer{T, L, C, S}(elements, allocator::Allocator=malloc) where {T, L, C, S}
    type = AppendBuffer{T, L, C, S}
    data = Vector{UInt8}(sizeof(type), allocator)
    ab = type(!, data)

    resize!(ab, length(elements))
    for (i, v) ∈ enumerate(elements)
        @inbounds ab[i] = v
    end
    return ab
end
"""
Creates a `AppendBuffer` from pre-allocated memory.

The memory at `ptr` is not checked!
"""
function AppendBuffer{T, L, C, S}(ptr::Ptr) where {T, L, C, S}
    type = AppendBuffer{T, L, C, S}
    data = unsafe_wrap(Array, Ptr{UInt8}(ptr), sizeof(type))
    ab = type(!, data)

    return ab
end

@generated function AppendBuffer{T, L, C}(args...) where {T, L, C}
    S = sizeof(T)
    AB = AppendBuffer{T, L, C, S}
    return quote
        @inline
        $AB(args...)
    end
end

@generated function AppendBuffer{T, L}(elements::StaticArray, args...) where {T, L}
    C = length(elements)
    S = sizeof(T)
    AB = AppendBuffer{T, L, C, S}
    return quote
        @inline
        $AB(elements, args...)
    end
end
function AppendBuffer{T, L}(elements, args...) where {T, L}
    C = length(elements)
    S = sizeof(T)
    return AppendBuffer{T, L, C, S}(elements, args...)
end

@generated function AppendBuffer{T}(elements::StaticArray, args...) where T
    C = length(elements)
    S = sizeof(T)
    AB = AppendBuffer{T, Int, C, S}
    return quote
        @inline
        $AB(elements, args...)
    end
end
function AppendBuffer{T}(elements, args...) where T
    C = length(elements)
    S = sizeof(T)
    return AppendBuffer{T, Int, C, S}(elements, args...)
end

@generated function AppendBuffer(elements::StaticArray, args...)
    T = eltype(elements)
    C = length(elements)
    S = sizeof(T)
    AB = AppendBuffer{T, Int, C, S}
    return quote
        @inline
        $AB(elements, args...)
    end
end
function AppendBuffer(elements, args...)
    T = eltype(elements)
    C = length(elements)
    S = sizeof(T)
    return AppendBuffer{T, Int, C, S}(elements, args...)
end
#endregion

#region conversion
import Base: convert
convert(::Type{T}, elements) where T <: AppendBuffer = T(elements)
convert(::Type{T}, ab::T) where T <: AppendBuffer = ab
#endregion

#region pointer
import Base: pointer
pointer(ab::AppendBuffer{T, L, C, S}, i::Integer) where {T, L, C, S} = Ptr{T}(pointer(ab.data, sizeof(L) + (i - 1)S + 1))
pointer(ab::AppendBuffer{T, L}, ::typeof(length)) where {T, L} = Ptr{L}(pointer(ab.data))
pointer(ab::AppendBuffer) = pointer(ab.data)

import Base: sizeof
sizeof(ab::AppendBuffer{T, L, C, S}) where {T, L, C, S} = sizeof(L) + length(ab)S
sizeof(::Type{AppendBuffer{T, L, C, S}}) where {T, L, C, S} = sizeof(L) + C * S

import Base: dataids
dataids(ab::AppendBuffer) = dataids(ab.data)

export isaliased
isaliased(::Type{<:AppendBuffer{T, L, C}}) where {T, L, C} = sizeof(T) > C

_isplaindata(::Type{<:AppendBuffer}) = true
#endregion

#region checks
import Base: checkbounds
checkbounds(ab::AppendBuffer, i::Integer) = 1 <= i <= length(ab) || throw(BoundsError(ab, i))
#endregion

#region StaticVector interface
import Base: size
size(ab::AppendBuffer) = (length(ab),)

import Base: length
length(ab::AppendBuffer) = _get(pointer(ab, length))

import Base: getindex
@inline function getindex(ab::AppendBuffer, i::Int)
    @boundscheck checkbounds(ab, i)
    return _get(pointer(ab, i))
end

import Base: setindex!
@inline function setindex!(ab::AppendBuffer, v, i::Int)
    @boundscheck checkbounds(ab, i)
    _set!(pointer(ab, i), v)
end

import StaticArrays: similar_type
similar_type(::Type{AppendBuffer{T, C, S, L}}, ::Type{NewT}, ::Size{NewSize}) where {T, C, S, L, NewT, NewSize} = AppendBuffer{NewT, first(NewSize), L}

import Base: similar
similar(::Type{T}, newT::Type, size::Size) where T <: AppendBuffer = similar_type(T, newT, size)()
#endregion

#region getters
export capacity
"""
Returns the number of elements the `AppendBuffer` can hold.
"""
capacity(::Type{<:AppendBuffer{T, L, C}}) where {T, L, C} = C
"""
Returns the number of elements the `AppendBuffer` can hold.
"""
capacity(::AppendBuffer{T, L, C}) where {T, L, C} = C

import Base: strides
strides(::Type{<:AppendBuffer{T, L, C, S}}) where {T, L, C, S} = (S,)
strides(::AppendBuffer{T, L, C, S}) where {T, L, C, S} = (S,)

import Base: stride
stride(::Type{<:AppendBuffer{T, L, C, S}}, i::Integer) where {T, L, C, S} = S
stride(::AppendBuffer{T, L, C, S}) where {T, L, C, S} = S

import Base: elsize
"""
Returns the size in memory of a single element.

This can be different from `stride` for aliases data, or padding.
"""
elsize(::Type{<:AppendBuffer{T}}) where T = sizeof(T)

import Base: firstindex
firstindex(ab::AppendBuffer) = 1

import Base: lastindex
lastindex(ab::AppendBuffer) = length(ab)
#endregion

#region transformations
import Base: resize!
function resize!(ab::AppendBuffer{T, L, C}, n::Integer) where {T, L, C}
    0 <= n <= C || throw(ArgumentError(lazy"n needs to be between 0 and $C. Given: $n"))
    _set!(pointer(ab, length), n)
end

import Base: clear!
clear!(ab::AppendBuffer) = resize!(ab, 0)

import Base: push!
function push!(ab::AppendBuffer, v)
    resize!(ab, length(ab) + 1)
    ab[end] = v
end
function push!(ab::AppendBuffer)
    resize!(ab, length(ab) + 1)
    return ab[end]
end

import Base: pop!
function pop!(ab::AppendBuffer)
    resize!(ab, length(ab) - 1)
    return ab[end+1]
end
#endregion
