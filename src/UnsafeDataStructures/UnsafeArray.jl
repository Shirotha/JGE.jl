#region UnsafeArray
export UnsafeArray
"""
A statically sized array with custom strides.

This can cause aliased elements!
"""
struct UnsafeArray{T, N, D, S} <: StaticArray{D, T, N}
    data::Vector{UInt8}

    UnsafeArray{T, N, D, S}(::typeof(!), data::Vector{UInt8}) where {T, N, D, S} = new{T, N, D, S}(data)
end

export UnsafeVector
const UnsafeVector{T, D, S} = UnsafeArray{T, 1, Tuple{D}, Tuple{S}}

export UnsafeMatrix
const UnsafeMatrix{T, Dx, Dy, Sx, Sy} = UnsafeArray{T, 2, Tuple{Dx, Dy}, Tuple{Sx, Sy}}
#endregion

#region constructors
"""
Creates an uninitialized `UnsafeArray`.
"""
function UnsafeArray{T, N, D, S}(allocator::Allocator=malloc) where {T, N, D, S}
    type = UnsafeArray{T, N, D, S}
    data = Vector{UInt8}(allocator, sizeof(type))
    ua = type(!, data)

    return ua
end
"""
Creates an `UnsafeArray` by copying an existion collection.

Be careful when dealing with aliased data!
"""
function UnsafeArray{T, N, D, S}(elements, allocator::Allocator=malloc) where {T, N, D, S}
    type = UnsafeArray{T, N, D, S}
    data = Vector{UInt8}(allocator, sizeof(type))
    ua = type(!, data)

    for (i, v) ∈ enumerate(elements)
        ua[i] = v
    end
    return ua
end
"""
Creates an `UnsafeArray` using a initializer function `init(I)::T`.
"""
function UnsafeArray{T, N, D, S}(init::Function, allocator::Allocator=malloc) where {T, N, D, S}
    type = UnsafeArray{T, N, D, S}
    data = Vector{UInt8}(allocator, sizeof(type))
    ua = type(!, data)

    for I ∈ keys(ua)
        ua[I] = init(I)
    end
    return ua
end
"""
Creats an `UnsafeArray` from pre-allocated memory.

The memory at `ptr` is not checked!
"""
function UnsafeArray{T, N, D, S}(ptr::Ptr) where {T, N, D, S}
    type = UnsafeArray{T, N, D, S}
    data = unsafe_wrap(Array, Ptr{UInt8}(ptr), sizeof(type))
    return type(!, data)
end

import Base: size_to_strides
@generated function UnsafeArray{T, N, D}(args...) where {T, N, D}
    sizes = tuple(D.parameters...)
    S = size_to_strides(sizeof(T), sizes...)
    UA = UnsafeArray{T, N, D, S}
    return quote
        @inline
        $UA(args...)
    end
end

@generated function UnsafeArray{T}(elements::StaticArray) where T
    sizes = size(elements)
    N = length(sizes)
    D = Tuple{sizes...}
    S = size_to_strides(sizeof(T), sizes...)
    UA = UnsafeArray{T, N, D, S}
    return quote
        @inline
        $UA(elements)
    end
end
function UnsafeArray{T}(elements) where T
    sizes = size(elements)
    N = length(sizes)
    D = Tuple{sizes...}
    S = size_to_strides(sizeof(T), sizes...)
    return UnsafeArray{T, N, D, S}(elements)
end

@generated function UnsafeArray(elements::StaticArray)
    T = eltype(elements)
    sizes = size(elements)
    N = length(sizes)
    D = Tuple{sizes...}
    S = size_to_strides(sizeof(T), sizes...)
    UA = UnsafeArray{T, N, D, S}
    return quote
        @inline
        $UA(elements)
    end
end
function UnsafeArray(elements)
    T = eltype(elements)
    sizes = size(elements)
    N = length(sizes)
    D = Tuple{sizes...}
    S = size_to_strides(sizeof(T), sizes...)
    return UnsafeArray{T, N, D, S}(elements)
end
#endregion

#region helpers
@generated function _dimsize(::Type{T}, ::Type{D}, ::Type{S}) where {T, D <: Tuple, S <: Tuple}
    sizes = tuple(D.parameters...)
    strides = tuple(S.parameters...)
    N = length(sizes)
    N == length(strides) || throw(ArgumentError(lazy"size and stride need to have the same length. Given ($N, $(length(stride)))"))
    
    N == 0 && return ()

    dimsize = MVector{N, Int}(undef)
    @inbounds dimsize[1] = (sizes[1] - 1)strides[1] + sizeof(T)
    for i ∈ 2:N
        @inbounds dimsize[i] = (sizes[i] - 1)strides[i] + dimsize[i - 1]
    end
    return Tuple(dimsize)
end
_dimsize(::Type{<:UnsafeArray{T, N, D, S}}) where {T, N, D, S} = _dimsize(T, D, S)
_dimsize(::UnsafeArray{T, N, D, S}) where {T, N, D, S} = _dimsize(T, D, S)
#endregion

#region pointer
import Base: pointer
@inline function pointer(ua::UnsafeArray{T, N}, I::NTuple{N, <:Integer}) where {T, N}
    offset = sum((I .- 1) .* strides(ua))
    return Ptr{T}(pointer(ua.data, offset + 1))
end
pointer(ua::UnsafeArray) = pointer(ua.data)

import Base: sizeof
sizeof(::Type{T}) where T <: UnsafeArray = last(_dimsize(T))
sizeof(ua::UnsafeArray) = last(_dimsize(ua))

import Base: dataids
dataids(ua::UnsafeArray) = dataids(ua.data)

export isaliased
isaliased(::Type{T}, d::Integer) where T <: UnsafeArray = stride(T, d) < elsize(T, d)
isaliased(::Type{T}) where T <: UnsafeArray = any(isaliased.(T, 1:ndims(T)))
isaliased(::T, d::Integer) where T <: UnsafeArray = isaliased(T, d)
isaliased(::T) where T <: UnsafeArray = isaliased(T)

_isplaindata(::Type{<:UnsafeArray}) = true
#endregion

#region getters
import Base: strides
strides(::Type{<:UnsafeArray{T, N, D, S}}) where {T, N, D, S} = tuple(S.parameters...)
strides(::UnsafeArray{T, N, D, S}) where {T, N, D, S} = tuple(S.parameters...)

import Base: stride
stride(::Type{<:UnsafeArray{T, N, D, S}}, i::Integer) where {T, N, D, S} = S.parameters[i]
stride(::UnsafeArray{T, N, D, S}, i::Integer) where {T, N, D, S} = S.parameters[i]

import Base: elsize
"""
Returns the size in memory of a single element.

This can be different from `stride` for aliases data, or padding.
"""
elsize(::Type{<:UnsafeArray{T}}) where T = sizeof(T)
"""
Returns the size in memory of a single element.

This can be different from `stride` for aliases data, or padding.
"""
elsize(::Type{T}, d::Integer) where T <: UnsafeArray = d == 1 ? elsize(T) : _dimsize(T)[d - 1]
#endregion

#region checks
import Base: checkbounds
checkbounds(::Type{<:UnsafeArray{T, N, D}}, I::Vararg{<:Integer, N}) where {T, N, D} = all(1 .<= I .<= tuple(D.parameters...)) || throw(BoundsError(ab, I))
checkbounds(::Type{<:UnsafeArray{T, N, D}}, i::Integer) where {T, N, D} = 1 <= i <= prod(D.parameters) || throw(BoundsError(ab, i))
checkbounds(::UnsafeArray{T, N, D}, I::Vararg{<:Integer, N}) where {T, N, D} = all(1 .<= I .<= tuple(D.parameters...)) || throw(BoundsError(ab, I))
checkbounds(::UnsafeArray{T, N, D}, i::Integer) where {T, N, D} = 1 <= i <= prod(D.parameters) || throw(BoundsError(ab, i))
#endregion

#region StaticArray interface
import Base: IndexStyle
IndexStyle(::Type{<:UnsafeArray}) = IndexCartesian()

import Base: getindex, _ind2sub
@inline function getindex(ua::UnsafeArray, i::Int)
    @boundscheck checkbounds(ua, i)
    return _get(pointer(ua, _ind2sub(ua, i)))
end
@inline function getindex(ua::UnsafeArray{T, N}, I::Vararg{Int, N}) where {T, N}
    @boundscheck checkbounds(ua, I...)
    return _get(pointer(ua, I))
end

import Base: setindex!, _ind2sub
function setindex!(ua::UnsafeArray, v, i::Int)
    @boundscheck checkbounds(ua, i)
    _set!(pointer(ua, _ind2sub(ua, i)), v)
end
function setindex!(ua::UnsafeArray{T, N}, v, I::Vararg{Int, N}) where {T, N}
    @boundscheck checkbounds(ua, I...)
    _set!(pointer(ua, I), v)
end

import StaticArrays: similar_type
similar_type(::Type{<:UnsafeArray}, ::Type{NewT}, ::Size{NewSize}) where {NewT, NewSize} = UnsafeArray{NewT, length(NewSize), Tuple{Tuple(NewSize)...}}

import Base: similar
similar(::Type{T}, newT::Type, size::Size) where T <: UnsafeArray = similar_type(T, newT, size)()

import Base: Tuple
Tuple(ua::UnsafeArray) = ntuple(i -> ua[i], length(ua))
#endregion
