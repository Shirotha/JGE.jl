#region UnsafeStruct
export UnsafeStruct
"""
A mutable, heap-allocated alternative to `NamedTuple` that guaranties continues memory allocation.
"""
struct UnsafeStruct{T <: Tuple, names, offsets, S, N} <: StaticScalar{T}
    "Raw memory buffer."
    data::SizedVector{S, UInt8, Vector{UInt8}}

    """
    Creates a `UnsafeStruct` using pre-allocated memory.
    """
    function UnsafeStruct{T, names, offsets, S, N}(data::Vector{UInt8}) where {T, names, offsets, S, N}
        _checkplain(T)
        N == length(names) || throw(ArgumentError(lazy"names length mismatch, expected $N. Given: $(length(names))"))
        N == length(offsets) || throw(ArgumentError(lazy"offsets length mismatch, expected $N. Given: $(length(offsets))"))
        
        fields = tuple(T.parameters...)
        N == length(fields) || throw(ArgumentError(lazy"T length mismatch, expected $N. Given: $(length(fields))"))
        
        len = maximum(offsets .+ sizeof.(fields))
        S >= len || throw(ArgumentError(lazy"S needs to be at least $len. Given $S"))

        return new{T, names, offsets, S, N}(SizedVector{S, UInt8}(data))
    end
end

export @UnsafeStruct
using MacroTools
"""
Create a `UnsafeStruct` type using a struct-like syntax.

Examples
==========

`julia
@UnsafeStruct 16 begin
    0::x::Int32
    4::y::Int32
    8::z::Int32
end
`

`julia
@UnsafeStruct auto begin
    0::int::Int
    0::float::Float64
end
`
"""
macro UnsafeStruct(size, typedef::Expr)
    @capture(typedef, begin fields__ end) || throw(ArgumentError(lazy"Expected block of field definitions"))
    
    function extract_field(field::Expr)
        @capture(field, offset_::name_::type_) || throw(ArgumentError(lazy"Expected offset::name::type, given: $field"))
        offset isa Integer && offset >= 0 || throw(ArgumentError(lazy"Offset needs ot be a non-negative Integer. Given: $offset"))
        name isa Symbol || throw(ArgumentError(lazy"Name needs to be a Symbol. Given: $name"))
        (offset, name, type)
    end

    data = extract_field.(Tuple(fields))
    offsets = getindex.(data, 1) .+ 1
    names = getindex.(data, 2)
    types = getindex.(data, 3)

    @esc names offsets
    
    if size == :auto || size isa QuoteNode && size.value == :auto
        return quote
            _construct_struct_type(Tuple{$(types...)}, $names, $offsets)
        end
    else
        @esc size
        return quote
            _construct_struct_type(Tuple{$(types...)}, $names, $offsets, $size)
        end
    end
end
#endregion

#region helpers
_construct_struct_type(::Type{T}, names, offsets, S, N) where T = UnsafeStruct{T, names, offsets, S, N}
_construct_struct_type(::Type{T}, names::NTuple{N}, offsets::NTuple{N}, S) where {T, N} = UnsafeStruct{T, names, offsets, S, N}
function _construct_struct_type(::Type{T}, names::NTuple{N}, offsets::NTuple{N}) where {T, N}
    fields = tuple(T.parameters...)
    N == length(fields) || throw(ArgumentError(lazy"T length mismatch, expected $N. Given: $(length(fields))"))
    S = maximum(offsets .+ sizeof.(fields))
    return UnsafeStruct{T, names, offsets, S, N}
end
import Base: front
function _construct_struct_type(::Type{T}, names::NTuple{N}) where {T, N}
    fields = tuple(T.parameters...)
    N == length(fields) || throw(ArgumentError(lazy"T length mismatch, expected $N. Given: $(length(fields))"))
    sizes = sizeof.(fields)
    offsets = @MVector zeros(N + 1)
    for i ∈ 1:N
        @inbounds offsets[i + 1] = offsets[i] + sizes[i]
    end
    return UnsafeStruct{T, names, front(offsets), last(offsets), N}
end
function _construct_struct_type(::Type{T}) where T
    fields = tuple(T.parameters...)
    N = length(fields)
    names = Symbol.(:f, 1:N)
    sizes = sizeof.(fields)
    offsets = @MVector zeros(N + 1)
    for i ∈ 1:N
        @inbounds offsets[i + 1] = offsets[i] + sizes[i]
    end
    return UnsafeStruct{T, names, front(offsets), last(offsets), N}
end
#endregion

#region constructors
"""
Creates a `UnsafeStruct` and set fields given by keyword arguments.

Be careful when dealing with aliased data!
"""
function UnsafeStruct{T, names, offsets, S, N}(allocator::AbstractArrayAllocator=calloc; kwargs...) where {T, names, offsets, S, N}
    data = Vector{UInt8}(allocator, S)
    us = UnsafeStruct{T, names, offsets, S, N}(data)

    for (field, v) ∈ kwargs
        us[field] = v
    end
    return us
end
# NOTE: needed to fix ambiguous call
function UnsafeStruct{T, names, offsets, S, N}(allocator::UndefAllocator; kwargs...) where {T, names, offsets, S, N}
    data = Vector{UInt8}(allocator, S)
    us = UnsafeStruct{T, names, offsets, S, N}(data)

    for (field, v) ∈ kwargs
        us[field] = v
    end
    return us
end
"""
Creates a `UnsafeStruct` and set an initial value.

Be careful when dealing with aliased data!
"""
function UnsafeStruct{T, names, offsets, S, N}(value::T, allocator::Allocator=calloc) where {T, names, offsets, S, N}
    data = Vector{UInt8}(allocator, S)
    us = UnsafeStruct{T, names, offsets, S, N}(data)

    us[] = value
    return us
end
"""
Creates a `UnsafeStruct` and set an initial value.

Be careful when dealing with aliased data!
"""
function UnsafeStruct{T, names, offsets, S, N}(value::Tuple{T}, allocator::Allocator=calloc) where {T, names, offsets, S, N}
    data = Vector{UInt8}(allocator, S)
    us = UnsafeStruct{T, names, offsets, S, N}(data)

    us[] = first(value)
    return su
end
"""
Creates a `UnsafeStruct` using the memory at `ptr`.

The memory at `ptr` is not checked!
"""
function UnsafeStruct{T, names, offsets, S, N}(ptr::Ptr) where {T, names, offsets, S, N}
    data = unsafe_wrap(Array, ptr, S)
    us = UnsafeStruct{T, names, offsets, S, N}(data)

    return us
end

const STRUCT_PARAMETER_NAMES = (:T, :names, :offsets, :S)
for i ∈ 1:length(STRUCT_PARAMETER_NAMES)
    params = STRUCT_PARAMETER_NAMES[1:i]
    @eval @generated function UnsafeStruct{$(params...)}(args...; kwargs...) where {$(params...)}
        US = _construct_struct_type($(params...))
        return Expr(:block, :@inline, Expr(:call, US, :(args...), :(kwargs...)))
    end
end

@generated function UnsafeStruct{T}(value::NamedTuple{names}) where {T, names}
    US = _construct_struct_type(T, names)
    return quote
        @inline
        $US(values(value))
    end
end
@generated function UnsafeStruct(value::T) where T <: Tuple
    US = _construct_struct_type(T)
    return quote
        @inline
        $US(value)
    end
end
@generated function UnsafeStruct(value::Tuple{T}) where T <: Tuple
    US = _construct_struct_type(T)
    return quote
        @inline
        $US(first(value))
    end
end
@generated function UnsafeStruct(value::NamedTuple{names, T}) where {names, T}
    US = _construct_struct_type(T, names)
    return quote
        @inline
        $US(values(value))
    end
end
@generated function UnsafeStruct(ptr::Ptr{T}) where T <: Tuple
    US = _construct_struct_type(T)
    return quote
        @inline
        $US(ptr)
    end
end
#endregion

#region pointer
import Base: pointer
pointer(us::UnsafeStruct) = pointer(getfield(us, :data))
@generated function pointer(us::UnsafeStruct{T, names, offsets}, ::Val{field}) where {T, names, offsets, field}
    findindex(field::Symbol) = findfirst(==(field), names)
    findindex(field::Integer) = field
    
    i = findindex(field)

    isnothing(i) && return quote
        throw(BoundsError(us, $field))
    end

    offset = offsets[i]
    type = T.parameters[i]
    ptrtype = Ptr{type}
    return quote
        $ptrtype(pointer(getfield(us, :data), $offset))
    end
end
pointer(us::UnsafeStruct, field::Symbol) = pointer(us, Val{field}())
pointer(us::UnsafeStruct, field::Integer) = pointer(us, Val{field}())

import Base: sizeof
sizeof(::Type{<:UnsafeStruct{T, names, offsets, S}}) where {T, names, offsets, S} = S
sizeof(::T) where T <: UnsafeStruct = sizeof(T)
sizeof(::Type{<:UnsafeStruct{T}}, i::Integer) where T = sizeof(T.parameters[i])
sizeof(::UnsafeStruct{T}, i::Integer) where T = sizeof(T.parameters[i])

import Base: dataids
dataids(us::UnsafeStruct) = dataids(getfield(us, :data))

export isaliased
import Base: front
isaliased(::Type{<:UnsafeStruct{T, names, offsets}}) where {T, names, offsets} = any(diff(SVector(offsets)) .< sizeof.(front(tuple(T.parameters...))))

_isplaindata(::Type{<:UnsafeStruct}) = true
#endregion

#region indexing
import Base: getindex
getindex(us::UnsafeStruct, field) = _get(pointer(us, field))
getindex(us::UnsafeStruct) = values(us)

import Base: setindex!
setindex!(us::UnsafeStruct, v, field) = _set!(pointer(us, field), v)
function setindex!(us::UnsafeStruct, v)
    for ((_, ptr), vᵢ) ∈ zip(us, values(v))
        _set!(ptr, vᵢ)
    end
    return v
end
function setindex!(us::UnsafeStruct{T, names1, offsets, S1}, v::UnsafeStruct{T, names2, offsets, S2}) where {T, names1, names2, offsets, S1, S2}
    unsafe_copyto!(pointer(us), pointer(v), min(S1, S2))
    return v
end
    
import Base: haskey
haskey(us::UnsafeStruct, ::Val{field}) where field = haskey(us, field)
haskey(::UnsafeStruct{T, names}, field::Symbol) where {T, names} = field in names
haskey(::UnsafeStruct{T, names, offsets, S, N}, field::Integer) where {T, names, offsets, S, N} = 1 <= field <= N

import Base: keys
keys(::UnsafeStruct{T, names}) where {T, names} = names

import Base: values
values(us::UnsafeStruct{T, names}) where {T, names} = getindex.((us,), names)

import Base: Tuple
Tuple(us::UnsafeStruct) = (values(us),)

import StaticArrays: similar_type
similar_type(::Type{<:UnsafeStruct}, ::Type{NewT}, ::Size{(1,)}) where {NewT} = _construct_struct_type(newT)

import Base: similar
similar(::Type{<:UnsafeStruct}, newT::Type, size::Size{(1,)}) = _construct_struct_type(newT)()
#endregion

#region properties
import Base: getproperty
getproperty(us::UnsafeStruct, name::Symbol) = us[Val{name}()]

import Base: setproperty!
setproperty!(us::UnsafeStruct, name::Symbol, v) = us[Val{name}()] = v

import Base: propertynames
propertynames(::UnsafeStruct{T, names}, private::Bool=false) where {T, names} = names
#endregion

#region iteration
import Base: length
length(::UnsafeStruct{T, names, offsets, S, N}) where {T, names, offsets, S, N} = N

import Base: eltype
eltype(::UnsafeStruct) = Tuple{Symbol, Ptr}

import Base: iterate
iterate(us::UnsafeStruct) = iterate(us, 1)
function iterate(us::UnsafeStruct{T, names, offsets, S, N}, i::Int) where {T, names, offsets, S, N}
    1 <= i <= N || return nothing

    return (names[i], pointer(us, i)), i + 1
end
#endregion

#region show
import Base: show
function show(io::IO, us::UnsafeStruct{T, N, names}) where {T, N, names}
    print(io, NamedTuple{names}(values(us)))
end
#endregion
