module UnsafeDataStructures

    include("../doc-templates.jl")

    using PointerArithmetic, StaticArrays, ArrayAllocators
    import Base: @propagate_inbounds as @prop, @assume_effects as @assume
    import .GC: @preserve as @pin

    #region interface declaration
    "Returns `true` if fields/elements of a type are aliased to each other, and `false` otherwise."
    function isaliased end
    isaliased(::Type) = false
    isaliased(::T) where T = isaliased(T) 

    _isplaindata(::Type{T}) where T = isbitstype(T)
    _isplaindata(::T) where T = _isplaindata(T)

    # NOTE: also needed:
    #   - sizeof: allocated memory for !isbitstype
    #   - pointer: start of memory for !isbitstype
    #   - length
    #endregion

    #region helpers
    Base.getindex(ptr::Ptr) = unsafe_load(ptr)
    Base.setindex!(ptr::Ptr, v) = unsafe_store!(ptr, v)

    const Allocator = Union{UndefInitializer, AbstractArrayAllocator}
    #endregion

    #region interface patch for SizedArray
    const FixedArray{T, S, N, M} = SizedArray{S, T, N, M, Array{T, M}}
    const FixedVector{T, D} = FixedArray{Tuple{D}, T, 1, 1}
    const FixedMatrix{T, D1, D2, M} = FixedArray{Tuple{D1, D2}, T, 2, M}
    
    _isplaindata(::Type{<:FixedArray{T}}) where T = _isplaindata(T)
    _checkplain(::Type{T}) where T = _isplaindata(T) || error(lazy"Type $T not supported.")
    
    (::Type{T})(ptr::Ptr) where T <: FixedArray = T(unsafe_wrap(Array, ptr, size(T)))
    Base.sizeof(::T) where {ElT, T <: FixedArray{ElT}} = sizeof(ElT)length(T)
    #endregion
    
    #region interface patch for Tuple
    _isplaindata(::Type{T}) where T <: Tuple = all(_isplaindata.(tuple(T.parameters...)))

    Base.length(::Type{T}) where T <: Tuple = length(T.parameters)
    #endregion



    include("UnsafeArray.jl")
    include("AppendBuffer.jl")
    
    include("UnsafeStruct.jl")


    # NOTE: this has to be after includes to recognise overloads
    #region unsafe memory access
    @generated function _get(ptr::Ptr{T}) where T
        _checkplain(T)
        if isbitstype(T)
            return quote
                @inline
                ptr[]
            end
        else
            return quote
                @inline
                $T(ptr)
            end
        end
    end

    @generated function _set!(ptr::Ptr{T}, v) where T
        _checkplain(T)
        N = sizeof(T)
        if isbitstype(v)
            if isbitstype(T)
                return quote
                    @inline
                    ptr[] = v
                end
            else
                M = length(v)
                length(T) >= M || throw(ArgumentError(lazy"expected $M sized collection. Given: $(length(v))"))
                V = NTuple{M, eltype(T)}
                return quote
                    @inline
                    Ptr{$V}(ptr)[] = Tuple(v)
                end
            end
        else
            sizeof(v) == N || throw(ArgumentError(lazy"size of $v is not matching. Given $(sizeof(v)), expected $N"))
            return quote
                @inline
                @pin v unsafe_copyto!(Ptr{UInt8}(ptr), Ptr{UInt8}(pointer(v)), $N)
                return v
            end
        end
    end
    #endregion

end