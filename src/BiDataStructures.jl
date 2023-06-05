module BiDataStructures

    using DataStructures
    import Base: @propagate_inbounds as @prop

    #region IndexedArray
    export IndexedArray
    mutable struct IndexedArray{T, N} <: AbstractArray{T, N}
        data::Array{T, N}
        index::SwissDict{T, Int}
    end

    export IndexedVector
    const IndexedVector{T} = IndexedArray{T, 1}
    export IndexedMatrix
    const IndexedMatrix{T} = IndexedArray{T, 2}
    #endregion

    #region constructors
    function IndexedArray{T, N}(args...) where {T, N}
        data = Array{T, N}(args...)
        index = SwissDict{T, Int}()
        for (i, x) in enumerate(data)
            haskey(index, x) && throw(ArgumentError(lazy"duplicate elements not allowed!"))
            index[x] = i
        end
        IndexedArray{T, N}(data, index)
    end
    IndexedArray{T, N}(a::IndexedArray) where {T, N} = 
        IndexedArray{T, N}(Array{T, N}(a.data), SwissDict{T, Int}(a.index))
    #endregion

    #region conversion
    import Base: convert
    convert(::Type{IndexedArray{T, N}}, a::AbstractArray) where {T, N} = 
        IndexedArray{T, N}(a)
    convert(::Type{IndexedArray{T, N}}, a::IndexedArray{T, N}) where {T, N} = a
    #endregion

    #region promotion
    import Base: promote_rule
    promote_rule(::Type{IndexedArray{T, N}}, ::Type{IndexedArray{S, N}}) where {T, S, N} =
        IndexedArray{promote_type(T, S), N}
    #endregion

    #region AbstractArray Interface
    import Base: size
    size(a::IndexedArray) = size(a.data)

    import Base: IndexStyle
    IndexStyle(::Type{<:IndexedArray}) = IndexLinear()

    import Base: getindex
    @prop getindex(a::IndexedArray, i::Int) = a.data[i]

    import Base: setindex!
    @prop function setindex!(a::IndexedArray, x, i::Int)
        delete!(a.index, a.data[i])
        a.data[i] = x
        a.index[x] = i
        return x
    end
    #endregion

    #region getters
    import Base: in
    in(x, a::IndexedArray) = haskey(a.index, x)

    export index
    @prop index(a::IndexedArray, x) = a.index[x]

    export try_index
    try_index(a::IndexedArray, x) = x in a ? a.index[x] : nothing
    #endregion

    #region transformations
    import Base: push!
    function push!(a::IndexedArray, x)
        haskey(a.index, x) && throw(ArgumentError(lazy"duplicate elements not allowed!"))
        push!(a.data, x)
        a.index[x] = length(a.data)
        return a
    end

    import Base: deleteat!
    function deleteat!(a::IndexedArray, i)
        delete!(a.index, a.data[i])
        deleteat!(a.data, i)
        return a
    end

    import Base: delete!
    function delete!(a::IndexedArray, x)
        deleteat!(a.data, a.index[x])
        delete!(a.index, x)
        return a
    end
    #endregion



    #region BiDict
    export BiDict
    mutable struct BiDict{TK, TV} <: AbstractDict{TK, TV}
        k2v::SwissDict{TK, TV}
        v2k::SwissDict{TV, TK}
    end
    #endregion

    #region constructors
    function BiDict{TK, TV}() where {TK, TV}
        k2v = SwissDict{TK, TV}()
        v2k = SwissDict{TV, TK}()
        BiDict(k2v, v2k)
    end
    function BiDict{TK, TV}(b::BiDict) where {TK, TV}
        k2v = SwissDict{TK, TV}(b.k2v)
        v2k = SwissDict{TV, TK}(b.v2k)
        BiDict(k2v, v2k)
    end
    function BiDict(d::AbstractDict{TK, TV}) where {TK, TV}
        b = BiDict{TK, TV}()
        for (k, v) in d
            b[k] = v
        end
        return b
    end
    function BiDict{TK, TV}(d::AbstractDict) where {TK, TV}
        b = BiDict{TK, TV}()
        for (k, v) in d
            b[k] = v
        end
        return b
    end
    #endregion

    #region conversion
    import Base: convert
    convert(::Type{Dict{TK, TV}}, b::BiDict) where {TK, TV} = Dict{TK, TV}(b.k2v)
    convert(::Type{BiDict{TK, TV}}, d::AbstractDict) where {TK, TV} = BiDict{TK, TV}(d)
    convert(::Type{BiDict{TK, TV}}, b::BiDict{TK, TV}) where {TK, TV} = b
    #endregion

    #region promotion
    import Base: promote_rule
    promote_rule(::Type{BiDict{TK, TV}}, ::Type{BiDict{SK, SV}}) where {TK, TV, SK, SV} =
        BiDict{promote_type(TK, SK), promote_type(TV, SV)}
    #endregion

    #region iteration
    import Base: eltype
    eltype(::BiDict{TK, TV}) where {TK, TV} = Pair{TK, TV}

    import Base: length
    length(b::BiDict) = length(b.k2v)

    import Base: iterate
    iterate(b::BiDict) = iterate(b.k2v)
    iterate(b::BiDict, s) = iterate(b.k2v, s)
    #endregion

    #region AbstractDict interface
    import Base: getindex
    @prop getindex(b::BiDict, k) = b.k2v[k]
    @prop getindex(b::BiDict, ::typeof(!), v) = b.v2k[v]

    import Base: setindex!
    @prop function setindex!(b::BiDict, v, k)
        haskey(b.k2v, k) && delete!(b.v2k, b.k2v[k])
        b.k2v[k] = v
        b.v2k[v] = k
        return v
    end
    @prop function Base.setindex!(b::BiDict, k, ::typeof(!), v)
        haskey(b.v2k, v) && delete!(b.k2v, b.v2k[v])
        b.v2k[v] = k
        b.k2v[k] = v
        return k
    end

    import Base: haskey
    haskey(b::BiDict, k) = haskey(b.k2v, k)

    export hasvalue
    hasvalue(b::BiDict, v) = haskey(b.v2k, v)
    
    import Base: keys
    keys(b::BiDict) = keys(b.k2v)

    import Base: values
    values(b::BiDict) = values(b.k2v)
    #endregion

end