using ModernGL
import GLAbstractionPatch as GLA

#region ProgramData
export ProgramData
struct ProgramData{T, names}
    program::GLA.Program
    data::NamedTuple{names, T}
end
#endregion

#region constructors
function ProgramData(program::Program, blocktypes::Vararg{Pair{Symbol, DataType}, N}; size::Union{Integer, NTuple{N, <:Integer}}=1024, mode::Union{GLenum, NTuple{N, GLenum}}=GL_STATIC_DRAW) where N
    param(x::Tuple) = x
    param(x) = ntuple(Returns(x), Val{N}())

    names = first.(blocktypes)
    types = last.(blocktypes)
    sizes = param(size)
    modes = param(mode)

    buffers = ntuple(Val{N}()) do i
        UniformVector{types[i]}(sizes[i], modes[i])
    end

    data = NamedTuple(zip(names, buffers))

    return ProgramData{names, typeof(buffers)}(program, data)
end
#endregion

#region indexing
import Base: getindex
getindex(pd::ProgramData, ::typeof(!), name::Symbol, i::Integer) = pd.data[name][i]
getindex(pd::ProgramData, ::typeof(!), i::Integer) = getindex.(values(pd.data), i)
getindex(pd::ProgramData, ::typeof(!), name::Symbol) = pd.data[name]
function getindex(pd::ProgramData, ::typeof(!), is::NTuple{N, <:Integer}) where N
    N == length(pd.data) || throw(ArgumentError(lazy"expected $(length(pd.data)) indices (Given: $N)"))
    return getindex.(values(pd.data), is)
end

getindex(pd::ProgramData, name::Symbol, i::Integer) = URef(pd.data[name], i)
getindex(pd::ProgramData, i::Integer) = URef.(values(pd.data), i)
getindex(pd::ProgramData, name::Symbol) = URef(pd.data[name])
function getindex(pd::ProgramData, is::NTuple{N, <:Integer}) where N
    N == length(pd.data) || throw(ArgumentError(lazy"expected $(length(pd.data)) indices (Given: $N)"))
    return URef.(values(pd.data), is)
end

import Base: setindex!
setindex!(pd::ProgramData, v, name::Symbol, i::Integer) = pd.data[name][i] = v
function setindex!(pd::ProgramData, vs::NTuple{N}, is::NTuple{N, <:Integer}) where N
    N == length(pd.data) || throw(ArgumentError(lazy"expected $(length(pd.data)) values (Given: $N)"))
    for (buffer, v, i) in zip(values(pd.data), vs, is)
        buffer[i] = v
    end
    return vs
end

setindex!(pd::ProgramData, v, ::typeof(!), name::Symbol, i::Integer) = pd.data[name][!, i] = v
function setindex!(pd::ProgramData, vs::NTuple{N}, ::typeof(!), is::NTuple{N, <:Integer}) where N
    N == length(pd.data) || throw(ArgumentError(lazy"expected $(length(pd.data)) values (Given: $N)"))
    for (buffer, v, i) in zip(values(pd.data), vs, is)
        buffer[!, i] = v
    end
    return vs
end
#endregion

#region getters
export isdirty
isdirty(pd::ProgramData) = any(isdirty, values(pd.data))

import Base: firstindex
firstindex(pd::ProgramData) = firstindex.(values(pd.data))
import Base: lastindex
lastindex(pd::ProgramData) = lastindex.(values(pd.data))
#endregion

#region transformations
import Base: push!
push!(pd::ProgramData, name::Symbol, v; upload::Bool=false) = push!(pd.data[name], v; upload)
function push!(pd::ProgramData, names::NTuple{N, Symbol}, vs::NTuple{N}; upload::Bool=false) where N
    for (name, v) in zip(names, vs)
        push!(pd, name, v; upload)
    end
    return pd
end
function push!(pd::ProgramData, vs::NTuple{N}; upload=false) where N
    N == length(pd.data) || throw(ArgumentError(lazy"expected $(length(pd.data)) values! (Given: $N)"))
    for (buffer, v) in zip(values(pd.data), vs)
        push!(buffer, v)
    end
    return pd
end

import Base: deleteat!
deleteat!(pd::ProgramData, name::Symbol, i::Integer) = deleteat!(pd.data[name], i)

export upload!
upload!(pd::ProgramData) = foreach(upload!, values(pd.data))
#endregion