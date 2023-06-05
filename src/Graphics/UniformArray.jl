using ModernGL
using .BiDataStructures
import GLA as GLAbstractionPatch

#region UniformArray
export UniformArray
mutable struct UniformArray{T, N, M} <: AbstractArray{T, N}
    capacity::Int
    gpu::GLA.UniformBuffer{T, M}
    cpu::Vector{T}
    dirty::BitSet
    map::IndexedVector{Int}
    reversemap::Vector{Int}
    slots::BitSet
end
const UniformVector{T, M} = UniformArray{T, 1, M}
const UniformMatrix{T, M} = UniformArray{T, 2, M}
#endregion

#region constructors
function UniformArray{T, N}(capacity::NTuple{N}, mode = GL_STATIC_DRAW) where {T, N}
    cap = prod(capacity)
    gpu = GLA.UniformBuffer(T, cap, mode)
    cpu = T[]
    dirty = BitSet()
    map = IndexedVector{Int}()
    slots = BitSet()
    M = length(gpu.offsets)
    return UniformArray{T, N, M}(cap, gpu, cpu, dirty, map, slots)
end
UniformVector{T}(capacity, mode = GL_STATIC_DRAW) where T =
    UniformArray{T, 1}((capacity,), mode)
UniformMatrix{T}(capacity1, capacity2, mode = GL_STATIC_DRAW) where T =
    UniformArray{T, 2}((capacity1, capacity2), mode)

import Base: OneTo
function UniformArray(a::UniformArray{T, N, M}, mode = GL_STATIC_DRAW; upload::Bool=false) where {T, N, M}
    gpu = GLA.UniformBuffer(T, a.capacity, mode)
    cpu = copy(a.cpu)
    dirty = upload ? BitSet() : BitSet(OneTo(length(cpu)))
    map = IndexedVector{Int}(a.map)
    slots = copy(a.slots)
    result = UniformArray{T, N, M}(a.capacity, gpu, cpu, dirty, map, slots)
    upload && upload!(result)
    return result
end
function UniformArray{T, N}(a::UniformArray, mode = GL_STATIC_DRAW; upload::Bool=false) where {T, N}
    gpu = GLA.UniformBuffer(T, a.capacity, mode)
    cpu = Vector{T}(a.cpu)
    dirty = upload ? BitSet() : BitSet(OneTo(length(cpu)))
    map = IndexedVector{Int}(a.map)
    slots = copy(a.slots)
    M = length(gpu.offsets)
    result = UniformArray{T, N, M}(a.capacity, gpu, cpu, dirty, map, slots)
    upload && upload!(result)
    return result
end
#endregion

#region AbstractArray interface
import Base: size
size(a::UniformArray) = size(a.cpu)

import Base: IndexStyle
IndexStyle(::Type{<:UniformArray}) = IndexLinear()

import Base: getindex
getindex(a::UniformArray, i::Integer) = a.cpu[i]

import Base: setindex!
function setindex!(a::UniformArray, v, i::Integer)
    a.cpu[i] = v
    push!(a.dirty, i)
end
function setindex!(a::UniformArray, v, ::typeof(!), i::Integer)
    a.cpu[i] = v
    upload!(a, i)
end
#endregion

#region getters
export capacity
capacity(a::UniformArray) = a.capacity

export isdirty
isdirty(a::UniformArray, i::Integer) = i in a.dirty
isdirty(a::UniformArray) = isempty(a.dirty)
#endregion

#region transformations
export upload!
function upload!(a::UniformArray)
    isdirty(a) || return a
    writer = UnsafeBufferWriter(a.gpu)
    open(writer) do w
        for i in a.dirty
            w[map[i]] = a.cpu[i]
        end
    end
    empty!(a.dirty)
    return a
end
function upload!(a::UniformArray, i::Integer)
    isdirty(a, i) || return a
    delete!(a.dirty, i)
    a.gpu[map[i]] = a.cpu[i]
    return a
end

function aquireslot!(a::UniformArray) 
    if isempty(a.slots)
        if length(a) == capacity(a)
            grow!(a)
        end
        return length(a) + 1
    else
        return pop!(a.slots)
    end
end

import Base: resize!
function resize!(a::UniformArray{T, N}, capacity::NTuple{N}) where {T, N}
    error("Not implemented!")
end

export grow!
function grow!(a::UniformArray)
    sizes = size(a)
    capacity = (sizes[begin:end-1]..., 2sizes[end])
    return resize!(a, capacity)
end

import Base: push!
function push!(a::UniformArray, v; upload::Bool=false)
    i = aquireslot!(a)
    push!(a.cpu, v)
    push!(a.map, i)
    if upload
        upload!(a, i)
    else
        push!(a.dirty, i)
    end
    return a
end

import Base: deleteat!
function deleteat!(a::UniformArray, i::Integer)
    deleteat!(a.cpu, i)
    delete!(a.dirty, i)
    push!(a.slots, map[i])
    deleteat!(a.map, i)
    return a
end
#endregion