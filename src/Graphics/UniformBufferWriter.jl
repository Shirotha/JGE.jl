using ModernGL
import GLAbstractionPatch as GLA

#region UnsafeBufferWriter
mutable struct UnsafeBufferWriter{T, N} <: AbstractVector{T}
    buffer::UniformBuffer{T, N}
    mapping::Ptr{UInt8}
    length::Int
end
#endregion

#region constructors
UnsafeBufferWriter(buffer::UniformBuffer{T, N}) where {T, N} =
    UnsafeBufferWriter{T, N}(buffer, C_NULL, 0)
#endregion

#region transformations
import Base: open
function open(w::UnsafeBufferWriter)
    @boundscheck w.mapping == C_NULL || error(lazy"Writer is alread open!")
    buff = w.buffer.buffer
    glBindBuffer(buff.buffertype, buff.id)
    w.mapping = Ptr{UInt8}(glMapBuffer(buff.buffertype, GL_WRITE_ONLY))
    w.length = length(buff)
    return w
end
function open(f, w::UnsafeBufferWriter)
    open(w)
    f(w)
    close(w)
end

import Base: close
function close(w::UnsafeBufferWriter)
    @boundscheck w.mapping == C_NULL && error(lazy"Writer is not open!")
    buff = w.buffer.buffer
    glUnmapBuffer(buff.buffertype)
    glBindBuffer(buff.buffertype, C_NULL)
    w.mapping = C_NULL
    w.length = 0
    return w
end
#endregion

#region indexing
import Base: setindex!
function setindex!(w::UnsafeBufferWriter{T}, v::T, i::Integer) where T
    @boundscheck w.mapping == C_NULL && error(lazy"Writer is not open!")
    for (offset, ptr, size) in GLA.iterate_fields(w.buffer, v, i)
        unsafe_copyto!(w.mapping + offset, ptr, size)
    end
    return v
end
#endregion