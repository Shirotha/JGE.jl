using ModernGL
import GLAbstractionPatch as GLA

#region URef
export URef
struct URef{T, N, M}
    source::UniformArray{T, N, M}
    index::GLint
end
#endregion

#region constructors
function URef(array::UniformArray{T, N, M}, i::Integer) where {T, N, M}
    checkbounds(array, i)
    URef{T, N, M}(array, a.map[i])
end
URef(array::UniformArray) = URef(array, 1)

URef(ref::URef{T, N, M}, i::Integer) where {T, N, M} =
    URef{T, N, M}(ref.source, i)
URef(ref::URef{T, N, M}) where {T, N, M} =
    URef{T, N, M}(ref.source, ref.index)
#endregion

#region getters
export isvalid
isvalid(ref::URef) = !isnothing(try_index(ref.source.map, ref.index))
#endregion

#region indexing
import Base: getindex
getindex(ref::URef) = ref.source[index(ref.source.map, ref.index)]

import Base: setindex!
setindex!(ref::URef, value) =
    ref.source[index(ref.source.map, ref.index)] = value
setindex!(ref::URef, value, ::typeof(!)) =
    ref.source[!, index(ref.source.map, ref.index)] = value
#endregion

GLA.gluniformblock(program::GLA.Program, name::Symbol, ref::URef) =
    GLA.gluniformblock(program, name, ref.source.gpu, ref.index)
