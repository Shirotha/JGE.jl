using StaticArraysCore, ModernGL, GeometryBasics, .UnsafeCollections
import .GLAbstractionPatch as GLA
import .SIMDPatch as SIMD
#region StructLayout
abstract type StructLayout end
#endregion

#region constructors
StructLayout(::Type{T}) where T = error("Can't create layout from type $T.")
#endregion

#region STD140
struct STD140 <: StructLayout end
#endregion

#region helpers
# TODO: support SizedArray, AppendBuffer, Simd.Vec (custom SIMD.Mat?)
function glsl_alignment_size(T)
    function ceil4(i)
        while i%4 != 0
            i += 1
        end
        return i
    end
    T <: Bool && return sizeof(Int32), sizeof(Int32)
    N = sizeof(T)
    T <: GLA.GLSLScalarTypes && return N, N
    T <: Function && return sizeof(Vec4f0), sizeof(Vec4f0) # sizeof(EmptyStruct) padded to Vec4f0
    ET = eltype(T)
    N  = sizeof(ET)
    if T <: SMatrix || T <: SizedMatrix
        nrows, ncols = size(T)
        ncols        = ceil4(ncols)
        return div(ncols, 4) * N, ncols * nrows * N
    end
    if T <: SVector || T <: SIMD.Vec || T <: SizedVector
        return ceil4(length(T)) * N, length(T) * N
    end
    # TODO: support aliased AppendBuffer
    if T <: AppendBuffer && !isaliased(T)
        return ceil4(length(T)) * N, length(T) * N
    end
    @error "Struct $T not supported yet. Please help by implementing all rules from https://khronos.org/registry/OpenGL/specs/gl/glspec45.core.pdf#page=159"
end

function std140_offsets(::Type{T}) where T
    elementsize = 0
    offsets = if T <: GLA.GLSLScalarTypes
        elementsize = sizeof(T)
        (0,)
    else
        offset = 0
        offsets = ntuple(fieldcount(T)) do i
            ft = fieldtype(T, i)
            alignement, sz = glsl_alignment_size(ft)
            if offset % alignement != 0
                offset = (div(offset, alignement) + 1) * alignement
            end
            of = offset
            offset += sz
            of
        end
        elementsize = offset
        offsets
    end
    offsets, elementsize
end
#endregion

#region constructors
function STD140(::Type{T}) where T
    types = Tuple{fieldtypes(T)...}
    N = fieldcount(T)
    names = fieldnames(T)
    offsets, size = std140_offsets(T)
    return UnsafeStruct{types, N, names, offsets .+ 1, size}
end
#endregion

# TODO: support STD430 (reuse glsl_alignment_size, but remove alignment requirement in std140)