module GLAbstractionPatch

    using ModernGL, StaticArrays
    using GLAbstraction
    const IMPORT_EXCEPTIONS = [
        :eval, :import, :include,
        :UniformBuffer, :Program, :LazyProgram,
        :data, :resize,
        :NATIVE_TYPES
    ]
    for name in names(GLAbstraction; all=true)
        name in IMPORT_EXCEPTIONS && continue
        startswith(string(name), '#') && continue
        @eval import GLAbstraction: $name
    end

    # GLAbstraction/src/uniformbuffer.jl
    """
    Statically sized uniform buffer.
    Supports push!, but with fixed memory, so it will error after reaching
    it's preallocated length.
    """
    mutable struct UniformBuffer{T, N}
        buffer::Buffer{T}
        offsets::NTuple{N, Int}
        elementsize::Int
        length::Int
    end
    
    """
    Pre allocates an empty buffer with `max_batch_size` size
    which can be used to store multiple uniform blocks of type T
    """
    function UniformBuffer(::Type{T}, max_batch_size = 1024, mode = GL_STATIC_DRAW) where T
        offsets, elementsize = std140_offsets(T)
        buffer = Buffer{T}(
            Ptr{T}(),
            elementsize * max_batch_size,
            GL_UNIFORM_BUFFER, mode
        )
        UniformBuffer(buffer, offsets, elementsize, 0)
    end
    """
        Creates an Uniform buffer with the contents of `data`
    """
    function UniformBuffer(data::T, mode = GL_STATIC_DRAW) where T
        buffer = UniformBuffer(T, 1, mode)
        push!(buffer, data)
        buffer
    end

    function glsl_alignment_size(T)
        function ceil4(i)
            while i%4 != 0
                i += 1
            end
            return i
        end
        T <: Bool && return sizeof(Int32), sizeof(Int32)
        N = sizeof(T)
        T <: GLSLScalarTypes && return N, N
        T <: Function && return sizeof(Vec4f0), sizeof(Vec4f0) # sizeof(EmptyStruct) padded to Vec4f0
        ET = eltype(T)
        N  = sizeof(ET)
        if T <: Matrix || T <: SMatrix
            nrows, ncols = size(T)
            ncols        = ceil4(ncols)
            return div(ncols, 4) * N, ncols * nrows * N
        end
        if T <: Vector || T <: SVector
            return ceil4(length(T)) * N, length(T) * N
        end
        @error "Struct $T not supported yet. Please help by implementing all rules from https://khronos.org/registry/OpenGL/specs/gl/glspec45.core.pdf#page=159"
    end

    function std140_offsets(::Type{T}) where T
        elementsize = 0
        offsets = if T <: GLSLScalarTypes
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

    Base.convert(::Type{UniformBuffer}, x) = UniformBuffer(x)
    Base.convert(::Type{UniformBuffer}, x::UniformBuffer) = x
    Base.eltype(::UniformBuffer{T}) where T = T

    function Base.setindex!(buffer::UniformBuffer{T}, element::T, idx::Integer) where T
        if idx > length(buffer.buffer)
            throw(BoundsError(buffer, idx))
        end
        buff = buffer.buffer
        glBindBuffer(buff.buffertype, buff.id)
        dptr = Ptr{UInt8}(glMapBuffer(buff.buffertype, GL_WRITE_ONLY))
        for (offset, ptr, size) in iterate_fields(buffer, element, idx)
            unsafe_copyto!(dptr + offset, ptr, size)
        end
        glUnmapBuffer(buff.buffertype)
        bind(buff, 0)
        element
    end

    function Base.push!(buffer::UniformBuffer{T}, element::T) where T
        buffer.length += 1
        buffer[buffer.length] = element
        buffer
    end

    function assert_blocksize(buffer::UniformBuffer, program, blockname::String)
        block_index = glGetUniformBlockIndex(program, blockname)
        blocksize_ref = Ref{GLint}(0)
        glGetActiveUniformBlockiv(
            program, block_index,
            GL_UNIFORM_BLOCK_DATA_SIZE, blocksize_ref
        )
        blocksize = blocksize_ref[]
        @assert buffer.elementsize * length(buffer.buffer) == blocksize
    end

    _getfield(x::GLSLScalarTypes, i) = x
    _getfield(x, i) = getfield(x, i)

    function iterate_fields(buffer::UniformBuffer{T, N}, x, index) where {T, N}
        offset = buffer.elementsize * (index - 1)
        x_ref = isimmutable(x) ? Ref(x) : x
        base_ptr = Ptr{UInt8}(pointer_from_objref(x_ref))
        ntuple(Val{N}()) do i
            offset + buffer.offsets[i], base_ptr + fieldoffset(T, i), sizeof(fieldtype(T, i))
        end
    end

    extract_val(::Val{X}) where X = X

    function gluniformblock(binding::Integer, buffer::UniformBuffer)
        buff = buffer.buffer
        glBindBufferBase(buff.buffertype, GLint(binding), buff.id)
    end
    function gluniformblock(binding::Integer, buffer::UniformBuffer, idx::Integer)
        buff = buffer.buffer
        size = buffer.elementsize
        offset = size * (idx - 1)
        glBindBufferRange(buff.buffertype, GLint(binding), buff.id, offset, size)
    end

    function gluniformsubroutines(stage::GLenum, indices::NTuple{N, <:Integer}) where N
        glUniformSubroutinesuiv(stage, GLsizei(N), Vector{GLuint}(indices))
    end
    function gluniformsubroutines(stage::GLenum, indices::Vector{<:Integer})
        glUniformSubroutinesuiv(stage, GLsizei(length(indices)), convert(Vector{GLuint}, indices))
    end

    # GLAbstraction/src/for_moderngl.jl
    function glGetActiveUniformBlock(programID::GLuint, index::Integer)
        actualLength  = GLsizei[1]
        binding       = GLint[1]
        maxcharsize   = glGetProgramiv(programID, GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH)
        name          = zeros(GLchar, maxcharsize)
    
        glGetActiveUniformBlockName(programID, index, maxcharsize, actualLength, name)
    
        actualLength[1] <= 0 && @error "No active uniform block at given index. Index: $index"
    
        uname = unsafe_string(pointer(name), actualLength[1])
        uname = Symbol(replace(uname, r"\[\d*\]" => ""))
    
        glGetActiveUniformBlockiv(programID, index, GL_UNIFORM_BLOCK_BINDING, binding)
    
        (uname, binding[1])
    end

    function glGetProgramStageiv(programID::GLuint, stage::GLenum, variable::GLenum)
        result = Ref{GLint}(-1)
        ModernGL.glGetProgramStageiv(programID, stage, variable, result)
        return result[]
    end

    function glGetActiveSubroutineName(programID::GLuint, stage::GLenum, index::Integer)
        actualLength  = GLsizei[1]
        maxcharsize   = glGetProgramStageiv(programID, stage, GL_ACTIVE_SUBROUTINE_MAX_LENGTH)
        name = zeros(GLchar, maxcharsize)

        ModernGL.glGetActiveSubroutineName(programID, stage, index, maxcharsize, actualLength, name)

        actualLength[1] <= 0 && @error "No active subroutine at given index. Index: $index"

        sname = unsafe_string(pointer(name), actualLength[1])
        sname = Symbol(replace(sname, r"\[\d*\]" => ""))

        return sname
    end

    function glGetActiveSubroutineUniform(programID::GLuint, stage::GLenum, index::Integer)
        actualLength  = GLsizei[1]
        maxcharsize   = glGetProgramStageiv(programID, stage, GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH)
        name = zeros(GLchar, maxcharsize)

        glGetActiveSubroutineUniformName(programID, stage, index, maxcharsize, actualLength, name)

        actualLength[1] <= 0 && @error "No active subroutine uniform at given index. Index: $index"

        sname = unsafe_string(pointer(name), actualLength[1])
        sname = Symbol(replace(sname, r"\[\d*\]" => ""))

        nvalues = GLint[1]

        glGetActiveSubroutineUniformiv(programID, stage, index, GL_NUM_COMPATIBLE_SUBROUTINES, nvalues)

        values = zeros(GLint, nvalues[1])

        glGetActiveSubroutineUniformiv(programID, stage, index, GL_COMPATIBLE_SUBROUTINES, values)

        size = GLint[1]

        glGetActiveSubroutineUniformiv(programID, stage, index, GL_UNIFORM_SIZE, size)

        return (sname, values, size[1])
    end


    # GLAbstraction/src/shader/program.jl
    const UniformBlockTuple = NamedTuple{(:name, :binding), Tuple{Symbol, GLint}}
    const SubroutineTuple = NamedTuple{(:name,), Tuple{Symbol}}
    const SubroutineUniformTuple = NamedTuple{(:name, :values, :size), Tuple{Symbol, Vector{Symbol}, GLint}}
    const INVALID_UNIFORMBLOCK = GLint(-1)
    const INVALID_SUBROUTINE = GLint(-1)
    const INVALID_SUBROUTINEUNIFORM = GLint(-1)

    # NOTE: assumes that binding is set in shader code
    function setup_uniformblocks(program::GLuint)
        info = UniformBlockTuple[]
        nblocks = glGetProgramiv(program, GL_ACTIVE_UNIFORM_BLOCKS)
        for i=1:nblocks
            name, binding = glGetActiveUniformBlock(program, i-1)
            push!(info, (name = name, binding = binding))
        end
        return info
    end

    function setup_subroutines(program::GLuint, stage::GLenum)
        info = SubroutineTuple[]
        nsubs = glGetProgramStageiv(program, stage, GL_ACTIVE_SUBROUTINES)
        for i=1:nsubs
            name = glGetActiveSubroutineName(program, stage, i-1)
            push!(info, (name = name,))
        end
        return info
    end

    function setup_subroutineuniforms(program::GLuint, stage::GLenum, subinfo::Vector{SubroutineTuple})
        info = SubroutineUniformTuple[]
        nsubs = glGetProgramStageiv(program, stage, GL_ACTIVE_SUBROUTINE_UNIFORMS)
        for i=1:nsubs
            name, values, size = glGetActiveSubroutineUniform(program, stage, i-1)
            values .= getindex.(getindex.(subinfo, values), (:name,))
            push!(info, (name = name, values = values, size = size))
        end
        return info
    end

    mutable struct Program <: AbstractProgram
        id                ::GLuint
        shaders           ::Vector{Shader}
        uniforms          ::Vector{UniformTuple}
        attributes        ::Vector{AttributeTuple}
        uniformblocks     ::Vector{UniformBlockTuple}
        subroutines       ::Vector{Vector{SubroutineTuple}}
        subroutineuniforms::Vector{Vector{SubroutineUniformTuple}}
        context
        function Program(shaders::Vector{Shader}, fragdatalocation::Vector{Tuple{Int, String}})
            # Remove old shaders
            exists_context()
            program = glCreateProgram()::GLuint
            glUseProgram(program)
            #attach new ones
            foreach(shaders) do shader
                glAttachShader(program, shader.id)
            end
    
            #Bind frag data
            for (location, name) in fragdatalocation
                glBindFragDataLocation(program, location, ascii(name))
            end
    
            #link program
            glLinkProgram(program)
            if !islinked(program)
                for shader in shaders
                    write(stdout, shader.source)
                    println("---------------------------")
                end
                @error "program $program not linked. Error in: \n $(join(map(x->x.id, shaders), " or ")), \n $(getinfolog(program))"
            end
    
            # generate the link locations
            uniforms           = setup_uniforms(program)
            attribs            = setup_attributes(program)
            blocks             = setup_uniformblocks(program)
            subroutines        = [setup_subroutines(program, shader.typ) for shader in shaders]
            subroutineuniforms = [setup_subroutine_uniforms(program, shader.typ, subinfo) for (shader, subinfo) in zip(shaders, subroutines)]
            prog = new(program, shaders, uniforms, attribs, blocks, subroutines, subroutineuniforms, current_context())
            finalizer(free!, prog)
            prog
        end
    end
    
    Program(shaders::Vector{Shader}) = Program(shaders, Tuple{Int, String}[])
    Program(shaders::Shader...) = Program([shaders...])
    
    Program(sh_string_typ...) = 
        Program([map(x -> Shader(x), sh_string_typ)...])
    
    free!(x::Program) = context_command(() -> glDeleteProgram(x.id), x.context)
    
    function shaderindex(program::Program, stage::GLenum) 
        result = findfirst(shader -> shader.typ == stage, program.shaders)
        isnothing(result) && @error "Shader of given type not found in Program. Type: $stage"
        return result
    end

    attributes(program::Program) = program.attributes
    uniforms(program::Program)   = program.uniforms
    uniform_names(program::Program) = [x.name for x in program.uniforms]
    uniformblocks(program::Program) = program.uniformblocks
    uniformblock_names(program::Program) = [b.name for b in program.uniformblocks]
    subroutines(program::Program, stage::GLenum) = program.subroutines[shaderindex(program, stage)]
    subroutine_names(program::Program, stage::GLenum) = [s.name for s in program.subroutines[shaderindex(program, stage)]]
    subroutineuniforms(program::Program, stage::GLenum) = program.subroutineuniforms[shaderindex(program, stage)]
    subroutineuniforms_names(program::Program, stage::GLenum) = [s.name for s in program.subroutineuniforms[shaderindex(program, stage)]]
    
    attribute(program::Program, name::Symbol) =
        getfirst(x -> x.name == name, program.attributes)
    uniform(program::Program, name::Symbol) =
        getfirst(x -> x.name == name, program.uniforms)
    uniformblock(program::Program, name::Symbol) =
        getfirst(x -> x.name == name, program.uniformblocks)
    subroutine(program::Program, stage::GLenum, name::Symbol) =
        getfirst(x -> x.name == name, program.subroutines[shaderindex(program, stage)])
    subroutineuniform(program::Program, stage::GLenum, name::Symbol) =
        getfirst(x -> x.name == name, program.uniformsubroutines[shaderindex(program, stage)])
    
    function attribute_location(program::Program, name::Symbol)
        att = attribute(program, name)
        return att !== nothing ? att.location : INVALID_ATTRIBUTE
    end
    
    function uniform_location(program::Program, name::Symbol)
        u = uniform(program, name)
        return u !== nothing ? u.location : INVALID_UNIFORM
    end
    
    function attribute_type(program::Program, name::Symbol)
        att = attribute(program, name)
        return att !== nothing ? att.T : Nothing
    end
    
    function uniform_type(program::Program, name::Symbol)
        u = uniform(program, name)
        return u !== nothing ? u.T : Nothing
    end
    
    function attribute_size(program::Program, name::Symbol)
        att = attribute(program, name)
        return att !== nothing ? att.size : INVALID_ATTRIBUTE
    end
    
    function uniform_size(program::Program, name::Symbol)
        u = uniform(program, name)
        return u !== nothing ? u.size : INVALID_UNIFORM
    end
    
    function uniformblock_binding(program::Program, name::Symbol)
        b = uniformblock(program, name)
        return b !== nothing ? b.binding : INVALID_UNIFORMBLOCK
    end

    function subroutine_index(program::Program, stage::GLenum, name::Symbol)
        i = findfirst(s -> s.name == name, program.subroutines[shaderindex(program, stage)])
        return i !== nothing ? i : INVALID_SUBROUTINE
    end

    function subroutineuniform_values(program::Program, stage::GLenum, name::Symbol)
        s = subroutineuniform(program, stage, name)
        return s !== nothing ? s.values : INVALID_SUBROUTINEUNIFORM
    end

    function subroutineuniform_size(program::Program, stage::GLenum, name::Symbol)
        s = subroutineuniform(program, stage, name)
        return s !== nothing ? s.size : INVALID_SUBROUTINEUNIFORM
    end
    
    bind(program::Program) = glUseProgram(program.id)
    
    function gluniform(program::Program, name::Symbol, vals...)
        u = uniform_location(program, name)
        if u != INVALID_UNIFORM
            gluniform(u, vals...)
        end
    end
    
    function gluniformblock(program::Program, name::Symbol, vals...)
        b = uniformblock_binding(program, name)
        if b != INVALID_UNIFORMBLOCK
            gluniformblock(b, vals...)
        end
    end

    function gluniformsubroutines(program::Program, stage::GLenum, names)
        indices = subroutine_index.((program,), stage, names)
        if all(!=(INVALID_SUBROUTINE))
            gluniformsubroutines(stage, indices)
        end
    end
    
    function Base.show(io::IO, p::Program)
        println(io, "Program: $(p.id)")
        println(io, "Shaders:")
        for shader in p.shaders
            println(io, shader)
        end
        println(io, "attributes:")
        for a in p.attributes
            println(io, "   ", a.name, "::", GLENUM(a.T).name)
        end
        println(io, "uniforms:")
        for u in p.uniforms
            println(io, "   ", u.name, "::", GLENUM(u.T).name)
        end
        println(io, "blocks:")
        for b in p.uniformblocks
            println(io, "   ", b.name, "(binding = ", b.binding, ")")
        end
    end
    
    infolog(program::Program) = getinfolog(program.id)
    
    update_bindings!(program::Program) = program.uniformblocks = setup_uniformblocks(program.id)
    
    mutable struct LazyProgram <: AbstractProgram
        sources::Vector
        data::Dict
        compiled_program::Union{Program, Nothing}
    end
    LazyProgram(sources...; data...) = LazyProgram(Vector(sources), Dict(data), nothing)
    
    function Program(lazy_program::LazyProgram)
        fragdatalocation = get(lazy_program.data, :fragdatalocation, Tuple{Int, String}[])
        shaders = haskey(lazy_program.data, :arguments) ? Shader.(lazy_program.sources, Ref(lazy_program.data[:arguments])) : Shader.()
        return Program([shaders...], fragdatalocation)
    end
    function bind(program::LazyProgram)
        iscompiled_orcompile!(program)
        bind(program.compiled_program)
    end
    
    function iscompiled_orcompile!(program::LazyProgram)
        if program.compiled_program === nothing
            program.compiled_program = Program(program)
        end
    end



    # GLAbstraction/src/shader/glsl_typenames.jl
    glsl_type(::UniformBuffer{T}) where T = T
    isa_gl_struct(x::Program) = false



    # GLAbstraction/src/vertexarray.jl
    function generate_buffers(program::Program, divisor::GLint=GEOMETRY_DIVISOR; name_buffers...)
        buflen  = 0
        buffers = BufferAttachmentInfo[]
        for (name, val) in pairs(name_buffers)
            loc = attribute_location(program, name)
            if loc != INVALID_ATTRIBUTE
                buflen = buflen == 0 ? length(val) : buflen 
                vallen = length(val)
                if vallen == buflen
                    push!(buffers, BufferAttachmentInfo(name, loc, Buffer(val, usage=GL_DYNAMIC_DRAW), divisor))
                elseif !isa(val, Vector)
                    push!(buffers, BufferAttachmentInfo(name, loc, Buffer(fill(val, buflen), usage=GL_DYNAMIC_DRAW), divisor))
                end
            else
                error("Invalid attribute: $name.")
            end
        end
        return buffers
    end



    # GLAbstraction/src/conversion.jl
    gl_convert(a::Program) = a
    gl_convert(::Type{Program}, a::Program; kw_args...) = a

end