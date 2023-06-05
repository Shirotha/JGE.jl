module Graphics

    include("../doc-templates.jl")

    include("../SIMD-Patch.jl")
    include("../BiDataStructures.jl")

    include("GLAbstraction-UniformBuffer-Patch.jl")

    #=
        uniform blocks
            global - same for all shaders (e.g. time) 
            => Use UniformBuffer(global)
            camera - one state per camera (pass camera to draw functions) (e.g. matrices)
            => Use upload!(::Camera)
            custom - owned by program, one state per (shared) material
            => use upload!(::ProgramData)
        uniforms
            model - matrix set for each model
            => draw argument
            custom - pass to draw functions as Symbol 
            => Pair{uniform, value} (need defaults to prevent leaking data, or make arguments required)
        varying
            store all together in mesh
            => use VertexArray

        # TODO: implement Material type
        #       - uniform values?
        #       - uniform block references
        #       - subroutine bindings
        # TODO: implement shader file format (ref: unity CG code)
        #       - opengl configuration (e.g. blending, stencil)
        #       - uniforms/uniform block types (julia + glsl)
        #       - interface blocks/varying
        #       - glsl shader code
        #       - glsl helper functions to include (e.g. MVP, lighting)
        # TODO: implement draw functions
        # TODO support compute shaders
        #       - implement UniformBuffer equivalent without multiple elements (ShaderStorageBuffer)
        #       ? only update parts to prevent re-uploading the whole struct ?
        #       - shader file compute section (including group size)
        #       - link to SSBO from within shader file (including access qualifiers)
        #       - attach SSBO to shaders
        #       - reference SSBO in materials
    =#
    
    include("UniformBufferWriter.jl")
    include("UniformArray.jl")

    include("URef.jl")
    include("ProgramData.jl")

    include("Camera.jl")

end