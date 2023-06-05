module SIMDPatch

    using SIMD, StaticArraysCore

    # swiwwel
    const COORD_NAMES = Base.ImmutableDict(
        'x' => 0,
        'y' => 1,
        'z' => 2,
        'w' => 3,

        'r' => 0,
        'g' => 1,
        'b' => 2,
        'a' => 3,
        
        'q' => 0,
        'r' => 1,
        's' => 2,
        't' => 3,
    )

    map_coord_name(order::Symbol) = Val{getindex.((COORD_NAMES,), Tuple(String(order)))}()
    @generated map_coord_name(::Val{order}) where order = map_coord_name(order)

    swiwwel(v::Vec, order) = shufflevector(v, map_coord_name(order))

    getproperty(v::Vec, order::Symbol) = order === :data ? getfield(v, :data) : swiwwel(v, Val{order}())

    # Vec mixed types
    promote_rule(::Type{Vec{N, T}}, ::Type{Vec{N, U}}) where {N, T, U} = Vec{N, promote_type(T, U)}
    SIMD.Vec(args...) = Vec(promote(args...)...)
    for (op, _, _) in SIMD.BINARY_OPS
        @eval @inline function $op(x::Vec{N}, y::Vec{N}) where N
            return $op(promote(x, y)...)
        end
        # NOTE: needs to be more specific than definition in SIMD
        for F in (Float32, Float64)
            @eval @inline function $op(x::Vec{N, T}, y::$F) where {N, T <: SIMD.IntegerTypes}
                return $op(convert(Vec{N, $F}, x), y)
            end
            @eval @inline function $op(x::$F, y::Vec{N, T}) where {N, T <: SIMD.IntegerTypes}
                return $op(x, convert(Vec{N, $F}, y))
            end
        end
    end

    # Vec iterate
    import Base: iterate
    iterate(v::Vec) = (v[1], 2)
    function iterate(v::Vec, state::Int)
        1 <= state <= length(v) || return nothing
        return v[state], state+1
    end


    # exports
    for name in names(SIMD)
        @eval export name
    end

    export simd
    simd(v::SVector) = Vec(v.data)
    simd(vs...) = simd.(vs)

end