#region Edge
export Edge
"""
An directed edge in a `DataGraph`.
"""
struct Edge{T}
    from::Node{T}
    to::Node{T}
end

Edge(from::Node{T}, to::Node{U}) where {T, U} = Edge(promote(from, to)...)
Edge(from, to::Node) = Edge(Instance(from), to)
Edge{T}(edge::Edge) where T = Edge{T}(edge.from, edge.to)
#endregion

#region conversion
import Base: convert
convert(::Type{T}, edge::Edge) where T <: Edge = T(edge)
convert(::Type{T}, edge::T) where T <: Edge = edge
#endregion

#region promotion
import Base: promote_rule
promote_rule(::Type{Edge{T}}, ::Type{Edge{U}}) where {T, U} = Edge{promote_type(T, U)}
#endregion

#region comparison
import Base: ==
==(lhs::Edge, rhs::Edge) = lhs.from == rhs.from && lhs.to == rhs.to

import Base: hash
hash(edge::Edge, h::UInt) = hash(edge.from, hash(edge.to, h))
#endregion

#region getters
export index
"""
Get the indices of origin and destination `Node` as a `Pair`.
"""
index(edge::Edge) = index(edge.from) => index(edge.to)

export orig
"""
Get the origin `Node` of an `Edge`.
"""
orig(edge::Edge) = edge.from

export dest
"""
Get the destination `Node` of an `Edge`.
"""
dest(edge::Edge) = edge.to
#endregion

#region show
import Base: show
show(io::IO, edge::Edge) = print(io, "$(edge.from) |> $(edge.to)")
#endregion