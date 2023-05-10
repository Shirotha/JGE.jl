#region Node
export Node
"""
A node in a `DataGraph`.
"""
struct Node{T}
    """
    An (integer) index into the adjacency list of a `DataGraph`.
    Invalid for `Virtual`.
    """
    index::T
    """
    Container to hold data, depending on the type of `Node`.
    - `Instance` - unused
    - `Shared` - multiplicity of the node connection
    - `Virtual` - arbitrary embedded data
    """
    data
end

Node(node::Node{T}) where T = Node{T}(node.index, node.data)
Node{T}(node::Node) where T = Node{T}(node.index, node.data)
#endregion

#region promotion
import Base: promote_rule
promote_rule(::Type{Node{T}}, ::Type{Node{U}}) where {T, U} = Node{promote_type(T, U)}
#endregion

#region comparison
import Base: ==
==(lhs::Node, rhs::Node) = lhs.index == rhs.index && lhs.data == rhs.data

import Base: isequal
isequal(lhs::Node, rhs::Node) = !isvirtual(lhs) && lhs.index == rhs.index

import Base: hash
hash(node::Node, h::UInt) = hash(node.index, h)
#endregion

#region checks
export isinstance
"""
Checks wether a `Node` is an `Instance`.
"""
isinstance(node::Node) = !iszero(node.index) && iszero(node.data)

export isshared
"""
Checks wether a `Node` is a `Shared`.
"""
isshared(node::Node) = !iszero(node.index) && !iszero(node.data)

export isvirtual
"""
Checks wether a `Node` is an `Virtual`
"""
isvirtual(node::Node) = iszero(node.index)

export nodetype
"""
Returns the type of `Node`.
Can return one of `Instance`, `Shared` or `Virtual`
"""
nodetype(node::Node) = iszero(node.index) ? Virtual : 
    iszero(node.data) ? Instance : Shared

export checkinstance
checkinstance(node::Node) = isinstance(node) || throw(ArgumentError(lazy"node has to be an Instance. (given $(nodetype(node)))"))

export checkshared
checkshared(node::Node) = isshared(node) || throw(ArgumentError(lazy"node has to be a Shared. (given $(nodetype(node)))"))

export checkvirtual
checkvirtual(node::Node) = isvirtual(node) || throw(ArgumentError(lazy"node has to be a Virtual. (given $(nodetype(node)))"))

export checknotvirtual
checknotvirtual(node::Node) = !isvirtual(node) || throw(ArgumentError(lazy"node can't be Virtual."))
#endregion

#region Instance
export Instance
"""
The basic type of `Node`.
"""
abstract type Instance{T} end

Instance(index::T) where T = Node{T}(index, zero(T))
Instance{T}(index) where T = Node{T}(index, zero(T))
function Instance(node::Node{T}) where T
    checkinstance(node)
    return Instance{T}(node.index)
end
function Instance{T}(node::Node) where T
    checkinstance(node)
    return Instance{T}(node.index)
end
#endregion

#region Shared
export Shared
"""
A type of `Node` that represents a template and not a unique instance.
Use `instantiate!` to create a `Instance` from a `Shared`.
"""
abstract type Shared{T} end

Shared(index::T, count::Integer) where T = Node{T}(index, count)
Shared{T}(index, count::Integer) where T = Node{T}(index, count)
Shared(node::Node{T}, count::Integer) where T = Shared{T}(index(node), count)
Shared{T}(node::Node, count::Integer) where T = Shared{T}(index(node), count)
Shared(node::Node{T}) where T = Shared{T}(index(node), isshared(node) ? count(node) : one(T))
Shared{T}(node::Node) where T = Shared{T}(index(node), isshared(node) ? count(node) : one(T))
#endregion

#region Virtual
export Virtual
"""
A type of `Node` that has no `index` accociated to it.
Data can be embedded into it, but it can't be used in a `Store`
"""
abstract type Virtual{T} end

Virtual{T}(data) where T = Node{T}(zero(T), data)
function Virtual(node::Node{T}) where T
    checkvirtual(node)
    return Virtual{T}(node.data)
end
function Virtual{T}(node::Node) where T
    checkvirtual(node)
    return Virtual{T}(node.data)
end
#endregion

#region conversion
import Base: convert
convert(::Type{T}, node::Node) where T <: Node = T(node)
convert(::Type{T}, node::Node) where T <: Instance = T(node)
convert(::Type{T}, node::Node) where T <: Shared = T(node)
convert(::Type{T}, node::Node) where T <: Virtual = T(node)
convert(::Type{T}, node::T) where T <: Node = node
#endregion

#region getters
export index
"""
Get the (integer) index corresponding to a `Node` that is not virtual.
"""
function index(node::Node)
    checknotvirtual(node)
    return node.index
end

import Base: count
"""
Get the number of connections to a shared `Node`.
"""
function count(node::Node)
    checkshared(node)
    return node.data
end

export data
"""
Get the data attached to a virtual `Node`.
"""
function data(node::Node)
    checkvirtual(node)
    return node.data
end
#endregion

#region show
import Base: show
function show(io::IO, node::Node{T}) where T
    N = nodetype(node)
    if N == Instance
        print(io, "{$(node.index)}")
    elseif N == Shared
        print(io, "{$(node.index): Shared $(node.data)}")
    else
        print(io, "{Virtual: $(node.data)}")
    end
end
#endregion