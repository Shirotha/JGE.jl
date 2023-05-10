using MacroTools

#region DataGraph
import Graphs: AbstractGraph
export DataGraph
"""
A directed graph that can hold arbitrary data attached to its `Node` and `Edge` objects.
Indices may be reused, keeping external references to them is not adviced.
"""
struct DataGraph{T} <: AbstractGraph{T}
    "Adjacency list containing target `Node` objects."
    forwards::Vector{Vector{Node{T}}}
    "Lookup to traveres edges backwards."
    backwards::Vector{Vector{T}}
    "Pool of available indices to be reused."
    slots::Set{T}
    "Data storage for both `Node` and `Edge` objects."
    store::Store{T}
end

DataGraph{T}() where T = DataGraph{T}(Vector{Node{T}}[], Vector{T}[], Set{T}(), Store{T}())
DataGraph(g::DataGraph{T}) where T = DataGraph{T}(deepcopy(g.forwards), deepcopy(g.backwards), deepcopy(g.slots), deepcopy(g.store))
DataGraph{T}(g::DataGraph) where T = DataGraph{T}(deepcopy(g.forwards), deepcopy(g.backwards), deepcopy(g.slots), deepcopy(g.store))
#endregion

#region conversion
import Base: convert
convert(::Type{T}, g::DataGraph) where T <: DataGraph = T(g)
convert(::Type{T}, g::T) where T <: DataGraph = g
#endregion

#region promotion
import Base: promote_rule
promote_rule(::Type{DataGraph{T}}, ::Type{DataGraph{U}}) where {T, U} = DataGraph{promote_type(T, U)}
#endregion

#region checks
import Base: checkbounds
@inline function checkbounds(g::DataGraph, node::Node)
    i = index(node) 
    if !(i in keys(g.backwards)) || i in g.slots
        throw(BoundsError(g, node))
    end
end
@inline function checkbounds(g::DataGraph{T}, i::T) where T
    if !(i in keys(g.backwards)) || i in g.slots
        throw(BoundsError(g, i))
    end
end
#endregion

#region EdgeIterator
struct EdgeIterator{T}
    g::DataGraph{T}
end

EdgeIterator(g::DataGraph{T}) where T = EdgeIterator{T}(g)
#endregion

#region EdgeIterator: helpers
_findnextedge(g::DataGraph) = _findnextedge(g, 1, 1)
function _findnextedge(g::DataGraph, from, to)
    ONE = one(from)
    while true
        while from in g.slots
            from += ONE
            to = ONE
        end
        from in keys(g.backwards) || return nothing

        to in keys(g.forwards[from]) && return from, to

        from += ONE
    end
end
#endregion

#region EdgeIterator: iteration
import Base: eltype
eltype(::EdgeIterator{T}) where T = Edge{T}

import Base: length
length(iter::EdgeIterator) = ne(iter.g)

import Base: iterate
function iterate(iter::EdgeIterator)
    isempty(iter.g) && return nothing
    
    from, to = _findnextedge(iter.g)
    return Edge(from, iter.g.forwards[from][to]), (from, to)
end
function iterate(iter::EdgeIterator, (from, to)::Tuple)
    next = _findnextedge(iter.g, from, to)
    isnothing(next) && return nothing

    from, to = next
    return Edge(from, iter.g.forwards[from][to]), (from, to)
end
#endregion

#region EdgeIterator: show
import Base: show
show(io::IO, iter::EdgeIterator) = print(io, "{$(join(iter, ", "))}")
function show(io::IO, ::MIME"text/plain", iter::EdgeIterator{T}) where T
    println(io, length(iter), "-element EdgeIterator{$T}")
    for edge in iter
        println(io, " ", edge)
    end
end
#endregion

#region AbstractGraph interface
import Base: eltype
eltype(::DataGraph{T}) where T = T

import Base: zero
zero(::Type{T}) where T <: DataGraph = T()

import Base: isempty
isempty(g::DataGraph) = iszero(nv(g))

import Graphs: is_directed
is_directed(::DataGraph) = true

import Graphs: edges
edges(g::DataGraph) = EdgeIterator(g)

import Graphs: edgetype
edgetype(::DataGraph{T}) where T = Edge{T}

import Graphs: ne
ne(g::DataGraph) = sum(length, g.backwards)

import Graphs: has_edge
import Base: @propagate_inbounds as @prop
@prop function has_edge(g::DataGraph, edge::Edge)
    has_vertex(g, edge.from) || return false
    i = index(g, edge.from)
    isnothing(i) && return false
    j = findfirst(isequal(edge.to), g.forwards[i])
    isnothing(j) || return true
    j = findfirst(==(edge.to), g.forwards[i])
    return !isnothing(j)
end
@prop has_edge(g::DataGraph, src::Node, dst::Node) = has_edge(g, Edge(src, dst))
@prop has_edge(g::DataGraph{T}, src::T, dst::T) where T = src in g.backwards[dst]

import Graphs: vertices
vertices(g::DataGraph) = setdiff(keys(g.backwards), g.slots)

import Graphs: nv
nv(g::DataGraph) = length(g.backwards) - length(g.slots)

# NOTE: this will not find virtual nodes
import Graphs: has_vertex
@prop has_vertex(g::DataGraph, v::Node) = has_vertex(g, index(v))
@prop has_vertex(g::DataGraph{T}, v::T) where T = v in keys(g.backwards) && !(v in g.slots)

import Graphs: inneighbors
inneighbors(g::DataGraph, v::Node) = inneighbors(g, index(g, v))
inneighbors(g::DataGraph{T}, v::T) where T = g.backwards[v]

import Graphs: outneighbors
outneighbors(g::DataGraph, v::Node) = outneighbors(g, index(g, v))
outneighbors(g::DataGraph{T}, v::T) where T = index.(g.forwards[v])
#endregion

#region getters
export nodes
"""
Similar to `Graphs.vertices`, but returns `Node` objects instead.
"""
nodes(g::DataGraph) = Instance.(vertices(g))

export index
"""
Returns the index of the node only if valid.
"""
function index(g::DataGraph, node::Node)
        isvirtual(node) && return nothing
        has_vertex(g, node) || throw(ArgumentError(lazy"Node not in graph. (given $node)"))
        return index(node)
    end
"""
Returns the index of the edge `from => to` only if valid.
"""
function index(g::DataGraph, from::Node, to::Node)
    i = index(g, from)
    isnothing(i) && return nothing
    j = findfirst(isequal(to), g.forwards[i])
    isnothing(j) || return i => j
    j = findfirst(==(to), g.forwards[i])
    isnothing(j) && throw(ArgumentError(lazy"Missing edge $from |> $to."))
    return i => j
end
"""
Returns the index of the edge only if valid.
"""
index(g::DataGraph, edge::Edge) = index(g, edge.from, edge.to)

export remap
"""
Maps the given `Node` to an already existing `Node` in `DataGraph` if available.
"""
function remap(g::DataGraph, node::Node)
    i = index(g, node)
    isnothing(i) ? node : Instance(i)
end
"""
Maps the given edge `from => to` to an already existing edge `(from, to)` in `DataGraph` if available.
"""
function remap(g::DataGraph, from::Node, to::Node)
    ij = index(g, from, to)
    isnothing(ij) && return (from, to)
    (i, j) = ij
    (Instance(i), g.forwards[i][j])
end
"""
Maps the given `Edge` to an already existing `Edge` in `DataGraph` if available.
"""
remap(g::DataGraph, edge::Edge) = Edge(remap(g, edge.from, edge.to)...)

export @remap
"""
Equivalent to `key = remap(g, key)`.
"""
macro remap(g, key)
    @esc g key
    return quote
        $key = remap($g, $key)
    end
end
"""
Equivalent to `from, to = remap(g, from, to)`.
"""
macro remap(g, from, to)
    @esc g from to
    return quote
        ($from, $to) = remap($g, $from, $to)
    end
end


export edges
"""
Similar to `Graphs.outneighbors`, but returns `Edge` instead.
"""
edges(g::DataGraph, node::Node) = edges(g, index(g, node))
"""
Similar to `Graphs.outneighbors`, but returns `Edge` instead.
"""
function edges(g::DataGraph{T}, v::T) where T
    @boundscheck checkbounds(g, v)
    return Edge.(v, g.forwards[v])
end
#endregion

#region transformations
import Graphs: add_vertex!
function add_vertex!(g::DataGraph{T}) where T
    if !isempty(g.slots)
        node = first(g.slots)
        delete!(g.slots, node)

        return Instance(node)
    end

    push!(g.forwards, Node{T}[])
    push!(g.backwards, T[])
    return Instance(length(g.backwards))
end
function add_vertex!(g::DataGraph, data...)
    node = add_vertex!(g)
    for d in data
        g[node] = d
    end
    return node
end

export add_node!
"Alias for `add_vertex!`."
add_node!(g::DataGraph) = add_vertex!(g)
"Alias for `add_vertex!`."
add_node!(g::DataGraph, data...) = add_vertex!(g, data...)

import Graphs: add_vertices!
import Base: OneTo
add_vertices!(g::DataGraph, n::Integer) = [add_vertex!(g) for _ in OneTo(n)]

import Graphs: add_edge!
function add_edge!(g::DataGraph, from::Node, to::Node)
    @boundscheck checkbounds(g, from)
    has_edge(g, from, to) && throw(ArgumentError(lazy"edge already exists. ($from |> $to)"))

    i = index(g, from)
    push!(g.forwards[i], to)
    isvirtual(to) && return Edge(from, to)
    
    j = index(g, to)
    push!(g.backwards[j], i)
    return Edge(from, to)
end
function add_edge!(g::DataGraph, from, to, data...)
    edge = add_edge!(g, from, to)
    for d in data
        g[edge] = d
    end
    return edge
end

import Base: delete!
"""
Deletes an `Edge` from `DataGraph`.
"""
function delete!(g::DataGraph, from::Node, to::Node)
    has_edge(g, from, to) || return nothing
    @remap g from to

    delete!(g.store, Edge(from, to))

    i, j = index(g, from, to)
    deleteat!(g.forwards[i], j)

    isvirtual(to) && return nothing

    j = index(g, to)
    i = findfirst(==(i), g.backwards[j])
    deleteat!(g.backwards[j], i)
    return nothing
end
"""
Deletes an `Edge` from `DataGraph`.
"""
delete!(g::DataGraph, edge::Edge) = delete!(g, edge.from, edge.to)
"""
Deletes a `Node` from `DataGraph` (and all connecting `Edge` objects).
"""
function delete!(g::DataGraph, node::Node)
    for edge in reverse(edges(g, node))
        delete!(g, edge)
    end
end

export recount!
"""
Updates the multiplicity of a `Shared` connection.
"""
function recount!(g::DataGraph, from::Node, to::Node, count::Integer)
    count >= 0 || throw(ArgumentError(lazy"count can't be negative. (given $count)"))
    @remap g from to
    checkshared(to)
    
    i, j = index(g, from, to)
    g.forwards[i][j] = Shared(to, count) 
end

export instantiate!
import Base: OneTo
"""
Creats a new `Node` from the destination of an `Edge`.

Only `Shared` nodes can be instantiated.
"""
function instantiate!(g::DataGraph, from::Node, to::Node)
    @remap g from to
    checkinstance(from)
    checkshared(to)

    instance = add_vertex!(g)
    copyto!(g.store, to, instance)
    for edge in edges(g, to)
        new = add_edge!(g, instance, edge.to)
        copyto!(g.store, edge, new)
    end
    new = add_edge!(g, from, instance)
    copyto!(g.store, Edge(from, to), new)

    if count(to) > 1
        recount!(g, from, to, count(to) - 1)
    else
        delete!(g, from, to)
    end

    return instance
end
"""
Creats a new `Node` from the destination of an `Edge`.

Only `Shared` nodes can be instantiated.
"""
instantiate!(g::DataGraph, edge::Edge) = instantiate!(g, edge.from, edge.to)
"""
Creats multiple new `Node` objects from the destination of an `Edge`.

Only `Shared` nodes can be instantiated.

Raises `ArgumentError` if multiplicity is too low.
"""
function instantiate!(g::DataGraph, from::Node, to::Node, count::Integer)
    count >= 0 || throw(ArgumentError(lazy"count can't be negative. (given $count)"))
    return [instantiate!(g, from, to) for _ in OneTo(count)]
end
"""
Creats multiple new `Node` objects from the destination of an `Edge`.

Only `Shared` nodes can be instantiated.

Raises `ArgumentError` if multiplicity is too low.
"""
instantiate!(g::DataGraph, edge::Edge, count::Integer) = instantiate!(g, edge.from, edge.to, count)
#endregion

#region indexing
import Base: getindex
"Forwarding data access to `Store`."
getindex(g::DataGraph, key::NodeOrEdge) = g.store[key]
"Forwarding data access to `Store`."
getindex(g::DataGraph, from::Node, to::Node) = g.store[remap(g, Edge(from, to))]
"Forwarding data access to `Store`."
getindex(g::DataGraph, key::NodeOrEdge, T) = g.store[key, T]
"Forwarding data access to `Store`."
getindex(g::DataGraph, from::Node, to::Node, T) = g.store[remap(g, Edge(from, to)), T]

import Base: setindex!
"Forwarding data access to `Store`."
setindex!(g::DataGraph, value, key::NodeOrEdge) = g.store[key] = value
"Forwarding data access to `Store`."
setindex!(g::DataGraph, value, from::Node, to::Node) = g.store[remap(g, Edge(from, to))] = value
"Forwarding data access to `Store`."
setindex!(g::DataGraph, value, key::NodeOrEdge, T) = g.store[key, T] = value
"Forwarding data access to `Store`."
setindex!(g::DataGraph, value, from::Node, to::Node, T) = g.store[remap(g, Edge(from, to)), T] = value

import Base: haskey
"Forwarding data access to `Store`."
haskey(g::DataGraph, key::NodeOrEdge) = haskey(g.store, key)
"Forwarding data access to `Store`."
haskey(g::DataGraph, from::Node, to::Node) = haskey(g.store, remap(g, Edge(from, to)))
"Forwarding data access to `Store`."
haskey(g::DataGraph, key::NodeOrEdge, T) = haskey(g.store, key, T)
"Forwarding data access to `Store`."
haskey(g::DataGraph, from::Node, to::Node, T) = haskey(g.store, remap(g, Edge(from, to)))
#endregion

#region show
import Base: show
function show(io::IO, ::MIME"text/plain", g::DataGraph{T}) where T
    println(io, "$(nv(g))-node DataGraph{$T}")
    for node in nodes(g)
        edge_dump = map(edges(g, node)) do edge
            isvirtual(edge.to) && return repr(edge.to)
            iter = g[edge]
            isempty(iter) && return repr(edge.to)
            data = map(iter) do (_, data)
                return data
            end
            return "$(edge.to)[$(join(data, ", "))]"
        end
        println(io, "$(node) => ", join(edge_dump, ", "))
        for (_, data) in g[node]
            println(io, "  ", data)
        end
    end
end
#endregion