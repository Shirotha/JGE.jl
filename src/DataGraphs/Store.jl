using DataStructures

#region Store
export Store
"""
Data storage container for graphs.
Can store dat attached to `Node` and `Edge` objects.
Data is accessed by type, so only one instance of each unique type can be attached to each object.
"""
struct Store{T}
    "Data attached to `Node` objects."
    node::SwissDict{Symbol, SwissDict{T}}
    "Data attached to `Edge` objects."
    edge::SwissDict{Symbol, SwissDict{Pair{T, T}}}
end

Store{T}() where T = Store(SwissDict{Symbol, SwissDict{T}}(), SwissDict{Symbol, SwissDict{Pair{T, T}}}())
Store(store::Store{T}) where T = Store{T}(deepcopy(store.node), deepcopy(store.edge))
Store{T}(store::Store) where T = Store{T}(deepcopy(store.node), deepcopy(store.edge))
#endregion

#region conversion
import Base: convert
convert(::Type{T}, store::Store) where T <: Store = T(store)
convert(::Type{T}, store::T) where T <: Store = store
#endregion

#region promotion
import Base: promote_rule
promote_rule(::Type{Store{T}}, ::Type{Store{U}}) where {T, U} = Store{promote_type(T, U)}
#endregion

#region helpers
_nodedata(store::Store{T}, ::Type{D}) where {T, D} = get!(store.node, Symbol(D)) do
    return SwissDict{T, D}()
end
_nodedata(store::Store{T}, typename::Symbol) where T = get!(store.node, typename) do
    return SwissDict{T, eval(typename)::DataType}()
end
_edgedata(store::Store{T}, ::Type{D}) where {T, D} = get!(store.edge, Symbol(D)) do
    return SwissDict{Pair{T, T}, D}()
end
_edgedata(store::Store{T}, typename::Symbol) where T = get!(store.edge, typename) do
    return SwissDict{Pair{T, T}, eval(typename)::DataType}()
end
_data(store::Store, ::Node, T) = _nodedata(store, T)
_data(store::Store, ::Edge, T) = _edgedata(store, T)
#endregion

#region StoreDataIterator
struct StoreDataIterator{T}
    data::SwissDict{Symbol, SwissDict{T}}
    key::T
end

StoreDataIterator(data::SwissDict{Symbol, SwissDict{T}}, key::T) where T = StoreDataIterator{T}(data, key)
StoreDataIterator(iter::StoreDataIterator{T}) where T = StoreDataIterator{T}(iter.data, iter.key)
StoreDataIterator{T}(iter::StoreDataIterator) where T = StoreDataIterator{T}(iter.data, iter.key)
#endregion

#region StoreDataIterator: iteration
import Base: length
length(iter::StoreDataIterator) = count(iter.data) do (_, data)
    return haskey(data, iter.key)
end

import Base: eltype
eltype(::StoreDataIterator) = Pair{Symbol}

import Base: iterate, @propagate_inbounds as @prop
@prop function iterate(iter::StoreDataIterator)
    data_val = iterate(iter.data)
    isnothing(data_val) && return nothing
    
    (key, data), state = data_val
    haskey(data, iter.key) && return key => data, state

    return iterate(iter, state)
end
@prop function iterate(iter::StoreDataIterator, state)
    while true
        data_val = iterate(iter.data, state)
        isnothing(data_val) && return nothing

        (key, data), state = data_val
        haskey(data, iter.key) || continue

        return key => data, state
    end
end
#endregion

#region StoreDataIterator: helpers
_data(store::Store, node::Node) = StoreDataIterator(store.node, index(node))
_data(store::Store, edge::Edge) = StoreDataIterator(store.edge, index(edge))
_data(store::Store{T}, key::T) where T = StoreDataIterator(store.node, key)
_data(store::Store{T}, key::Pair{T, T}) where T = StoreDataIterator(store.edge, key)
#endregion

#region checks
import Base: checkbounds
checkbounds(store::Store, key::NodeOrEdge, type) = haskey(store, key, type) || throw(BoundsError(store, (key, type)))
#endregion

#region indexing
import Base: getindex
"""
Retrieves data attached to a `Node` or `Edge` of a specific type.

Raises `BoundsError` when data is not found.
"""
function getindex(store::Store, key::NodeOrEdge, type)
    @boundscheck checkbounds(store, key, type)
    return @inbounds _data(store, key, type)[index(key)]
end
"""
Retrives all data attached to a `Node` or `Edge` as a list of `type_name => data`.
"""
function getindex(store::Store, key::NodeOrEdge)
    i = index(key) 
    return map(_data(store, key)) do (typename, data)
        typename => data[i]
    end
end

import Base: setindex!
"""
Attaches data to a `Node` or `Edge` (overwrites already existing data).
"""
function setindex!(store::Store, value::T, key::NodeOrEdge) where T
    if T <: Array || T <: Tuple
        for v in value
            store[key] = v
        end
    else
        @inbounds _data(store, key, T)[index(key)] = value
    end
end
"""
Attaches data to a `Node` or `Edge` as a specific type (overwrites already existing data).
"""
function setindex!(store::Store, value, key::NodeOrEdge, type)
    if value isa Array || value isa Tuple
        for (v, t) in zip(value, type)
            store[key, t] = v
        end
    else
        @inbounds _data(store, key, type) = value
    end
end

import Base: haskey
"""
Checks if a `Node` or `Edge` has data of a specific type attached to it.
"""
haskey(store::Store, key::NodeOrEdge, type) = haskey(_data(store, key, type), index(key))
"""
Checks if a `Node` or `Edge` has any data attached to it.
"""
function haskey(store::Store, key::NodeOrEdge) 
    i = index(key)
    for (_, data) in _data(store, key)
        haskey(data, i) && return true
    end
    return false
end
#endregion

#region transformations
import Base: delete!
"""
Deletes data of a spcific type already attached to a `Node` or `Edge`.
"""
delete!(store::Store, key::NodeOrEdge, type) = delete!(_data(store, key, type), index(key))
"""
Deletes all data attached to a `Node` or `Edge`.
"""
function delete!(store::Store, key::NodeOrEdge)
    i = index(key)
    for (_, data) in _data(store, i)
        delete!(data, i)
    end
end

import Base: copyto!
"""
Copy all attached data from a `Node` or `Edge` to another.
"""
function copyto!(store::Store, src::T, dst::T) where T <: NodeOrEdge
    delete!(store, dst)

    i = index(src)
    j = index(dst)
    for (_, data) in @inbounds _data(store, i)
        data[j] = @inbounds data[i]
    end
    return nothing
end
#endregion