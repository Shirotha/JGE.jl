module DataGraphs

    include("../doc-templates.jl")

    include("Node.jl")
    include("Edge.jl")

    const NodeOrEdge{T} = Union{Node{T}, Edge{T}}

    include("Store.jl")

    include("DataGraph.jl")

end