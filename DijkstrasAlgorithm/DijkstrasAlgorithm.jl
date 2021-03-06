## This will activate the environment in the folder
import Pkg
Pkg.activate("./DijkstrasAlgorithm")

#packages
using LinearAlgebra
using SparseArrays

#
m = rand(Float64, (100, 100)) .≤ 0.2
m[diagind(m)] .= false

M = sparse(m)

origin, destination = 1, 10

unvisited = collect(1:size(M,1))
distance = fill(Inf, size(M,1))
distance[origin] = 0.0

while !isempty(unvisited)
    shortest = distance .== minimum(distance[unvisited])
    todo = map(x -> (x in unvisited), 1:size(M,1))
    current = findfirst(todo .* shortest)
    d = distance[current]
    @info "Currently visiting $(current)"
    neighborhood = M[current,:].nzind
    for neighbor in neighborhood
        distance[neighbor] = min(distance[neighbor], d+1.0)
    end
    filter!(!isequal(current), unvisited)
    if !(destination ∈ unvisited)
        @info "$(distance[destination])"
        break
    end
    sleep(0.5)
end

# A graph as a vector of sorted vectors. This is very similar to the way sparse matrices
# are represented except each vertex has its own sorted vector (making it much faster to
# add/remove edges). Using BTrees instead of sorted vector would improve performance for
# adding/removing edges.

#WeightedGraph = Vector{Vector{(UInt32, Float64)}}
Graph = Vector{Vector{UInt32}} # Make more generic...

# There has to be a better way (map?):
function make_graph(order)
    g = Graph()
    for i in 1:order
        push!(g, Vector{UInt32}())
    end
    g
end

function to_dense_matrix(g::Graph)
    order = graph_order(g)
    m = zeros(order, order) # Using integers?
    for v in 1:order
        for head in g[v]
            m[v, head] = 1.0
        end
    end
    m
end

function from_dense_matrix(m::Array{Float64,2})
    # if size(m)[1] != size(m)[2] -> WTF!
    g = make_graph(size(m)[1])
    for c in 1:order
        for r in 1:order
            if m[r, c] == 1.0
                add_edge(g, r, c)
            end
        end
    end
    g
end

function make_random_graph(order, p::Float64)
    g = make_graph(order)
    for v in 1:order
        # Simpler way to cast to UInt32?
        push!(g, filter(x -> x > 0, [convert(UInt32, v > p) * convert(UInt32, i) for (i, v) in enumerate(rand(Float64, order))]))
    end
    g
end

function make_random_geometric_graph(order, threshold::Float64)
    g = make_graph(order)
    xs = rand(Float64, order)
    ys = rand(Float64, order)
    for u in 1:order
        for v in 1:order
            if hypot(xs[u] - xs[v], ys[u] - ys[v]) < threshold
                add_edge(g, u, v)
            end
        end
    end
    g
end

# The graph's order (number of vertices, |V|)
function graph_order(g::Graph) size(g)[1] end

# The graph's size (number of edges, |E|)
function graph_size(g::Graph) sum(map(size, g)) end

# Check for the presence of an edge (blows up if tail > |V|).
function has_edge(g::Graph, tail, head)
    !isempty(g[tail]) && g[tail][searchsortedfirst(g[tail], head)] == head
end

# Add an edge to the graph
function add_edge(g::Graph, tail, head)
    if isempty(g[tail]) || head > last(g[tail])
        push!(g[tail], head)
    else
        idx = searchsortedfirst(g[tail], head)
        if g[tail][idx] != head
            insert!(g[tail], idx, head)
        end
    end
end

function add_edges(g::Graph, edges)
    for edge in edges add_edge(g, edge[1], edge[2]) end
end

# Dijkstra shortest path algorithm
function shortest_path(g::Graph, tail, head)
    order = graph_order(g)
    if tail == 0 || head == 0 || tail > order || head > order
        return (Vector{UInt32}(), Inf)
    end

    openset = Set(1:order)
    dist = fill(Inf, order)
    came_from = fill(0, order)

    dist[tail] = 0

    while !isempty(openset)
        current = 0
        smallest_dist = Inf
        for v in openset
            if dist[v] < smallest_dist
                current = v
                smallest_dist = dist[v]
            end
        end

        # No possible paths:
        if smallest_dist == Inf
            break
        end

        # Shortest path found:
        if current == head
            path = Vector{UInt32}()
            while came_from[current] != 0
                push!(path, current)
                current = came_from[current]
            end
            push!(path, tail)
            return (reverse(path), dist[head])
        end

        delete!(openset, current)
        for v in g[current]
            if v in openset
                alt = dist[current] + 1
                if alt < dist[v]
                    dist[v] = alt
                    came_from[v] = current
                end
            end
        end

    end
    (Vector{UInt32}(), Inf)
end

# Dijkstra shortest path algorithm.... Basically ONE line is different, again: probably a better way to do this.
function dense_shortest_path(g::Array{Float64,2}, tail, head)
    order = size(g)[1]
    if tail == 0 || head == 0 || tail > order || head > order
        return (Vector{UInt32}(), Inf)
    end

    openset = Set(1:order)
    dist = fill(Inf, order)
    came_from = fill(0, order)

    dist[tail] = 0

    while !isempty(openset)
        current = 0
        smallest_dist = Inf
        for v in openset
            if dist[v] < smallest_dist
                current = v
                smallest_dist = dist[v]
            end
        end

        # No possible paths:
        if smallest_dist == Inf
            break
        end

        # Shortest path found:
        if current == head
            path = Vector{UInt32}()
            while came_from[current] != 0
                push!(path, current)
                current = came_from[current]
            end
            push!(path, tail)
            return (reverse(path), dist[head])
        end

        delete!(openset, current)
        for v in 1:order
            if g[current, v] > 0.0 && v in openset
                alt = dist[current] + g[current, v]
                if alt < dist[v]
                    dist[v] = alt
                    came_from[v] = current
                end
            end
        end

    end
    (Vector{UInt32}(), Inf)
end

g = make_graph(12)
add_edges(g, [(1, 5), (1, 3), (5, 2), (3, 7), (3, 4), (4, 5), (4, 6), (7, 8), (7, 5), (8, 9), (9, 11), (9, 12), (9, 10)])
@assert shortest_path(g, 1, 12) == (UInt32[0x00000001, 0x00000003, 0x00000007, 0x00000008, 0x00000009, 0x0000000c], 5.0)
@assert shortest_path(g, 11, 1) == (UInt32[], Inf)
