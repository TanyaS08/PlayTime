## This will activate the environment in the code sub-folder
import Pkg
Pkg.activate(".")

using LinearAlgebra
using SparseArrays

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
