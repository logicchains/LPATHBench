immutable Route
    dst :: Int
    cost :: Int
end

type Node
    neighbours :: Array{Route, 1}
end

function readPlaces(f)
    n = int(readline(f))

    nodes = Array(Node, n)

    for i in 1:n 
        nodes[i] = Node([]) 
    end

    while !eof(f)
        route = split(readline(f))

        src = int(route[1]) + 1
        dst = int(route[2]) + 1
        cost = int(route[3])

	push!(nodes[src].neighbours, Route(dst, cost))
    end
    n, nodes
end

function getLongestPath(nodes :: Array{Node, 1}, nodeid :: Int, visited :: Array{Bool, 1})
    visited[nodeid] = true
    m = 0
    for neighbour in nodes[nodeid].neighbours
        @inbounds if !visited[neighbour.dst]
            const dist = neighbour.cost + getLongestPath(nodes, neighbour.dst, visited)
            m = max(dist, m)
        end
    end
    visited[nodeid] = false
    m
end

function main()
    n, nodes = open(readPlaces, "agraph")
    visited = fill(false, n)

    getLongestPath([Node([])], 1, visited)

    t = @elapsed len = getLongestPath(nodes, 1, visited)

    println(len, " LANGUAGE Julia ", int(t*1000), " ms")
end

main()
