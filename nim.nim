import strutils, sequtils, times

type
    Route = object
        dest, cost: int
    Node = object
        neighbours: seq[Route]

proc readPlaces(numNodes: var int): seq[Node] =
    # Read places...
    var lines = toSeq("agraph".lines)
    numNodes  = lines[0].parseInt
    result    = newSeqWith(numNodes, Node(neighbours: newSeq[Route]()))

    for i in 1.. < lines.len:
        let nums = lines[i].split(' ')
        if nums.len < 3:
            break

        let node      = nums[0].parseInt
        let neighbour = nums[1].parseInt
        let cost      = nums[2].parseInt

        result[node].neighbours.add(
            Route(dest: neighbour, cost: cost));

proc getLongestPath(nodes: seq[Node], nodeId: int, visited: var seq[bool]): int =
    # Get longest path
    visited[nodeId] = true
    for neighbour in nodes[nodeId].neighbours:
        if not visited[neighbour.dest]:
            let dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited)
            if dist > result:
                result = dist

    visited[nodeId] = false

proc main =
    var numNodes = 0
    let nodes    = readPlaces(numNodes)
    var visited  = newSeqWith(numNodes, false)

    let start    = cpuTime()
    let result   = getLongestPath(nodes, 0, visited)
    let duration = cpuTime() - start

    echo result, " LANGUAGE NIM ", duration

main()
