# Set cc = clang in compiler's config/nim.cfg for better performance
import strutils, sequtils, times

type
    Route = object
        dest, cost: int
    Node = object
        neighbours: seq[Route]

proc readPlaces: tuple[nodes: seq[Node], num: int] =
    let lines    = toSeq("agraph".lines)
    let numNodes = lines[0].parseInt
    var nodes    = newSeqWith(numNodes, Node(neighbours: newSeq[Route]()))

    for i in 1.. < lines.len:
        let nums = lines[i].split(' ')
        if nums.len < 3:
            break

        let node      = nums[0].parseInt
        let neighbour = nums[1].parseInt
        let cost      = nums[2].parseInt

        nodes[node].neighbours.add(
            Route(dest: neighbour, cost: cost))

    return (nodes, numNodes)

proc getLongestPath(nodes: seq[Node], nodeId: int, visited: var seq[bool]): int =
    visited[nodeId] = true
    for neighbour in nodes[nodeId].neighbours:
        if not visited[neighbour.dest]:
            let dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited)
            if dist > result:
                result = dist

    visited[nodeId] = false

proc main =
    let (nodes, numNodes) = readPlaces()
    var visited  = newSeqWith(numNodes, false)
    let start    = cpuTime()
    let result   = getLongestPath(nodes, 0, visited)
    let duration = cpuTime() - start

    echo result, " LANGUAGE NIM ", int(duration * 1000)

main()
