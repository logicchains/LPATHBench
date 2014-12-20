import strutils, sequtils, times

type
    Route = object
        dest, cost: int
    Node = object
        neighbours: seq[Route]

let lines    = toSeq("agraph".lines)
let numNodes = lines[0].parseInt
var nodes    = newSeqWith(numNodes, Node(neighbours: newSeq[Route]()))
var visited  = newSeq[bool](numNodes)

for i in 1..lines.high:
    let nums = lines[i].split(' ')
    if nums.len < 3:
        break

    let node      = nums[0].parseInt
    let neighbour = nums[1].parseInt
    let cost      = nums[2].parseInt

    nodes[node].neighbours.add(Route(dest: neighbour, cost: cost))

proc getLongestPath(nodeId: int): int =
    visited[nodeId] = true

    for neighbour in nodes[nodeId].neighbours:
        if not visited[neighbour.dest]:
            let dist = neighbour.cost + getLongestPath(neighbour.dest)
            if dist > result:
                result = dist

    visited[nodeId] = false

let start    = cpuTime()
let result   = getLongestPath(0)
let duration = cpuTime() - start

echo result, " LANGUAGE Nim ", int(duration * 1000)
