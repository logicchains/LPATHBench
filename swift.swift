// Based on hyp's original Swift 1 implementation: https://github.com/hyp/LPATHBench/blob/1920f809a2f22f6fbfe4393ab87557bea8364416/swift.swift
// Compile with: swiftc -O swift.swift

import Foundation
import QuartzCore

struct Route {
    let dest: Int
    let cost: Int
}

struct Node {
    var neighbours: [Route] = []
}

func readPlaces() -> [Node] {
    var places = [Node]()
    if let data = try? String(contentsOfFile: "agraph", encoding: .utf8) {
        data.enumerateLines { line, stop in
            places = Array(repeating: Node(), count: Int(line) ?? 0)
            stop = true
        }
        data.enumerateLines { (line, stop) in
            let components = line.components(separatedBy: CharacterSet.whitespaces)
            guard components.count >= 3 else {
                return
            }
            switch (Int(components[0]), Int(components[1]), Int(components[2])) {
            case let (.some(node), .some(dest), .some(cost)):
                places[node].neighbours.append(Route(dest: dest, cost: cost))
            default:
                break
            }
        }
    }
    return places
}

func getLongestPath(nodes: [Node], nodeId: Int, visited: inout [Bool]) -> Int {
    visited[nodeId] = true

    var max = 0
    for neighbour in nodes[nodeId].neighbours {
        guard !visited[neighbour.dest] else {
            continue
        }

        let dist = neighbour.cost + getLongestPath(nodes: nodes, nodeId: neighbour.dest, visited: &visited)
        if dist > max {
            max = dist
        }
    }

    visited[nodeId] = false

    return max
}

func getLongestPath(nodes: [Node]) -> Int {
    var visited = Array<Bool>(repeating: false, count: nodes.count)
    return getLongestPath(nodes: nodes, nodeId: 0, visited: &visited)
}

let nodes = readPlaces()
let startTime = CACurrentMediaTime()
let length = getLongestPath(nodes: nodes)
let endTime = CACurrentMediaTime()
let ms = Int((endTime - startTime) * 1000)
print("\(length) LANGUAGE Swift \(ms)")
