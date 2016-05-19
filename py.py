# Python implementation to find longest path for an directed acyclic graph.
# A path is a sequence of vertices such that no vertices are repeated
# and every adjacent pair of vertices correspond to an actual edge in
# the graph.
# 
# Graph is represented as adjacency lists.
#
# To run: python py.py
#
# Author: Siew Vui Chee 

import time

# Global constants
TRUE = 1
FALSE = 0

class Edge:
    def __init__(self, weight, neighbourId):
        self.w = weight
        self.n = neighbourId

class Node:
    def __init__(self, nodeId, edges):
        self.neighbours = edges
        self.id = nodeId
       
def longestPath(srcId, graph, visited):
    visited[srcId] = TRUE
    maxDist = 0
    for e in graph[srcId].neighbours:
        if visited[e.n] == FALSE:
            dist = e.w + longestPath(e.n, graph, visited)
            if dist > maxDist:
                maxDist = dist
    visited[srcId] = FALSE            
    return maxDist   

def explore(src, graph, visited, stack):
    visited[src] = TRUE
    for e in graph[src].neighbours:
        if visited[e.n] == FALSE:
            explore(e.n, graph, visited, stack)
    stack.append(src)

def dfs(graph):
    visited = [0 for i in range(len(graph))]
    stack = []
    for node in graph:
        if visited[node.id] == FALSE:
            explore(node.id, graph, visited, stack)
    return stack

def topologicalSort(graph):
    seq = dfs(graph)    
    n = len(seq) 
    return [ seq[n - i - 1] for i in range(n) ]

# Guaranteed longest path for directed, acyclic graph.
def longestPathWithTopoSort(graph, numNodes):
    visited = [0 for i in range(numNodes)]
    sortedVertices = topologicalSort(graph)
    return longestPath( sortedVertices[0], graph, visited )

def readGraphFile(filePath, mode):
    numNodes = 0
    file = open(filePath, mode)
    line = file.readline().strip()
    numNodes = int(line)
    graph = [ Node(i, []) for i in range(numNodes)]    
    # Last line is an empty string.
    while line != ['']:
         line = file.readline().strip().split(" ")
         try:
            src = int(line[0])
            dest = int(line[1])
            weight = int(line[2])
            edge = Edge(weight, dest)
            graph[src].neighbours.append(edge)
         except ValueError:
            # Last line.
            break
    return [numNodes, graph]

def printGraph(graph):
    srcId = 0
    for node in graph:
        neighboursStr = str(node.id) + ": "
        for e in node.neighbours:
            neighboursStr += str((e.n, e.w)) + " "
        print neighboursStr
        
def __main__():
    file = "agraph"   
    result = readGraphFile(file, "r+")
    numNodes = result[0]
    graph = result[1]    
    start = time.time()    
    print "Longest Path with topologicalSort is " + str( longestPathWithTopoSort(graph, numNodes) ) + "."
    end = time.time()
    print  "Time taken = " + str( (int((end - start) * 100) )/100.0 )
    
__main__()    