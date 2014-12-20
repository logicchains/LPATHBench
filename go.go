package main

import (
	"fmt"
	"io"
	"os"
	"time"
)

type route struct {
	to   int32
	cost int32
}

type node struct {
	neighbours []route
}

func readPlaces() ([]node, int) {
	f, err := os.Open("agraph")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	var numNodes int
	if _, err := fmt.Fscanln(f, &numNodes); err != nil {
		panic(err)
	}

	nodes := make([]node, numNodes)
	for i := range nodes {
		nodes[i].neighbours = make([]route, 0, numNodes/2)
	}

	for {
		var node int32
		var r route

		switch _, err := fmt.Fscanln(f, &node, &r.to, &r.cost); {
		case err == io.EOF:
			return nodes, numNodes
		case err != nil:
			panic(err)
		}
		nodes[node].neighbours = append(nodes[node].neighbours, r)
	}
}

func getLongestPath(nodes []node, nodeID int32, visited []bool) int32 {
	visited[nodeID] = true
	var max int32
	for _, neighbour := range nodes[nodeID].neighbours {
		if !visited[neighbour.to] {
			dist := neighbour.cost + getLongestPath(nodes, neighbour.to, visited)
			if dist > max {
				max = dist
			}
		}

	}
	visited[nodeID] = false
	return max
}

func main() {
	nodes, nNodes := readPlaces()
	visited := make([]bool, nNodes)
	start := time.Now()
	len := getLongestPath(nodes, 0, visited)
	duration := time.Now().Sub(start).Nanoseconds() / 1000000
	fmt.Printf("%v LANGUAGE Go %v\n", len, duration)

}
