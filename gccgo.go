package main

import(
	"io/ioutil"
	"strconv"
	"strings"
	"time"
	"fmt"
)

type route struct{
	to int32
	cost int32
}

type node struct {
	neighbours []route
}

func readPlaces()([]node, int){
	bytes, err := ioutil.ReadFile("agraph")
	if err != nil {
		panic(err)
	}
	lines := strings.Split(string(bytes),"\n")
	numNodes, err := strconv.Atoi(lines[0])
	if err != nil{
		panic(err)
	}
	lines = lines[1:]
	nodes := make([]node, numNodes)
	for i := range nodes{
		nodes[i].neighbours = make([]route, 0, numNodes/2)
	}
	for _,ln := range lines{
		nums := strings.Split(ln," ")
		if len(nums) < 3{
			break
		}
		node, err1 := strconv.Atoi(nums[0])
		neighbour, err2 := strconv.Atoi(nums[1])
		cost, err3 := strconv.Atoi(nums[2])
		if err1 != nil || err2 != nil || err3 != nil{
			panic("Error: encountered a line that wasn't three integers")
		}
		nodes[node].neighbours = append(nodes[node].neighbours, route{to:int32(neighbour), cost:int32(cost)})
	}
        return nodes, numNodes
}

func getLongestPath(nodes []node, nodeID int32, visited []bool) int32{
	visited[nodeID] = true
	var max int32
	for _, neighbour := range nodes[nodeID].neighbours{
		if !visited[neighbour.to]{
			dist := neighbour.cost + getLongestPath(nodes, neighbour.to, visited)
			if dist > max{
				max = dist
			}
		}
		
	}
	visited[nodeID] = false
	return max
}

func main(){
	nodes, nNodes := readPlaces()
	visited := make([]bool, nNodes)
	start := time.Now()
	len := getLongestPath(nodes, 0, visited)
	duration := time.Now().Sub(start).Nanoseconds() / 1000000
	fmt.Printf("%v LANGUAGE GCCGo %v\n", len, duration)

}
