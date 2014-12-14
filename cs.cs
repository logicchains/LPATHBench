using System.Collections.Generic;
using System.Text.RegularExpressions;
using System;

class longestPathFinder{
   public List<node> readPlaces(out int numNodes) {
	string[] lines = System.IO.File.ReadAllLines("agraph");
	numNodes = System.Convert.ToInt32(lines[0]);
	List<node> nodes = new List<node>(numNodes);
	for(int i = 0; i < numNodes; i++){
	    node n = new node();
	    n.neighbours = new List<route>();
	    nodes.Add(n);
	}
	for(int i = 1; i < lines.Length; i++) {
	    string[] nums = lines[i].Split(' ');
	    if(nums.Length < 3){
		break;
	    }
	    int node = System.Convert.ToInt32(nums[0]);
	    int neighbour = System.Convert.ToInt32(nums[1]);
	    int cost = System.Convert.ToInt32(nums[2]);
	    nodes[node].neighbours.Add(new route(neighbour, cost));
	}
	return nodes;
    }

    public int getLongestPath(List<node> nodes, int nodeID, bool[] visited){
	visited[nodeID] = true;
	int max=0;
	foreach(route neighbour in nodes[nodeID].neighbours){
	    if (!visited[neighbour.dest]){
		int dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited);
		if (dist > max){
		    max = dist;
		}
	    }    
	}
	visited[nodeID] = false;
	return max;
    }
    
}

class route{
	public int dest, cost;
	public route(int dest, int cost){
	    this.dest =dest;
	    this.cost =cost;
	}
    }

class node {
    public List<route> neighbours;
}

public class cs{
    public static void Main(string[] args){
	int numNodes = 0;
	longestPathFinder p = new longestPathFinder();
        List<node> nodes = p.readPlaces(out numNodes);
	bool[] visited = new bool[numNodes];
	for(int i = 0; i < numNodes; i++){
	    visited[i] = false;
	}
	long start= DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
	int result = p.getLongestPath(nodes, 0, visited);
	long duration = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond - start;
	System.Console.WriteLine(result + " LANGUAGE CSharp "+ duration);
    }
}
