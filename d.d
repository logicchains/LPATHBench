import std.file;
import std.stdio;
import std.array;
import std.conv;
import std.string;
import std.typecons;

struct route{
  int dest, cost;
}

struct node {
  route[] neighbours;
}

node[] readPlaces(ref int numNodes){
  auto bytes = cast(byte[]) read("agraph");
  auto text = cast(string) bytes;
  auto lines = splitLines(text);
  numNodes = to!int(lines[0]);
  lines = lines[1..$];
  node[] nodes = uninitializedArray!(node[])(numNodes);
  foreach(ref node n; nodes){
    n.neighbours = uninitializedArray!(route[])(0);
  }
  foreach(string ln; lines){
    auto nums = ln.split(" ");
    if(nums.length < 3){
      break;
    }
    auto node = to!int(nums[0]);
    auto neighbour = to!int(nums[1]);
    auto cost = to!int(nums[2]);
    nodes[node].neighbours.insertInPlace(0, route(neighbour,cost));
  }
  return nodes;
}

int getLongestPath(node[] nodes, int nodeID, bool[] visited){
  visited[nodeID] = true;
  int dist, max=0;
  foreach(route neighbour; nodes[nodeID].neighbours){
    if (!visited[neighbour.dest]){
      dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited);
      if (dist > max){
	max = dist;
      }
    }    
  }
  visited[nodeID] = false;
  return max;
}


void main(){
  int numNodes = 0;
  auto nodes = readPlaces(numNodes);
  auto visited = uninitializedArray!(bool[])(numNodes);
  foreach(ref bool b; visited){
    b = false;
  }
  getLongestPath(nodes, 0, visited).writeln; 
}
