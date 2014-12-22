import std.file;
import std.stdio;
import std.array;
import std.conv;
import std.string;
import std.datetime;
import std.exception;
import std.algorithm;

version = fast;

struct route{
  int dest, cost;
}

struct node {
  route[] neighbours;
}

node[] readPlaces(string text) pure {
  auto lines = splitLines(text);
  auto numNodes = to!int(lines.front);
  lines.popFront();
  node[] nodes =  minimallyInitializedArray!(node[])(numNodes);
  foreach(lineNum, string ln; lines){
    auto nums = ln.splitter(" ").array.map!(to!int);
    enforce(nums.length >= 3, "missing an edge on: " ~ (lineNum+1).to!string);
    auto node = nums[0];
    auto neighbour = nums[1];
    auto cost = nums[2];
    nodes[node].neighbours.insertInPlace(0, route(neighbour,cost));
  }
  return nodes;
}

int getLongestPath(immutable(node[]) nodes, const int nodeID, bool[] visited) nothrow @safe{
  visited[nodeID] = true;

  version(fast) {
    int identifiedMax=0;
    foreach(immutable route neighbour; nodes[nodeID].neighbours){
      if (!visited[neighbour.dest]){
        const int distance = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited);
        identifiedMax = max(distance, identifiedMax);
      }
    }
  } else {
    // Slight increase to runtime for LDC
    // Greater increase to runtime for DMD and GDC

    int dist(immutable route r) nothrow @safe{
      return r.cost + getLongestPath(nodes, r.dest, visited);
    }

    auto list = nodes[nodeID].neighbours.filter!(x=>!visited[x.dest]).map!dist;
    auto identifiedMax = reduce!(max)(0, list);
  }

  visited[nodeID] = false;
  return identifiedMax;
}


void main(){
  immutable nodes = readPlaces(readText("agraph"));
  auto visited = uninitializedArray!(bool[])(nodes.length);
  visited[] = false;

  StopWatch sw;
  sw.start;  
  int len = getLongestPath(nodes, 0, visited);
  sw.stop;
  printf("%d LANGUAGE D %d\n", len, sw.peek().msecs);
}
