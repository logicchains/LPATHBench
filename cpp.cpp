#include <ctime>
#include <vector>
#include <bitset>
#include <fstream>
#include <iostream>
#include <string>
#include <chrono>

using namespace std;
using namespace std::chrono;

struct route{
  int dest, cost;
};

struct node {
  vector<route> neighbours;
};

vector<node> readPlaces(){
  ifstream text("agraph");
  int numNodes; text >> numNodes;
  vector<node> nodes(numNodes);
  int node, neighbour, cost;
  while (text >> node >> neighbour >> cost){
    nodes[node].neighbours.push_back(route{neighbour, cost});
  }
  return nodes;
}

template <int T>
int getLongestPath(const vector<node> &nodes, const int nodeID, bitset<T> visited){
  visited[nodeID] = true;
  int max=0;
  for(const route &neighbour: nodes[nodeID].neighbours){
    if (visited[neighbour.dest] == false){
      const int dist = neighbour.cost + getLongestPath<T>(nodes, neighbour.dest, visited);
      if (dist > max){
        max = dist;
      }
    }
  }
  visited[nodeID] = false;
  return max;
}

int getLongestPath(const vector<node> &nodes)
{
  if (nodes.size() <= 16) {
     return getLongestPath<16>(nodes, 0, bitset<16>());
  } else if (nodes.size() <= 256) {
    return getLongestPath<256>(nodes, 0, bitset<256>());
  } else if (nodes.size() <= 4096) {
    return getLongestPath<4096>(nodes, 0, bitset<4096>());
  } else if (nodes.size() <= 65536) {
    return getLongestPath<65536>(nodes, 0, bitset<65536>());
  } else if (nodes.size() <= 1048576) {
    return getLongestPath<1048576>(nodes, 0, bitset<1048576>());
  } else if (nodes.size() <= 16777216) {
    return getLongestPath<16777216>(nodes, 0, bitset<16777216>());
  } else {
    return -1;
  }
}

int main(int argc, char** argv){
  auto nodes = readPlaces();
  auto start = high_resolution_clock::now();
  int len = getLongestPath(nodes);
  auto end = high_resolution_clock::now();
  auto duration = (int)(0.001 * duration_cast<microseconds>(end - start).count());
  cout << len << " LANGUAGE C++ " << duration << std::endl;
}
