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
  int nBegin = 0;
  int nCount = 0;
};

std::pair<vector<node>, vector<route>> readPlaces(){
  ifstream text("agraph");
  int numNodes; text >> numNodes;
  vector<node> nodes(numNodes);
  // first pass: count neighbours per-node and total
  int node, neighbour, cost;
  int totalNeighbours = 0;
  while (text >> node >> neighbour >> cost){
    nodes[node].nCount += 1;
    totalNeighbours += 1;
  }
  // second pass: compute each node's offset in allNeighbours
  std::vector<route> allNeighbours(totalNeighbours);
  int cumulative = 0;
  for (auto &node : nodes) {
    node.nBegin = cumulative;
    cumulative += node.nCount;
    node.nCount = 0;
  }
  // fill in neighbours arrays
  text = ifstream("agraph");
  text >> numNodes;
  while (text >> node >> neighbour >> cost){
    auto &n = nodes[node];
    allNeighbours[n.nBegin + n.nCount] = { neighbour, cost };
    ++n.nCount;
  }
  return std::make_pair(std::move(nodes), std::move(allNeighbours));
}

template <int T>
int getLongestPath(const vector<node> &nodes, const vector<route> &neighbours, const int nodeID, bitset<T> visited){
  visited[nodeID] = true;
  int longest = 0;
  auto const &n = nodes[nodeID];
  for (int i = 0; i < n.nCount; ++i) {
    const route &neighbour = neighbours[n.nBegin + i];
    if (visited[neighbour.dest] == false){
      const int dist = neighbour.cost + getLongestPath<T>(nodes, neighbours, neighbour.dest, visited);
      longest = max(longest, dist);
    }
  }
  visited[nodeID] = false;
  return longest;
}

int getLongestPath(const vector<node> &nodes, const vector<route> &neighbours)
{
  if (nodes.size() <= 16) {
     return getLongestPath<16>(nodes, neighbours, 0, bitset<16>());
  } else if (nodes.size() <= 256) {
    return getLongestPath<256>(nodes, neighbours, 0, bitset<256>());
  } else if (nodes.size() <= 4096) {
    return getLongestPath<4096>(nodes, neighbours, 0, bitset<4096>());
  } else if (nodes.size() <= 65536) {
    return getLongestPath<65536>(nodes, neighbours, 0, bitset<65536>());
  } else if (nodes.size() <= 1048576) {
    return getLongestPath<1048576>(nodes, neighbours, 0, bitset<1048576>());
  } else if (nodes.size() <= 16777216) {
    return getLongestPath<16777216>(nodes, neighbours, 0, bitset<16777216>());
  } else {
    return -1;
  }
}

int main(int argc, char** argv){
  auto data = readPlaces();
  auto start = high_resolution_clock::now();
  int len = getLongestPath(data.first, data.second);
  auto end = high_resolution_clock::now();
  auto duration = (int)(0.001 * duration_cast<microseconds>(end - start).count());
  cout << len << " LANGUAGE C++ " << duration << std::endl;
}
