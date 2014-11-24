#include <vector>
#include <fstream>
#include <iostream>
#include <string>

using namespace std;

struct route{
  int dest, cost;
};

struct node {
  vector<route> neighbours;
};

vector<node> readPlaces(){
  ifstream text("agraph");
  string numNodesText;
  text >> numNodesText;
  int numNodes = stoi(numNodesText);
  vector<node> nodes(numNodes);
  string nodeS, neighbourS, costS;
  while (text >> nodeS >> neighbourS >> costS){
    nodes[stoi(nodeS)].neighbours.push_back(route{stoi(neighbourS), stoi(costS)});
  }
  return nodes;
}

int getLongestPath(vector<node> &nodes, int nodeID, vector<bool> &visited){
  visited[nodeID] = true;
  int max=0;
  for(route neighbour: nodes[nodeID].neighbours){
    if (visited[neighbour.dest] == false){
      int dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited);
      if (dist > max){
	max = dist;
      }
    }
  }
  visited[nodeID] = false;
  return max;
}

int main(int argc, char** argv){
  auto nodes = readPlaces();
  vector<bool> visited(nodes.size(), false);
  cout << getLongestPath(nodes, 0, visited);
}
