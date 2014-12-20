#define _POSIX_C_SOURCE 200809L
#include <ctime>
#include <vector>
#include <fstream>
#include <iostream>
#include <string>

double getTime(){
  timespec spec;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &spec);
  double s = spec.tv_sec;
  double ms = spec.tv_nsec;
  return (s*1000 + ms / 1000000);
}

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

int getLongestPath(const vector<node> &nodes, const int nodeID, vector<bool> &visited){
  visited[nodeID] = true;
  int max=0;
  for(const route& neighbour: nodes[nodeID].neighbours){
    if (visited[neighbour.dest] == false){
      const int dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited);
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
  double start = getTime();
  int len = getLongestPath(nodes, 0, visited);
  double duration = getTime() - start;
  cout << len << " LANGUAGE C++ " << (int)duration << "\n";
}
