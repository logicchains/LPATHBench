import 'dart:io';

class Route {
  final int dest, cost;

  Route(this.dest, this.cost);

  String toString() => "$dest $cost\n";
}

class Node{
  final List<Route> neighbours = new List<Route>();

  Node();
}

void readPlacesAndFindPath() {
  List<String> lines = new File('agraph').readAsLinesSync();
  int numNodes = int.parse(lines[0]);
  List nodes = new List<Node>.generate(numNodes, (int index) => new Node());

  for(int i = 1; i < lines.length; i++){
    List nums = lines[i].split(' ');
    int node = int.parse(nums[0]);
    int neighbour = int.parse(nums[1]);
    int cost = int.parse(nums[2]);

    nodes[node].neighbours.add(new Route(neighbour,cost));
  }

  var visited = new List<bool>.generate(numNodes, (int index) => false);
  var start = new DateTime.now();
  int len = getLongestPath(nodes, 0, visited);
  var duration = new DateTime.now().difference(start);

  print("$len LANGUAGE Dart ${duration.inMilliseconds}");
}

int getLongestPath(List<Node> nodes, int nodeID, List<bool> visited){
  visited[nodeID] = true;
  int max = 0;

  for(var i = 0; i < nodes[nodeID].neighbours.length; i++) {
    if (!visited[nodes[nodeID].neighbours[i].dest]) {
      final int dist = nodes[nodeID].neighbours[i].cost + getLongestPath(nodes, nodes[nodeID].neighbours[i].dest, visited);
      if (dist > max){
        max = dist;
      }
    }
  }

  visited[nodeID] = false;

  return max;
}

void main() {
  readPlacesAndFindPath();
}

