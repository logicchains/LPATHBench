import 'dart:io';
import 'dart:math' as Math;

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
  final List<Route> neighbors = nodes[nodeID].neighbours;
  for (var i = 0, llen = neighbors.length; i < llen; i++) {
    final Route neighbor = neighbors[i];
    final int dest = neighbor.dest;
    if (!visited[dest]) {
      final int dist = neighbor.cost +
          getLongestPath(nodes, dest, visited);
      max = Math.max(dist, max);
    }
  }

  visited[nodeID] = false;

  return max;
}

void main() {
  readPlacesAndFindPath();
}

