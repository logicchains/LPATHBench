import 'dart:io';
import 'dart:math' as math;

class Route {
  final int dest, cost;
  Route(this.dest, this.cost);

  String toString() => "$dest $cost\n";
}

class Node {
  final neighbours = <Route>[];
}

void readPlacesAndFindPath() {
  var lines = new File('agraph').readAsLinesSync();
  var numNodes = int.parse(lines[0]);
  var nodes = new List.generate(numNodes, (_) => new Node());

  for (var i = 1; i < lines.length; i++) {
    var nums = lines[i].split(' ');
    var node = int.parse(nums[0]);
    var neighbour = int.parse(nums[1]);
    var cost = int.parse(nums[2]);

    nodes[node].neighbours.add(new Route(neighbour, cost));
  }

  var visited = new List<bool>.generate(numNodes, (int index) => false);
  var start = new DateTime.now();
  var len = getLongestPath(nodes, 0, visited);
  var duration = new DateTime.now().difference(start);

  print("$len LANGUAGE Dart ${duration.inMilliseconds}");
}

int getLongestPath(List<Node> nodes, int nodeID, List<bool> visited) {
  visited[nodeID] = true;
  var max = 0;
  var neighbors = nodes[nodeID].neighbours;
  var neighborsLength = neighbors.length;
  for (var i = 0; i < neighborsLength; i++) {
    var neighbor = neighbors[i];
    var dest = neighbor.dest;
    if (!visited[dest]) {
      var dist = neighbor.cost + getLongestPath(nodes, dest, visited);
      max = math.max(dist, max);
    }
  }

  visited[nodeID] = false;

  return max;
}

void main() {
  readPlacesAndFindPath();
}
