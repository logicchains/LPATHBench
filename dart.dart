import 'dart:io';

class Route {
  final int dest, cost;
  Route(this.dest, this.cost);

  function toString(){
    print("$dest, $cost\n");
  }
}

class Node{
  List<Route> neighbours;
  Node(){
    this.neighbours = new List<Route>();
  }
}

readPlacesAndFindPath() {
  var nodes;
  new File('agraph').readAsLines().then((List<String> lines) {
    final int numNodes = int.parse(lines[0]);
    final nodes = new List<Node>.generate(numNodes, (int index) => new Node()); 
    for(int i = 1; i < lines.length; i++){
      final nums = lines[i].split(' ');
      int node = int.parse(nums[0]);
      int neighbour = int.parse(nums[1]);      
      int cost = int.parse(nums[2]);
      nodes[node].neighbours.add(new Route(neighbour,cost));
    }
    var visited = new List<Bool>.generate(numNodes, (int index) => false);
    print(getLongestPath(nodes, 0, visited));
  });
  return nodes;
}

getLongestPath(List<Node> nodes, int nodeID, List<Bool> visited){
  visited[nodeID] = true;
  int max=0;
  nodes[nodeID].neighbours.forEach((Route neighbour) {
    if (!visited[neighbour.dest]){
      final int dist = neighbour.cost + getLongestPath(nodes, neighbour.dest, visited);
      if (dist > max){
	max = dist;
      }
    }
  });    
  visited[nodeID] = false;
  return max;
}



main() {
  readPlacesAndFindPath();
}