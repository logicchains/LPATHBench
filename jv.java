import java.util.ArrayList;
import java.io.BufferedReader;
import java.io.FileReader;

class refInt{
    public int val;
}

class longestPathFinder{
   ArrayList<node> readPlaces(refInt numNodes) throws Exception{
	BufferedReader in = new BufferedReader(new FileReader("agraph"));
	String s = in.readLine();
	numNodes.val = Integer.parseInt(s);
	ArrayList<node> nodes = new ArrayList<>(numNodes.val);
	for(int i = 0; i < numNodes.val; i++){
	    node n = new node();
	    n.neighbours = new ArrayList<>();
	    nodes.add(n);
	}
	while (in.ready()) {
	    String ln = in.readLine();
	    String[] nums = ln.split("[ \t]+");
	    if(nums.length != 3){
		break;
	    }
	    int node = Integer.parseInt(nums[0]);
	    int neighbour = Integer.parseInt(nums[1]);
	    int cost = Integer.parseInt(nums[2]);
	    nodes.get(node).neighbours.add(new route(neighbour, cost));
	}
	in.close();	
	return nodes;
    }

    int getLongestPath(ArrayList<node> nodes, int nodeID, boolean[] visited){
	visited[nodeID] = true;
	int dist, max=0;
	for(route neighbour: nodes.get(nodeID).neighbours){
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
    
}

class route{
	int dest, cost;
	route(int dest, int cost){
	    this.dest =dest;
	    this.cost =cost;
	}
    }

class node {
    ArrayList<route> neighbours;
}

public class jv{
    public static void main(String[] args) throws Exception{
	refInt numNodes = new refInt();
	longestPathFinder p = new longestPathFinder();
        ArrayList<node> nodes = p.readPlaces(numNodes);
	boolean[] visited = new boolean[numNodes.val];
	for(boolean b: visited){
	    b = false;
	}
	System.out.println(p.getLongestPath(nodes, 0, visited)); 
    }
}
