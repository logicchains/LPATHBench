import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Arrays;

public class ojv{

	static int numNodes = -1;
	static final int[][] nodes;
	static final boolean[] visited;

	static {
		nodes = readPlaces();
		visited = new boolean[numNodes];
	}

	public static void main(final String[] args) throws Exception{
		final long start = System.currentTimeMillis();
		final int len = getLongestPath(0);
		final long duration = System.currentTimeMillis() - start;
		System.out.printf("%d LANGUAGE OracleJava %d\n", len, duration);
	}

	/**
	 * int[node][dest|cost|...]
	 */
	static int[][] readPlaces() {
		try (BufferedReader in = new BufferedReader(new FileReader("agraph"))) {
			final String s = in.readLine();
			numNodes = Integer.parseInt(s);
			final int[][] nodes = new int[numNodes][];
			for(int i = 0; i < numNodes; i++){
				nodes[i]= new int[0];
			}
			while (in.ready()) {
				final String ln = in.readLine();
				final String[] nums = ln.split("[ \t]+");
				if(nums.length != 3){
					break;
				}
				final int node = Integer.parseInt(nums[0]);
				final int neighbour = Integer.parseInt(nums[1]);
				final int cost = Integer.parseInt(nums[2]);

				final int index = nodes[node].length;
				final int[] replacement = Arrays.copyOf(nodes[node], index + 2);
				replacement[index] = neighbour;
				replacement[index+1] = cost;

				nodes[node] = replacement;
			}
			return nodes;
		} catch (Exception e) {
			return null;
		}
	}

	static int getLongestPath(final int nodeID){
		visited[nodeID] = true;
		int dist, max=0;

		final int length = nodes[nodeID].length;

		for (int i = 0; i < length; i+=2) {

			final int dest = nodes[nodeID][i];

			if (!visited[dest]) {
				dist = nodes[nodeID][i + 1] + getLongestPath(dest);
				if (dist > max) {
					max = dist;
				}
			}
		}

		visited[nodeID] = false;
		return max;
	}

}
