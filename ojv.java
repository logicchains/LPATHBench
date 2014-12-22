import java.io.File;
import java.util.Arrays;
import java.util.Scanner;

public class ojv {
	private static int numNodes = -1;
	private static final int[][] nodes;
	private static final boolean[] visited;
	
	static {
		nodes = readPlaces();
		visited = new boolean[numNodes];
	}
	
	public static void main(final String[] args) {
		// warm up a bit
		for (int i = 0; i < 10; i++) {
			getLongestPath(0, nodes[0], visited);
		}
		
		final long start = System.nanoTime();
		final int len = getLongestPath(0, nodes[0], visited);
		final long duration = (System.nanoTime() - start) / 1000000; // ns -> ms
		System.out.printf("%d LANGUAGE OracleJava %d\n", len, duration);
	}
	
	/**
	 * int[node][dest|cost|...]
	 */
	private static int[][] readPlaces() {
		try (Scanner in = new Scanner(new File("agraph"))) {
			numNodes = in.nextInt();
			int[][] nodes = new int[numNodes][];
			for (int i = 0; i < numNodes; i++) {
				nodes[i] = new int[0];
			}
			while (in.hasNextInt()) {
				int node = in.nextInt();
				int neighbour = in.nextInt();
				int cost = in.nextInt();
				int index = nodes[node].length;
				int[] replacement = Arrays.copyOf(nodes[node], index + 2);
				replacement[index] = neighbour;
				replacement[index + 1] = cost;
				nodes[node] = replacement;
			}
			// reduce any fragmentation
			int[][] newNodes = new int[numNodes][];
			for (int i = 0; i < numNodes; i++) {
				newNodes[i] = Arrays.copyOf(nodes[i], nodes[i].length);
			}
			return newNodes;
		} catch (Exception e) {
			return null;
		}
	}
	
	private static int getLongestPath(int nodeID, int[] row, boolean[] visited) {
		visited[nodeID] = true;
		int max = 0;
		
		for (int i = 0; i < row.length; i += 2) {
			final int dest = row[i];
			if (!visited[dest]) {
				final int dist = row[i + 1] + getLongestPath(dest, nodes[dest], visited);
				if (dist > max) {
					max = dist;
				}
			}
		}
		
		visited[nodeID] = false;
		return max;
	}
}
