import java.io.File;
import java.util.Arrays;
import java.util.Scanner;

public class jv {
	private static int numNodes = -1;
	private static final int[][] nodes;
	private static final byte[] visited;
	
	static {
		nodes = readPlaces();
		visited = new byte[numNodes];
	}
	
	public static void main(final String[] args) {
		// warm up a bit
		for (int i = 0; i < 10; i++) {
			getLongestPath(0, nodes[0], visited);
		}
		
		final long start = System.nanoTime();
		final int len = getLongestPath(0, nodes[0], visited);
		final long duration = (System.nanoTime() - start) / 1000000; // ns -> ms
		System.out.printf("%d LANGUAGE Java %d\n", len, duration);
	}
	
	/**
	 * int[node][cost|dest|...]
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
				replacement[index] = cost;
				replacement[index + 1] = neighbour;
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
	
	private static int getLongestPath(final int nodeID, final int[] row, final byte[] visited) {
		visited[nodeID] = 1;
		int max = 0;
		
		for (int i = 1; i < row.length; i += 2) {
			final int dest = row[i];
			if (visited[dest] == 0) {
				max = Math.max(row[i - 1] + getLongestPath(dest, nodes[dest], visited), max);
			}
		}
		
		visited[nodeID] = 0;
		return max;
	}
}
