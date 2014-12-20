using System;
using System.Collections.Generic;

class longestPathFinder
{
	public node[] ReadPlaces(string path)
	{
		string[] lines = System.IO.File.ReadAllLines(path);
		int numNodes = Int32.Parse(lines[0]);

		node[] nodes = new node[numNodes];
		List<route>[] routes = new List<route>[numNodes];
		for (int i = 0; i < numNodes; i++)
		{
			nodes[i] = new node();
			routes[i] = new List<route>();
		}

		for (int i = 1; i < lines.Length; i++)
		{
			string[] nums = lines[i].Split(' ');
			if (nums.Length < 3)
			{
				break;
			}
			int node = Int32.Parse(nums[0]);
			int neighbour = Int32.Parse(nums[1]);
			int cost = Int32.Parse(nums[2]);
			routes[node].Add(new route(neighbour, cost));
		}

		for (int i = 0; i < routes.Length; i++)
		{
			nodes[i].neighbours = routes[i].ToArray();
		}

		return nodes;
	}

	public unsafe int getLongestPath(node[] nodes, int nodeIndex, bool[] visited)
	{
		fixed (bool* pVisited = visited)
		{
			return getLongestPath(nodes, 0, pVisited);
		}
	}

	[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
	private unsafe int getLongestPath(node[] nodes, int nodeIndex, bool* visited)
	{
		int max = 0;
		visited[nodeIndex] = true;

		route[] neighbours = nodes[nodeIndex].neighbours;
		fixed (route* pNeighbours = neighbours)
		{
			route* pLength = pNeighbours + neighbours.Length;
			for (route* neighbour = pNeighbours; neighbour < pLength; neighbour++)
			{
				if (!visited[neighbour->dest])
				{
					int dist = neighbour->cost + getLongestPath(nodes, neighbour->dest, visited);
					if (dist > max)
					{
						max = dist;
					}
				}
			}
		}

		visited[nodeIndex] = false;
		return max;
	}
}

struct route
{
	public int dest, cost;
	public route(int dest, int cost)
	{
		this.dest = dest;
		this.cost = cost;
	}
}

struct node
{
	public route[] neighbours;
}

class cs
{
	static void Main(string[] args)
	{
		longestPathFinder p = new longestPathFinder();
		node[] nodes = p.ReadPlaces("agraph");
		bool[] visited = new bool[nodes.Length];

		System.Diagnostics.Stopwatch timer = new System.Diagnostics.Stopwatch();
		timer.Start();

		int result = p.getLongestPath(nodes, 0, visited);

		timer.Stop();
		Console.WriteLine("{0} LANGUAGE CSharp {1} ms", result, Math.Round(timer.Elapsed.TotalMilliseconds));
	}
}