#include <chrono>
#include <fstream>
#include <iostream>
#include <vector>

struct Route
{
    int dest;
    int cost;
};

using Node = std::vector<Route>;

std::vector<char> visited;
std::vector<Node> nodes;

static
int GetLongestPath(int index)
{
    int max = 0;
    visited[index] = true;

    for (auto neighbour : nodes[index])
    {
        if (!visited[neighbour.dest])
        {
            auto dist = neighbour.cost + GetLongestPath(neighbour.dest);
            max = std::max(max, dist);
        }
    }

    visited[index] = false;
    return max;
}


int main()
{
    std::ifstream in("agraph");

    int num_nodes;
    in >> num_nodes;
    nodes.resize(num_nodes);
    visited.resize(num_nodes);

    int index;
    int neighbour;
    int cost;
    while (in >> index >> neighbour >> cost)
    {
        nodes[index].push_back({neighbour, cost});
    }

    auto start = std::chrono::steady_clock::now();
    auto len = GetLongestPath(0);
    auto stop = std::chrono::steady_clock::now();
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    std::cout << len << " LANGUAGE C++plain/" << COMPILER << " " << ms.count() << "\n";
}
