record Route, dest, cost

struct Node
  getter neighbours

  def initialize
    @neighbours = [] of Route
  end
end

def read_places
  lines = File.read_lines("agraph")
  num_nodes = lines.shift.to_i
  nodes = Array.new(num_nodes) { Node.new }

  lines.each do |line|
    nums = line.split.map &.to_i
    break if nums.length < 3

    node, neighbour, cost = nums
    nodes[node].neighbours << Route.new(neighbour, cost)
  end

  nodes
end

def get_longest_path(nodes, node_id, visited)
  max = 0

  visited[node_id] = true
  nodes[node_id].neighbours.each do |neighbour|
    unless visited[neighbour.dest]
      dist = neighbour.cost + get_longest_path(nodes, neighbour.dest, visited)
      max = dist if dist > max
    end
  end
  visited[node_id] = false

  max
end

nodes = read_places
visited = Array.new(nodes.length, false)

time = Time.now
len = get_longest_path nodes, 0, visited
duration = Time.now - time

STDOUT << len << " LANGUAGE CRYSTAL " << duration.total_milliseconds.to_i << '\n'
