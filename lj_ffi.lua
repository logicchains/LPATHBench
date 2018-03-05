local ffi = require "ffi"

ffi.cdef [[
  struct path {
    unsigned int cost;
    unsigned int dst;
  };

  struct path_list {
    unsigned int count;
    struct path paths[?];
  };
]]

local function readPlaces()
  local nodesDst, nodesCost = {}, {}
  local firstline=true
  for line in io.lines("agraph") do
    if firstline then
      local numnodes = tonumber(line)
      for i = 1, numnodes do
        nodesDst[i], nodesCost[i] = {}, {}
      end
      firstline = false
    else
      local node, dest, cost = line:match("^(%d+) (%d+) (%d+) $")
      table.insert(nodesDst[tonumber(node) + 1], tonumber(dest))
      table.insert(nodesCost[tonumber(node) + 1], tonumber(cost))
    end
  end
  return nodesDst, nodesCost
end

local function getLongestPath(nodes, visited, nodeid)
  visited[nodeid] = true
  local max = 0
  for i=0,nodes[nodeid].count-1 do
    if not visited[nodes[nodeid].paths[i].dst] then
      local dist = nodes[nodeid].paths[i].cost + getLongestPath(nodes, visited, nodes[nodeid].paths[i].dst)
      if dist > max then
        max = dist
      end
    end
  end
  visited[nodeid] = false
  return max
end

local nodesDst, nodesCost = readPlaces()

local node_count = #nodesDst
local nodes = ffi.new("struct path_list *[?]", node_count)

-- Use an lua table to store path_lists to avoid GC.
local nodes_ = {}

for i=1,node_count do
  local dst, cost = nodesDst[i], nodesCost[i]
  local count = #dst

  local list = ffi.new("struct path_list", count)
  list.count = count

  for j=1,count do
    local path = list.paths[j-1]
    path.dst = dst[j]
    path.cost = cost[j]
  end

  nodes[i-1] = list
  nodes_[i] = list
end

local visited = ffi.new("bool [?]", node_count)
for i=0,node_count-1 do
  visited[i] = false
end

local start = os.clock()
local length = getLongestPath(nodes, visited, 0)
local duration = os.clock() - start

io.stdout:write(length, " LANGUAGE LuaJit ", math.floor(duration*1000), "\n")
