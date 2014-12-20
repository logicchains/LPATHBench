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
      table.insert(nodesDst[tonumber(node) + 1], tonumber(dest) + 1)
      table.insert(nodesCost[tonumber(node) + 1], tonumber(cost))
    end
  end
  return nodesDst, nodesCost
end

local function getLongestPath(nodesDst, nodesCost, nodeid, visited)
  visited[nodeid] = true
  local neighboursDst, neighboursCost = nodesDst[nodeid], nodesCost[nodeid]
  local max = 0
  for i = 1, #neighboursDst do
    if not visited[neighboursDst[i]] then
      local dist = neighboursCost[i] + getLongestPath(nodesDst, nodesCost, neighboursDst[i], visited)
      if dist > max then
        max = dist
      end
    end
  end
  visited[nodeid] = false
  return max
end

local nodesDst, nodesCost = readPlaces()
local visited = {}
for i=1, #nodesDst do
  visited[i] = false
end
local start = os.clock()
local length = getLongestPath(nodesDst, nodesCost, 1, visited)
local duration = os.clock() - start
io.stdout:write(length, " LANGUAGE LuaJit ", math.floor(duration*1000), "\n")
