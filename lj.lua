-- String split function thanks to Bart Kiers on Stack overflow; http://stackoverflow.com/a/11204889/2553416
local function splitstr(str, sep)
        if sep == nil then
                sep = "%s"
        end
        local t={} ; i=1
        for str in string.gmatch(str, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
end

local function readPlaces()
  local nodes = {}
  local firstline=true
  for line in io.lines("agraph") do
    if firstline then
      numnodes = tonumber(line)
      for i = 1, numnodes do
        nodes[i] = {}
      end
      firstline = false
    else
      nums = splitstr(line)
      node = tonumber(nums[1]) + 1
      dest = tonumber(nums[2]) + 1
      cost = tonumber(nums[3])
      table.insert(nodes[node], {dest, cost})
    end
  end
  return nodes
end

local function getLongestPath(nodes, nodeid, visited)
  visited[nodeid] = true
  local max = 0
  for _,neighbour in ipairs(nodes[nodeid]) do
    if not visited[neighbour[1]] then
      local dist = neighbour[2] + getLongestPath(nodes, neighbour[1], visited)
      if dist > max then
        max = dist
      end
    end
  end
  visited[nodeid] = false
  return max
end

local nodes = readPlaces()
local visited = {}
for i=1, #nodes do
  visited[i] = false
end
local start = os.clock()
local length = getLongestPath(nodes, 1, visited)
local duration = os.clock() - start
io.stdout:write(length .. " LANGUAGE LuaJit " .. math.floor(duration*1000) .. "\n")

