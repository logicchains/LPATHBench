#!/usr/bin/env ruby

def get_nodes path
  lines = File.readlines path
  count = lines.shift
  nodes = Hash.new {|h, k| h[k] = {}}
  
  lines.each do |line|
    id, neighbour, cost = line.split(' ').map &:to_i
    nodes[id][neighbour] = cost
  end
  
  nodes
end

def get_longest_path nodes, id, visited
  visited[id] = true
  neighbours = nodes[id]
  max = 0
  
  neighbours.each do |neighbour, cost|
    if !visited[neighbour]
      dist = cost + get_longest_path(nodes, neighbour, visited)
      max = dist if dist > max
    end
  end
  
  visited[id] = false
  max
end
 
nodes = get_nodes 'agraph'
t = Time.now
n = get_longest_path nodes, 0, {}
e = Time.now
puts "#{n} LANGUAGE Ruby #{(e - t)*1000}"
