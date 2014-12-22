var fs = require('fs');

  var nodes = [], visited = [];
  var visitCache = {};

  function readPlaces() {
    var lines = fs.readFileSync('agraph','utf8').split('\n');
    var numnodes = lines[0]|0;
    for(var i=0;i<numnodes;i++) {
      nodes[i] = [];
      visited[i] = false;
    }
    for(i=1;i<lines.length;i++) {
      var line = lines[i].split(' ');
      nodes[line[0]|0].push({dst: line[1]|0, cost: line[2]|0});
    }
  }

  function getLongestPath(nodes, nodeid, visited) {
    visited[nodeid] = true;
    var neighbours = nodes[nodeid];
    var max = 0
    for(var i=0;i<neighbours.length;i++) {
      if (!visited[neighbours[i].dst]) {
        var dist = neighbours[i].cost + getLongestPath(nodes, neighbours[i].dst, visited);
        if (dist > max) {
          max = dist
        }
      }
    }
    visited[nodeid] = false;
    return max;
  }

  function getLongestPathCached(nodes, nodeid, visited, depth) {
    var idx;
    visited |= 1 << nodeid;
    if (depth == nodes.length - 7) {
      idx = (visited * 32) + nodeid;
      if (visitCache[idx]) {
        return visitCache[idx];
      }
    }
    var neighbours = nodes[nodeid];
    var max = 0
    for(var i=0;i<neighbours.length;i++) {
      if (!(visited & (1 << neighbours[i].dst))) {
        var dist = neighbours[i].cost + getLongestPathCached(nodes, neighbours[i].dst, visited, depth+1);
        if (dist > max) {
          max = dist
        }
      }
    }
    if (idx) {
      visitCache[idx] = max;
    }
    if (typeof visited != 'number') {
      visited[nodeid] = false;
    }
    return max;
  }

  readPlaces();
  var start = Date.now();
  var length;
  if (nodes.length < 32) {
    length = getLongestPathCached(nodes, 0, 0, 0);
  } else {
    length = getLongestPath(nodes, 0, visited);
  }
  console.log(length+' LANGUAGE JavascriptWithCacheAlg: '+(Date.now() - start));
