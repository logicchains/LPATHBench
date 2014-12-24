<?php

$nodes = $visited = $visitCache = [];
function readPlaces()
{
    global $nodes, $visited;
    $lines = [];
    $fp = fopen('agraph', 'rb');
    $nodeCount = trim(fgets($fp));
    while (false !== ($line = fgets($fp))) {
        $lines[] = str_getcsv(trim($line), ' ');
    }
    $nodes = array_fill(0, $nodeCount, []);
    $visited = array_fill(0, $nodeCount, false);

    foreach($lines as $line) {
        $nodes[(int)$line[0]][(int)$line[1]] = (int)$line[2];
    }
}

function getLongestPath($nodeId)
{
    global $nodes, $visited;
    $visited[$nodeId] = true;
    $neighbors = $nodes[$nodeId];
    $max = 0;
    foreach($neighbors as $neighborId => $neighborCost) {
        if (!$visited[$neighborId]) {
            $dist = $neighborCost + getLongestPath($neighborId, $nodeId);
            if ($dist > $max) {
                $max = $dist;
            }
        }
    }

    $visited[$nodeId] = false;
    return $max;
}

readPlaces();

$start = microtime(true);

$length = getLongestPath(0);

echo $length . ' LANGUAGE PHP ' . round((microtime(true) - $start) * 1000) . "\n";