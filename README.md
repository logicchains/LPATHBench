LPATHBench
==========

Benchmarks of the longest path problem in various languages

`sh runbench.sh 8981 x86 x86html` to run the benchmark locally.
`sh runArmBench.sh 8981` to run the benchmark on an ARM device (edit the script to set the ssh and path settings)
`sh resdiff.sh x86 arm > diffgraph.html` to create the diff table
`python makeblog.py` to make the blog

Where 8981 is the distance of the longest path in the graph

If you want to make a new graph:

/mkgraph -places=NUM_NODES -worldsize=WORLD_SIZE, where NUM_NODES is the number of nodes in the graph, and WORLD_SIZE is the maximum distance between nodes. Each node has at least one connection, to the next node, and will on average have NUM_NODES/2 connections. Graphs are directed; a path of length N from Node1 to Node2 doesn't imply a similar path from Node2 to Node1.
