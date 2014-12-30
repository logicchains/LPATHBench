/*
   Copyright (c) 2014, Cosmin Gorgovan <cosmin [at] linux-geek [dot] org>
   All rights reserved.

   Redistribution and use in source and binary forms, with or without modification,
   are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this
   list of conditions and the following disclaimer in the documentation and/or other
   materials provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
   ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
   OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
   IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   *******************
   
   See https://github.com/logicchains/LPATHBench

   The edges are stored as an array of pointers to arrays of edges, i.e: the edges
   from node 0 are in the array pointed to by costs[0]. This structure has two
   useful properties:
     * it allows the arrays of edge structures to be dynamically allocated, to
        maximize the density of useful data in caches
     * it achieves performance almost as good as a statically sized matrix, while
       allowing dynamic sizing
       
   This has only been optimised for Tegra K1, a Cortex-A15-based SoC when compiled
   with gcc 4.8.2. Performance seems highly dependent on code alignment.
   
   Peformance (with provided benchmark graph): around 10% speedup compared to cpp,
   and around 15% compared to C/HIGHBIT.
   
   Note that C/HIGHBIT has been observed to be faster on a second sparse graph with
   35 nodes.
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <sys/time.h>

typedef struct edge edge_t;

struct edge {
  int cost;
  int target;
};

void insert_node_edges(edge_t **costs, int index, edge_t* buf, int no_of_nodes, int no_of_edges) {
  int wp = 0;

  costs[index] = malloc(sizeof(edge_t) * (no_of_edges + 1));
  for (int i = 0; i < no_of_nodes; i++) {
    if (buf[i].cost > 0 || buf[i].target != 0) {
      costs[index][wp].target = buf[i].target;
      costs[index][wp].cost = buf[i].cost;
      wp++;
    }
  }
  costs[index][wp].cost = -1;
  bzero(buf, sizeof(edge_t) * no_of_nodes);
}

void parse_graph(edge_t ***costs_p, int *no_of_nodes_p) {
  FILE *f;
  int ret;
  int c_node;
  int prev_node = -1;
  int target_node;
  int cost;
  int index = 0;
  int wp;
  int no_of_nodes;
  edge_t **costs;
  
  f = fopen("agraph", "r");
  assert(f != NULL);
  
  ret = fscanf(f, "%d", no_of_nodes_p);
  assert (ret == 1);
  no_of_nodes = *no_of_nodes_p;
  
  costs = malloc(sizeof(edge_t*) * no_of_nodes);
  assert(costs != NULL);
  *costs_p = costs;
  edge_t *buf = malloc(sizeof(edge_t) * no_of_nodes);
  assert(buf != NULL);
  
  while(fscanf(f, "%d %d %d\n", &c_node, &target_node, &cost) == 3) {
    assert((c_node == prev_node || c_node == (prev_node + 1)) && c_node < no_of_nodes && cost >= 0);
    if (c_node != prev_node) {
      if (prev_node >= 0) {
        insert_node_edges(costs, prev_node, buf, no_of_nodes, index);
      }

      index = 0;
      prev_node = c_node;
    }
    buf[target_node].target = target_node;
    buf[target_node].cost = cost;
 
    index++;
  }
  
  insert_node_edges(costs, prev_node, buf, no_of_nodes, index);
}

int get_max_cost_small(edge_t **c, const int c_node, uint32_t visited) {
  int max = 0;
  int dist;
  
  visited |= 1 << c_node;
  for (int index = 0; c[c_node][index].cost >= 0; index++) {
    if (!(visited & (1 << c[c_node][index].target))) {
      dist = c[c_node][index].cost + get_max_cost_small(c, c[c_node][index].target, visited);
      if (dist > max) max = dist;
    }
  }
  visited &= ~(1 << c_node);
  
  return max;
}

int get_max_cost(edge_t **c, const int c_node, uint32_t *visited) {
  int max = 0;
  int dist;
  int target;
  
  visited[c_node >> 5] |= 1 << (c_node & 0x1f);
  for (int index = 0; c[c_node][index].cost >= 0; index++) {
    target = c[c_node][index].target;
    if (!( visited[target >> 5] & (1 << (target & 0x1F)) )) {
      dist = c[c_node][index].cost + get_max_cost(c, target, visited);
      if (dist > max) max = dist;
    }
  }
  visited[c_node >> 5] &= ~(1 << (c_node & 0x1f));
  
  return max;
}

int main() {
  int result;
  uint32_t *visited;
  uint64_t ms;
  struct timeval start, end, duration;
  edge_t **costs;
  int no_of_nodes;
  
  parse_graph(&costs, &no_of_nodes);

  gettimeofday(&start, NULL);
  if (no_of_nodes > 32) {
    visited = malloc( sizeof(uint32_t) * ((no_of_nodes >> 5) + 1) );
    assert(visited != NULL);
    result = get_max_cost(costs, 0, visited);
  } else {
    result = get_max_cost_small(costs, 0, 0);
  }
  gettimeofday(&end, NULL);
  
  timersub(&end, &start, &duration);
  ms = duration.tv_sec*1000 + duration.tv_usec/1000;
  printf("%d LANGUAGE C-arm %llu\n", result, ms);
}

