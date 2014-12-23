// C implementation for Pathfinding Benchmark by nate@verse.com
// See https://github.com/logicchains/LPATHBench for details

// Summary of benchmarks (see bottom for full numbers)
// 8981 LANGUAGE C 623
// 8981 LANGUAGE C++/clang 734
// 8981 LANGUAGE C++/gcc 755
// Best results compiling with GCC 4.7 or 4.8 -O2
// clang, icc and GCC 4.9 slightly worse with -O1, -O2, -O3, -Ofast
// -O3 and -Ofast much worse for all GCC.  -O1 mixed but worse.

// Three compilation options are possible:
// gcc -g -std=gnu99 -Wall -Wextra c.c  -O2 -march=native -o c -DUSE_BITMAP
// gcc -g -std=gnu99 -Wall -Wextra c.c  -O2 -march=native -o c -DUSE_HIGHBIT
// gcc -g -std=gnu99 -Wall -Wextra c.c  -O2 -march=native -o c -DUSE_BRANCHLESS

// USE_BITMAP:  Standard approach using a per-node bitmap to track visited status
// USE_HIGHBIT: Track visited status using high bit of existing per node numPaths
// USE_BRANCHLESS: Use high bit plus reduce branch misprediction (default)

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/time.h>

#define GRAPH_FILE "agraph"
#define MAX_NEIGHBORS_PER_NODE 100
#define BITSET_NODES 1000           // only used #ifdef USE_BITMAP
#define VISITED_BIT (1U << 31)      // used bare or #ifdef USE_SIMPLE

typedef struct {
    uint32_t neighbor;
    uint32_t distance;
} path_t;

typedef struct {
    uint32_t numPaths;
    path_t paths[];
} node_t;

typedef uint64_t bitmap_t;
#include <stdbool.h>
static inline void bitmap_set_bit(bitmap_t *bitmap, size_t pos) {
    bitmap[pos/64] |= (1UL << (pos % 64));
}
static inline bool bitmap_is_set(bitmap_t *bitmap, size_t pos) {
    return bitmap[pos/64] & (1UL << (pos % 64));
}
static inline void bitmap_clear_bit(bitmap_t *bitmap, size_t pos) {
    bitmap[pos/64] &= ~(1UL << (pos % 64));
}

// Standard approach with separate bitmap to track visited status
uint32_t max_distance_bitmap(node_t **nodeTable, uint32_t nodeNum, bitmap_t *visited) {
    node_t *node = nodeTable[nodeNum];
    uint32_t numPaths = node->numPaths;
    uint32_t max = 0;

    bitmap_set_bit(visited, nodeNum);

    for (uint32_t i = 0; i < numPaths; i++) {
        path_t *path = node->paths + i;
        uint32_t distance = path->distance;
        uint32_t neighbor = path->neighbor;
        if (bitmap_is_set(visited, neighbor)) continue;
        uint32_t subDistance = max_distance_bitmap(nodeTable, neighbor, visited);
        distance += subDistance;
        if (distance > max) max = distance;
    }

    bitmap_clear_bit(visited, nodeNum);

    return max;
}

// Use high bit of count of neighbors as flag for visited status
// No extra space required for bitmap, and slighty faster
uint32_t max_distance_simple(node_t **nodeTable, node_t *node) {
    uint32_t maxDistance = 0;
    uint32_t numPaths = node->numPaths;
    node->numPaths |= VISITED_BIT;

    for (uint32_t i = 0; i < numPaths; i++) {
        path_t *path = node->paths + i;
        node_t *subNode = nodeTable[path->neighbor];
        if (subNode->numPaths & VISITED_BIT) continue;
        uint32_t subDistance = max_distance_simple(nodeTable, subNode);
        uint32_t totalDistance = path->distance + subDistance;
        if (totalDistance > maxDistance) maxDistance = totalDistance;
    }

    node->numPaths &= ~VISITED_BIT;
    return maxDistance;
}

// Copying unvisited nodes to local array reduces branch mispredicts by half
// Various fragile loop optimizations give another few percent of benefit
uint32_t max_distance_branchless(node_t **nodeTable, node_t *node) {
    node_t *unvisitedNodes[MAX_NEIGHBORS_PER_NODE];
    uint32_t distances[MAX_NEIGHBORS_PER_NODE];

    uint32_t numPaths = node->numPaths;
    node->numPaths |= VISITED_BIT;

    uint32_t numNodes = 1; // start at one for loop optimizations
    path_t *path = node->paths;
    uint32_t maxDistance = 0;

    do {
        node_t *potentialNode = nodeTable[path->neighbor];
        unvisitedNodes[numNodes] = potentialNode;
        distances[numNodes] = path->distance;
        // must compile to an adc or cmovc to avoid branch mispredict
        if (! (potentialNode->numPaths & VISITED_BIT) ) numNodes++;
        path = path + 1;  // increment ends up faster than indexed array
    } while (--numPaths); // do-while allows for slightly better code

    while (--numNodes) {  // predecrement allows slightly faster loop
        node_t *subNode = unvisitedNodes[numNodes];
        uint32_t subDistance = max_distance_branchless(nodeTable, subNode);
        uint32_t totalDistance = distances[numNodes] + subDistance;
        if (totalDistance > maxDistance) maxDistance = totalDistance;
    }

    node->numPaths &= ~VISITED_BIT;
    return maxDistance;
}

// returns nodeTable[numNode] that indexes into byte array of graph data
// no particular benefit in speed, but extremely compact representation
#define DIE(error...) { printf(error); exit(1); }
node_t **read_graph_file(char *filename) {
    FILE *graphFile = fopen(filename, "r");
    if (! graphFile) DIE("Could not open graph file '%s'\n", GRAPH_FILE);

    uint32_t numNodes;
    if (fscanf(graphFile, "%u\n", &numNodes) != 1 || numNodes == 0){
        DIE("First line should be the non-zero number of nodes\n");
    }

    char *data = malloc(MAX_NEIGHBORS_PER_NODE * numNodes * sizeof(path_t) +
                        numNodes * sizeof(uint32_t));
    node_t **nodeTable = calloc(numNodes, sizeof(node_t *));

    uint32_t lineNum = 1;
    uint32_t previousNode = -1;
    uint32_t numPaths = 0;
    uint32_t node, neighbor, distance;
    while (fscanf(graphFile, "%u %u %u\n", &node, &neighbor, &distance) == 3) {
        lineNum++;
        if (node != previousNode) {
            if (lineNum > 2) { // go back to fill in previous numPaths if not first node
                nodeTable[previousNode]->numPaths = numPaths;
                data = data + sizeof(path_t) * numPaths + sizeof(((node_t *)0)->numPaths);
                if (node < previousNode || node >= numNodes || neighbor >= numNodes) {
                    DIE("Invalid node at line %d\n", lineNum);
                }
            }
            nodeTable[node] = (node_t *)data;
            previousNode = node;
            numPaths = 0;
        }

        nodeTable[node]->paths[numPaths].neighbor = neighbor;
        nodeTable[node]->paths[numPaths].distance = distance;
        numPaths++;
    }

    if (! feof(graphFile)) DIE("Error at line %d before end of file\n", lineNum);

    nodeTable[previousNode]->numPaths = numPaths;

    fclose(graphFile);
    return nodeTable;
}

int main(void) {
    node_t **nodeTable = read_graph_file(GRAPH_FILE);
    struct timeval timeStart, timeFinish, timeTotal;

    gettimeofday(&timeStart, NULL);

#if defined USE_BITMAP
    char *suffix = "-bitset";
    bitmap_t *visited = calloc(BITSET_NODES / 8, 1);
    uint32_t max = max_distance_bitmap(nodeTable, 0, visited);
#elif defined USE_HIGHBIT
    char *suffix = "-simple";
    uint32_t max = max_distance_simple(nodeTable, nodeTable[0]);
#elif defined USE_BRANCHLESS
    char *suffix = "-branchless";
    uint32_t max = max_distance_branchless(nodeTable, nodeTable[0]);
#else // default to branchless but without suffix
    char *suffix = "";
    uint32_t max = max_distance_branchless(nodeTable, nodeTable[0]);
#endif

    gettimeofday(&timeFinish, NULL);
    timersub(&timeFinish, &timeStart, &timeTotal);

    uint64_t milliseconds = timeTotal.tv_sec*1000 + timeTotal.tv_usec/1000;
    printf("%d LANGUAGE C%s %ld\n", max, suffix, milliseconds);
}

#ifdef BEST_COMPILER_ASSEMBLY

// best assembly from "gcc-4.8 -g -std=gnu99 -Wall -Wextra c.c  -O2 -march=native -o c"

0000000000400a10 <max_distance_branchless>:
  400a10:	41 56                	push   %r14
  400a12:	48 8d 46 04          	lea    0x4(%rsi),%rax
  400a16:	49 89 f6             	mov    %rsi,%r14
  400a19:	41 55                	push   %r13
  400a1b:	41 54                	push   %r12
  400a1d:	55                   	push   %rbp
  400a1e:	53                   	push   %rbx
  400a1f:	48 81 ec b0 04 00 00 	sub    $0x4b0,%rsp
  400a26:	8b 16                	mov    (%rsi),%edx
  400a28:	41 89 d0             	mov    %edx,%r8d
  400a2b:	41 81 c8 00 00 00 80 	or     $0x80000000,%r8d
  400a32:	85 d2                	test   %edx,%edx
  400a34:	44 89 06             	mov    %r8d,(%rsi)
  400a37:	0f 84 83 00 00 00    	je     400ac0 <max_distance_semibranchless+0xb0>
  400a3d:	49 89 fd             	mov    %rdi,%r13
  400a40:	bb 01 00 00 00       	mov    $0x1,%ebx
  400a45:	0f 1f 00             	nopl   (%rax)
  400a48:	8b 08                	mov    (%rax),%ecx
  400a4a:	89 de                	mov    %ebx,%esi
  400a4c:	8b 78 04             	mov    0x4(%rax),%edi
  400a4f:	49 8b 4c cd 00       	mov    0x0(%r13,%rcx,8),%rcx
  400a54:	89 3c b4             	mov    %edi,(%rsp,%rsi,4)
  400a57:	81 39 00 00 00 80    	cmpl   $0x80000000,(%rcx)
  400a5d:	48 89 8c f4 90 01 00 	mov    %rcx,0x190(%rsp,%rsi,8)
  400a64:	00
  400a65:	83 d3 00             	adc    $0x0,%ebx
  400a68:	48 83 c0 08          	add    $0x8,%rax
  400a6c:	83 ea 01             	sub    $0x1,%edx
  400a6f:	75 d7                	jne    400a48 <max_distance_semibranchless+0x38>
  400a71:	31 ed                	xor    %ebp,%ebp
  400a73:	83 eb 01             	sub    $0x1,%ebx
  400a76:	74 2c                	je     400aa4 <max_distance_semibranchless+0x94>
  400a78:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  400a7f:	00
  400a80:	41 89 dc             	mov    %ebx,%r12d
  400a83:	4c 89 ef             	mov    %r13,%rdi
  400a86:	4a 8b b4 e4 90 01 00 	mov    0x190(%rsp,%r12,8),%rsi
  400a8d:	00
  400a8e:	e8 7d ff ff ff       	callq  400a10 <max_distance_semibranchless>
  400a93:	42 03 04 a4          	add    (%rsp,%r12,4),%eax
  400a97:	39 c5                	cmp    %eax,%ebp
  400a99:	0f 42 e8             	cmovb  %eax,%ebp
  400a9c:	83 eb 01             	sub    $0x1,%ebx
  400a9f:	75 df                	jne    400a80 <max_distance_semibranchless+0x70>
  400aa1:	45 8b 06             	mov    (%r14),%r8d
  400aa4:	41 81 e0 ff ff ff 7f 	and    $0x7fffffff,%r8d
  400aab:	89 e8                	mov    %ebp,%eax
  400aad:	45 89 06             	mov    %r8d,(%r14)
  400ab0:	48 81 c4 b0 04 00 00 	add    $0x4b0,%rsp
  400ab7:	5b                   	pop    %rbx
  400ab8:	5d                   	pop    %rbp
  400ab9:	41 5c                	pop    %r12
  400abb:	41 5d                	pop    %r13
  400abd:	41 5e                	pop    %r14
  400abf:	c3                   	retq
  400ac0:	31 ed                	xor    %ebp,%ebp
  400ac2:	eb e0                	jmp    400aa4 <max_distance_semibranchless+0x94>
  400ac4:	66 66 66 2e 0f 1f 84 	data32 data32 nopw %cs:0x0(%rax,%rax,1)
  400acb:	00 00 00 00 00

#endif

#ifdef SAMPLE_TIMINGS

// Sample timings for Intel Haswell i7-4770 CPU @ 3.40GHz
// x86_64 Linux 3.13, Turboboost Off, Hyperthreading Off

// Performance with -O3 not shown, but worse with all versions of GCC, unchanged with clang/icc
// Note that the first line for "LANGUAGE C" is the same code as "LANGUAGE C-branchless"
// Reference numbers for C++ versions are at end C language samples

nate@haswell:~/git/LPATHBench$ CC=gcc-4.9 make clean-c run-c
rm -f c c-bitmap c-highbit c-branchless
gcc-4.9 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -o c
gcc-4.9 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BITMAP -o c-bitmap
gcc-4.9 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_HIGHBIT -o c-highbit
gcc-4.9 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BRANCHLESS -o c-branchless
for prog in c c-bitmap c-highbit c-branchless; do $prog; done
8981 LANGUAGE C 647
8981 LANGUAGE C-bitset 859
8981 LANGUAGE C-simple 817
8981 LANGUAGE C-branchless 648

nate@haswell:~/git/LPATHBench$ CC=gcc-4.8 make clean-c run-c
rm -f c c-bitmap c-highbit c-branchless
gcc-4.8 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -o c
gcc-4.8 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BITMAP -o c-bitmap
gcc-4.8 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_HIGHBIT -o c-highbit
gcc-4.8 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BRANCHLESS -o c-branchless
for prog in c c-bitmap c-highbit c-branchless; do $prog; done
8981 LANGUAGE C 623
8981 LANGUAGE C-bitset 895
8981 LANGUAGE C-simple 746
8981 LANGUAGE C-branchless 619

nate@haswell:~/git/LPATHBench$ CC=gcc-4.7 make clean-c run-c
rm -f c c-bitmap c-highbit c-branchless
gcc-4.7 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -o c
gcc-4.7 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BITMAP -o c-bitmap
gcc-4.7 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_HIGHBIT -o c-highbit
gcc-4.7 c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BRANCHLESS -o c-branchless
for prog in c c-bitmap c-highbit c-branchless; do $prog; done
8981 LANGUAGE C 630
8981 LANGUAGE C-bitset 955
8981 LANGUAGE C-simple 840
8981 LANGUAGE C-branchless 630

nate@haswell:~/git/LPATHBench$ clang -v
Ubuntu clang version 3.5-1ubuntu1 (trunk) (based on LLVM 3.5)
nate@haswell:~/git/LPATHBench$ CC=clang make clean-c run-c
rm -f c c-bitmap c-highbit c-branchless
clang c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -o c
clang c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BITMAP -o c-bitmap
clang c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_HIGHBIT -o c-highbit
clang c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BRANCHLESS -o c-branchless
for prog in c c-bitmap c-highbit c-branchless; do $prog; done
8981 LANGUAGE C 681
8981 LANGUAGE C-bitset 823
8981 LANGUAGE C-simple 761
8981 LANGUAGE C-branchless 680

nate@haswell:~/git/LPATHBench$ icc -v
icc version 14.0.3 (gcc version 4.8.0 compatibility)
nate@haswell:~/git/LPATHBench$ CC=icc make clean-c run-c
rm -f c c-bitmap c-highbit c-branchless
icc c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -o c
icc c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BITMAP -o c-bitmap
icc c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_HIGHBIT -o c-highbit
icc c.c -g -std=gnu99 -Wall -Wextra -O2 -march=native -DUSE_BRANCHLESS -o c-branchless
for prog in c c-bitmap c-highbit c-branchless; do $prog; done
8981 LANGUAGE C 662
8981 LANGUAGE C-bitset 841
8981 LANGUAGE C-simple 849
8981 LANGUAGE C-branchless 660

nate@haswell:~/git/LPATHBench$ make clean-cpp run-cpp
rm -f cpp-gcc-gcc cpp-gcc-clang cpp-clang-clang cpp-clang-gcc cpp-gcc-icpc cpp-clang-icpc cpp-gcc-icpc cpp-cached-icpc cpp-cached-clang
g++ cpp.cpp -std=c++11 -Wall -O2 -march=native -DCOMPILER='"gcc"' -o cpp-gcc-gcc
g++ cpp.cpp -std=c++11 -Wall -O2 -march=native -DCOMPILER='"clang"' -o cpp-gcc-clang
clang++ cpp.cpp -std=c++11 -Wall -O2 -march=native -DCOMPILER='"clang"' -o cpp-clang-clang
clang++ cpp.cpp -std=c++11 -Wall -O2 -march=native -DCOMPILER='"gcc"' -o cpp-clang-gcc
icpc cpp.cpp -std=c++11 -Wall -O2 -march=native -DCOMPILER='"gcc"' -o cpp-gcc-icpc
icpc cpp.cpp -std=c++11 -Wall -O2 -march=native -DCOMPILER='"clang"' -o cpp-clang-icpc
icpc cpp_cached.cpp -std=c++11 -Wall -O2 -march=native -o cpp-cached-icpc
icpc cpp_cached.cpp -std=c++11 -Wall -O2 -march=native -o cpp-cached-clang
for prog in cpp-gcc-gcc cpp-gcc-clang cpp-clang-clang cpp-clang-gcc cpp-gcc-icpc cpp-clang-icpc cpp-cached-icpc cpp-cached-clang; do $prog; done
8981 LANGUAGE C++/gcc 755
8981 LANGUAGE C++/clang 757
8981 LANGUAGE C++/clang 734
8981 LANGUAGE C++/gcc 735
8981 LANGUAGE C++/gcc 764
8981 LANGUAGE C++/clang 766
8981 LANGUAGE C++Cached 16
8981 LANGUAGE C++Cached 16

#endif
