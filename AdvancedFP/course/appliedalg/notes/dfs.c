#include<stdio.h>

typedef int Bool;
#define False 0
#define True 1

#define MAXV  100       // maxumum number of edges
#define MAXDEGREE 50    // maximum vertex out degree

typedef struct {
	int edges[MAXV+1][MAXDEGREE]; // adjacency info
	int degree[MAXV+1];           // outdegree of each vertex
	int nvertices;                // number of vertices
	int nedges;                   // number of edges
} graph;

/* if g is a graph
graph g;

g->edges[x]      is an array of successor nodes of node x
g->degree[x]     the number of successors nodes for x

*/

void printGraph(graph *g)
{ int i,j;
  for (i=0;i < g->nvertices; i++) {
	  printf("%d: ",i);
	  for (j=0; j < g->degree[i]; j++)
	      printf(" %d",g->edges[i][j]);
	  printf("\n");
  }
}

Bool valid_edge(int node)
{}

void process_edge(int parent, int child)
{}

void process_vertex(int v)
{}

Bool finished;
Bool processed[MAXV];
Bool discovered[MAXV];
int parent[MAXV];

void dfs(graph *g, int v)
{ int i; // counter
  int y; // successor vertexes of i, as we go around loop

  if (finished) return;
  discovered[v] = True;
  process_vertex(v);

  for (i=0; i < g->degree[v]; i++)
    { y = g->edges[v][i];
	  if (valid_edge(g->edges[v][i]) == True)
	     { if (discovered[y] == False)
	          { parent[y] = v;
			   dfs(g,y);
			  }
		   else if (processed[y] == False) process_edge(v,y);
         }
      if (finished) return;
    }
  processed[v] = True;
}


