#include <vector>
#include <queue>

struct vertex {
  std::vector<int> edges;
  int degree;
  bool deleted;

  void insert_edge(int v, bool switcheroo) {
    edges.push_back(v);
    if (!switcheroo) degree++;
  }

  void mark_null() {
    degree = -1;
    deleted = true;
    edges.clear();
  }
};

// This representation is misleading. To keep things simple, the reduce()
// function puts tombstones on deleted vertices; otherwise the invariant that
// a vertex identifier is its position in the vector of vertices would not hold.
// Thus when reduce() is finished it still reports nverts == 5 for the example
// graph, even though only two (vertices 0 and 4) are live.
struct graph {
  std::vector<vertex> vertices;
  int nverts;

  graph() {}
  graph(int n) : vertices(n), nverts(n) {}

  void insert_edge(int u, int v) {
    vertices[u].insert_edge(v, false);
    vertices[v].insert_edge(u, false);
  }
};

void reduce(graph& g) {
  // stage 1: enqueue vertices that start with degree 2
  std::queue<int> q;
  for (int i=0; i < g.nverts; i++)
    if (g.vertices[i].degree == 2)
      q.push(i);
  // stage 2: reduce the graph
  while (!q.empty()) {
    vertex& u = g.vertices[q.front()];
    q.pop();
    if (u.degree == 2) {
      u.deleted = true;
      // find two live neighbors
      bool fstfound = false;
      int v, w;
      for (int i : u.edges) {
        if (!g.vertices[i].deleted) {
          if (!fstfound) {
            v = i; fstfound = true;
          } else {
            w = i; break;
          }
        }
      }
      // see if it's a duplicate edge
      bool dupe = false;
      for (int i : g.vertices[v].edges) {
        if (i == w) {
          dupe = true;
          if (--g.vertices[v].degree == 2) q.push(v);
          if (--g.vertices[w].degree == 2) q.push(w);
          break;
        }
      }
      if (!dupe) {
        g.vertices[v].insert_edge(w, true);
        g.vertices[w].insert_edge(v, true);
      }
    }
  }
  // stage 3: put tombstones on deleted vertices and replace edges (hopefully
  //          this doesn't leak memory)
  for (int i=0; i < g.nverts; i++) {
    if (g.vertices[i].deleted) {
      g.vertices[i].mark_null();
      continue;
    }
    std::vector<int> live_edges;
    for (int j : g.vertices[i].edges) {
      if (!g.vertices[j].deleted)
        live_edges.push_back(j);
    }
    g.vertices[i].edges = live_edges;
  }
}

int main() {
  //   2
  //  /|\
  // 3 | 1
  //  \|/ \
  //   4   0
  graph g(5);
  g.insert_edge(0, 1);
  g.insert_edge(1, 2);
  g.insert_edge(2, 3);
  g.insert_edge(3, 4);
  g.insert_edge(4, 1);
  g.insert_edge(2, 4);

  reduce(g);
  return 0;
}