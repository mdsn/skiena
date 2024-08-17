#include <queue>
#include <vector>

struct vertex {
  int degree;
  bool discarded;
  std::vector<int> edges;
  vertex(std::initializer_list<int> e)
      : degree(e.size()), edges(e), discarded(false) {}
  void add_edge(int u) {
    edges.push_back(u);
    degree++;
  }
};

struct graph {
  std::vector<vertex> vertices;
  void add_vertex(std::initializer_list<int> e) { vertices.emplace_back(e); }
};

graph maximum_induced_subgraph(graph &g, int k) {
  std::queue<int> q;
  for (int v = 0; v < g.vertices.size(); v++) {
    if (g.vertices[v].degree < k)
      q.push(v);
  }

  while (!q.empty()) {
    int v = q.front();
    q.pop();
    if (!g.vertices[v].discarded) {
      g.vertices[v].discarded = true;
      for (int u : g.vertices[v].edges) {
        if (--g.vertices[u].degree < k)
          q.push(u);
      }
    }
  }

  graph h;
  for (int v = 0; v < g.vertices.size(); v++) {
    h.add_vertex({});
    if (!g.vertices[v].discarded) {
      for (int u : g.vertices[v].edges) {
        if (!g.vertices[u].discarded)
          h.vertices[v].add_edge(u);
      }
    }
  }
  return h;
}

int main() {
  graph g;
  g.add_vertex({1});       // 0
  g.add_vertex({0, 2, 3}); // 1
  g.add_vertex({1});       // 2
  g.add_vertex({1, 4, 6}); // 3
  g.add_vertex({3, 5, 6}); // 4
  g.add_vertex({4});       // 5
  g.add_vertex({3, 4});    // 6
  graph h = maximum_induced_subgraph(g, 2);
  return 0;
}