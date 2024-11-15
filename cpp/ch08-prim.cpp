#include <limits>
#include <print>
#include <vector>

struct Edge {
  int v;
  int weight;
  Edge(int v, int weight) : v(v), weight(weight) {}
};

struct Vertex {
  bool in_tree;
  int parent = -1;
  std::vector<Edge> edges;
};

struct Graph {
  std::vector<Vertex> vertices;

  explicit Graph(int n) : vertices(n) {}
  int size() const { return vertices.size(); }

  void add_edge(int u, int v, int weight) {
    vertices[u].edges.emplace_back(v, weight);
    vertices[v].edges.emplace_back(u, weight);
  }
};

void prim(Graph &g) {
  int start = 0, u = start;
  std::vector distance(g.size(), std::numeric_limits<int>::max());
  distance[u] = 0;
  while (!g.vertices[u].in_tree) {
    g.vertices[u].in_tree = true;

    if (u != start)
      std::println("Edge ({}, {}) added to the tree", g.vertices[u].parent, u);

    for (Edge &e : g.vertices[u].edges) {
      if (distance[e.v] > e.weight && !g.vertices[e.v].in_tree) {
        distance[e.v] = e.weight;
        g.vertices[e.v].parent = u;
      }
    }

    int dist = std::numeric_limits<int>::max();
    for (int i = 0; i < g.size(); i++) {
      if (!g.vertices[i].in_tree && dist > distance[i]) {
        dist = distance[i];
        u = i;
      }
    }
  }
}

int main() {
  Graph g{7};
  g.add_edge(0, 1, 5);
  g.add_edge(0, 2, 7);
  g.add_edge(0, 5, 12);
  g.add_edge(1, 2, 9);
  g.add_edge(1, 3, 7);
  g.add_edge(2, 3, 4);
  g.add_edge(2, 4, 3);
  g.add_edge(2, 5, 4);
  g.add_edge(3, 4, 2);
  g.add_edge(3, 6, 5);
  g.add_edge(4, 5, 7);
  g.add_edge(4, 6, 2);
  prim(g);
  return 0;
}