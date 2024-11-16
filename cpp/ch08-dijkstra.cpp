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

void dijkstra(Graph &g, int s, int t) {
  const int N = g.size();
  std::vector distance(N, std::numeric_limits<int>::max());
  std::vector parent(N, -1);

  distance[s] = 0;
  int u = s;
  while (!g.vertices[u].in_tree) {
    g.vertices[u].in_tree = true;

    if (u != s)
      std::println("Edge ({},{}) added to the tree", parent[u], u);

    // Update all shortest known paths with `u` now in the tree
    for (Edge &e : g.vertices[u].edges) {
      if (distance[e.v] > distance[u] + e.weight) {
        distance[e.v] = distance[u] + e.weight;
        parent[e.v] = u;
      }
    }

    // Determine next `u`--a vertex in the fringe at minimum distance from s
    int dist = std::numeric_limits<int>::max();
    for (int i = 1; i < N; i++) {
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
  dijkstra(g, 0, 6);
  return 0;
}