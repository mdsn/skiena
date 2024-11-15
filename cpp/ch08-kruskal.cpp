#include <print>
#include <queue>
#include <vector>

struct UnionFind {
  std::vector<int> parent;
  std::vector<int> size;
  int n;

  explicit UnionFind(int n) : parent(n), size(n), n(n) {
    for (int i = 0; i < n; i++) {
      parent[i] = i;
      size[i] = 1;
    }
  }

  void join(int i, int j) {
    if (i == j)
      return;

    const int ri = find(i);
    const int rj = find(j);
    if (ri == rj)
      return;

    if (size[ri] <= size[rj]) {
      parent[ri] = rj;
      size[rj] += size[ri];
    } else {
      parent[rj] = ri;
      size[ri] += size[rj];
    }
  }

  int find(int i) const {
    while (parent[i] != i)
      i = parent[i];
    return i;
  }
};

struct Edge {
  int u, v;
  int weight;
  Edge(int u, int v, int weight) : u(u), v(v), weight(weight) {}
};

struct Graph {
  std::vector<std::vector<Edge>> vertices;

  explicit Graph(int n) : vertices(n) {}
  int size() const { return vertices.size(); }

  void add_edge(int u, int v, int weight) {
    vertices[u].emplace_back(u, v, weight);
    vertices[v].emplace_back(v, u, weight);
  }

  std::vector<Edge> edges() const {
    std::vector<Edge> vec{};
    for (auto &v : vertices)
      vec.insert(vec.end(), v.begin(), v.end());
    return vec;
  }
};

void kruskal(Graph &g) {
  UnionFind components{g.size()};
  std::priority_queue q{
      [](const Edge &e1, const Edge &e2) { return e1.weight > e2.weight; },
      g.edges()};
  int count = 0;
  while (count < g.size() - 1) {
    Edge e = q.top();
    if (components.find(e.u) != components.find(e.v)) {
      count++;
      std::println("Edge ({}, {}) added to the tree", e.u, e.v);
      components.join(e.u, e.v);
    }
    q.pop();
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
  kruskal(g);
  return 0;
}