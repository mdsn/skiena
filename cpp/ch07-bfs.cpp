#include <print>
#include <queue>
#include <scn/scan.h>
#include <vector>

struct vertex_t {
  std::vector<int> edges;
  void add_edge(int y) { edges.push_back(y); }
  int degree() { return edges.size(); }
};

struct graph_t {
  std::vector<vertex_t> xs;
  int n; // n vertices
  int m; // m edges
  bool directed;

  graph_t(bool directed, int n) : directed(directed), n(n), m(0) {
    xs.resize(n);
  }

  void insert_edge(int x, int y) {
    xs[x].add_edge(y);
    if (!directed)
      xs[y].add_edge(x);
    m++;
  }
};

void bfs(graph_t &g, int start) {
  std::vector<bool> discovered(g.n, false);
  std::vector<bool> processed(g.n, false);
  std::vector<int> parent(g.n, -1);
  std::queue<int> q;

  q.push(start);
  discovered[start] = true;
  while (!q.empty()) {
    int x = q.front();
    q.pop();
    processed[x] = true;
    std::println("processed vertex {}", x);
    for (int y : g.xs[x].edges) {
      if (!processed[y] || g.directed) {
        std::println(" processed edge ({},{})", x, y);
      }
      if (!discovered[y]) {
        q.push(y);
        discovered[y] = true;
        parent[y] = x;
      }
    }
  }
}

graph_t read_graph(bool directed) {
  auto [n, m] = scn::input<int, int>("{} {}")->values();
  graph_t g(directed, n);
  for (int i = 0; i < m; i++) {
    auto [x, y] = scn::input<int, int>("{} {}")->values();
    g.insert_edge(x, y);
  }
  return g;
}

void print_graph(graph_t &g) {
  for (int i = 0; i < g.n; i++) {
    std::print("{}:", i);
    for (auto y : g.xs[i].edges)
      std::print(" {}", y);
    std::println("");
  }
}

int main() {
  auto g0 = read_graph(false);
  print_graph(g0);
  bfs(g0, 0);
  return 0;
}