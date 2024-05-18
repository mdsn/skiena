#include <print>
#include <scn/scan.h>
#include <stack>
#include <vector>

using vertex = std::vector<int>;

struct graph {
  int n;
  std::vector<vertex> xs;
  graph(int n) : n(n) { xs.resize(n); }
  void add_edge(int x, int y) {
    xs[x].push_back(y);
    xs[y].push_back(x);
  }
};

graph read_graph() {
  auto read_pair = []() { return scn::input<int, int>("{} {}")->values(); };
  auto [n, m] = read_pair();
  graph g(n);
  for (int i = 0; i < m; i++) {
    auto [x, y] = read_pair();
    g.add_edge(x, y);
  }
  return g;
}

void dfs(graph &g, int start) {
  std::vector<bool> discovered(g.xs.size(), false);
  std::vector<bool> processed(g.xs.size(), false);
  std::vector<int> parent(g.xs.size(), -1);
  std::function<void(int)> f = [&](int x) {
    std::println("> visiting {}", x);
    discovered[x] = true;
    for (int y : g.xs[x]) {
      if (!discovered[y]) {
        std::println("  found edge ({},{})", x, y);
        parent[y] = x;
        f(y);
      }
    }
    processed[x] = true;
    std::println("< processed {}", x);
  };
  f(start);
}

int main() {
  graph g = read_graph();
  dfs(g, 0);
  return 0;
}