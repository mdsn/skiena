#include <vector>
#include <print>

struct vertex {
  std::vector<int> edges;
  vertex(std::initializer_list<int> e) : edges(e) {}
};

struct dag {
  std::vector<vertex> vertices;
  void add_vertex(std::initializer_list<int> edges) {
    vertices.emplace_back(edges);
  }
};

int dfs(dag &g, int v, int depth) {
  if (g.vertices[v].edges.empty())
    return depth;
  int max = -1;
  for (int w : g.vertices[v].edges)
    max = std::max(max, dfs(g, w, depth + 1));
  return max;
}

int rows(dag &g) {
  std::vector<bool> hated(g.vertices.size(), false);
  for (int i = 0; i < g.vertices.size(); i++) {
    for (int j : g.vertices[i].edges)
      hated[j] = true;
  }

  int max = -1;
  for (int i = 0; i < g.vertices.size(); i++) {
    if (!hated[i])
      max = std::max(max, dfs(g, i, 1));
  }
  return max;
}

int main() {
  dag g;
  g.add_vertex({2, 3}); // 0
  g.add_vertex({5});    // 1
  g.add_vertex({5});    // 2
  g.add_vertex({4, 6}); // 3
  g.add_vertex({6});    // 4
  g.add_vertex({7});    // 5
  g.add_vertex({7});    // 6
  g.add_vertex({});     // 7
  std::println("{}", rows(g));
  return 0;
}