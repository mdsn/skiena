#include <print>
#include <queue>
#include <vector>

struct vertex {
  int parent;
  std::vector<int> edges;
  vertex(std::initializer_list<int> e) : parent(-1), edges(e) {}
};

struct graph {
  std::vector<vertex> vertices;
  void add_vertex(std::initializer_list<int> e) { vertices.emplace_back(e); }
  void reset() {
    for (vertex &v : vertices)
      v.parent = -1;
  }
};

void bfs(graph &g, int start) {
  std::queue<int> q;
  q.push(start);
  while (!q.empty()) {
    int u = q.front();
    q.pop();
    for (int v : g.vertices[u].edges) {
      if (g.vertices[u].parent == v)
        continue;
      if (g.vertices[v].parent == -1) {
        g.vertices[v].parent = u;
        q.push(v);
      } else if (g.vertices[v].parent == g.vertices[u].parent) {
        std::println("{}: triangle found: {},{},{}", start,
                     g.vertices[v].parent, v, u);
        return;
      }
    }
  }
  std::println("{}: no triangle found", start);
}

int main() {
  graph g;                    // triangle: 2, 4, 6
  g.add_vertex({1});          // 0
  g.add_vertex({0, 2});       // 1
  g.add_vertex({1, 3, 4, 6}); // 2
  g.add_vertex({2});          // 3
  g.add_vertex({2, 5, 6});    // 4
  g.add_vertex({4});          // 5
  g.add_vertex({2, 4});       // 6

  for (int i = 0; i < 7; i++) {
    bfs(g, i);
    g.reset();
  }

  return 0;
}
