#include <print>
#include <stack>
#include <vector>

enum state { undiscovered, discovered, processed };

struct vertex {
  state st;
  std::vector<int> edges;
  vertex(std::initializer_list<int> e) : edges(e), st(undiscovered) {}
};

struct dag {
  std::vector<vertex> vertices;
  void add_vertex(std::initializer_list<int> edges) {
    vertices.emplace_back(edges);
  }
};

void dfs(dag &g, int v, std::stack<int> &s) {
  g.vertices[v].st = discovered;
  for (int w : g.vertices[v].edges) {
    if (g.vertices[w].st == undiscovered)
      dfs(g, w, s);
  }
  g.vertices[v].st = processed;
  s.push(v);
}

std::stack<int> toposort(dag &g) {
  std::stack<int> s;
  for (int i = 0; i < g.vertices.size(); i++) {
    if (g.vertices[i].st == undiscovered) {
      dfs(g, i, s);
    }
  }
  return s;
}

int main() {
  dag g;
  g.add_vertex({2});
  g.add_vertex({2, 3});
  g.add_vertex({4});
  g.add_vertex({4});
  g.add_vertex({});
  std::stack<int> s = toposort(g);
  while (!s.empty()) {
    std::print("{} ", s.top());
    s.pop();
  }
  std::println("");
  return 0;
}