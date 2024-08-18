#include <limits>
#include <print>
#include <vector>

enum state { undiscovered, discovered, processed };

struct vertex {
  state st;
  int dist = std::numeric_limits<int>::max();
  std::vector<int> edges;
  bool connected_to_target() { return dist != std::numeric_limits<int>::max(); }
};

struct graph {
  std::vector<vertex> vertices;
  void add_vertex() { vertices.emplace_back(); }
  void add_edge(int u, int v) { vertices[u].edges.push_back(v); }
};

int shortest_path_len = std::numeric_limits<int>::max();
int shortest_path_count = 0;

void update(int len) {
  if (len == shortest_path_len)
    shortest_path_count++;
  else if (len < shortest_path_len) {
    shortest_path_count = 1;
    shortest_path_len = len;
  }
}

int dfs(graph &g, int u, int target, int depth) {
  if (u == target) {
    g.vertices[u].st = processed;
    g.vertices[u].dist = 0;
    update(depth);
    return 0;
  }

  int dist = std::numeric_limits<int>::max();
  for (int v : g.vertices[u].edges) {
    switch (g.vertices[v].st) {
    case undiscovered:
      g.vertices[v].st = discovered;
      // if a vertex is not connected to target dfs() will return int::max and
      // this +1 will overflow!!
      dist = std::min(dist, dfs(g, v, target, depth + 1) + 1);
      break;
    case processed:
      if (g.vertices[v].connected_to_target()) {
        dist = std::min(dist, g.vertices[v].dist + 1);
        update(depth + dist);
      }
      break;
    }
  }

  g.vertices[u].dist = dist;
  g.vertices[u].st = processed;
  return dist;
}

int count_shortest_paths(graph &g, int start, int target) {
  g.vertices[start].st = discovered;
  dfs(g, start, target, 0);
}

int main() {
  graph g;
  for (int i = 0; i < 10; i++)
    g.add_vertex();
  g.add_edge(0, 4);
  g.add_edge(4, 7);
  g.add_edge(0, 1);
  g.add_edge(1, 2);
  g.add_edge(2, 3);
  g.add_edge(3, 9);
  g.add_edge(4, 5);
  g.add_edge(5, 3);
  g.add_edge(5, 6);
  g.add_edge(7, 8);
  g.add_edge(8, 6);
  g.add_edge(6, 9);
  g.add_edge(3, 0);
  count_shortest_paths(g, 0, 9);
  std::println("len:{} count:{}", shortest_path_len, shortest_path_count);
  return 0;
}