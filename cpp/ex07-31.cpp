#include <print>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

enum state { undiscovered, discovered, processed };

struct vertex {
  state st;
  std::unordered_set<char> edges;
};

struct dag {
  std::unordered_map<char, vertex> vertices;
};

// Traverse the vector of words and build the DAG.
void traverse(const std::vector<std::string> &words, dag &g) {
  for (size_t i = 0; i < words.size() - 1; i++) {
    for (size_t j = 0; j < words[i].size(); j++) {
      if (words[i][j] != words[i + 1][j]) {
        std::println("{} -> {}", words[i][j], words[i + 1][j]);
        g.vertices[words[i][j]].edges.insert(words[i + 1][j]);
        break;
      }
    }
  }
}

void dfs(dag &g, char v, std::stack<char> &s) {
  g.vertices[v].st = discovered;
  for (char w : g.vertices[v].edges) {
    if (g.vertices[w].st == undiscovered)
      dfs(g, w, s);
  }
  g.vertices[v].st = processed;
  s.push(v);
}

std::stack<char> toposort(dag &g) {
  std::stack<char> s;
  for (auto [k, v] : g.vertices) {
    if (v.st == undiscovered)
      dfs(g, k, s);
  }
  return s;
}

int main() {
  std::vector<std::string> words{
      "QQZ", "QZZ", "XQZ", "XQX", "XXX",
  };
  dag g;
  traverse(words, g);

  auto s = toposort(g);
  std::print("Alphabetic order: ");
  while (!s.empty()) {
    std::print("{} ", s.top());
    s.pop();
  }
  std::println("");
  return 0;
}