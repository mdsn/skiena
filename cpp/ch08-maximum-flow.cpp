#include <array>
#include <print>
#include <queue>
#include <ranges>
#include <set>
#include <vector>
#include <limits>

using std::array;
using std::optional;
using std::vector;
namespace ranges = std::ranges;
namespace views = std::ranges::views;

using Spec = array<std::tuple<size_t, size_t, int>, 22>;

struct E {
  size_t v;
  int capacity; int flow = 0; int residual;
  E(const size_t target, const int cap) : v(target), capacity(cap), residual(cap) {}
  bool operator==(const size_t &w) const { return this->v == w; }
};

struct V { std::vector<E> edges; };

template <size_t N> struct G {
  array<V, N> vertices;
  explicit constexpr G(const Spec &edges) { for (auto [u, v, cap] : edges) add(u, v, cap); }
  V &operator[](size_t i) { return vertices[i]; }
  const V &operator[](size_t i) const { return vertices[i]; }
  static constexpr size_t size() { return N; }
  void add(size_t u, size_t v, int cap) { vertices[u].edges.emplace_back(v, cap); }
  const E &edge(size_t u, size_t v) const { return *ranges::find(vertices[u].edges, v, &E::v); }
  E &edge(size_t u, size_t v) { return *ranges::find(vertices[u].edges, v, &E::v); }
};

template <size_t N> using Parent = array<optional<size_t>, N>;

template <size_t N> Parent<N> bfs(const G<N> &g, const size_t source) {
  Parent<N> parent;
  std::set<size_t> seen; seen.insert(source);
  std::queue<size_t> q; q.push(source);
  while (!q.empty()) {
    auto u = q.front(); q.pop();
    for (auto &e : g[u].edges | views::filter([&](const auto &e) {
      return !seen.contains(e.v) and e.residual > 0;
    })) { parent[e.v] = u; seen.insert(e.v); q.push(e.v); }
  }
  return parent;
}

// Skiena's recursive implementation:
// template <size_t N>
// int residual_capacity(const G<N> &g, const Parent<N> &parent, const size_t u,
//                       const size_t v) {
//   if (!parent[v]) return 0;
//   size_t pv = *parent[v];
//   const E &e = g.edge(pv, v);
//   if (u == pv) return e.residual;
//   return std::min(residual_capacity(g, parent, u, pv), e.residual);
// }

template <size_t N>
int residual_capacity(const G<N> &g, const Parent<N> &parent, size_t v) {
  if (!parent[v]) return 0;
  int cap = std::numeric_limits<int>::max();
  while (auto pv = parent[v]) { cap = std::min(g.edge(*pv, v).residual, cap); v = *pv; }
  return cap;
}

template <size_t N>
void augment_path(G<N> &g, const Parent<N> &parent, const size_t u, size_t v,
                  int volume) {
  auto pv = parent[v];
  while (pv and v != u) {
    auto &e = g.edge(*pv, v);
    e.flow += volume; e.residual -= volume;
    v = *pv; pv = parent[v];
  }
}

template <size_t N> void maximum_flow(G<N> &g, size_t source, size_t sink) {
  auto parent = bfs(g, source);
  int volume = residual_capacity(g, parent, sink);
  while (volume > 0) {
    augment_path(g, parent, source, sink, volume);
    parent = bfs(g, source);
    volume = residual_capacity(g, parent, sink);
  }
}

template <size_t N> void print_flow(const G<N> &g) {
  int i = 0;
  for (size_t u = 0; u < g.size(); u++)
    for (const E &e : g[u].edges) {
      std::println("{}) {} -> {}: flow={}/{} residual={}",
        i, u, e.v, e.flow, e.capacity, e.residual);
      i++;
    }
}

int main() {
  constexpr Spec edges{
      {{0, 1, 9},   {0, 2, 12}, {1, 3, 6},  {1, 4, 4},  {2, 4, 5},
       {2, 5, 7},   {3, 6, 12}, {4, 3, 3},  {4, 7, 7},  {5, 8, 9},
       {5, 9, 6},   {6, 12, 7}, {6, 13, 8}, {7, 6, 3},  {7, 12, 2},
       {8, 7, 5},   {8, 10, 4}, {9, 10, 3}, {10, 7, 2}, {10, 11, 12},
       {11, 12, 6}, {12, 13, 9}}};
  G<14> g{edges};
  maximum_flow(g, 0, 13);
  print_flow(g);
}
