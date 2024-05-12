// exercise 3-29, a partial solution
// this only shows how to build the segment tree for the corresponding. The code
// is ghastly.
// the tree is not self balancing. on each insert, it traverses through the
// existing ranges the key belongs to until hitting one it's not contained in,
// and extends it. The insertion only ever transforms a leaf node into a non
// leaf, extends its range, and creates two leaf children. This gives the tree
// more height than it needs, as there are cases where it's preferred to
// rebalance and pack the tree tightly.
#include <vector>

struct node {
  int k, v;
  std::pair<int, int> s;
  node *l, *r;
  node(int k, int v)
      : k(k), v(v), s(std::make_pair(k, k)), l(nullptr), r(nullptr) {}

  bool is_leaf() { return s.first == s.second; }
  bool contains(int k) { return s.first <= k and k <= s.second; }
};

struct tree {
  node *t;
  tree() : t(nullptr) {}

  void insert(int k, int v) {
    if (t == nullptr)
      t = new node(k, v);
    else
      traverse_insert(t, k, v);
  }

private:
  void traverse_insert(node *p, int k, int v) {
    if (p->is_leaf()) {
      if (p->k == k) {
        // should actually overwrite the node
        return;
      }
      // k is meaningless in a non-leaf node, but v is the partial sum of the
      // represented range
      int lv = p->v;
      int rv = v;
      if (k < p->k) {
        lv = v;
        rv = p->v;
      }
      p->v = lv + rv;
      p->s = std::make_pair(std::min(p->k, k), std::max(p->k, k));
      p->l = new node(p->s.first, lv);
      p->r = new node(p->s.second, rv);
    } else {
      if (!p->contains(k)) {
        p->s =
            std::make_pair(std::min(p->s.first, k), std::max(p->s.second, k));
      }
      p->v += v;
      int dist_l = std::abs(p->l->s.second - k);
      int dist_r = std::abs(p->r->s.first - k);
      if (dist_l <= dist_r)
        traverse_insert(p->l, k, v);
      else
        traverse_insert(p->r, k, v);
    }
  }
};

int main() {
  tree t;
  t.insert(-3, 10);
  t.insert(9, 12);
  t.insert(-1, 3);
  t.insert(7, 8);
  t.insert(0, 0);
  t.insert(5, -3);
  t.insert(1, -5);
  return 0;
}