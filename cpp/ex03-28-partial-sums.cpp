// exercise 3-28, partial sums
// given an array xs, provide a way to calculate partial sums and update
// individual values, both operations in O(log n).
//
// the solution uses a segment tree. A fixed vector of size 4n is allocated and
// the segment tree is implicitly packed in it. The root node represents the sum
// of the entire array. For a node that represents the range [l,r], its left
// child represents [l, m] and its right child [m+1, r], for m = (l+r)/2.
// segment_sum() sums an arbitrary range within the original array by finding
// the nodes in the segment tree that cover the range and adding their
// precomputed partial sums.
// the segment tree is internally 1-indexed, but the sum()/partial_sum() api
// takes in 0-based indices (into the original xs).
#include <vector>

struct tree {
  std::vector<int> t;
  int n;

  tree(std::vector<int> &xs) {
    t.resize(4 * xs.size());
    n = xs.size();
    write_node(xs, 1, 0, xs.size() - 1);
  }

  int sum(int l, int r) {
    return segment_sum(1, std::make_pair(1, n), std::make_pair(l + 1, r + 1));
  }

  int partial_sum(int i) { return sum(0, i); }

  void add(int i, int y) { update_node(1, 1, 7, i + 1, y); }

private:
  int write_node(std::vector<int> &xs, int k, int l, int r) {
    if (l == r)
      t[k] = xs[l];
    else {
      int m = (l + r) / 2;
      int left = write_node(xs, left_child(k), l, m);
      int right = write_node(xs, right_child(k), m + 1, r);
      t[k] = left + right;
    }
    return t[k];
  }

  int segment_sum(int k, std::pair<int, int> r, std::pair<int, int> q) {
    // case 1: current range is exactly the query
    if (q.first == r.first and q.second == r.second)
      return t[k];

    // case 2: query is entirely contained in one child
    int m = (r.first + r.second) / 2;
    auto left = std::make_pair(r.first, m);
    auto right = std::make_pair(m + 1, r.second);
    auto inside = [&](auto a, auto b) {
      return a.first <= b.first and b.second <= a.second; // is b inside a
    };

    if (inside(left, q)) {
      return segment_sum(left_child(k), left, q);
    } else if (inside(right, q)) {
      return segment_sum(right_child(k), right, q);
    }

    // case 3: query intersects both children
    auto left_intersection =
        std::make_pair(std::max(q.first, left.first), left.second);
    auto right_intersection =
        std::make_pair(right.first, std::min(q.second, right.second));
    return segment_sum(left_child(k), left, left_intersection) +
           segment_sum(right_child(k), right, right_intersection);
  }

  void update_node(int k, int l, int r, int i, int y) {
    t[k] += y;
    if (l == r)
      return;
    int m = (l + r) / 2;
    if (i <= m)
      update_node(left_child(k), l, m, i, y);
    else
      update_node(right_child(k), m + 1, r, i, y);
  }

  int left_child(int k) { return 2 * k; }
  int right_child(int k) { return 2 * k + 1; }
};

int main() {
  std::vector<int> x0{1, 3, -5, 8, 0, -1, 7};
  auto t = tree(x0);
  int y0 = t.partial_sum(3);
  t.add(2, 3);
  int y1 = t.partial_sum(3);
  return 0;
}