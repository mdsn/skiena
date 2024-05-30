// ch04-heap.cpp a heap data structure and an example sort.
#include <print>
#include <vector>

struct pqueue {
  std::vector<int> xs;

  pqueue() {}
  pqueue(std::initializer_list<int> ys) {
    for (int y : ys) {
      insert(y);
    }
  }

  int parent(int i) {
    if (i == 0)
      return -1;
    return (i - 1) / 2;
  }
  int left(int i) { return 2 * i + 1; }
  int right(int i) { return 2 * i + 2; }

  void insert(int x) {
    xs.push_back(x);
    bubble_up(xs.size() - 1);
  }

  void pop() {
    xs[0] = xs[xs.size() - 1];
    xs.pop_back();
    heapify(0);
  }

  int top() { return xs[0]; }

  bool empty() { return xs.empty(); }

private:
  // restore the heap property by promoting the minimum across i and its
  // children until i is back to the level where it belongs
  void heapify(int i) {
    int min = min_of_three(i);
    if (min != i) {
      std::swap(xs[i], xs[min]);
      heapify(min);
    }
  }

  // promote a higher priority element to the level of the heap where it belongs
  void bubble_up(int i) {
    int p = parent(i);
    if (p == -1)
      return;
    if (xs[p] > xs[i]) {
      std::swap(xs[p], xs[i]);
      bubble_up(p);
    }
  }

  // index of the minimum element between a node and its two children
  int min_of_three(int i) {
    int min_ix = i;
    int l = left(i);
    int r = right(i);
    if (l <= xs.size() and xs[min_ix] > xs[l])
      min_ix = l;
    if (r <= xs.size() and xs[min_ix] > xs[r])
      min_ix = r;
    return min_ix;
  }
};

int main() {
  pqueue h{1783, 1804, 1776, 1963, 1492, 1918, 1941, 1945, 1865, 2001};
  // heapsort!
  // 1492 1776 1783 1804 1865 1918 1941 1945 1963 2001
  while (!h.empty()) {
    std::print("{} ", h.top());
    h.pop();
  }
  std::println("");
  return 0;
}