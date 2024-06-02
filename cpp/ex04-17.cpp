// exercise 4-17, find the k smallest elements in an unsorted array in
// O(n + k log n) time.
#include <vector>

using vec = std::vector<int>;

int left(int i) { return 2 * i + 1; }
int right(int i) { return left(i) + 1; }

int min3(vec &xs, int i) {
  int ix = i;
  int l = left(i), r = right(i);
  if (l < xs.size() and xs[l] < xs[ix]) ix = l;
  if (r < xs.size() and xs[r] < xs[ix]) ix = r;
  return ix;
}

void sift(vec &xs, int i) {
  int min = min3(xs, i);
  if (min != i) {
    std::swap(xs[i], xs[min]);
    sift(xs, min);
  }
}

void heapify(vec &xs) {
  for (int i = xs.size() / 2; i >= 0; i--)
    sift(xs, i);
}

// remove the element at the front, O(log n)
void pop(vec &xs) {
  xs[0] = xs[xs.size() - 1];
  xs.pop_back();
  sift(xs, 0);
}

int main() {
  vec xs{1783, 1804, 1776, 1963, 1492, 1918, 1941, 1945, 1865, 2001};
  vec ys;
  heapify(xs);
  for (int k = 0; k < 4; k++) {
    ys.push_back(xs[0]);
    pop(xs);
  }
  return 0;
}