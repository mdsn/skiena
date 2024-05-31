// exercise 4-17, find the k smallest elements in an unsorted array in
// O(n + k log n) time.
#include <vector>

int left(int i) { return 2 * i + 1; }
int right(int i) { return 2 * i + 2; }

int min_of_three(std::vector<int> &xs, int i) {
  int ix = i;
  int l = left(i), r = right(i);
  if (l < xs.size() and xs[l] < xs[ix])
    ix = l;
  if (r < xs.size() and xs[r] < xs[ix])
    ix = r;
  return ix;
}

void bubble_down(std::vector<int> &xs, int i) {
  int min = min_of_three(xs, i);
  if (min != i) {
    std::swap(xs[i], xs[min]);
    bubble_down(xs, min);
  }
}

// heapify in asymptotically linear time
void heapify_fast(std::vector<int> &xs) {
  for (int i = xs.size()/2; i >= 0; i--)
    bubble_down(xs, i);
}

// remove the element at the front, O(log n)
void pop(std::vector<int> &xs) {
  xs[0] = xs[xs.size()-1];
  xs.pop_back();
  bubble_down(xs, 0);
}

int main() {
  std::vector<int> xs{1783, 1804, 1776, 1963, 1492,
                      1918, 1941, 1945, 1865, 2001};
  std::vector<int> ys;
  heapify_fast(xs);
  for (int k = 0; k < 4; k++) {
    ys.push_back(xs[0]);
    pop(xs);
  }
  return 0;
}