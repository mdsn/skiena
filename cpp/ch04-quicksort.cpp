#include <algorithm>
#include <random>
#include <vector>

std::random_device rd;
std::mt19937 g(rd());

using vec = std::vector<int>;

void shuffle(vec &xs) { std::shuffle(xs.begin(), xs.end(), g); }

int sedgewick(vec &xs, int l, int r) {
  int p = l;
  l--;
  r++;
  while (1) {
    do {
      l++;
    } while (xs[l] < xs[p]);
    do {
      r--;
    } while (xs[p] < xs[r]);
    if (l >= r) {
      std::swap(xs[p], xs[r]);
      return r;
    }
    std::swap(xs[l], xs[r]);
  }
}

int lomuto(vec &xs, int l, int r) {
  for (int i = l; i < r; i++) {
    if (xs[i] < xs[r]) {
      std::swap(xs[i], xs[l]);
      l++;
    }
  }
  std::swap(xs[r], xs[l]);
  return l;
}

template <auto partition>
void quicksort(vec &xs, int l, int r) {
  if (r <= l)
    return;
  int p = partition(xs, l, r);
  quicksort<partition>(xs, l, p - 1);
  quicksort<partition>(xs, p + 1, r);
}

template <auto partition>
void quicksort(vec &xs) {
  quicksort<partition>(xs, 0, xs.size() - 1);
}

int main() {
  vec xs{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  shuffle(xs);
  quicksort<sedgewick>(xs);
  return 0;
}