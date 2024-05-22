// exercise 4-7, compute the h-index of a scientist's papers.
// sorted in descending order, the h-index is the number of elements whose
// value is larger than its (0-based) index.
#include <vector>
#include <algorithm>
#include <print>

int h(std::vector<int>& papers) {
  auto xs = papers;
  std::sort(xs.begin(), xs.end(), std::greater<int>());
  for (int i=0; i < xs.size(); i++) {
    if (xs[i] <= i)
      return i;
  }
  return xs.size();
}

int main() {
  std::vector<int> x0{3,4,3,6,5};
  std::println("6,5,4,3,3 \t\t h = {}, expected 3", h(x0));
  std::vector<int> x1{5,5,5,5,5};
  std::println("5,5,5,5,5 \t\t h = {}, expected 5", h(x1));
  std::vector<int> x2{4,5,2,1,0,1,4};
  std::println("5,4,4,2,1,1,0 \t h = {}, expected 3", h(x2));
  std::vector<int> x3{3,3,3,3,3,3};
  std::println("3,3,3,3,3,3 \t h = {}, expected 3", h(x3));
  std::vector<int> x4{5,5,6,5,5};
  std::println("6,5,5,5,5 \t\t h = {}, expected 5", h(x4));
  std::vector<int> x5{0,0,0,0};
  std::println("0,0,0,0 \t\t h = {}, expected 0", h(x5));
  return 0;
}