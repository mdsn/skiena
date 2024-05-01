// exercise 3-26
// Consider an unsorted array, to make things easier, no elements repeat. We
// want to build a data structure that will allow us to query for the minimum
// element within a range [i, j] in O(log n) time, taking O(n) space.
//
// We will put together a tree of pairs (i, x), where i is the index of the
// element in the input array, and x is the value. The root will be the
// minimum element in the array, that is, in the range [0, n-1]. This element
// logically partitions the array and leaves subarrays [0, i-1] and [i+1, n-1]
// to its left and right. The left and right subtrees, naturally, will hold
// as their roots the minimum element of their respective subranges, and so on
// and so forth.
//
// Querying is straightforward. If the root of the tree is included in the
// range, we know the (global) minimum must be the minimum in the range. If
// the root index is not in the range, then the interval must be either entirely
// to the left or the right of the root's index. Traverse down the tree until
// hitting a node whose index is included in the interval. This will be the
// first minimum to belong in the interval, and therefore the minimum we want.
//
// To build the structure, we first pair up each element with its index into
// a new array (O(n)) and sort it (O(n log n)). Then we insert each pair into
// the tree based on indices (O(n log n)). The minimum will be inserted first,
// the second minimum after that, and so on. Overall, building the structure
// is O(n log n) time; sorting and the n O(log n) insertions dominate.
#include <vector>

struct tree {
    size_t index;
    int value;
    tree *left;
    tree *right;

    tree(size_t index, int value) : index(index), value(value), left(nullptr), right(nullptr) {}
};

tree *make_tree(std::vector<int> &xs) {
    std::vector<std::pair<size_t, int>> ordered;
    size_t i = 0;
    for (auto x: xs) {
        ordered.push_back(std::make_pair(i, x));
        i++;
    }
    std::sort(ordered.begin(), ordered.end(), [](auto l, auto r) {
        return l.second < r.second;
    });

    tree *t = nullptr;
    tree **p = &t;
    for (auto [i, x]: ordered) {
        while (*p != nullptr) {
            if (i < (*p)->index)
                p = &(*p)->left;
            else
                p = &(*p)->right;
        }
        *p = new tree(i, x);
        p = &t;
    }
    return t;
}

int query(tree *t, size_t i, size_t j) {
    while (!(i <= t->index and t->index <= j)) {
        if (i < t->index)
            t = t->left;
        else
            t = t->right;
    }
    return t->value;
}

int main() {
    std::vector<int> xs{-1, 8, 5, 7, -10, 9, 7, 16, -27, 32, 56, 0, 6};
    tree *t = make_tree(xs);
    int minus_ten = query(t, 3, 7);
    return 0;
}