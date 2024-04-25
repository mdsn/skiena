// exercise 3-14
// merge two binary search trees into an ordered doubly linked list.
#include <functional>
#include <fmt/core.h>


struct tree {
    struct treenode {
        int value;
        treenode* left;
        treenode* right;

        treenode(int value) : value(value), left(nullptr), right(nullptr) {}

        void traverse(std::function<void(treenode* t)> f) {
            if (left != nullptr)
                left->traverse(f);
            f(this);
            if (right != nullptr)
                right->traverse(f);
        }
    };

    treenode *root;
    tree() : root(nullptr) {}

    void insert(int x) {
        treenode** p = &root;
        while (*p != nullptr) {
            if (x < (*p)->value)
                p = &(*p)->left;
            else
                p = &(*p)->right;
        }
        *p = new treenode(x);
    }

    void traverse(std::function<void(treenode*)> f) {
        if (root != nullptr)
            root->traverse(f);
    }
};

struct list {
    struct listnode {
        listnode* prev;
        listnode* next;
        int value;

        listnode(int value) : value(value), prev(nullptr), next(nullptr) {}
    };

    listnode* head;
    listnode* tail;

    list() : head(nullptr), tail(nullptr) {}

    void append(int value) {
        listnode* p = new listnode(value);
        if (head == nullptr) {
            head = p;
        } else {
            tail->next = p;
            p->prev = tail;
        }
        tail = p;
    }

    void traverse(std::function<void(listnode*)> f) {
        listnode* p = head;
        while (p != nullptr) {
            f(p);
            p = p->next;
        }
    }
};

int main() {
    std::vector<int> xs{3, 1, 4, 5, 9, 0, -1, 7, 33};
    std::vector<int> ys{8, 6, 10, -3, 92, 2};
    tree t, u;
    for (int x : xs) t.insert(x);
    for (int y : ys) u.insert(y);

    // Merge by traversing one tree and inserting into the other
    u.traverse([&t](auto node) {
        t.insert(node->value);
    });

    // Traverse the final merged tree to build up the list by appends
    list l;
    t.traverse([&l](auto node) {
        l.append(node->value);
    });

    l.traverse([](auto node) {
        fmt::print("{} ", node->value);
    });
    fmt::print("\n");
    return 0;
}