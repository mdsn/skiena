#include <stack>

// exercise 3-3
// traverse the list pushing nodes into a stack, then link them in LIFO order
struct list_t {
    int value;
    list_t* next;
    list_t(int x) : value(x), next(nullptr) {}
    list_t(int x, list_t *next) : value(x), next(next) {}
};

list_t* reverse_list(list_t* l) {
    if (l == nullptr)
        return nullptr;
    std::stack<list_t*> s;
    while (l->next != nullptr) {
        s.push(l);
        l = l->next;
    }
    list_t* p = l;
    while (!s.empty()) {
        p->next = s.top();
        p = p->next;
        s.pop();
    }
    p->next = nullptr;
    return l;
}

int main() {
    // exercise 3-3
    list_t* two = new list_t(2);
    list_t* one = new list_t(1, two);
    list_t* zero = new list_t(0, one);
    list_t* reversed = reverse_list(zero);

    return 0;
}
