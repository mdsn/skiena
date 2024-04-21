// exercise 3-4
// support constant-time deletion on a linked list.

struct node_t {
    int value;
    node_t* next;
    node_t() : value(-1), next(nullptr) {}
    node_t(int x) : value(x), next(nullptr) {}
    node_t(int x, node_t* next) : value(x), next(next) {}
};

struct list_t {
    node_t* head;
    node_t* tail;  // sentinel

    list_t() {
        tail = new node_t();
        head = tail;
    }

    bool empty() { return head == tail; }

    void insert(int x) {
        node_t* p = new node_t(x, head);
        head = p;
    }

    node_t* search(int x) {
        if (empty())
            return nullptr;
        node_t* p = head;
        while (p != tail and p->value != x)
            p = p->next;
        if (p == tail)
            return nullptr;
        return p;
    }

    void remove(node_t* p) {
        if (p == head) {
            head = head->next;
            delete p;
        } else if (p->next == tail) {
            delete tail;
            tail = p;
        } else {
            node_t* q = p->next;
            *p = *q;
            delete q;
        }
    }
};

node_t* search(node_t* l, int x) {
    if (l == nullptr)
        return nullptr;
    while (l->value != x)
        l = l->next;
    return l;
}

int main() {
    list_t l;
    for (int i = 0; i < 5; i++)
        l.insert(i);

    node_t* two = l.search(2);
    l.remove(two);
    node_t* zero = l.search(0);
    l.remove(zero);
    node_t* four = l.search(4);
    l.remove(four);
    return 0;
}