#include <stack>

// exercise 3-1
// when an open paren is hit, push the current index to the stack.
// when a close paren is hit, try to pop from the stack.
// if the stack is empty when popping, that's an unbalanced string (dangling close paren).
// if the stack is not empty after going through the string, that's an unbalanced string (unmatched open paren).
bool balanced_parens(std::string& s) {
    std::stack<size_t> st;
    size_t i{0};
    for (char c : s) {
        if (c == '(') {
            st.push(i);
        } else {
            if (st.empty())
                return false;
            st.pop();
        }
        i++;
    }
    return st.empty();
}

int main() {
    // exercise 3-1
    std::string s0{"((())())()"};
    bool b0 = balanced_parens(s0);
    std::string s1{")()("};
    bool b1 = balanced_parens(s1);
    std::string s2{"())"};
    bool b2 = balanced_parens(s2);

    return 0;
}
