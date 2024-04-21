use std::collections::LinkedList;

// exercise 3-4
// get the O(1) find_min by using a stack lol
#[derive(Debug)]
struct Stack {
    items: LinkedList<i32>,
    min: LinkedList<i32>,
}

// exercise 3-4
impl Stack {
    pub fn new() -> Self {
        Stack {
            items: LinkedList::new(),
            min: LinkedList::new(),
        }
    }

    pub fn push(&mut self, x: i32) {
        self.items.push_back(x);
        self.compare_and_set_min(x);
    }

    pub fn pop(&mut self) -> Option<i32> {
        if let Some(x) = self.items.pop_back() {
            if let Some(y) = self.min.back() {
                if x == *y {
                    self.min.pop_back();
                }
            }
            Some(x)
        } else {
            None
        }
    }

    pub fn find_min(&self) -> Option<&i32> {
        self.min.back()
    }

    fn compare_and_set_min(&mut self, x: i32) {
        match self.min.back() {
            None => self.min.push_back(x),
            Some(y) => {
                if x <= *y {
                    self.min.push_back(x);
                }
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    // exercise 3-4
    #[test]
    fn test_ex3_4() {
        let mut stack = Stack::new();
        stack.push(3);
        stack.push(10);
        stack.push(-1);
        stack.push(0);
        stack.push(-5);
        assert_eq!(stack.find_min(), Some(-5).as_ref());
        assert_eq!(stack.pop(), Some(-5));
        assert_eq!(stack.find_min(), Some(-1).as_ref());
        assert_eq!(stack.pop(), Some(0));
        assert_eq!(stack.find_min(), Some(-1).as_ref());
        assert_eq!(stack.pop(), Some(-1));
        assert_eq!(stack.find_min(), Some(3).as_ref());
        assert_eq!(stack.pop(), Some(10));
        assert_eq!(stack.find_min(), Some(3).as_ref());
        assert_eq!(stack.pop(), Some(3));
        assert_eq!(stack.find_min(), None);
        assert_eq!(stack.pop(), None);
    }
}