use std::collections::VecDeque;

// exercise 3-1
#[derive(Debug, Eq, PartialEq)]
enum IsBalanced {
    // The string is balanced and properly nested
    Balanced,
    // The string is unbalanced, usize indicates index of first offending paren
    Unbalanced(usize),
}

// exercise 3-1
fn balanced_parens(s: &str) -> IsBalanced {
    let mut stack: VecDeque<usize> = VecDeque::new();
    for (i, c) in s.chars().enumerate() {
        match c {
            '(' => stack.push_back(i),
            ')' => {
                if stack.is_empty() {
                    return IsBalanced::Unbalanced(i);
                }
                stack.pop_back();
            }
            _ => unreachable!(),
        }
    }

    match stack.pop_front() {
        None => IsBalanced::Balanced,
        Some(i) => IsBalanced::Unbalanced(i),
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    // exercise 3-1
    #[test]
    fn test_ex3_1() {
        assert_eq!(balanced_parens("((())())()"), IsBalanced::Balanced);
        assert_eq!(balanced_parens(")()("), IsBalanced::Unbalanced(0));
        assert_eq!(balanced_parens("())"), IsBalanced::Unbalanced(2));
        assert_eq!(balanced_parens("((()()"), IsBalanced::Unbalanced(0));
    }
}