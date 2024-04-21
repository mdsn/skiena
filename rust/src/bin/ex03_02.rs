// exercise 3-2
// instead of using a stack, just keep a counter of currently
// open parens (m) and a counter of balanced pairs (n) increased
// whenever one is opened or an expected one is closed.
// at the end, m contains the number of unmatched open parens, so
// subtract that from the total

fn longest_balanced_parens(s: &str) -> usize {
    let mut n = 0;
    let mut m = 0;
    for c in s.chars() {
        match c {
            '(' => {
                m += 1;
                n += 1;
            }
            ')' if m > 0 => {
                m -= 1;
                n += 1;
            }
            _ => unreachable!()
        }
    }
    n - m
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ex3_2() {
        assert_eq!(longest_balanced_parens(")()(())()()))())))("), 12);
    }
}