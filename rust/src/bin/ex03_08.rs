// exercise 3-8
// design a data structure that models a game of tic tac toe for n rows/columns
// and reports if the last move was a winning move in constant time.
// for simplicity we can ignore the actual board state and assume all moves are legal.

#[derive(Debug, Clone, Copy)]
enum Player {
    X = 0,
    O = 1,
}

// counter in a row/col/diag indexed by the Player enumeration
#[derive(Debug, Clone)]
struct Counter {
    counter: [u32; 2],
}

impl Counter {
    fn new() -> Self {
        Counter { counter: [0; 2] }
    }
}

type Move = (usize, usize, Player);

// for a game in an n x n board, we need 2n+2 counters
// - n rows
// - n columns
// - 2 diagonals
// each counter keeping track of how many xs and os have been placed on that row/col/diag.
// the last move on the board won the game if it brought its related row/col/diag (if it
// was on that diagonal) counter to n.
#[derive(Debug)]
struct TicTacToe {
    n: usize,
    rows: Vec<Counter>,
    cols: Vec<Counter>,
    // diag[0]: from lower left to top right: (0, n-1) -> (n-1, 0)
    // diag[1]: from top left to lower right: (0, 0)   -> (n-1, n-1)
    diag: [Counter; 2],
    last_move: Option<Move>,
}

impl TicTacToe {
    pub fn new(n: usize) -> Self {
        let mut rows = Vec::with_capacity(n);
        let mut cols = Vec::with_capacity(n);
        rows.resize(n, Counter::new());
        cols.resize(n, Counter::new());
        TicTacToe {
            n,
            rows,
            cols,
            diag: [Counter::new(), Counter::new()],
            last_move: None,
        }
    }

    pub fn play(&mut self, moves: &Vec<Move>) -> bool {
        for (row, col, player) in moves {
            self.play_one(*row, *col, *player);
        }
        if let Some((row, col, player)) = self.last_move {
            let n = self.n.try_into().unwrap();
            let r = self.rows[row].counter[player as usize];
            let c = self.cols[col].counter[player as usize];

            let is_diag0 = row + col == self.n - 1;
            let d0 = self.diag[0].counter[player as usize];

            let is_diag1 = row == col;
            let d1 = self.diag[1].counter[player as usize];

            return r == n || c == n || is_diag0 && d0 == n || is_diag1 && d1 == n;
        }
        false
    }

    fn play_one(&mut self, row: usize, col: usize, player: Player) {
        self.rows[row].counter[player as usize] += 1;
        self.cols[col].counter[player as usize] += 1;
        // these two ifs are not mutually exclusive because for odd n the
        // center square is shared by both diagonals.
        if row + col == self.n - 1 {
            self.diag[0].counter[player as usize] += 1;
        }
        if row == col {
            self.diag[1].counter[player as usize] += 1;
        }
        self.last_move = Some((row, col, player));
    }
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ex03_08() {
        let mut ttt = TicTacToe::new(3);
        assert!(ttt.play(&vec![
            (0, 0, Player::X), // left top
            (2, 1, Player::O), // right center
            (1, 2, Player::X), // center bottom
            (0, 1, Player::O), // left center
            (2, 0, Player::X), // right top
            (1, 1, Player::O), // center center, checkmate
        ]));
        assert!(!ttt.play(&vec![
            (0, 0, Player::X), // left top
            (2, 1, Player::O), // right center
            (1, 2, Player::X), // center bottom
            (0, 1, Player::O), // left center
            (2, 0, Player::X), // right top
        ]));
    }
}