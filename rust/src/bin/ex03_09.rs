// exercise 3-9
// given a dictionary of words and a sequence of digits, list
// all matching words when typing the sequence in a telephone
// keypad.

// we can model the hash table as a hashmap
use std::collections::HashMap;

// with 3 to 4 possible letters associated to each digit, given
// a sequence of s digits there are up to 4^s possible combinations
// of letters to look up. This will not do.
// our strategy will instead be to hash all the dictionary entries
// by mapping each word to its keypad representation. If the average
// length of the words in the dictionary is m, and there are n words
// in total, hashing all of them will be O(mn), assuming it takes O(m)
// to hash a word of length m. We will collect all words mapping to
// each sequence of digits into a vector, so when querying we will
// get them all at once in O(1) time.

static WORDS: [&str; 26] = [
    "any", "ale", "bob", "box", "sky", "tree", "map", "dog", "yak", "boy", "bye", "tie", "son",
    "bed", "fridge", "pyramid", "polynomial", "number", "string", "language", "dad", "nom", "fab",
    "bff", "def", "fed"
];

fn hash_char(c: char) -> char {
    match c {
        'a' | 'b' | 'c' => '2',
        'd' | 'e' | 'f' => '3',
        'g' | 'h' | 'i' => '4',
        'j' | 'k' | 'l' => '5',
        'm' | 'n' | 'o' => '6',
        'p' | 'q' | 'r' | 's' => '7',
        't' | 'u' | 'v' => '8',
        'w' | 'x' | 'y' | 'z' => '9',
        _ => unreachable!(),
    }
}

fn hash_dictionary() -> HashMap<String, Vec<String>> {
    let mut result: HashMap<String, Vec<String>> = HashMap::new();
    for (key, word) in WORDS.iter().map(|w| {
        let hash: String = w.chars().map(hash_char).collect();
        (hash, w.to_string())
    }) {
        result.entry(key).or_default().push(word);
    }
    result
}

fn punch_keypad(sequence: &str) -> Vec<String> {
    match hash_dictionary().get(sequence) {
        Some(words) => words.clone(),
        None => vec![]
    }
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ex03_09() {
        assert_eq!(punch_keypad("269"), vec!["any", "box", "boy"]);
        assert_eq!(punch_keypad("374343"), vec!["fridge"]);
        assert_eq!(punch_keypad("333"), vec!["def", "fed"]);
    }
}