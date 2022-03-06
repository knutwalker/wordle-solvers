//! Various method to prepare a list of words into an FST that can be used with the [`Wordle`] automaton

use fst::Map;
use std::{collections::HashSet, fmt::Display};

/// Clean a word list by removing all words of the wrong length, all plural forms, and all blocked words
pub fn clean_word_list<I, F, const N: usize>(words: I, mut block_list: F) -> Vec<String>
where
    I: IntoIterator,
    I::Item: Into<String> + AsRef<str>,
    F: FnMut(&str) -> bool,
{
    fn valid_word<const N: usize>(word: &str) -> bool {
        (word.len() == N || word.len() == N - 1) && word.bytes().all(|b| matches!(b, b'a'..=b'z'))
    }

    let mut possible_words = Vec::with_capacity(1024);
    let mut possible_stems = HashSet::with_capacity(1024);
    let mut possible_plurals = HashSet::with_capacity(1024);

    for word in words {
        let word_ref = word.as_ref();

        if valid_word::<N>(word_ref) && !block_list(word_ref) {
            let word: String = word.into();
            if word.len() == N - 1 {
                let _ = possible_stems.insert(word);
            } else {
                let idx = possible_words.len();
                if word.ends_with('s') {
                    let _ = possible_plurals.insert(idx);
                }
                possible_words.push(word);
            }
        }
    }

    let mut words = Vec::with_capacity(possible_words.len());

    for (idx, word) in possible_words.into_iter().enumerate() {
        if possible_plurals.contains(&idx) && possible_stems.contains(&word[..N - 1]) {
            continue;
        }
        words.push(word);
    }

    words
}

/// Score a wordlist by counting letter frequency
pub fn score_word_list<W, const N: usize>(words: Vec<W>) -> impl Iterator<Item = (W, u64)>
where
    W: AsRef<str>,
{
    // build frequency table of letters at their respective position
    let mut letter_pos_frequency = [[0_u32; 26]; N];

    for word in &words {
        let word = word.as_ref();
        for (pos, byte) in word.bytes().enumerate() {
            let idx = (byte - b'a') as usize;
            letter_pos_frequency[pos][idx] += 1;
        }
    }

    words.into_iter().map(move |word| {
        let mut freq = [0; 26];
        for (pos, b) in word.as_ref().bytes().enumerate() {
            let idx = (b - b'a') as usize;
            // no adding, duplicate letters should not contribute multiple times
            freq[idx] = letter_pos_frequency[pos][idx];
        }
        (word, freq.into_iter().map(u64::from).sum())
    })
}

/// Build an FST from a scored and sorted word list
///
/// # Errors
///
/// [`UnsortedInput`] if the input is not sorted
pub fn build_fst<I, K>(words: I) -> Result<Map<Vec<u8>>, UnsortedInput>
where
    I: IntoIterator<Item = (K, u64)>,
    K: AsRef<[u8]>,
{
    Map::from_iter(words).map_err(UnsortedInput)
}

#[derive(Debug)]
/// Word list input is not sorted
pub struct UnsortedInput(fst::Error);

impl Display for UnsortedInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad("The word list input is not sorted")
    }
}

impl std::error::Error for UnsortedInput {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.0)
    }
}
