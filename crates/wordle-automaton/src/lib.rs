/*!
Building blocks to create a Wordle solver.

This crates provides the type [`Wordle`], which is an [`Automaton`].
It accepts only states that satisfy all constraints that are provided from the game.

The expected usage pattern is to search for some accepted state and refine the automaton
based on the feedback that the game provides.

# Example

```rust
use fst::{IntoStreamer, Set, Streamer};
use wordle_automaton::WordleBuilder;

// // Build an FST from a word list - we use some random words
let set = fst::Set::from_iter(&["crush", "morty", "party", "solid"]).unwrap();

// Create an empty Wordle
let wordle = WordleBuilder::new().build();

// Search with the current Wordle
let mut stream = set.search_with_state(wordle.clone()).into_stream();

// Select a guess from the stream
// In the example we take the first one
let (guess, guess_state) = stream.next().unwrap();
// The guess is an Option<SolveState> where a None represents an invalid match
// The fst crate should take care to not return invalid matches, so it should be safe to unwrap
let guess_state = guess_state.unwrap();

// In the first round, the guess is the first word, since all are valid
assert_eq!(guess, b"crush");

// Present the guess to the game and gather feedback
let mut next = WordleBuilder::from(wordle, guess_state);

// Let's say the correct word is 'party'

// The first letter 'c' is not in the word at all, it can _never_ be a part of the solution
next.never(guess[0]);

// The second letter 'r' is in the word, but in the _wrong position_
// The position is 0-based, corresponding to the byte index
next.wrong_pos(1, guess[1]);

// None of the following letters are part of the solution, we can eliminate them in bulk
next.never_all(&guess[2..]);


// let's try the next round
let wordle = next.build();

let mut stream = set.search_with_state(wordle.clone()).into_stream();
let (guess, guess_state) = stream.next().unwrap();
let guess_state = guess_state.unwrap();

// the next valid guess is 'morty' as 'crush' is eliminated, as is 'solid'
assert_eq!(guess, b"morty");

// Present the guess to the game for feedback
let mut next = WordleBuilder::from(wordle, guess_state);
// 'm' and 'o' are not in 'party'
next.never_all(&guess[..2]);

// The remaining letters are all in their correct position
next.correct_pos(2, guess[2]);
next.correct_pos(3, guess[3]);
next.correct_pos(4, guess[4]);

// Let's try the final round
let wordle = next.build();
let mut stream = set.search_with_state(wordle.clone()).into_stream();
let (guess, guess_state) = stream.next().unwrap();
let guess_state = guess_state.unwrap();

// Only 'party' remains as a candidate that fulfills all requirements
assert_eq!(guess, b"party");

// after asking the game, we can verify that we have arrived at a solution
let mut solution = WordleBuilder::from(wordle, guess_state);
solution.correct_pos(0, guess[0]);
solution.correct_pos(1, guess[1]);

// We don't need to add all the remaining characters, as they are already known to be correct
let solution = solution.build();
assert!(solution.is_solved());
assert_eq!(solution.decode_str(), String::from("party"));
```
*/

#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#![warn(
    bad_style,
    const_err,
    dead_code,
    improper_ctypes,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    no_mangle_generic_items,
    non_shorthand_field_patterns,
    overflowing_literals,
    path_statements,
    patterns_in_fns_without_body,
    private_in_public,
    rust_2018_idioms,
    trivial_casts,
    trivial_numeric_casts,
    unconditional_recursion,
    unsafe_code,
    unused_allocation,
    unused_comparisons,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_parens,
    unused_qualifications,
    unused_results,
    unused,
    while_true
)]

use fst::Automaton;
use std::num::NonZeroU32;

pub mod types;

use types::{Constraint, Letter, LetterList, LetterSet};

/// An automaton that matches valid guesses
#[allow(missing_copy_implementations)]
#[derive(Clone, Debug)]
pub struct Wordle {
    never: LetterSet,
    eventually: LetterList,
    positions: [Constraint; 5],
}

const _: () = assert!(
    std::mem::size_of::<Wordle>() == 28,
    "Wordle should be 28 bytes"
);

impl Wordle {
    /// Create a new automaton without any constraints
    #[must_use]
    pub const fn new() -> Self {
        Self {
            never: LetterSet::new(),
            eventually: LetterList::new(),
            positions: [Constraint::new(); 5],
        }
    }

    /// Check if the automaton would only match the full solution
    #[must_use]
    pub fn is_solved(&self) -> bool {
        self.positions.iter().all(|p| !p.is_free())
    }

    /// Test if the automaton matches the solution for a single position
    ///
    /// # Panics
    ///
    /// Panics if the `pos` is out of bounds (5..)
    #[must_use]
    pub const fn has_solution_at(&self, pos: usize) -> bool {
        !self.positions[pos].is_free()
    }

    /// Iterate over all bytes in the full solution
    ///
    /// # Panics
    ///
    /// Panics if the automaton does not have a solution, i.e. [`Wordle::is_solved()`] must return `true`
    pub fn decode(self) -> impl Iterator<Item = u8> {
        let mut ev = LetterList::new();
        for pos in self.positions {
            assert!(!pos.is_free(), "Trying to decode an unsolved wordle");
            ev = ev.add(pos.must_letter());
        }

        ev.iter().map(u8::from)
    }

    /// Create a new string from the full solution
    ///
    /// # Panics
    ///
    /// Panics if the automaton does not have a solution, i.e. [`Wordle::is_solved()`] must return `true`
    #[must_use]
    pub fn decode_str(self) -> String {
        self.decode().map(char::from).collect()
    }
}

/// A Builder to create instances of the [`Wordle`] automaton.
///
/// # Examples
///
/// ```rust
/// # use wordle_automaton::WordleBuilder;
/// let wordle = WordleBuilder::new().never_all(b"nope").correct_pos(1, b'f').wrong_pos(2, b'x').build();
/// ```
#[derive(Clone, Debug)]
pub struct WordleBuilder(Wordle);

impl WordleBuilder {
    /// Create a new builder without any constraints
    #[must_use]
    pub const fn new() -> Self {
        Self(Wordle::new())
    }

    /// Create a new builder based on existing constraints
    #[must_use]
    pub const fn from(mut wordle: Wordle, state: SolveState) -> Self {
        wordle.eventually = state.eventually;
        Self(wordle)
    }

    /// Signal that the given `letter` is never part of any solution on any position
    ///
    /// This corresponds to the darkened result is the game
    pub fn never(&mut self, letter: u8) -> &mut Self {
        self.0.never = self.0.never.add(Letter::new(letter));
        self
    }

    /// Signal that any of the given letters are never part of any solution on any position
    ///
    /// This is equivalent to calling [`WordleBuilder::never()`] on every item of the iterator
    pub fn never_all(&mut self, letters: impl AsRef<[u8]>) -> &mut Self {
        self.0.never = letters
            .as_ref()
            .iter()
            .copied()
            .map(Letter::new)
            .fold(self.0.never, LetterSet::add);
        self
    }

    /// Signal that the given letter is correct for the given position
    ///
    /// This is equivalent to the green result in the game
    pub fn correct_pos(&mut self, pos: usize, letter: u8) -> &mut Self {
        self.0.positions[pos] = self.0.positions[pos].must(Letter::new(letter));
        self
    }

    /// Signal that the given letter is in a wrong position but part of the solution
    ///
    /// This is equivalent to the yellow result in the game
    pub fn wrong_pos(&mut self, pos: usize, letter: u8) -> &mut Self {
        let letter = Letter::new(letter);
        self.0.eventually = self.0.eventually.add(letter);
        self.0.positions[pos] = self.0.positions[pos].must_not(letter);
        self
    }

    /// Build the final automaton for the added constraints
    pub fn build(&mut self) -> Wordle {
        std::mem::replace(&mut self.0, Wordle::new())
    }

    /// Return the current automaton
    ///
    /// The automaton is in a valid state, i.e. can be used to search in an FST
    #[must_use]
    pub const fn current(&self) -> &Wordle {
        &self.0
    }
}

/// The state to use during automaton matching
///
/// Cannot be used directly, but it is returned from [`fst::Set::search_with_state`] and can be
/// passed to [`WordleBuilder::from`] to refine a search.
#[derive(Copy, Clone, Debug)]
pub struct SolveState {
    pos: NonZeroU32,
    eventually: LetterList,
}

const _: () = assert!(
    std::mem::size_of::<SolveState>() == 8,
    "SolveState should be 8 bytes"
);
const _: () = assert!(
    std::mem::size_of::<Option<SolveState>>() == 8,
    "SolveState should have a niche"
);

impl Automaton for Wordle {
    type State = Option<SolveState>;

    fn start(&self) -> Self::State {
        Some(SolveState {
            pos: NonZeroU32::new(1).unwrap(),
            eventually: self.eventually,
        })
    }

    fn is_match(&self, state: &Self::State) -> bool {
        state.map_or(false, |s| s.pos.get() == 6)
    }

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        // we are already a non-match
        let state = (*state)?;

        // cannot match past 5 letters
        let pos = state.pos.get();
        if pos == 6 {
            return None;
        }

        // byte is invalid, i.e. not [a-zA-Z]
        let letter = Letter::try_new(byte)?;

        // letter is never a possibility
        if self.never.contains(letter) {
            return None;
        }

        let current_pos = (pos - 1) as usize;

        let slot = self.positions[current_pos];
        // letter is not possible in the current position
        if !slot.accept(letter) {
            return None;
        }

        // check if the letter is one that we eventually need to add
        let (removed, eventually) = state.eventually.remove(letter);
        if !removed {
            // need to check that we have enough free slots to eventually fill those
            let need_to_fill = eventually.len();
            let available = self.positions[current_pos + 1..]
                .iter()
                .map(|p| u32::from(p.is_free()))
                .sum::<u32>();

            // there are not enough slots to eventually fill all undecided ones
            if available < need_to_fill {
                return None;
            }
        }

        let pos = NonZeroU32::new(pos + 1).expect("Adding one to any value cannot be zero");
        Some(SolveState { pos, eventually })
    }

    fn can_match(&self, state: &Self::State) -> bool {
        state.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fst::{IntoStreamer, Set};

    #[test]
    fn test_no_conditions() {
        let wordle = WordleBuilder::new().build();

        test_fst(wordle, ["abcd", "abcde", "abcdef"], &["abcde"]);
    }

    #[test]
    fn test_never_accept_a() {
        let wordle = WordleBuilder::new().never(b'a').build();

        test_fst(wordle, ["abcde", "bcdef", "cdefa"], &["bcdef"]);
    }

    #[test]
    fn test_never_accept_abcd() {
        let wordle = WordleBuilder::new().never_all("abcd").build();

        test_fst(wordle, ["abcde", "cdefg", "efghi"], &["efghi"]);
    }

    #[test]
    fn test_require_a_in_third_pos() {
        let wordle = WordleBuilder::new().correct_pos(2, b'a').build();

        test_fst(
            wordle,
            ["abcde", "bacde", "bcade", "bcdae", "bcdea"],
            &["bcade"],
        );
    }

    #[test]
    fn test_dont_allow_a_in_third_pos() {
        let wordle = WordleBuilder::new().wrong_pos(2, b'a').build();

        test_fst(
            wordle,
            ["abcde", "bacde", "bcade", "bcdae", "bcdea"],
            &["abcde", "bacde", "bcdae", "bcdea"],
        );
    }

    #[test]
    fn test_use_partially_solved_letter() {
        let wordle = WordleBuilder::new()
            .correct_pos(0, b'a')
            .correct_pos(1, b'b')
            .correct_pos(2, b'c')
            // if we disallow d at the fourth pos, we must require it at the last position
            .wrong_pos(3, b'd')
            .build();

        test_fst(wordle, ["abcde", "abced", "abcef"], &["abced"]);
    }

    fn test_fst<'a>(
        wordle: Wordle,
        words: impl IntoIterator<Item = &'a str>,
        expected: &[&'a str],
    ) {
        fn inner_test<'a>(
            wordle: Wordle,
            words: impl IntoIterator<Item = &'a str>,
            expected: &[&'a str],
        ) -> fst::Result<()> {
            let set = Set::from_iter(words)?;
            let stream = set.search(wordle).into_stream();
            let matches = stream.into_strs()?;

            assert_eq!(&matches[..], expected);

            Ok(())
        }

        inner_test(wordle, words, expected).unwrap();
    }
}
