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

// Create an empty Wordle with support for 5 letter words
let wordle = WordleBuilder::<5>::new().build();

// Search with the current Wordle
let mut stream = set.search(wordle.clone()).into_stream();

// Select a guess from the stream
// In the example we take the first one
let guess = stream.next().unwrap();;

// In the first round, the guess is the first word, since all are valid
assert_eq!(guess, b"crush");

// Present the guess to the game and gather feedback
let mut next = WordleBuilder::from(wordle);

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

let mut stream = set.search(wordle.clone()).into_stream();
let guess = stream.next().unwrap();

// the next valid guess is 'morty' as 'crush' is eliminated, as is 'solid'
assert_eq!(guess, b"morty");

// Present the guess to the game for feedback
let mut next = WordleBuilder::from(wordle);
// 'm' and 'o' are not in 'party'
next.never_all(&guess[..2]);

// The remaining letters are all in their correct position
next.correct_pos(2, guess[2]);
next.correct_pos(3, guess[3]);
next.correct_pos(4, guess[4]);

// Let's try the final round
let wordle = next.build();
let mut stream = set.search(wordle.clone()).into_stream();
let guess = stream.next().unwrap();

// Only 'party' remains as a candidate that fulfills all requirements
assert_eq!(guess, b"party");

// after asking the game, we can verify that we have arrived at a solution
let mut solution = WordleBuilder::from(wordle);
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
use std::num::NonZeroUsize;

pub mod prepare;
mod types;

pub use types::{Constraint, Letter, LetterList, LetterSet};

/// An automaton that matches valid guesses
#[allow(missing_copy_implementations)]
#[derive(Clone, Debug)]
pub struct Wordle<const N: usize> {
    never: LetterSet,
    eventually: LetterList<N>,
    positions: [Constraint; N],
}

impl<const N: usize> Wordle<N> {
    /// Create a new automaton without any constraints
    #[must_use]
    pub const fn new() -> Self {
        Self {
            never: LetterSet::new(),
            eventually: LetterList::new(),
            positions: [Constraint::new(); N],
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
    /// Panics if the `pos` is out of bounds (N..)
    #[must_use]
    pub const fn has_solution_at(&self, pos: usize) -> bool {
        !self.positions[pos].is_free()
    }

    /// Return the solution for a single position, if available
    ///
    /// # Panics
    ///
    /// Panics if the `pos` is out of bounds (N..)
    #[must_use]
    pub const fn solution_at(&self, pos: usize) -> Option<u8> {
        let cons = self.positions[pos];
        if cons.is_free() {
            None
        } else {
            Some(cons.must_letter().into_byte())
        }
    }

    /// Iterate over all bytes in the full solution
    ///
    /// # Panics
    ///
    /// Panics if the automaton does not have a solution, i.e. [`Wordle::is_solved()`] must return `true`
    pub fn decode(&self) -> impl Iterator<Item = u8> + '_ {
        self.positions.iter().map(|cons| {
            assert!(!cons.is_free(), "Trying to decode an unsolved wordle");
            u8::from(cons.must_letter())
        })
    }

    /// Create a new string from the full solution
    ///
    /// # Panics
    ///
    /// Panics if the automaton does not have a solution, i.e. [`Wordle::is_solved()`] must return `true`
    #[must_use]
    pub fn decode_str(&self) -> String {
        self.decode().map(char::from).collect()
    }

    /// Decode a guess for a single position
    ///
    /// The result only makes sense if the result had been see before,
    /// otherwise is may erroneously be reported as 'correct'.
    ///
    /// For a fallible variant, use `[Wordle::try_decode_guess()]`.
    ///
    /// # Panics
    ///
    /// Panics if the guess is not in [a-zA-Z]
    /// Panics if the `pos` is out of bounds (N..)
    #[must_use]
    pub const fn decode_guess(&self, pos: usize, guess: u8) -> Guess {
        let letter = Letter::new(guess);
        if self.never.contains(letter) {
            Guess::Absent
        } else if self.positions[pos].accept(letter) {
            Guess::Correct
        } else {
            Guess::Present
        }
    }

    /// Decode a guess for a single position
    ///
    /// Returns Ok(`[Guess]`) if there is a known result about that guess.
    ///
    /// # Errors
    ///
    /// Returns an error if the guess was never seen at this position
    ///
    /// # Panics
    ///
    /// Panics if the guess is not in [a-zA-Z]
    /// Panics if the `pos` is out of bounds (N..)
    pub const fn try_decode_guess(&self, pos: usize, guess: u8) -> Result<Guess, GuessError> {
        let guess = self.decode_guess(pos, guess);
        match guess {
            Guess::Correct if self.positions[pos].is_free() => Err(GuessError::NeverSeen),
            Guess::Present if !self.positions[pos].is_free() => Err(GuessError::NeverSeen),
            guess => Ok(guess),
        }
    }
}

/// A Builder to create instances of the [`Wordle`] automaton.
///
/// # Examples
///
/// ```rust
/// # use wordle_automaton::WordleBuilder;
/// let wordle = WordleBuilder::<5>::new().never_all(b"nope").correct_pos(1, b'f').wrong_pos(2, b'x').build();
/// ```
#[derive(Clone, Debug)]
pub struct WordleBuilder<const N: usize>(Wordle<N>, LetterList<N>);

impl<const N: usize> WordleBuilder<N> {
    /// Create a new builder without any constraints
    #[must_use]
    pub const fn new() -> Self {
        Self(Wordle::new(), LetterList::new())
    }

    /// Create a new builder based on existing constraints
    #[must_use]
    pub fn from(mut wordle: Wordle<N>) -> Self {
        let old_eventually = std::mem::replace(&mut wordle.eventually, LetterList::new());
        Self(wordle, old_eventually)
    }

    /// Signal that the given `letter` is never part of any solution on any position
    ///
    /// This corresponds to the darkened result is the game
    ///
    /// # Panics
    /// Panics if the letter is not in [a-zA-Z]
    pub fn never(&mut self, letter: u8) -> &mut Self {
        self.0.never = self.0.never.add(Letter::new(letter));
        self
    }

    /// Signal that any of the given letters are never part of any solution on any position
    ///
    /// This is equivalent to calling [`WordleBuilder::never()`] on every item of the iterator
    ///
    /// # Panics
    /// Panics if any of the letters is not in [a-zA-Z]
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
    ///
    /// # Panics
    /// Panics if the letter is not in [a-zA-Z]
    /// Panics if the `pos` is out of bounds (N..)
    pub fn correct_pos(&mut self, pos: usize, letter: u8) -> &mut Self {
        self.0.positions[pos] = self.0.positions[pos].must(Letter::new(letter));
        self
    }

    /// Signal that the given letter is in a wrong position but part of the solution
    ///
    /// This is equivalent to the yellow result in the game
    ///
    /// # Panics
    /// Panics if the letter is not in [a-zA-Z]
    /// Panics if the `pos` is out of bounds (N..)
    pub fn wrong_pos(&mut self, pos: usize, letter: u8) -> &mut Self {
        let letter = Letter::new(letter);
        self.0.eventually.add(letter);
        self.0.positions[pos] = self.0.positions[pos].must_not(letter);
        self
    }

    /// Signal that the given letter is part of the solution
    ///
    /// There is no equivalent in the game. It is either a green or a yello result, without knowing
    /// which one
    ///
    /// # Panics
    /// Panics if the letter is not in [a-zA-Z]
    pub fn eventually(&mut self, letter: u8) -> &mut Self {
        let letter = Letter::new(letter);
        self.0.never = self.0.never.remove(letter);
        self.0.eventually.add(letter);
        self
    }

    /// Build the final automaton for the added constraints
    pub fn build(&mut self) -> Wordle<N> {
        let mut wordle = std::mem::replace(&mut self.0, Wordle::new());

        // if we signal something as 'never' but it's also green or yellow somewhere, we must
        // remove it from 'never' as we would otherwise not find a match
        let mut never = wordle
            .eventually
            .iter()
            .fold(wordle.never, |n, l| n.remove(*l));

        // we also keep the orignal never around to pass it to all undecided constraints
        let global_never = never;

        // we also collect a set of all letters that are already solved
        let mut solved = LetterSet::new();

        for cons in wordle.positions.iter_mut() {
            // remove all letters with a known constraint from the global never set
            if cons.is_free() {
                never = never.remove_all(cons.must_not_letters());
                // add all global nevers to the prohibited set of this constraint
                // we need to do this _after_ the previous step
                // otherwise we would always remove all never letters
                *cons = cons.must_not_all(global_never);
            } else {
                never = never.remove(cons.must_letter());
                solved = solved.add(cons.must_letter());
            };
        }

        // we add all previous eventual letters if they are not already handles
        for &letter in self.1.iter() {
            // if the letter needed to be eventually in the result but is not excluded from it
            // we cannot produce a valid result. By blocking all letters, we would never match.
            if never.contains(letter) {
                never = LetterSet::full();
            }
            // add it only if it hasn't been solved now or was already added
            if !solved.contains(letter) {
                wordle.eventually.add_if_absent(letter);
            }
        }

        wordle.never = never;
        wordle
    }

    /// Return the current automaton
    ///
    /// The automaton may not be in a valid state, i.e. should not be used to search in an FST
    #[must_use]
    pub const fn current(&self) -> &Wordle<N> {
        &self.0
    }
}

/// A guess result for a given letter
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Guess {
    /// The letter is never part of the result ('Gray')
    Absent,
    /// The letter is in the result but at the wrong position ('Yellow')
    Present,
    /// The letter is in its correct position ('Green')
    Correct,
}

/// Error results for a guess result for a given letter
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GuessError {
    /// The letter has never been seen
    NeverSeen,
}

#[derive(Clone, Debug)]
#[doc(hidden)]
pub struct SolveState<const N: usize> {
    pos: NonZeroUsize,
    eventually: LetterList<N>,
}

const _: () = assert!(
    std::mem::size_of::<Option<SolveState<5>>>() == std::mem::size_of::<SolveState<5>>(),
    "SolveState should have a niche"
);

impl<const N: usize> Automaton for Wordle<N> {
    type State = Option<SolveState<N>>;

    fn start(&self) -> Self::State {
        Some(SolveState {
            pos: NonZeroUsize::new(1).unwrap(),
            eventually: self.eventually.clone(),
        })
    }

    fn is_match(&self, state: &Self::State) -> bool {
        state.as_ref().map_or(false, |s| s.pos.get() - 1 == N)
    }

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        // we are already a non-match
        let state = state.as_ref()?;

        // cannot match past 5 letters
        let pos = state.pos.get() - 1;
        if pos == N {
            return None;
        }

        // byte is invalid, i.e. not [a-zA-Z]
        let letter = Letter::try_new(byte)?;

        // letter is never a possibility
        if self.never.contains(letter) {
            return None;
        }

        let constraint = self.positions[pos];
        // letter is not possible in the current position
        if !constraint.accept(letter) {
            return None;
        }

        // check if the letter is one that we eventually need to add
        let mut state = state.clone();
        let removed = state.eventually.remove(letter);
        if !removed {
            // need to check that we have enough free slots to eventually fill those
            let need_to_fill = state.eventually.len();
            let available = self.positions[pos + 1..]
                .iter()
                .map(|p| usize::from(p.is_free()))
                .sum::<usize>();

            // there are not enough slots to eventually fill all undecided ones
            if available < need_to_fill {
                return None;
            }
        }

        // adding 1 because we store 1-based, adding another one to actually advance
        state.pos = NonZeroUsize::new(pos + 2).expect("Adding to non-zero value is non-zero");
        Some(state)
    }

    fn can_match(&self, state: &Self::State) -> bool {
        !self.never.is_full() && state.as_ref().map_or(false, |s| s.pos.get() <= N)
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

    #[test]
    fn test_guess_with_repeated_letters_as_yellow() {
        // word is abcde, guess was abcee
        let wordle = WordleBuilder::new()
            .correct_pos(0, b'a')
            .correct_pos(1, b'b')
            .correct_pos(2, b'c')
            .wrong_pos(3, b'e')
            .correct_pos(4, b'e')
            .build();

        // there is actually no correct solution as e may not appear in the 4th pos
        // which means it must appear somewhere else, but all the other positions
        // are already determined

        test_fst(wordle, ["abcde", "abcee"], &[]);

        // if we free up the last slot, we get the correct solution
        let wordle = WordleBuilder::new()
            .correct_pos(0, b'a')
            .correct_pos(1, b'b')
            .correct_pos(2, b'c')
            .wrong_pos(3, b'e')
            .build();

        test_fst(wordle, ["abcde", "abcee"], &["abcde"]);
    }

    #[test]
    fn test_guess_with_repeated_letters_as_gray() {
        // word is abcde, guess was abcee
        let wordle = WordleBuilder::new()
            .correct_pos(0, b'a')
            .correct_pos(1, b'b')
            .correct_pos(2, b'c')
            .never(b'e')
            .correct_pos(4, b'e')
            .build();

        // the e in 4th position is different from the e in last position
        // disallowing any e while another e is known to be valid should
        // still yield a solution

        test_fst(wordle, ["abcde", "abcee"], &["abcde"]);
    }

    #[test]
    fn test_guess_with_repeated_correct_letters_in_wrong_pos() {
        // word is abcde, guess was acebb
        let wordle = WordleBuilder::new()
            .correct_pos(0, b'a')
            .wrong_pos(1, b'c')
            .wrong_pos(2, b'e')
            .wrong_pos(3, b'b')
            .never(b'b')
            .build();

        // the b in 4th position is different from the b in last position
        // disallowing any b while another b is known to be valid eventually should
        // still yield a solution

        test_fst(wordle, ["abcde", "acebb"], &["abcde"]);
    }

    fn test_fst<'a>(
        wordle: Wordle<5>,
        words: impl IntoIterator<Item = &'a str>,
        expected: &[&'a str],
    ) {
        fn inner_test<'a>(
            wordle: Wordle<5>,
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
