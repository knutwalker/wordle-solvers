//! Types and data structures to help with the automaton implementation

use smallvec::SmallVec;

/// A possible letter, can only be lowercase ASCII characters, i.e. [a-z]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct Letter(u8);

impl Letter {
    /// Create a new letter
    ///
    /// Returns None if the letter is not in [a-zA-Z]
    #[must_use]
    pub const fn try_new(b: u8) -> Option<Self> {
        match b {
            b'a'..=b'z' => Some(Self(b - b'a')),
            b'A'..=b'Z' => Some(Self(b - b'A')),
            _ => None,
        }
    }

    /// Create a new letter
    ///
    /// # Panics
    /// Panics if the letter is not in [a-zA-Z]
    #[must_use]
    pub const fn new(b: u8) -> Self {
        match b {
            b'a'..=b'z' => Self(b - b'a'),
            b'A'..=b'Z' => Self(b - b'A'),
            _ => panic!("Invalid letter, only accept [a-zA-Z]"),
        }
    }
}

impl From<Letter> for u8 {
    fn from(letter: Letter) -> Self {
        b'a' + letter.0
    }
}

/// A set of letters
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct LetterSet(u32);

impl LetterSet {
    /// Create an empty set
    #[must_use]
    pub const fn new() -> Self {
        Self(0)
    }

    /// Test if a letter is contained in this set, O(1)
    #[must_use]
    pub const fn contains(self, letter: Letter) -> bool {
        (self.0 >> letter.0) & 1 == 1
    }

    /// Add a letter to this set, O(1)
    #[must_use]
    pub const fn add(self, letter: Letter) -> Self {
        Self(self.0 | (1 << letter.0))
    }

    /// Add all letter to this set, O(1)
    #[must_use]
    pub const fn add_all(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }

    /// Remove a letter from this set, O(1)
    #[must_use]
    pub const fn remove(self, letter: Letter) -> Self {
        Self(self.0 & !(1 << letter.0))
    }

    /// Remove all letters from this set that are present in the `rhs` set, O(1)
    #[must_use]
    pub const fn remove_all(self, rhs: Self) -> Self {
        Self(self.0 & !rhs.0)
    }
}

/// A list of letters
#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct LetterList<const N: usize>(SmallVec<[Letter; N]>);

impl<const N: usize> LetterList<N> {
    /// Create an empty list
    #[must_use]
    pub const fn new() -> Self {
        Self(SmallVec::new_const())
    }

    /// Return the number of entries in this list
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true iff the list is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Adds a letter in O(1)
    ///
    /// # Panics
    /// Panic if the list is full
    pub fn add(&mut self, letter: Letter) {
        self.0.push(letter);
    }

    /// Removes a letter by comparing the value in O(n)
    ///
    /// Returns (true, newList) if item was in this list, otherwise returns (false, self)
    #[must_use]
    pub fn remove(&mut self, letter: Letter) -> bool {
        if let Some(idx) = self.0.iter().position(|&l| l == letter) {
            let _ = self.0.remove(idx);
            true
        } else {
            false
        }
    }

    /// Create an iterator over all entries in this list.
    #[must_use]
    pub fn iter(&self) -> std::slice::Iter<'_, Letter> {
        self.0.iter()
    }
}

impl<'a, const N: usize> IntoIterator for &'a LetterList<N> {
    type Item = &'a Letter;

    type IntoIter = std::slice::Iter<'a, Letter>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Constraint for a single position
///
/// Combines a [`LetterSet`] with an additional quasi-Option of a single entry
/// The set flags all letters that may never appear on this position (eff. 26 bits for 26 letters)
/// The entry is set if a known letter is at that position (5 bits to write 1 value in 0..26)
/// The last bit encodes whether the set or the entry are in use (1-set, 0-entry)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct Constraint(u32);

impl Constraint {
    /// Create an empty constraint
    #[must_use]
    pub const fn new() -> Self {
        Self(1 << 31)
    }

    /// Test if the letter is accepted by the this constraint
    ///
    /// If the known-entry is set, the letter must match that entry
    /// Otherwise the letter must not be in the set
    #[must_use]
    pub const fn accept(self, letter: Letter) -> bool {
        // cast is safe since we shift by 26, which leaves 6 bits
        #[allow(clippy::cast_possible_truncation)]
        let must = (self.0 >> 26) as u8;
        if must < 32 {
            must == letter.0
        } else {
            !LetterSet(self.0).contains(letter)
        }
    }

    /// Test is the constraint is still unknown, i.e. does not have a known entry
    #[must_use]
    pub const fn is_free(self) -> bool {
        self.0 >> 31 == 1
    }

    /// Set the letter as the known entry
    #[must_use]
    pub const fn must(self, letter: Letter) -> Self {
        Self((letter.0 as u32) << 26 | (self.0 & 0x03FF_FFFF))
    }

    /// Flag a letter as invalid for this constraint
    #[must_use]
    pub const fn must_not(self, letter: Letter) -> Self {
        Self(LetterSet(self.0).add(letter).0)
    }

    /// Flags all letters as invalid for this constraint
    #[must_use]
    pub const fn must_not_all(self, letters: LetterSet) -> Self {
        Self(LetterSet(self.0).add_all(letters).0)
    }

    /// Return the letter that is known to be required
    ///
    /// The result is unreliable if [`Constraint::is_free()`] returns `true`.
    /// _Some_ arbitrary letter is returned.
    #[must_use]
    pub const fn must_letter(self) -> Letter {
        // cast is safe since we shift by 26 and mask by 0x1F, which leaves 5 bits
        #[allow(clippy::cast_possible_truncation)]
        Letter(((self.0 >> 26) & 0x1F) as _)
    }

    /// Returns the letters that are known to not be in this position
    ///
    /// The result is unreliable if [`Constraint::is_free()`] returns `false`.
    /// _Some_ arbitrary letters are returned.
    #[must_use]
    pub const fn must_not_letters(self) -> LetterSet {
        LetterSet(self.0 & 0x03FF_FFFF)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_letter_try_new() {
        assert_eq!(Letter::try_new(b'a'), Some(Letter(0)));
        assert_eq!(Letter::try_new(b'z'), Some(Letter(25)));
        assert_eq!(Letter::try_new(b'A'), Some(Letter(0)));
        assert_eq!(Letter::try_new(b'Z'), Some(Letter(25)));
        assert_eq!(Letter::try_new(b' '), None);
        assert_eq!(Letter::try_new(b'0'), None);
        assert_eq!(Letter::try_new(b'9'), None);
        assert_eq!(Letter::try_new(b'.'), None);
        assert_eq!(Letter::try_new(b'!'), None);
    }

    #[test]
    fn test_letter_new() {
        assert_eq!(Letter::new(b'a'), Letter(0));
        assert_eq!(Letter::new(b'z'), Letter(25));
        assert_eq!(Letter::new(b'A'), Letter(0));
        assert_eq!(Letter::new(b'Z'), Letter(25));
    }

    #[test]
    fn test_letter_into() {
        assert_eq!(u8::from(Letter::new(b'a')), b'a');
        assert_eq!(u8::from(Letter::new(b'z')), b'z');
        assert_eq!(u8::from(Letter::new(b'A')), b'a');
        assert_eq!(u8::from(Letter::new(b'Z')), b'z');
    }

    #[test]
    #[should_panic(expected = "Invalid letter, only accept [a-zA-Z]")]
    const fn test_letter_new_invalid() {
        let _ = Letter::new(b' ');
    }

    #[test]
    fn test_set() {
        let set = LetterSet::new();
        assert!(!set.contains(Letter(0)));
        assert!(!set.contains(Letter(1)));
        let set = set.add(Letter(0));
        assert!(set.contains(Letter(0)));
        assert!(!set.contains(Letter(1)));
        let set = set.add(Letter(1));
        assert!(set.contains(Letter(0)));
        assert!(set.contains(Letter(1)));
        // set semantics
        let set = set.add(Letter(0));
        assert!(set.contains(Letter(0)));
        assert!(set.contains(Letter(1)));
    }

    #[test]
    fn test_set_add_all() {
        let set = LetterSet::new()
            .add(Letter(0))
            .add(Letter(1))
            .add(Letter(2));

        let set2 = LetterSet::new().add_all(set);
        assert_eq!(set, set2);
    }

    #[test]
    fn test_set_remove() {
        let set = LetterSet::new().add(Letter(0)).add(Letter(1));
        assert!(set.contains(Letter(0)));
        assert!(set.contains(Letter(1)));

        let set = set.remove(Letter(0));
        assert!(!set.contains(Letter(0)));
        assert!(set.contains(Letter(1)));

        let set = set.remove(Letter(1));
        assert!(!set.contains(Letter(0)));
        assert!(!set.contains(Letter(1)));

        // removing a letter is idempotent
        let set2 = set.remove(Letter(1));
        assert_eq!(set, set2);
    }

    #[test]
    fn test_set_remove_all() {
        let set = LetterSet::new()
            .add(Letter(0))
            .add(Letter(1))
            .add(Letter(2));
        assert!(set.contains(Letter(0)));
        assert!(set.contains(Letter(1)));
        assert!(set.contains(Letter(2)));
        assert!(!set.contains(Letter(3)));

        let to_remove = LetterSet::new()
            .add(Letter(1))
            .add(Letter(2))
            .add(Letter(3));

        let set = set.remove_all(to_remove);
        assert!(set.contains(Letter(0)));
        assert!(!set.contains(Letter(1)));
        assert!(!set.contains(Letter(2)));
        assert!(!set.contains(Letter(3)));

        // removing letters is idempotent
        let set2 = set.remove_all(to_remove);
        assert_eq!(set, set2);
    }

    #[test]
    fn test_list() {
        let mut list = LetterList::<5>::new();
        assert_eq!(list.len(), 0);

        list.add(Letter(0));
        assert_eq!(list.len(), 1);

        list.add(Letter(1));
        assert_eq!(list.len(), 2);

        // list semantics, not set
        list.add(Letter(0));
        assert_eq!(list.len(), 3);
    }

    fn contains<const N: usize>(ev: &LetterList<N>, letter: Letter) -> bool {
        ev.iter().any(|&l| l == letter)
    }

    #[test]
    fn test_list_contains() {
        let mut list = LetterList::<5>::new();
        assert!(!contains(&list, Letter(0)));
        assert!(!contains(&list, Letter(1)));

        list.add(Letter(0));
        assert!(contains(&list, Letter(0)));
        assert!(!contains(&list, Letter(1)));

        list.add(Letter(1));
        assert!(contains(&list, Letter(0)));
        assert!(contains(&list, Letter(1)));

        list.add(Letter(0));
        assert!(contains(&list, Letter(0)));
        assert!(contains(&list, Letter(1)));
        assert_eq!(list.len(), 3);
    }

    #[test]
    fn test_list_remove() {
        let mut list = LetterList::<5>::new();
        let removed = list.remove(Letter(0));
        assert!(!removed);

        list.add(Letter(0));
        let removed = list.remove(Letter(0));
        assert!(removed);
        assert!(!contains(&list, Letter(0)));

        list.add(Letter(0));
        list.add(Letter(1));
        let removed = list.remove(Letter(1));
        assert!(removed);
        assert!(contains(&list, Letter(0)));
        assert!(!contains(&list, Letter(1)));
        assert_eq!(list.len(), 1);

        list.add(Letter(1));
        list.add(Letter(0));
        let removed = list.remove(Letter(0));
        assert!(removed);
        assert!(contains(&list, Letter(0)));
        assert!(contains(&list, Letter(1)));
        assert_eq!(list.len(), 2);
    }

    #[test]
    fn test_list_iter() {
        let mut list = LetterList::<5>::new();
        list.add(Letter(0));
        list.add(Letter(1));
        list.add(Letter(2));

        let letters = list.iter().copied().collect::<Vec<_>>();
        assert_eq!(letters, &[Letter(0), Letter(1), Letter(2)]);
    }

    #[test]
    fn test_constraint() {
        let cons = Constraint::new();
        assert!(cons.accept(Letter(0)));
        assert!(cons.accept(Letter(1)));

        let cons = cons.must_not(Letter(0));
        assert!(!cons.accept(Letter(0)));
        assert!(cons.accept(Letter(1)));

        let cons = cons.must(Letter(0));
        assert!(cons.accept(Letter(0)));
        assert!(!cons.accept(Letter(1)));
    }

    #[test]
    fn test_constraint_is_free() {
        let cons = Constraint::new();
        assert!(cons.is_free());

        let cons = cons.must_not(Letter(0));
        assert!(cons.is_free());

        let cons = cons.must(Letter(0));
        assert!(!cons.is_free());
    }

    #[test]
    fn test_constraint_must_not_all() {
        let must_not = LetterSet::new().add(Letter(0)).add(Letter(1));
        let cons = Constraint::new().must_not_all(must_not);

        assert!(cons.is_free());
        assert_eq!(cons.must_not_letters(), must_not);
        assert!(!cons.accept(Letter(0)));
        assert!(!cons.accept(Letter(1)));
    }

    #[test]
    fn test_constraint_changing_must_not_after_must() {
        let cons = Constraint::new();
        assert!(cons.accept(Letter(0)));
        assert!(cons.accept(Letter(1)));
        assert!(cons.accept(Letter(2)));
        assert!(cons.is_free());

        let cons = cons.must(Letter(0));
        assert!(cons.accept(Letter(0)));
        assert!(!cons.accept(Letter(1)));
        assert!(!cons.accept(Letter(2)));
        assert!(!cons.is_free());

        let cons = cons.must_not(Letter(1));
        assert!(cons.accept(Letter(0)));
        assert!(!cons.accept(Letter(1)));
        assert!(!cons.accept(Letter(2)));
        assert!(!cons.is_free());

        let cons = cons.must_not_all(LetterSet::new().add(Letter(1)).add(Letter(2)));
        assert!(cons.accept(Letter(0)));
        assert!(!cons.accept(Letter(1)));
        assert!(!cons.accept(Letter(2)));
        assert!(!cons.is_free());
    }

    #[test]
    fn test_constraint_must_letter() {
        let cons = Constraint::new();

        let cons = cons.must(Letter(1));
        assert_eq!(cons.must_letter(), Letter(1));

        let cons = cons.must(Letter(2));
        assert_eq!(cons.must_letter(), Letter(2));
    }

    #[test]
    fn test_constraint_must_not_letters() {
        let cons = Constraint::new();
        let expected = LetterSet::new();

        let cons = cons.must_not(Letter(1));
        let expected = expected.add(Letter(1));
        assert_eq!(cons.must_not_letters(), expected);

        let cons = cons.must_not(Letter(2));
        let expected = expected.add(Letter(2));
        assert_eq!(cons.must_not_letters(), expected);

        let cons = cons.must_not(Letter(4));
        let expected = expected.add(Letter(4));
        assert_eq!(cons.must_not_letters(), expected);
    }
}
