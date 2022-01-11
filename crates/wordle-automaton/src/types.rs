//! Types and data structures to help with the automaton implementation

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

    /// Test if a letter is contained in this set
    #[must_use]
    pub const fn contains(self, letter: Letter) -> bool {
        (self.0 >> letter.0) & 1 == 1
    }

    /// Add a letter to this set
    #[must_use]
    pub const fn add(self, letter: Letter) -> Self {
        Self(self.0 | (1 << letter.0))
    }
}

/// A list of letters with support for up to 5 entries
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct LetterList(u32);

impl LetterList {
    /// Create an empty list
    #[must_use]
    pub const fn new() -> Self {
        Self(0)
    }

    /// Return the number of entries in this list
    #[must_use]
    pub const fn len(self) -> u32 {
        self.0 & 0x1F
    }

    /// Returns true iff the list is empty
    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Adds a letter in O(1)
    ///
    /// # Panics
    /// Panic if the list is full
    #[must_use]
    pub const fn add(self, letter: Letter) -> Self {
        let len = self.len() + 1;
        debug_assert!(len <= 5, "maximum capacity of 5 is reached");
        Self((letter.0 as u32 + 1) << (5 * len) | (self.0 & !0x1F) | len)
    }

    /// Removes a letter by comparing the value in O(n)
    ///
    /// Returns (true, newList) if item was in this list, otherwise returns (false, self)
    #[must_use]
    pub const fn remove(self, letter: Letter) -> (bool, Self) {
        let t = letter.0 as u32 + 1;
        let mut r = 0;
        let mut rs = 5;
        let mut n = self.0 >> 5;
        while n != 0 {
            let x = n & 0x1F;
            n >>= 5;

            if x == t {
                return (true, Self(r | n << rs | (self.len() - 1)));
            }

            r |= x << rs;
            rs += 5;
        }

        (false, self)
    }

    /// Create an iterator over all entries in this list.
    #[must_use]
    pub const fn iter(self) -> LetterListIter {
        LetterListIter(self.0 >> 5)
    }
}

/// List entry iterator. See [`LetterList::iter()`].
#[allow(missing_copy_implementations)] // Iterators should not be copy
#[derive(Clone, Debug)]
pub struct LetterListIter(u32);

impl Iterator for LetterListIter {
    type Item = Letter;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            return None;
        }
        let x = (self.0 & 0x1F) - 1;
        // this should never fail
        let x = u8::try_from(x).ok()?;
        self.0 >>= 5;
        Some(Letter(x))
    }
}

impl IntoIterator for LetterList {
    type Item = Letter;

    type IntoIter = LetterListIter;

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

    /// Return the letter that is known to be required
    ///
    /// The result is unreliable if [`Constraint::is_free()`] returns `false`.
    /// _Some_ arbitrary letter is returned.
    #[must_use]
    pub const fn must_letter(self) -> Letter {
        // cast is safe since we shift by 26 and mask by 0x1F, which leaves 5 bits
        #[allow(clippy::cast_possible_truncation)]
        Letter(((self.0 >> 26) & 0x1F) as _)
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
    fn test_list() {
        let list = LetterList::new();
        assert_eq!(list.len(), 0);

        let list = list.add(Letter(0));
        assert_eq!(list.len(), 1);

        let list = list.add(Letter(1));
        assert_eq!(list.len(), 2);

        // list semantics, not set
        let list = list.add(Letter(0));
        assert_eq!(list.len(), 3);
    }

    fn contains(ev: LetterList, letter: Letter) -> bool {
        ev.iter().any(|l| l == letter)
    }

    #[test]
    fn test_list_contains() {
        let list = LetterList::new();
        assert!(!contains(list, Letter(0)));
        assert!(!contains(list, Letter(1)));

        let list = list.add(Letter(0));
        assert!(contains(list, Letter(0)));
        assert!(!contains(list, Letter(1)));

        let list = list.add(Letter(1));
        assert!(contains(list, Letter(0)));
        assert!(contains(list, Letter(1)));

        let list = list.add(Letter(0));
        assert!(contains(list, Letter(0)));
        assert!(contains(list, Letter(1)));
    }

    #[test]
    fn test_list_remove() {
        let list = LetterList::new();
        let (removed, _) = list.remove(Letter(0));
        assert!(!removed);

        let list = list.add(Letter(0));
        let (removed, list2) = list.remove(Letter(0));
        assert!(removed);
        assert!(contains(list, Letter(0)));
        assert!(!contains(list2, Letter(0)));

        let list = list.add(Letter(1));
        let (removed, list2) = list.remove(Letter(1));
        assert!(removed);
        assert!(contains(list, Letter(0)));
        assert!(contains(list, Letter(1)));
        assert!(contains(list2, Letter(0)));
        assert!(!contains(list2, Letter(1)));

        let list = list.add(Letter(0));
        let (removed, list2) = list.remove(Letter(0));
        assert!(removed);
        assert!(contains(list, Letter(0)));
        assert!(contains(list, Letter(1)));
        assert!(contains(list2, Letter(0)));
        assert!(contains(list2, Letter(1)));
    }

    #[test]
    fn test_list_iter() {
        let list = LetterList::new()
            .add(Letter(0))
            .add(Letter(1))
            .add(Letter(2));

        let letters = list.into_iter().collect::<Vec<_>>();
        assert_eq!(letters, &[Letter(0), Letter(1), Letter(2)]);
    }

    #[test]
    #[should_panic(expected = "maximum capacity of 5 is reached")]
    const fn test_list_full() {
        let list = LetterList(5);
        let _ = list.add(Letter(0));
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
    fn test_constraint_must_letter() {
        let cons = Constraint::new();

        let cons = cons.must(Letter(1));
        assert_eq!(cons.must_letter(), Letter(1));

        let cons = cons.must(Letter(2));
        assert_eq!(cons.must_letter(), Letter(2));
    }
}
