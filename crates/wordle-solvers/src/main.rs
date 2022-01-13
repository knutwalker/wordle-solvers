//! A Wordle solver
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

use clap::{App, Arg};
use eyre::{Result, WrapErr};
use fst::{Automaton, IntoStreamer, Map, Streamer};
use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    str::FromStr,
};
use wordle_automaton::{Letter, Wordle, WordleBuilder};

const MAX_SIZE: usize = 16;

fn main() -> Result<()> {
    let opts = parse_opts();
    match opts.size.0 {
        1 => run::<1>(&opts),
        2 => run::<2>(&opts),
        3 => run::<3>(&opts),
        4 => run::<4>(&opts),
        5 => run::<5>(&opts),
        6 => run::<6>(&opts),
        7 => run::<7>(&opts),
        8 => run::<8>(&opts),
        9 => run::<9>(&opts),
        10 => run::<10>(&opts),
        11 => run::<11>(&opts),
        12 => run::<12>(&opts),
        13 => run::<13>(&opts),
        14 => run::<14>(&opts),
        15 => run::<15>(&opts),
        16 => run::<16>(&opts),
        otherwise => panic!("Unsupported size: {}", otherwise),
    }
}

fn run<const N: usize>(opts: &Opts) -> Result<()> {
    let words = load_word_list::<N>(
        &opts.word_list,
        opts.block_list.as_deref(),
        opts.no_tiered,
        opts.sort,
    )?;
    let fsts = build_fsts(words, opts.penalty.0)?;
    let solution = solve::<N>(&fsts)?;
    match solution {
        Solution::None => println!("Could not find a solution"),
        Solution::Quit => {}
        Solution::Solved(mut solution) => {
            solution.make_ascii_uppercase();
            println!("Solution: {}", solution);
        }
    }

    Ok(())
}

#[derive(Debug)]
struct Opts {
    word_list: PathBuf,
    block_list: Option<PathBuf>,
    sort: bool,
    penalty: Penalty,
    no_tiered: bool,
    size: Size,
}

#[derive(Debug)]
struct Penalty(f64);

impl FromStr for Penalty {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<f64>().map_err(|e| e.to_string()).and_then(|f| {
            if (0.0..=1.0).contains(&f) {
                Ok(Self(1.0 - f))
            } else {
                Err(String::from("The value must be in [0..1]"))
            }
        })
    }
}

#[derive(Debug)]
struct Size(usize);

impl FromStr for Size {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<usize>().map_err(|e| e.to_string()).and_then(|s| {
            if (1..=MAX_SIZE).contains(&s) {
                Ok(Self(s))
            } else {
                Err(format!("The value must be in [1..{}]", MAX_SIZE))
            }
        })
    }
}

fn parse_opts() -> Opts {
    let matches = App::new(env!("CARGO_PKG_NAME"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .version(env!("CARGO_PKG_VERSION"))
        .long_about(None)
        .arg(
            Arg::new("word-list")
                .takes_value(true)
                .value_name("WORD_LIST")
                .required(false)
                .help("The word list to use")
                .long_help(concat!(
                    "The word list to use. ",
                    "The list must contain one word per line. ",
                    "Invalid guesses (non-ascii, or non-five-letter words) are allowed ",
                    "and will we used to build a frequency table. ",
                    "The list must be sorted unless --sort is given."
                ))
                .allow_invalid_utf8(true)
                .default_value("/usr/share/dict/words"),
        ).arg(
            Arg::new("block-list")
                .takes_value(true)
                .value_name("BLOCK_LIST")
                .required(false)
                .help("A list of block words that are known to not be accepted. Must be one word per line, only lowercase.")
                .long_help(concat!(
                    "The list of word list to never use. ",
                    "The list must contain one word per line. ",
                    "The word must be all-lowercase and five letters. ",
                    "Invalid words will be ignored. ",
                    "The list can be in any order, it does not have to be sorted."
                ))
                .short('b')
                .long("block-list")
                .allow_invalid_utf8(true),
        )
        .arg(
            Arg::new("sort")
                .help("Make sure that the input list is sorted")
                .short('c')
                .long("sort"),
        )
        .arg(
            Arg::new("penalty")
                .help("Penalty for words with duplicated letters")
                .validator(str::parse::<Penalty>)
                .long_help(concat!(
                    "Penalty for a waord having duplicated letters. ",
                    "The default behavior is prioritize words that have no repeated characters and ",
                    "only onclude the other words if their letters are quite frequent. ",
                    "This flag controls how much penalty a score gets. ",
                    "The penalty must be between 0 and 1, inclusive.",
                    "A penalty of 0 means no penalty, score the same as other words.",
                    "A penalty of 1 means to only show words with duplicates after all other results.",
                ))
                .short('p')
                .long("penalty")
                .takes_value(true)
                .required(false)
                .default_value("0.5"),
        )
        .arg(
            Arg::new("no-tiered")
                .help("Merge the fst that are being searched")
                .long_help(concat!(
                    "Merge the fst that are being searched. ",
                    "The default behavior is prioritize words that have no repeated characters and ",
                    "only onclude the other words if their letters are quite frequent. ",
                    "This flag merged both lists and searches them together. ",
                    "This has the same behavior as `--penalty 0`, but might be faster",
                ))
                .long("no-tiered"),
        ).arg(
            Arg::new("size")
                .help("word size")
                .short('s')
                .long("size")
                .validator(str::parse::<usize>) // todo: limit size
                .default_value("5"),
        )
        .get_matches();

    let word_list = PathBuf::from(matches.value_of_os("word-list").unwrap());
    let block_list = matches.value_of_os("block-list").map(PathBuf::from);
    let sort = matches.is_present("sort");
    let penalty = matches.value_of_t("penalty").unwrap();
    let no_tiered = matches.is_present("no-tiered");
    let size = matches.value_of_t("size").unwrap();

    Opts {
        word_list,
        block_list,
        sort,
        penalty,
        no_tiered,
        size,
    }
}

struct Words {
    different_letters: Vec<String>,
    duplicate_letters: Vec<String>,
    letter_frequency: HashMap<u8, u64>,
}

fn clean_word_list<I, const N: usize>(
    words: I,
    block_list: &HashSet<String>,
    merge: bool,
    sort: bool,
) -> Words
where
    I: IntoIterator,
    I::Item: Into<String> + AsRef<str>,
{
    fn valid_word<const N: usize>(word: &str) -> bool {
        (word.len() == N || word.len() == N - 1) && word.bytes().all(|b| matches!(b, b'a'..=b'z'))
    }

    fn has_no_duplicate_letters(word: &str) -> bool {
        let mut word = word.as_bytes();
        while let Some((head, tail)) = word.split_first() {
            if tail.contains(head) {
                return false;
            }
            word = tail;
        }
        true
    }

    let mut possible_words = Vec::with_capacity(1024);
    let mut possible_stems = HashSet::with_capacity(1024);
    let mut possible_plurals = HashSet::with_capacity(1024);
    let mut letter_frequency = HashMap::<u8, u64>::with_capacity(32);

    for word in words {
        let word_ref = word.as_ref();

        // build frequency table
        if word_ref.is_ascii() {
            // maps uppercase to lowercase, ignore non alphabetic chars
            for byte in word_ref.bytes().filter_map(Letter::try_new).map(u8::from) {
                *letter_frequency.entry(byte).or_default() += 1;
            }
        }

        if valid_word::<N>(word_ref) && !block_list.contains(word_ref) {
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

    let is_singular = move |(idx, word): (usize, String)| {
        (!(possible_plurals.contains(&idx) && possible_stems.contains(&word[..N - 1])))
            .then(|| word)
    };

    let (mut different_letters, mut duplicate_letters) = possible_words
        .into_iter()
        .enumerate()
        .filter_map(is_singular)
        .partition::<Vec<_>, _>(|w| merge || has_no_duplicate_letters(w));

    if sort {
        different_letters.sort();
        duplicate_letters.sort();
    }

    Words {
        different_letters,
        duplicate_letters,
        letter_frequency,
    }
}

fn load_word_list<const N: usize>(
    file: &Path,
    block_list: Option<&Path>,
    merge: bool,
    sort: bool,
) -> Result<Words> {
    let lines = BufReader::new(
        File::open(file).wrap_err_with(|| format!("The file '{}' is missing.", file.display()))?,
    );

    let block_list = block_list
        .map(|file| {
            BufReader::new(
                File::open(file)
                    .wrap_err_with(|| format!("The file '{}' is missing.", file.display()))?,
            )
            .lines()
            .map(|l| l.map_err(eyre::Report::from))
            .collect::<Result<HashSet<_>>>()
        })
        .transpose()
        .wrap_err("The block list could not be read.")?;

    Ok(clean_word_list::<_, N>(
        lines.lines().map_while(Result::ok),
        &block_list.unwrap_or_default(),
        merge,
        sort,
    ))
}

struct Fsts {
    different_letters: Map<Vec<u8>>,
    duplicate_letters: Map<Vec<u8>>,
}

fn build_fsts(
    Words {
        different_letters,
        duplicate_letters,
        letter_frequency,
    }: Words,
    penalty: f64,
) -> Result<Fsts> {
    let score = move |s: &str| -> u64 {
        s.bytes()
            .map(|b| letter_frequency.get(&b).copied().unwrap_or_default())
            .sum()
    };

    let different_letters = Map::from_iter(different_letters.into_iter().map(|s| {
        let score = score(&s);
        (s, score)
    }))
    .wrap_err("The input file must be sorted. Try adding --sort.")?;

    let duplicate_letters = Map::from_iter(duplicate_letters.into_iter().map(|s| {
        let score = score(&s);
        // score and penalty are both bound in a way that will not run into those issues
        #[allow(
            clippy::cast_possible_truncation,
            clippy::cast_precision_loss,
            clippy::cast_sign_loss
        )]
        let score = ((score as f64) * penalty) as u64;

        (s, score)
    }))
    .wrap_err("The input file must be sorted. Try adding --sort.")?;

    Ok(Fsts {
        different_letters,
        duplicate_letters,
    })
}

enum Solution {
    None,
    Quit,
    Solved(String),
}

const QUIT: &str = "-- QUIT I don't want to play anymore";

fn solve<const N: usize>(fsts: &Fsts) -> Result<Solution> {
    let mut wordle = WordleBuilder::<N>::new().build();

    let mut solutions =
        Vec::with_capacity(fsts.different_letters.len() + fsts.duplicate_letters.len());

    loop {
        if let Some(solution) = find_all_solutions(fsts, &wordle, &mut solutions) {
            return Ok(solution);
        }

        eprintln!("Found {} possible solutions", solutions.len());

        let word = match find_word_to_play(&wordle, &mut solutions)? {
            Ok(word) => word,
            Err(solution) => return Ok(solution),
        };

        wordle = match apply_feedback(wordle, word)? {
            Ok(wordle) => wordle,
            Err(solution) => return Ok(solution),
        };
    }
}

fn find_all_solutions<const N: usize>(
    fsts: &Fsts,
    wordle: &Wordle<N>,
    buf: &mut Vec<(Vec<u8>, u64)>,
) -> Option<Solution> {
    if wordle.is_solved() {
        return Some(Solution::Solved(wordle.decode_str()));
    }

    let mut differents = fsts.different_letters.search(&wordle).into_stream();
    let mut duplicates = fsts.duplicate_letters.search(&wordle).into_stream();

    buf.clear();

    while let Some((word, score)) = differents.next() {
        buf.push((word.to_vec(), score));
    }
    while let Some((word, score)) = duplicates.next() {
        buf.push((word.to_vec(), score));
    }

    if buf.is_empty() {
        return Some(Solution::None);
    }

    if buf.len() == 1 {
        let (word, _) = buf.pop().unwrap();
        let word = String::from_utf8(word).expect("input was strings");
        return Some(Solution::Solved(word));
    }

    buf.sort_by_key(|(_, score)| Reverse(*score));

    None
}

fn find_word_to_play<const N: usize>(
    wordle: &Wordle<N>,
    options: &mut [(Vec<u8>, u64)],
) -> Result<Result<Vec<u8>, Solution>> {
    let mut items = &mut *options;

    Ok(Ok(loop {
        let mid = items.len().min(10);
        let (current, rest) = items.split_at_mut(mid);

        let mut selection = dialoguer::Select::new();
        for (word, _) in current.iter() {
            let word = std::str::from_utf8(word).expect("input was strings");
            let _ = selection.item(word);
        }

        let selection = selection
            .with_prompt("Do you want to play any of these words?")
            .default(0)
            .item("-- No, I want new words")
            .item("-- No, I want to redo the current guess from the top")
            .item("-- No, I want to manually enter a word")
            .item(QUIT)
            .interact()?;

        match selection.checked_sub(current.len()) {
            Some(0) => items = rest,
            Some(1) => items = options,
            Some(2) => {
                let word = dialoguer::Input::new()
                    .with_prompt("Please enter the word you want to play")
                    .validate_with(|s: &String| {
                        if s.len() != N {
                            return Err(format!("The word must be {} letters long", N));
                        }
                        if !s.is_ascii() {
                            return Err(String::from("The word must be in ASCII"));
                        }
                        Ok(())
                    })
                    .interact_text()?;

                if !test_word(&word, wordle)
                    && !dialoguer::Confirm::new()
                        .with_prompt(format!(
                            concat!(
                                "The selected word '{}' does not match the currenct constraints. ",
                                "Do you want to play in anyway?"
                            ),
                            word
                        ))
                        .default(false)
                        .interact()?
                {
                    items = options;
                    continue;
                }

                break word.into_bytes();
            }

            Some(_) => return Ok(Err(Solution::Quit)),
            None => break std::mem::take(&mut current[selection]).0,
        }
    }))
}

fn test_word<const N: usize>(word: &str, wordle: &Wordle<N>) -> bool {
    let mut state = wordle.start();
    for b in word.bytes() {
        if !wordle.can_match(&state) {
            return false;
        }
        state = wordle.accept(&state, b);
    }
    wordle.is_match(&state)
}

fn apply_feedback<const N: usize>(
    wordle: Wordle<N>,
    word: Vec<u8>,
) -> Result<Result<Wordle<N>, Solution>> {
    let mut wb = WordleBuilder::from(wordle);
    let mut selection = 0;

    for (pos, b) in word.into_iter().enumerate() {
        if wb.current().has_solution_at(pos) {
            eprintln!(
                "Letter '{}' on position {}: Green:  Correct Letter in Correct Position",
                char::from(b.to_ascii_uppercase()),
                pos + 1
            );
            continue;
        }

        selection = dialoguer::Select::new()
            .with_prompt(format!(
                "Letter '{}' on position {}",
                char::from(b.to_ascii_uppercase()),
                pos + 1
            ))
            .items(&[
                "Grey:   Wrong Letter",
                "Yellow: Wrong Position",
                "Green:  Correct Letter in Correct Position",
                "SkIP:   Do nothing with that letter",
                QUIT,
            ])
            .default(selection)
            .interact()?;

        let _ = match selection {
            0 => wb.never(b),
            1 => wb.wrong_pos(pos, b),
            2 => wb.correct_pos(pos, b),
            3 => &mut wb,
            _ => return Ok(Err(Solution::Quit)),
        };
    }

    Ok(Ok(wb.build()))
}
