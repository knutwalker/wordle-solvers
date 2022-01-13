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
use fst::{IntoStreamer, Map, Streamer};
use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    str::FromStr,
};
use wordle_automaton::{types::Letter, WordleBuilder};

fn main() -> Result<()> {
    let opts = parse_opts();
    let words = load_word_list(
        &opts.word_list,
        opts.block_list.as_deref(),
        opts.no_tiered,
        opts.sort,
    )?;
    let fsts = build_fsts(words)?;
    let solution = solve(&fsts, opts.penalty.0)?;
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
        )
        .get_matches();

    let word_list = PathBuf::from(matches.value_of_os("word-list").unwrap());
    let block_list = matches.value_of_os("block-list").map(PathBuf::from);
    let sort = matches.is_present("sort");
    let penalty = matches.value_of_t("penalty").unwrap();
    let no_tiered = matches.is_present("no-tiered");

    Opts {
        word_list,
        block_list,
        sort,
        penalty,
        no_tiered,
    }
}

struct Words {
    different_letters: Vec<String>,
    duplicate_letters: Vec<String>,
    letter_frequency: HashMap<u8, u64>,
}

fn clean_word_list<I>(words: I, block_list: &HashSet<String>, merge: bool, sort: bool) -> Words
where
    I: IntoIterator,
    I::Item: Into<String> + AsRef<str>,
{
    fn valid_word(word: &str) -> bool {
        (word.len() == 5 || word.len() == 4) && word.bytes().all(|b| matches!(b, b'a'..=b'z'))
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

        if valid_word(word_ref) && !block_list.contains(word_ref) {
            let word: String = word.into();
            if word.len() == 4 {
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
        (!(possible_plurals.contains(&idx) && possible_stems.contains(&word[..4]))).then(|| word)
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

fn load_word_list(
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

    Ok(clean_word_list(
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
) -> Result<Fsts> {
    // TODO: assumes sorted input. Could fallback to sort if from_iter fails

    let score = move |s: &str| -> u64 {
        s.bytes()
            .map(|b| letter_frequency.get(&b).copied().unwrap_or_default())
            .sum()
    };

    let different_letters = Map::from_iter(different_letters.into_iter().map(|s| {
        let score = score(&s);
        (s, score)
    }))
    .wrap_err("The input file must be sorted.")?;

    let duplicate_letters = Map::from_iter(duplicate_letters.into_iter().map(|s| {
        let score = score(&s);
        (s, score)
    }))
    .wrap_err("The input file must be sorted.")?;

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

fn solve(fsts: &Fsts, penalty: f64) -> Result<Solution> {
    const QUIT: &str = "-- QUIT I don't want to play anymore";

    let mut solutions = Vec::with_capacity(10);
    let mut wordle = WordleBuilder::new().build();

    loop {
        if wordle.is_solved() {
            return Ok(Solution::Solved(wordle.decode_str()));
        }

        let mut differents = fsts.different_letters.search(&wordle).into_stream();
        let mut duplicates = fsts.duplicate_letters.search(&wordle).into_stream();

        solutions.clear();
        while let Some((word, score)) = differents.next() {
            solutions.push((word.to_vec(), score));
        }
        while let Some((word, score)) = duplicates.next() {
            // score and penalty are both bound in a way that will not run into those issues
            #[allow(
                clippy::cast_possible_truncation,
                clippy::cast_precision_loss,
                clippy::cast_sign_loss
            )]
            solutions.push((word.to_vec(), ((score as f64) * penalty) as u64));
        }

        if solutions.is_empty() {
            return Ok(Solution::None);
        }

        if solutions.len() == 1 {
            let (word, _) = solutions.pop().unwrap();
            let word = String::from_utf8(word).expect("input was strings");
            return Ok(Solution::Solved(word));
        }

        solutions.sort_by_key(|(_, score)| Reverse(*score));
        eprintln!("Found {} possible solutions", solutions.len());

        let mut items = &mut solutions[..];

        let word = loop {
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
                .item(QUIT)
                .interact()?;

            match selection.checked_sub(current.len()) {
                Some(0) => items = rest,
                Some(1) => items = &mut solutions[..],
                Some(_) => return Ok(Solution::Quit),
                None => break std::mem::take(&mut current[selection]).0,
            }
        };

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
                    QUIT,
                ])
                .default(selection)
                .interact()?;

            let _ = match selection {
                0 => wb.never(b),
                1 => wb.wrong_pos(pos, b),
                2 => wb.correct_pos(pos, b),
                _ => return Ok(Solution::Quit),
            };
        }

        wordle = wb.build();
    }
}
