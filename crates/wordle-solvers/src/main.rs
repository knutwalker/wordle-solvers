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
use fst::{IntoStreamer, Set, Streamer};
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};
use wordle_automaton::WordleBuilder;

fn main() -> Result<()> {
    let opts = parse_opts();
    let words = load_word_list(&opts.word_list, opts.block_list.as_deref(), opts.no_tiered)?;
    let initial_guess = take_best_guess(&words.main).cloned();
    let fsts = build_fsts(words)?;
    let solution = solve(&fsts, initial_guess)?;
    match solution {
        Solution::None => println!("Could not find a solution"),
        Solution::Quit => {}
        Solution::Solved(solution) => println!("Solution: {}", solution),
    }

    Ok(())
}

#[derive(Debug)]
struct Opts {
    word_list: PathBuf,
    block_list: Option<PathBuf>,
    no_tiered: bool,
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
                .help("The word list to use. Must be sorted and one word per line.")
                .long_help(None)
                .allow_invalid_utf8(true)
                .default_value("/usr/share/dict/words"),
        ).arg(
            Arg::new("block-list")
                .takes_value(true)
                .value_name("BLOCK_LIST")
                .required(false)
                .help("A list of block words that are known to not be accepted. Must be one word per line.")
                .long_help(None)
                .short('b')
                .long("block-list")
                .allow_invalid_utf8(true),
        )
        .arg(
            clap::Arg::new("no-tiered")
                .help("Merge the fst that are being searched")
                .long_help(concat!("Merge the fst that are being searched. ",
    "The default behavior is to only search for word that have no repeated characters and ",
    "only fall back to the full word list if that first one is exhausted. ",
    "This flag merged both lists and searches them together."))
                .long("no-tiered"),
        )
        .get_matches();

    let word_list = PathBuf::from(matches.value_of_os("word-list").unwrap());
    let block_list = matches.value_of_os("block-list").map(PathBuf::from);
    let no_tiered = matches.is_present("no-tiered");

    Opts {
        word_list,
        block_list,
        no_tiered,
    }
}

struct Pair<T> {
    main: T,
    fallback: T,
}

fn clean_word_list<I>(words: I, block_list: &HashSet<String>, merge: bool) -> Pair<Vec<String>>
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

    for word in words {
        if valid_word(word.as_ref()) {
            if block_list.contains(word.as_ref()) {
                continue;
            }
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

    let (main, fallback) = possible_words
        .into_iter()
        .enumerate()
        .filter_map(is_singular)
        .partition(|w| merge || has_no_duplicate_letters(w));

    Pair { main, fallback }
}

fn load_word_list(
    file: &Path,
    block_list: Option<&Path>,
    merge: bool,
) -> Result<Pair<Vec<String>>> {
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
            .map(|l| Ok::<_, eyre::Report>(l?))
            .collect::<Result<HashSet<_>>>()
        })
        .transpose()
        .wrap_err("The block list could not be read.")?;

    Ok(clean_word_list(
        lines.lines().map_while(Result::ok),
        &block_list.unwrap_or_default(),
        merge,
    ))
}

fn take_best_guess(main: &[String]) -> Option<&String> {
    let mut freqs = HashMap::<u8, usize>::with_capacity(32);
    for byte in main.iter().flat_map(|s| s.bytes()) {
        *freqs.entry(byte).or_default() += 1;
    }

    main.iter()
        .max_by_key(|s| s.bytes().map(|b| freqs[&b]).sum::<usize>())
}

fn build_fsts(Pair { main, fallback }: Pair<Vec<String>>) -> Result<Pair<Set<Vec<u8>>>> {
    // TODO: assumes sorted input. Could fallback to sort if from_iter fails

    let main = Set::from_iter(main).wrap_err("The input file must be sorted.")?;
    let fallback = Set::from_iter(fallback).wrap_err("The input file must be sorted.")?;

    Ok(Pair { main, fallback })
}

enum Solution {
    None,
    Quit,
    Solved(String),
}

fn solve(fsts: &Pair<Set<Vec<u8>>>, mut initial_guess: Option<String>) -> Result<Solution> {
    const QUIT: &str = "-- QUIT I don't want to play anymore";

    let mut solutions = Vec::with_capacity(10);
    let mut wordle = WordleBuilder::new().build();

    'outer: loop {
        if wordle.is_solved() {
            let mut solution = wordle.decode_str();
            solution.make_ascii_uppercase();
            return Ok(Solution::Solved(solution));
        }

        let mut is_fallback = false;
        let mut stream = fsts.main.search(&wordle).into_stream();

        let word = loop {
            solutions.clear();
            match initial_guess.take() {
                Some(guess) => solutions.push(guess),
                None => {
                    while solutions.len() < solutions.capacity() {
                        if let Some(w) = stream.next() {
                            match std::str::from_utf8(w) {
                                Ok(word) => solutions.push(word.to_string()),
                                // this should never fail since we use strings as input
                                Err(_) => continue,
                            }
                        } else if is_fallback {
                            break;
                        } else {
                            is_fallback = true;
                            stream = fsts.fallback.search(&wordle).into_stream();
                        };
                    }
                }
            }

            if solutions.is_empty() {
                return Ok(Solution::None);
            }

            let mut selection = dialoguer::Select::new();
            for word in &solutions {
                let _ = selection.item(word);
            }

            let selection = selection
                .with_prompt("Do you want to play any of these words?")
                .default(0)
                .item("-- No, I want new words")
                .item("-- No, I want to redo the current guess from the top")
                .item(QUIT)
                .interact()?;

            match selection.checked_sub(solutions.len()) {
                Some(0) => continue,
                Some(1) => continue 'outer,
                Some(_) => return Ok(Solution::Quit),
                None => break solutions.swap_remove(selection),
            }
        };

        let mut wb = WordleBuilder::from(wordle);
        let mut selection = 0;

        for (pos, b) in word.bytes().enumerate() {
            if wb.current().has_solution_at(pos) {
                continue;
            }

            selection = dialoguer::Select::new()
                .with_prompt(format!("Letter {} on position {}", char::from(b), pos + 1))
                .items(&[
                    "Grey: Wrong Letter",
                    "Yellow: Wrong Position",
                    "Green: Correct Letter in Correct Position",
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
