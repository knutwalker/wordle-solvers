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

use clap::{Arg, Command};
use eyre::{Result, WrapErr};
use fst::{Automaton, IntoStreamer, Map, Streamer};
use std::{
    cmp::Reverse,
    collections::HashSet,
    convert::identity,
    ffi::OsStr,
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    str::FromStr,
};
use wordle_automaton::{prepare, Guess, Wordle, WordleBuilder};

mod words;

const MAX_SIZE: usize = 16;

fn main() -> Result<()> {
    let opts = parse_opts();
    match opts.size.0 {
        #[cfg(wordle_size = "1")]
        1 => run::<1>(&opts),
        #[cfg(wordle_size = "2")]
        2 => run::<2>(&opts),
        #[cfg(wordle_size = "3")]
        3 => run::<3>(&opts),
        #[cfg(wordle_size = "4")]
        4 => run::<4>(&opts),
        #[cfg(wordle_size = "5")]
        5 => run::<5>(&opts),
        #[cfg(wordle_size = "6")]
        6 => run::<6>(&opts),
        #[cfg(wordle_size = "7")]
        7 => run::<7>(&opts),
        #[cfg(wordle_size = "8")]
        8 => run::<8>(&opts),
        #[cfg(wordle_size = "9")]
        9 => run::<9>(&opts),
        #[cfg(wordle_size = "10")]
        10 => run::<10>(&opts),
        #[cfg(wordle_size = "11")]
        11 => run::<11>(&opts),
        #[cfg(wordle_size = "12")]
        12 => run::<12>(&opts),
        #[cfg(wordle_size = "13")]
        13 => run::<13>(&opts),
        #[cfg(wordle_size = "14")]
        14 => run::<14>(&opts),
        #[cfg(wordle_size = "15")]
        15 => run::<15>(&opts),
        #[cfg(wordle_size = "16")]
        16 => run::<16>(&opts),
        otherwise => panic!("Unsupported size: {}", otherwise),
    }
}

fn run<const N: usize>(opts: &Opts) -> Result<()> {
    let words = load_word_list::<N>(&opts.word_list, opts.block_list.as_deref(), opts.sort)?;
    let fsts = build_fst(words)?;
    loop {
        let solution = if opts.reverse {
            solve::<N, _, _>(&fsts, identity)
        } else {
            solve::<N, _, _>(&fsts, Reverse)
        }?;
        match solution {
            Solution::Solved(mut solution, guesses) => {
                solution.make_ascii_uppercase();
                println!("Solution: {solution}");
                println!();
                println!("Wordle {}/6", guesses.len());
                for guess in guesses {
                    let guess = guess
                        .iter()
                        .map(|g| match g {
                            Guess::Absent => '⬛',
                            Guess::Present => '🟨',
                            Guess::Correct => '🟩',
                        })
                        .collect::<String>();
                    println!("{guess}");
                }
                println!();
                println!("Solution: {solution}");
            }
            Solution::None => println!("Could not find a solution"),
            Solution::Restart => continue,
            Solution::Quit => {}
        };
        break;
    }

    Ok(())
}

#[derive(Debug)]
struct Opts {
    word_list: WordList,
    block_list: Option<PathBuf>,
    sort: bool,
    size: Size,
    reverse: bool,
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

#[derive(Debug)]
enum WordList {
    Read(PathBuf),
    Stdin,
    Wordle,
    Solutions,
}

impl WordList {
    fn from_os(s: &OsStr) -> Result<Self, String> {
        if s.eq_ignore_ascii_case("-") {
            return Ok(Self::Stdin);
        }
        if s.eq_ignore_ascii_case("@wordle") {
            return Ok(Self::Wordle);
        }
        if s.eq_ignore_ascii_case("@solutions") {
            return Ok(Self::Solutions);
        }
        let path = Path::new(s);
        let file_name = || s.to_string_lossy();
        match path.metadata() {
            Ok(md) => {
                if md.is_file() {
                    Ok(Self::Read(path.to_path_buf()))
                } else {
                    Err(format!("File '{}' is not a file", file_name()))
                }
            }
            Err(e) => Err(format!("File '{}' is not accessible: {e}", file_name())),
        }
    }
}

fn parse_opts() -> Opts {
    let matches = Command::new(env!("CARGO_PKG_NAME"))
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
                    "but will be ignored. ",
                    "The list must be sorted unless --sort is given. ",
                    "There are three special values accepted: - reads from stdin, ",
                    "@wordle uses all the words that the game accepts, ",
                    "@solutions uses all accepted solutions."
                ))
                .allow_invalid_utf8(true)
                .validator_os(WordList::from_os)
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
            Arg::new("size")
                .help("word size")
                .short('s')
                .long("size")
                .validator(str::parse::<Size>)
                .default_value("5"),
        )
        .arg(
            Arg::new("reverse")
                .help("Return the worst possible guess")
                .short('r')
                .long("reverse"),
        )
        .get_matches();

    let word_list = WordList::from_os(matches.value_of_os("word-list").unwrap()).unwrap();
    let block_list = matches.value_of_os("block-list").map(PathBuf::from);
    let sort = matches.is_present("sort");
    let size = matches.value_of_t("size").unwrap();
    let reverse = matches.is_present("reverse");

    Opts {
        word_list,
        block_list,
        sort,
        size,
        reverse,
    }
}

fn load_word_list<const N: usize>(
    file: &WordList,
    block_list: Option<&Path>,
    sort: bool,
) -> Result<Vec<(String, u64)>> {
    match file {
        WordList::Read(file) => {
            let lines = BufReader::new(
                File::open(file)
                    .wrap_err_with(|| format!("The file '{}' is missing.", file.display()))?,
            );
            let lines = lines.lines().map_while(Result::ok);
            load_words::<_, N>(lines, block_list, sort)
        }
        WordList::Stdin => {
            let stdin = std::io::stdin();
            let lock = stdin.lock();
            let lines = lock.lines().map_while(Result::ok);
            load_words::<_, N>(lines, block_list, sort)
        }
        WordList::Wordle => {
            let words = words::word_list(false);
            load_words::<_, N>(words, block_list, true)
        }
        WordList::Solutions => {
            let words = words::word_list(true);
            load_words::<_, N>(words, block_list, true)
        }
    }
}

fn load_words<I, const N: usize>(
    words: I,
    block_list: Option<&Path>,
    sort: bool,
) -> Result<Vec<(String, u64)>>
where
    I: IntoIterator,
    I::Item: Into<String> + AsRef<str>,
{
    let block_list = block_list
        .map(|file| {
            BufReader::new(
                File::open(file)
                    .wrap_err_with(|| format!("The file '{}' is not readable.", file.display()))?,
            )
            .lines()
            .map(|l| l.map_err(eyre::Report::from))
            .collect::<Result<HashSet<_>>>()
        })
        .transpose()
        .wrap_err("The block list could not be read.")?;

    Ok(clean_word_list::<_, N>(
        words,
        &block_list.unwrap_or_default(),
        sort,
    ))
}

fn clean_word_list<I, const N: usize>(
    words: I,
    block_list: &HashSet<String>,
    sort: bool,
) -> Vec<(String, u64)>
where
    I: IntoIterator,
    I::Item: Into<String> + AsRef<str>,
{
    let mut words = prepare::clean_word_list::<_, _, N>(words, |w| block_list.contains(w));
    if sort {
        words.sort_unstable();
    }
    prepare::score_word_list::<_, N>(words).collect()
}

fn build_fst(words: Vec<(String, u64)>) -> Result<Map<Vec<u8>>> {
    prepare::build_fst(words).wrap_err("The input file must be sorted. Try adding --sort.")
}

enum Solution<const N: usize> {
    Solved(String, Vec<[Guess; N]>),
    None,
    Restart,
    Quit,
}

const QUIT: &str = "-- QUIT I don't want to play anymore";

fn solve<const N: usize, F, K>(fst: &Map<Vec<u8>>, mut order_by: F) -> Result<Solution<N>>
where
    F: FnMut(u64) -> K,
    K: Ord,
{
    let mut wordle = WordleBuilder::<N>::new().build();
    let mut guesses = Vec::with_capacity(6);

    let mut solutions = Vec::with_capacity(fst.len());
    let wordle = loop {
        let (next_worlde, word) = match guess_next(fst, wordle, &mut solutions, &mut order_by)? {
            Ok(next) => next,
            Err(solution) => return Ok(solution),
        };

        wordle = next_worlde;

        guesses.push(extract_guess(&wordle, &word));
        if wordle.is_solved() {
            break wordle;
        }
    };

    Ok(Solution::Solved(wordle.decode_str(), guesses))
}

fn guess_next<const N: usize, F, K>(
    fst: &Map<Vec<u8>>,
    wordle: Wordle<N>,
    solutions: &mut Vec<(Vec<u8>, u64)>,
    order_by: F,
) -> Result<Result<(Wordle<N>, Vec<u8>), Solution<N>>>
where
    F: FnMut(u64) -> K,
    K: Ord,
{
    if let Some(solution) = find_all_solutions(fst, &wordle, solutions, order_by) {
        match solution {
            Ok(word) => return Ok(Ok((mark_as_correct(wordle, &word), word))),
            Err(solution) => return Ok(Err(solution)),
        }
    }

    eprintln!("Found {} possible solutions", solutions.len());

    let word = match find_word_to_play(&wordle, solutions)? {
        Ok(word) => word,
        Err(solution) => return Ok(Err(solution)),
    };

    let wordle = match apply_feedback(wordle, &word)? {
        Ok(wordle) => wordle,
        Err(solution) => return Ok(Err(solution)),
    };

    Ok(Ok((wordle, word)))
}

fn find_all_solutions<const N: usize, F, K>(
    fst: &Map<Vec<u8>>,
    wordle: &Wordle<N>,
    buf: &mut Vec<(Vec<u8>, u64)>,
    mut order_by: F,
) -> Option<Result<Vec<u8>, Solution<N>>>
where
    F: FnMut(u64) -> K,
    K: Ord,
{
    buf.clear();

    let mut stream = fst.search(&wordle).into_stream();
    while let Some((word, score)) = stream.next() {
        buf.push((word.to_vec(), score));
    }

    if buf.is_empty() {
        return Some(Err(Solution::None));
    }

    if buf.len() == 1 {
        let (word, _) = buf.pop().unwrap();
        return Some(Ok(word));
    }

    buf.sort_by_key(|(_, score)| order_by(*score));

    None
}

fn mark_as_correct<const N: usize>(wordle: Wordle<N>, word: &[u8]) -> Wordle<N> {
    let mut wb = WordleBuilder::from(wordle);
    word.iter()
        .enumerate()
        .fold(&mut wb, |wb, (pos, b)| wb.correct_pos(pos, *b))
        .build()
}

fn find_word_to_play<const N: usize>(
    wordle: &Wordle<N>,
    options: &mut [(Vec<u8>, u64)],
) -> Result<Result<Vec<u8>, Solution<N>>> {
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
            .item("-- No, I want to throw away all guesses and restart from the beginning")
            .item("-- No, I want to manually enter a word")
            .item(QUIT)
            .interact()?;

        match selection.checked_sub(current.len()) {
            Some(0) => items = rest,
            Some(1) => items = options,
            Some(2) => return Ok(Err(Solution::Restart)),
            Some(3) => match enter_word(wordle)? {
                Some(word) => break word,
                None => items = options,
            },
            Some(_) => return Ok(Err(Solution::Quit)),
            None => break std::mem::take(&mut current[selection]).0,
        }
    }))
}

fn enter_word<const N: usize>(wordle: &Wordle<N>) -> Result<Option<Vec<u8>>> {
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

    Ok((test_word(&word, wordle)
        || dialoguer::Confirm::new()
            .with_prompt(format!(
                concat!(
                    "The selected word '{}' does not match the currenct constraints. ",
                    "Do you want to play in anyway?"
                ),
                word
            ))
            .default(false)
            .interact()?)
    .then(|| word.into_bytes()))
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
    word: &[u8],
) -> Result<Result<Wordle<N>, Solution<N>>> {
    let mut wb = WordleBuilder::from(wordle);
    let mut selection = 0;

    for (pos, &b) in word.iter().enumerate() {
        if wb.current().solution_at(pos) == Some(b) {
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
                "??:     Correct Letter but unknown Position",
                "SKIP:   Do nothing with that letter",
                QUIT,
            ])
            .default(selection)
            .interact()?;

        let _ = match selection {
            0 => wb.never(b),
            1 if wb.current().has_solution_at(pos) => wb.eventually(b),
            1 => wb.wrong_pos(pos, b),
            2 if wb.current().has_solution_at(pos) => return Ok(Err(Solution::None)),
            2 => wb.correct_pos(pos, b),
            3 => wb.eventually(b),
            4 => &mut wb,
            _ => return Ok(Err(Solution::Quit)),
        };
    }

    Ok(Ok(wb.build()))
}

fn extract_guess<const N: usize>(wordle: &Wordle<N>, word: &[u8]) -> [Guess; N] {
    let mut pos = 0;
    [(); N].map(|_| {
        let guess = wordle.decode_guess(pos, word[pos]);
        pos += 1;
        guess
    })
}
