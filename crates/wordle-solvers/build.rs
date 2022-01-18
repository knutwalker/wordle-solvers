use std::{num::ParseIntError, ops::RangeInclusive};

fn parse_range(s: &str) -> Result<RangeInclusive<u8>, ParseIntError> {
    match s.split_once("..=") {
        Some((start, end)) => Ok(start.parse()?..=end.parse()?),
        None => {
            let s = s.parse()?;
            Ok(s..=s)
        }
    }
}

fn main() {
    let size = match std::env::var("WORDLE_SIZE") {
        Ok(size) => match parse_range(&size) {
            Ok(size) => size,
            Err(e) => {
                eprintln!("Could not parse WORDLE_SIZE ('{size}') into a valid number or inclusive range (..=): {}", e);
                std::process::exit(1);
            }
        },
        Err(_) => 5..=5,
    };

    println!("cargo:rerun-if-env-changed=WORDLE_SIZE");
    for size in size {
        println!("cargo:rustc-cfg=wordle_size=\"{size}\"");
    }
}
