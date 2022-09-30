use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

fn main() {
    let (metadata, path) = parse_args();

    let lines = file_lines(path);
    // This could be done inside the loop, but I just want to show how you'd filter empty lines in a pipeline.
    if !metadata {
        // Note: `parse_line` expects the string not to contain an EOL marker;
        // `BufRead::lines()` strips them, but e.g. `BufRead::read_line()` does not.
        let lines = lines.map(|res| gb_sym_file::parse_line(&res));

        for (line_no, line) in lines.enumerate() {
            let line_no = line_no + 1;
            match line {
                None => println!("Line {} is empty", line_no),
                Some(Err(err)) => println!("Parse error on line {}: {}", line_no, err),
                Some(Ok((name, loc))) => {
                    println!(
                        "Line {} defines \"{}\" as {:x?}",
                        line_no,
                        name.escape_debug(),
                        loc
                    )
                }
            }
        }
    } else {
        for (line_no, line) in lines.enumerate() {
            let line_no = line_no + 1;
            match gb_sym_file::parse_line_with_metadata::<Vec<_>>(&line) {
                None => (),
                Some(Err(err)) => println!("Parse error on line {}: {}", line_no, err),
                Some(Ok((name, loc, metadata))) => {
                    println!(
                        "Line {} defines \"{}\" as {:x?} with metadata {:?}",
                        line_no,
                        name.escape_debug(),
                        loc,
                        &metadata
                    )
                }
            }
        }
    }
}

fn file_lines<P: AsRef<Path>>(path: P) -> impl Iterator<Item = String> {
    let file = File::open(path).expect("Failed to open file");
    BufReader::new(file)
        .lines()
        // Note: failing to read a line is a read possibility, such as if it is not valid UTF-8.
        // This example is kept simple, though.
        .map(|line| line.expect("Failed to read line"))
}

fn parse_args() -> (bool, String) {
    // Quick & dirty
    fn usage(argv0: Option<&str>) -> ! {
        eprintln!(
            "Usage: {} [-m] <path/to/file.sym> (-m prints metadata tokens and squashes empty lines)",
            argv0.unwrap_or("sym_dump")
        );
        std::process::exit(1);
    }
    let mut args = env::args();
    let argv0 = args.next();
    let parsed = match args.next() {
        None => usage(argv0.as_deref()),
        Some(opt) if opt == "-m" => (true, args.next().unwrap_or_else(|| usage(argv0.as_deref()))),
        Some(path) => (false, path),
    };
    if args.next().is_some() {
        usage(argv0.as_deref());
    }
    parsed
}

#[cfg(test)]
mod tests {
    use gb_sym_file::{Location, ParseError};

    //use super::*;

    #[test]
    fn check_conformity() {
        let results = [
            Ok(None),
            Ok(Some(("NULL", &Location::Banked(0, 0), [].as_slice()))),
            Ok(Some(("EntryPoint", &Location::Banked(0, 0x0100), &[]))),
            Ok(None),
            Ok(None),
            Ok(None),
            Ok(None),
            Ok(Some(("Main", &Location::Banked(0, 0x1234), &[]))),
            Ok(None),
            Ok(Some(("CrashHandler", &Location::Banked(0, 0x38), &[]))),
            Ok(None),
            Ok(Some(("CallHL", &Location::Banked(0, 0x30), &[]))),
            Ok(None),
            Ok(Some(("UnicodeSupport‽", &Location::Unbanked(0xCAFE), &[]))),
            Ok(Some(("AsciiToo<3", &Location::Unbanked(0xBABE), &[]))),
            Ok(Some(("Nul\0Terminator", &Location::Unbanked(0), &[]))),
            Ok(None),
            Ok(None),
            Ok(Some(("NotInVRAM", &Location::Banked(1, 0x8000), &["ROMX"]))),
            Ok(None),
            Ok(Some(("RTile", &Location::Boot(0x72), &[]))),
            Ok(Some(("RTile.end", &Location::Boot(0x7A), &[]))),
            Ok(None),
            Ok(Some(("wWorkRam", &Location::Boot(0xD000), &[]))),
            Ok(None),
            Ok(None),
            Ok(Some(("SomeAddress", &Location::Unbanked(0xC0DE), &[]))),
            Ok(Some(("LowerHexWorksToo", &Location::Unbanked(0xDEAD), &[]))),
            Ok(None),
            Ok(None),
            Err(ParseError::BadBank(
                u16::from_str_radix("$", 16).unwrap_err(),
            )),
            Err(ParseError::BadBank(
                u16::from_str_radix("", 16).unwrap_err(),
            )),
            Err(ParseError::BadAddress(
                u16::from_str_radix("", 16).unwrap_err(),
            )),
            Err(ParseError::BadAddress(
                u16::from_str_radix("12345", 16).unwrap_err(),
            )),
            Err(ParseError::BadChar('!')),
            Err(ParseError::BadFirstChar('1')),
            Err(ParseError::BadChar(':')),
            Err(ParseError::BadChar('‽')),
            Err(ParseError::TruncatedEscape),
            Err(ParseError::TruncatedEscape),
            Err(ParseError::BadEscape('E')),
            Err(ParseError::TruncatedEscape),
            Err(ParseError::TruncatedEscape),
        ];
        let mut results = results.iter();

        let lines = include_str!("sample.sym").lines();
        for (line_no, line) in lines.enumerate() {
            let line_no = line_no + 1;
            let res = gb_sym_file::parse_line_with_metadata::<Vec<_>>(line).transpose();
            let expected = results.next().unwrap();
            match res {
                Ok(opt) => {
                    assert_eq!(
                        &opt.as_ref().map(|(name, loc, metadata)| (
                            name.as_str(),
                            loc,
                            metadata.as_slice()
                        )),
                        expected.as_ref().unwrap(),
                        "Line {line_no}"
                    );
                }
                Err(err) => {
                    assert_eq!(
                        err.to_string(),
                        expected.as_ref().unwrap_err().to_string(),
                        "Line {line_no}"
                    );
                }
            }
        }
        assert!(results.next().is_none());
    }
}
