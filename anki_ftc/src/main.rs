use anki_ft_codegen::{GenerationError, Generator};
use std::{
    collections::BTreeSet,
    env::Args,
    fmt::Display,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    process::exit,
};

#[derive(Debug, Ord, Eq, PartialOrd, PartialEq)]
enum Flag {
    Consume,
}

#[derive(Debug)]
enum CommandLineError {
    MissingFile,
    UnknownFlag(String),
    GenerationError(String, GenerationError),
    IoError(std::io::Error),
}

impl std::error::Error for CommandLineError {}

impl Display for CommandLineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingFile => write!(f, "No files provided"),
            Self::UnknownFlag(s) => s.fmt(f),
            Self::GenerationError(filepath, e) => write!(f, "{filepath}:{e}"),
            Self::IoError(e) => e.fmt(f),
        }
    }
}

impl From<std::io::Error> for CommandLineError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}

type Result<T> = std::result::Result<T, CommandLineError>;

fn generate_files(filepath: impl AsRef<Path>, flags: &BTreeSet<Flag>) -> Result<()> {
    let filename = filepath.as_ref();
    let mut src = String::new();
    File::open(filename)?.read_to_string(&mut src)?;

    let stemless = PathBuf::from(filename.file_stem().unwrap());
    let path = PathBuf::from(format!("./{}.txt", stemless.display()));

    match Generator::new(&src, ';', path).generate() {
        Ok(warnings) => {
            for w in warnings {
                eprintln!("{}:{w}", filepath.as_ref().display());
            }
        }
        Err(e) => return Err(CommandLineError::GenerationError(filepath.as_ref().display().to_string(), e)),
    };

    if flags.contains(&Flag::Consume) {
        std::fs::remove_file(filepath)?;
    }

    Ok(())
}

fn parse_args(args: Args) -> Result<()> {
    let mut flags = BTreeSet::new();
    let mut files = vec![];
    for arg in args {
        match arg.as_str() {
            s if s.starts_with("--") => {
                flags.insert(parse_long_flag(s)?);
            }
            s if s.starts_with("-") => flags.extend(parse_flag(s)?),
            f => files.push(f.to_string()),
        }
    }

    if files.is_empty() {
        return Err(CommandLineError::MissingFile);
    }

    for f in files {
        generate_files(&f, &flags)?;
    }
    Ok(())
}

fn parse_long_flag(s: &str) -> Result<Flag> {
    match s {
        "--consume" => Ok(Flag::Consume),
        _ => Err(CommandLineError::UnknownFlag(s.to_string())),
    }
}

fn parse_flag(s: &str) -> Result<BTreeSet<Flag>> {
    fn parse_flag_aux(s: &str, mut acc: BTreeSet<Flag>) -> Result<BTreeSet<Flag>> {
        if s.is_empty() {
            Ok(acc)
        } else {
            match s.split_at(1) {
                ("-", s) => parse_flag_aux(s, acc),
                ("c", s) => {
                    acc.insert(Flag::Consume);
                    parse_flag_aux(s, acc)
                }
                (f, _) => Err(CommandLineError::UnknownFlag(f.to_string())),
            }
        }
    }
    parse_flag_aux(s, BTreeSet::new())
}

fn main() {
    let mut args = std::env::args();
    let program_name = args.next().unwrap();

    match parse_args(args) {
        Ok(_) => (),
        Err(CommandLineError::UnknownFlag(e)) => {
            eprintln!("{program_name}:{e}");
            exit(1);
        }
        Err(CommandLineError::IoError(e)) => {
            eprintln!("{program_name}:{e}");
            exit(1);
        }
        Err(e @ CommandLineError::MissingFile) => {
            eprintln!("{program_name}:{e}");
            exit(1);
        }
        Err(CommandLineError::GenerationError(file, e)) => {
            eprintln!("{program_name}: Compilation failed:");
            eprintln!("{file}:{e}");
            exit(1);
        }
    }
}
