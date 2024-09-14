#![allow(dead_code)]
use std::{
    collections::BTreeMap,
    error::Error,
    fmt::Display,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

use anki_ft_lexer::span::Span;
use anki_ft_parse::{Command, Let, Note, ParseError, Parser, Rhs, Sanitizer, Separator, TokenKind};

type NFields = usize;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct NoteId {
    notetype: String,
    deck: String,
    sanitizer: Sanitizer,
}

#[derive(Debug)]
pub struct Generator<'a> {
    parser: Parser<'a>,
    warnings: Vec<String>,

    path: PathBuf,
    header: Header,
    notedeck_map: BTreeMap<NoteId, (NFields, Vec<Note>)>,
    statements: BTreeMap<Box<str>, Rhs>,
}

#[derive(Debug, Clone)]
pub struct Header {
    deck: Option<String>,
    notetype: Option<String>,
    separator: Separator,
    html: bool,
    columns: Option<Vec<String>>,
    notetype_column: Option<usize>,
    deck_column: Option<usize>,
    tags_column: Option<usize>,
    guid_column: Option<usize>,
}

impl Default for Header {
    fn default() -> Self {
        Header {
            deck: None,
            notetype: None,
            separator: Separator::Semicolon,
            html: true,
            columns: None,
            notetype_column: None,
            deck_column: None,
            tags_column: None,
            guid_column: None,
        }
    }
}

impl Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "#html:{}", self.html)?;
        self.columns.as_ref().and_then(|v| {
            writeln!(
                f,
                "#columns:{}",
                // TODO SHEEEEEEEEEEESH
                v.join(&self.separator.to_string()).to_string()
            )
            .ok()
        });
        writeln!(f, "#separator:{}", self.separator)?;
        self.notetype_column
            .as_ref()
            .map(|v| writeln!(f, "#notetype column:{}", v));
        self.guid_column
            .as_ref()
            .map(|v| writeln!(f, "#guid column:{}", v));
        writeln!(f, "#deck column:1")?;
        writeln!(f, "#notetype column:2")?;
        Ok(())
    }
}

fn into_bool(s: &str, span: Span) -> Result<bool> {
    s.parse()
        .map_err(|_| GenerationError::Other(span, format!("Expected bool (true|false): got {s}")))
}

#[derive(Debug)]
pub enum GenerationError {
    ParseError(ParseError),
    MissingHeader(MissingHeader),
    BadSeparator(Span, String),
    IOError(std::io::Error),
    Other(Span, String),
}

#[derive(Debug)]
pub enum MissingHeader {
    Deck,
    Notetype,
}

impl From<std::io::Error> for GenerationError {
    fn from(value: std::io::Error) -> Self {
        Self::IOError(value)
    }
}

impl Error for MissingHeader {}

impl Display for MissingHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deck => write!(f, "Missing deck"),
            Self::Notetype => write!(f, "Missing notetype"),
        }
    }
}

impl Display for GenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError(e) => e.fmt(f),
            Self::IOError(e) => e.fmt(f),
            Self::MissingHeader(s) => write!(f, "{s}"),
            Self::Other(span, s) => write!(f, "{span}: {s}"),
            Self::BadSeparator(span, s) => write!(f, "{span}: {s}"),
        }
    }
}

impl From<ParseError> for GenerationError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

type Result<T> = std::result::Result<T, GenerationError>;

impl<'a> Generator<'a> {
    pub fn new(src: &'a str, field_separator: char, path: PathBuf) -> Self {
        Self {
            parser: Parser::new(src, field_separator),
            warnings: vec![],
            path,
            header: Header::default(),
            statements: BTreeMap::new(),
            notedeck_map: BTreeMap::new(),
        }
    }

    pub fn generate(mut self) -> Result<Vec<String>> {
        loop {
            let token = match self.parser.next_token()? {
                Some(t) => t,
                None => break,
            };
            match token.kind {
                TokenKind::Command(cmd) => self.generate_cmd(cmd)?,
                TokenKind::Note(note) => self.generate_note(note)?,
                TokenKind::Field(_) | TokenKind::Notetype(_) => unreachable!(),
            }
        }

        self.write_file()
    }

    fn write_file(self) -> Result<Vec<String>> {
        let mut file = BufWriter::new(File::create(&self.path)?);
        let header = self.header;
        let mut warnings = self.warnings;
        file.write(header.to_string().as_bytes())?;
        file.write(&[b'\n'])?;
        for (
            NoteId {
                deck,
                notetype,
                sanitizer,
            },
            (n, notes),
        ) in self.notedeck_map
        {
            for note in notes {
                if note.fields.len() != n {
                    warnings.push(format!(
                        "{span}:WARNING:Note lengths do not match:
Expect: {expect}
Got: {got}",
                        span = note.span,
                        expect = n,
                        got = note.fields.len()
                    ));
                }

                file.write(
                    note.format(sanitizer, &deck, &notetype, header.separator)
                        .as_bytes(),
                )?;
                file.write(&[b'\n'])?;
            }
        }
        file.flush()?;

        Ok(warnings)
    }

    fn generate_cmd(&mut self, cmd: Command) -> Result<()> {
        match cmd {
            Command::Let(v) => self.generate_let(v),
        }
    }

    fn generate_note(&mut self, note: Note) -> Result<()> {
        let n_fields = note.fields.len();
        let notetype = match note.notetype.notetype {
            Some(ref notetype) => notetype.clone(),
            None => self
                .header
                .notetype
                .clone()
                .ok_or(GenerationError::MissingHeader(MissingHeader::Notetype))?,
        };
        let deck = self
            .header
            .deck
            .clone()
            .ok_or(GenerationError::MissingHeader(MissingHeader::Deck))?;

        let sanitizer = Sanitizer {
            ignore_newlines: self
                .statements
                .get("ignore_newlines")
                .map(|v| v.as_str().parse::<bool>())
                .unwrap_or(Ok(false))
                .map_err(|e| GenerationError::Other(note.span, e.to_string()))?,
        };

        self.notedeck_map
            .entry(NoteId {
                notetype,
                deck,
                sanitizer,
            })
            .and_modify(|(_, v)| v.push(note.clone()))
            .or_insert((n_fields, vec![note]));

        Ok(())
    }

    fn generate_let(&mut self, v: Let) -> Result<()> {
        let (lhs, rhs) = v.command();

        match lhs {
            "deck" => self.header.deck = Some(rhs.as_str()),
            "notetype" | "default_card" => self.header.notetype = Some(rhs.as_str()),
            "separator" => {
                self.header.separator = rhs
                    .as_str()
                    .parse::<Separator>()
                    .map_err(|e| ParseError::BadSeparator(v.span, e))?
            }
            "html" => self.header.html = into_bool(&rhs.as_str(), v.span)?,
            // TODO
            //"columns" => self.current_header.columns = Some(rhs.as_str()),
            //"notetype_column" => self.current_header.notetype_column = Some(rhs.as_str()),
            //"deck_column" => self.current_header.deck_column = Some(rhs.as_str()),
            //"tags_column" => self.current_header.tags_column = Some(rhs.as_str()),
            //"guid_column" => self.current_header.guid_column = Some(rhs.as_str()),
            _ => {
                self.statements
                    .entry(lhs.into())
                    .and_modify(|v| *v = rhs.clone())
                    .or_insert(rhs);
            }
        }
        Ok(())
    }
}
