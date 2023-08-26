use std::{
    collections::BTreeMap,
    error::Error,
    fmt::Display,
    fs::File,
    hash::{Hash, Hasher},
    io::{BufWriter, Write},
    path::PathBuf,
};

use anki_ft_lexer::span::Span;
use anki_ft_parse::{
    Command, Let, Note, Notetype, ParseError, Parser, Rhs, TokenKind,
};

type NFields = usize;

#[derive(Debug)]
pub struct Generator<'a> {
    parser: Parser<'a>,

    current_header: HeaderBuilder,
    statements: BTreeMap<String, Rhs>,
    filemap: BTreeMap<Header, (NFields, Vec<Note>)>,
}

#[derive(Debug, Clone)]
pub struct HeaderBuilder {
    deck: Option<String>,
    notetype: Option<String>,
    separator: Option<char>,
    html: Option<bool>,
    columns: Option<Vec<String>>,
    notetype_column: Option<usize>,
    deck_column: Option<usize>,
    tags_column: Option<usize>,
    guid_column: Option<usize>,
}

impl HeaderBuilder {
    pub fn build(&mut self, note_span: Span, notetype: &Notetype) -> Result<Header> {
        let header = Header {
            deck: self.deck.clone().ok_or(GenerationError::MissingHeader(
                note_span,
                MissingHeader::Deck,
            ))?,
            notetype: match notetype.notetype {
                Some(ref notetype) => notetype.clone(),
                None => self.notetype.clone().ok_or(GenerationError::MissingHeader(
                    note_span,
                    MissingHeader::Notetype,
                ))?,
            },
            separator: self.separator.unwrap_or(';'),
            html: self.html.unwrap_or(true),
            columns: self.columns.clone(),
            notetype_column: self.notetype_column,
            deck_column: self.deck_column,
            tags_column: self.tags_column,
            guid_column: self.guid_column,
        };
        Ok(header)
    }
}

impl Default for HeaderBuilder {
    fn default() -> Self {
        HeaderBuilder {
            deck: None,
            notetype: None,
            separator: None,
            html: None,
            columns: None,
            notetype_column: None,
            deck_column: None,
            tags_column: None,
            guid_column: None,
        }
    }
}

#[derive(Debug, Clone, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct Header {
    deck: String,
    notetype: String,
    separator: char,
    html: bool,
    columns: Option<Vec<String>>,
    notetype_column: Option<usize>,
    deck_column: Option<usize>,
    tags_column: Option<usize>,
    guid_column: Option<usize>,
}

impl Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#deck: {}\n", self.deck)?;
        write!(f, "#notetype: {}\n", self.notetype)?;
        write!(f, "#html: {}\n", self.html)?;
        self.columns.as_ref().and_then(|v| {
            write!(
                f,
                "#columns: {}\n",
                // TODO SHEEEEEEEEEEESH
                v.join(&self.separator.to_string()).to_string()
            )
            .ok()
        });
        write!(f, "#separator: {}\n", self.separator)?;
        self.notetype_column
            .as_ref()
            .map(|v| write!(f, "#notetype column: {}\n", v));
        self.deck_column
            .as_ref()
            .map(|v| write!(f, "#deck column: {}\n", v));
        self.tags_column
            .as_ref()
            .map(|v| write!(f, "#tags column: {}\n", v));
        self.guid_column
            .as_ref()
            .map(|v| write!(f, "#guid column: {}\n", v));
        Ok(())
    }
}

impl From<&Header> for PathBuf {
    fn from(header: &Header) -> PathBuf {
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        header.hash(&mut hasher);
        format!(
            "{}_{:x}.txt",
            header.notetype.to_lowercase().replace([' ', '\n'], "-"),
            hasher.finish()
        )
        .into()
    }
}

fn single_char(s: &str, span: Span) -> Result<char> {
    match s.as_bytes() {
        [b] => Ok(*b as char),
        _ => Err(GenerationError::Other(
            span,
            String::from("Separator character must be single char"),
        )),
    }
}

fn into_bool(s: &str, span: Span) -> Result<bool> {
    s.parse()
        .map_err(|_| GenerationError::Other(span, format!("Expected bool (true|false): got {s}")))
}

#[derive(Debug)]
pub enum GenerationError {
    ParseError(ParseError),
    MissingHeader(Span, MissingHeader),
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
            Self::MissingHeader(span, s) => write!(f, "{span}: {s}"),
            Self::Other(span, s) => write!(f, "{span}: {s}"),
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
    pub fn new(src: &'a str, field_separator: char) -> Self {
        Self {
            parser: Parser::new(src, field_separator),
            current_header: HeaderBuilder::default(),
            statements: BTreeMap::new(),
            filemap: BTreeMap::new(),
        }
    }

    pub fn generate(mut self) -> Result<()> {
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

    fn write_file(self) -> Result<()> {
        for (header, (n, notes)) in self.filemap {
            let mut file = BufWriter::new(File::create(PathBuf::from(&header))?);
            file.write(header.to_string().as_bytes())?;
            file.write(&[b'\n'])?;

            for note in notes {
                if note.fields.len() != n {
                    println!(
                        "{}:WARNING:Note lengths do not match: {} {}",
                        note.span,
                        note.fields.len(),
                        n
                    );
                }

                file.write(note.format(header.separator).as_bytes())?;
                file.write(&[b'\n'])?;
            }

            file.flush()?;
        }

        Ok(())
    }

    fn generate_cmd(&mut self, cmd: Command) -> Result<()> {
        match cmd {
            Command::Let(v) => self.generate_let(v),
        }
    }

    fn generate_note(&mut self, note: Note) -> Result<()> {
        let n_fields = note.fields.len();
        let header = self.current_header.build(note.span, &note.notetype)?;

        self.filemap
            .entry(header)
            .and_modify(|(_, v)| v.push(note.clone()))
            .or_insert((n_fields, vec![note]));
        Ok(())
    }

    fn generate_let(&mut self, v: Let) -> Result<()> {
        let (lhs, rhs) = v.command();

        match lhs {
            "deck" => self.current_header.deck = Some(rhs.as_str()),
            "notetype" | "default_card" => self.current_header.notetype = Some(rhs.as_str()),
            "separator" => {
                self.current_header.separator = Some(single_char(&rhs.as_str(), v.span)?)
            }
            "html" => self.current_header.html = Some(into_bool(&rhs.as_str(), v.span)?),
            // TODO
            //"columns" => self.current_header.columns = Some(rhs.as_str()),
            //"notetype_column" => self.current_header.notetype_column = Some(rhs.as_str()),
            //"deck_column" => self.current_header.deck_column = Some(rhs.as_str()),
            //"tags_column" => self.current_header.tags_column = Some(rhs.as_str()),
            //"guid_column" => self.current_header.guid_column = Some(rhs.as_str()),
            _ => {
                self.statements
                    .entry(lhs.to_string())
                    .and_modify(|v| *v = rhs.clone())
                    .or_insert(rhs);
            }
        }
        Ok(())
    }
}
