use std::collections::BTreeMap;
use std::fmt::Display;
use std::fs::File;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::path::PathBuf;

use anki_ft_parse::Card;
use anki_ft_parse::Command;
use anki_ft_parse::Parser;
use anki_ft_parse::StringReader;
use anki_ft_parse::Token;

#[derive(Debug)]
pub struct Generator<'a> {
    pub src: &'a str,
    pub parser: Parser<'a>,
    statements: BTreeMap<String, String>,
    file_map: BTreeMap<Header, (usize, Fields)>,
}

#[derive(Debug, Clone, Eq, Ord, PartialOrd, PartialEq, Hash)]
pub struct Header {
    deck: String,
    notetype: String,
    separator: Option<Separator>,
    html: bool,
    columns: Option<Vec<String>>,
    notetype_column: Option<usize>,
    deck_column: Option<usize>,
    tags_column: Option<usize>,
    guid_column: Option<usize>,
}

impl Display for Header {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#deck: {}", self.deck)?;
        write!(f, "\n#notetype: {}", self.notetype)?;
        write!(f, "\n#html: {}", self.html)?;
        self.columns.as_ref().and_then(|v| {
            write!(
                f,
                "\n#columns: {}",
                v.join(
                    self.separator
                        .as_ref()
                        .unwrap_or(&Separator::Semicolon)
                        .to_string()
                        .as_str()
                )
            )
            .ok()
        });
        self.separator
            .as_ref()
            .and_then(|v| write!(f, "\n#separator: {}", v).ok());
        self.notetype_column
            .as_ref()
            .and_then(|v| write!(f, "\n#notetype column: {}", v).ok());
        self.deck_column
            .as_ref()
            .and_then(|v| write!(f, "\n#deck column: {}", v).ok());
        self.tags_column
            .as_ref()
            .and_then(|v| write!(f, "\n#tags column: {}", v).ok());
        self.guid_column
            .as_ref()
            .and_then(|v| write!(f, "\n#guid column: {}", v).ok());
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

#[derive(Debug, Clone, Eq, Ord, PartialOrd, PartialEq, Hash, Default)]
pub enum Separator {
    Comma,
    #[default]
    Semicolon,
    Tab,
    Space,
    Pipe,
    Colon,
}

impl Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Separator::Comma => write!(f, ","),
            Separator::Semicolon => write!(f, ";"),
            Separator::Tab => write!(f, "\t"),
            Separator::Space => write!(f, " "),
            Separator::Pipe => write!(f, "|"),
            Separator::Colon => write!(f, ":"),
        }
    }
}

type Fields = Vec<String>;

impl<'a> Generator<'a> {
    pub fn new(src: &'a str) -> Self {
        let statements = BTreeMap::from([("notetype".to_owned(), "Basic".to_owned())]);

        Self {
            src,
            parser: Parser::new(StringReader::new(src)),
            file_map: BTreeMap::new(),
            statements,
        }
    }

    pub fn begin(mut self) {
        loop {
            let token = self.parser.next_token().unwrap();
            match token {
                Token::Card(card) => {
                    let notetype = card
                        .notetype
                        .notetype
                        .as_ref()
                        .map(|n| n.to_string())
                        .unwrap_or(self.statements.get("notetype").unwrap().to_owned());

                    let deck = self
                        .statements
                        .get("deck")
                        .expect("Deck is undefined")
                        .to_string();

                    let header = Header {
                        deck,
                        notetype,
                        html: true,
                        separator: None,
                        columns: None,
                        notetype_column: None,
                        deck_column: None,
                        tags_column: None,
                        guid_column: None,
                    };
                    let header_str = header.to_string();

                    fn push_field(fields: &mut Fields, card: &Card, n: usize) {
                        let card_fields_n = card.to_string().chars().filter(|c| *c == ';').count() + 1;
                        if n != card_fields_n {
                            println!(
                                "{} WARNING: Card field lengths do not match: {} {}",
                                card.span, n, card_fields_n
                            );
                        }
                        fields.push(card.to_string());
                    }

                    self.file_map
                        .entry(header)
                        .and_modify(|(n, fields)| push_field(fields, &card, *n))
                        .or_insert((
                            card.to_string().chars().filter(|c| *c == ';').count() + 1,
                            vec![header_str, card.to_string()],
                        ));
                }
                Token::Command(Command::LetStatement(statement)) => {
                    self.statements
                        .insert(statement.lhs.to_string(), statement.rhs.to_string());
                }
                Token::EOF => break,
                _ => (),
            };
        }
        for (k, (_, v)) in self.file_map.iter() {
            let mut file = File::create(PathBuf::from(k)).unwrap();
            file.write(v.as_slice().join("\n").as_bytes()).unwrap();
        }
        //dbg!(&self);
    }

    pub fn next_card(&mut self) -> String {
        let token = self.parser.next_token().unwrap();
        match token {
            Token::Card(card) => card.to_string(),
            Token::EOF => "".to_owned(),
            _ => self.next_card(),
        }
    }
}
