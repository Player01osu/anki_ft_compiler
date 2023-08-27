#![allow(dead_code)]
use std::{error::Error, fmt::Display, fmt::Write, str::FromStr};

use anki_ft_lexer::{Keyword, Lexer, Token as LexerToken, TokenKind as LexerTokenKind, Literal, span::Span};

#[derive(Debug)]
pub struct Parser<'a> {
    src: &'a str,
    lexer: Lexer<'a>,

    current_token: LexerToken,
    backup_token: Option<LexerToken>,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TokenKind {
    Command(Command),
    Note(Note),
    Field(Field),
    Notetype(Notetype),
}

#[derive(Debug, Clone)]
pub struct Note {
    pub fields: Vec<Field>,
    pub notetype: Notetype,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub field: String,
    pub separated: Option<Span>,
    pub span: Span,
}

impl Field {
    pub fn sanitize_field(&self) -> String {
        self.field.replace('\n', "<br>")
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Notetype {
    pub notetype: Option<String>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Command {
    Let(Let),
}

#[derive(Debug)]
pub struct Let {
    lhs: Ident,
    rhs: Rhs,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ident(String);

#[derive(Debug, Clone)]
pub enum Rhs {
    Ident(Ident),
    Literal(Literal),
}

#[derive(Debug)]
pub enum ParseError {
    Unexpected(Span),
    BadSeparator(Span, String),
    EmptyNote(Span),
}

#[derive(Debug, Clone, Copy)]
pub enum Separator {
    Comma,
    Semicolon,
    Tab,
    Space,
    Pipe,
    Colon,
}

impl FromStr for Separator {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "Comma" => Ok(Self::Comma),
            "Semicolon" => Ok(Self::Semicolon),
            "Tab" => Ok(Self::Tab),
            "Space" => Ok(Self::Space),
            "Pipe" => Ok(Self::Pipe),
            "Colon" => Ok(Self::Colon),
            _ => Err(s.to_string()),
        }
    }
}

impl Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Separator::Comma => write!(f, "Comma"),
            Separator::Semicolon => write!(f, "Semicolon"),
            Separator::Tab => write!(f, "Tab"),
            Separator::Space => write!(f, "Space"),
            Separator::Pipe => write!(f, "Pipe"),
            Separator::Colon => write!(f, "Colon"),
        }
    }
}

impl Separator {
    pub fn as_char(&self) -> char {
        match self {
            Separator::Comma => ',',
            Separator::Semicolon => ';',
            Separator::Tab => '\t',
            Separator::Space => ' ',
            Separator::Pipe => '|',
            Separator::Colon => ':',
        }
    }
}

pub const SEPARATORS: &'static str = "
    \"Comma\",
    \"Semicolon\",
    \"Tab\",
    \"Space\",
    \"Pipe\",
    \"Colon\",
";

impl Note {
    pub fn format(self, deck: &str, notetype: &str, separator: Separator) -> String {
        let mut buf = String::new();
        write!(&mut buf, "{deck}{separator}{notetype}{separator}", separator = separator.as_char()).unwrap();
        for field in self.fields {
            buf.push_str(field.sanitize_field().as_str());
            if field.separated.is_some() {
                buf.push(separator.as_char());
            }
        }
        buf
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Unexpected(span) => write!(f, "{span}:Unexpect token"),
            ParseError::EmptyNote(span) => write!(f, "{span}:Empty note"),
            ParseError::BadSeparator(span, s) => write!(f, "{span}:Bad Separator: \"{s}\"
Expected: {}", SEPARATORS),
        }
    }
}

impl Error for ParseError {
}

type Result<T> = std::result::Result<T, ParseError>;

macro_rules! expect_lexer {
    ($self: ident, $kind: pat) => {{
        match $self.bump_lexer_no_white() {
            lexer_token @ LexerToken { kind: $kind, .. } => lexer_token,
            LexerToken { span, .. } => return Err(ParseError::Unexpected(span)),
        }
    }};
}

impl Let {
    pub fn command(&self) -> (&str, Rhs) {
        (self.lhs.as_str(), self.rhs.clone())
    }
}

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Rhs {
    pub fn as_str(&self) -> String {
        match self {
            Self::Ident(ident) => ident.as_str().to_owned(),
            Self::Literal(literal) => literal.to_string(),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, separator: char) -> Self {
        Self {
            src,
            lexer: Lexer::new(src, separator),

            current_token: LexerToken::dummy(),
            backup_token: None,
        }
    }

    fn bump_lexer(&mut self) -> LexerToken {
        self.lexer.next_token()
    }

    fn bump_lexer_no_white(&mut self) -> LexerToken {
        match self.lexer.next_token() {
            LexerToken {
                kind: LexerTokenKind::Whitespace,
                ..
            } => self.bump_lexer_no_white(),
            lexer_token => lexer_token,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        self.current_token = match self.backup_token.take() {
            Some(token) => token,
            None => self.bump_lexer(),
        };

        match self.current_token.kind {
            LexerTokenKind::LineComment => self.next_token(),
            LexerTokenKind::BlockComment => self.next_token(),
            LexerTokenKind::Whitespace => self.next_token(),

            LexerTokenKind::Ident(_) => todo!(),
            LexerTokenKind::Literal(_) => todo!(),
            LexerTokenKind::Keyword(_) => todo!(),
            LexerTokenKind::Notetype(ref notetype) => Ok(Some(self.parse_note(notetype.clone())?)),
            LexerTokenKind::CardField(_) => todo!(),
            LexerTokenKind::FieldSeparator { .. } => todo!(),
            LexerTokenKind::BeginCommand => Ok(Some(self.parse_command()?)),
            LexerTokenKind::Eof => return Ok(None),
            LexerTokenKind::EndCommand => todo!(),
            LexerTokenKind::Assignment => todo!(),
            LexerTokenKind::Illegal => todo!(),
            LexerTokenKind::Dummy => todo!(),
        }
    }

    fn parse_command(&mut self) -> Result<Token> {
        match self.bump_lexer_no_white() {
            LexerToken {
                kind: LexerTokenKind::Keyword(Keyword::Let),
                ..
            } => self.parse_let(),
            LexerToken { span, .. } => Err(ParseError::Unexpected(span)),
        }
    }

    fn parse_note(&mut self, notetype: Option<String>) -> Result<Token> {
        let start_span = self.current_token.span;
        let notetype = Notetype {
            notetype,
            span: self.current_token.span,
        };
        let fields = self.parse_field()?;
        let end_span = fields
            .last()
            .expect("Fields should propagate error if empty.")
            .span;

        let span = start_span.join(end_span);
        let kind = TokenKind::Note(Note {
            fields,
            notetype,
            span,
        });
        Ok(Token { kind, span })
    }

    fn parse_field(&mut self) -> Result<Vec<Field>> {
        self.parse_field_aux(vec![])
    }

    fn parse_field_aux(&mut self, mut acc: Vec<Field>) -> Result<Vec<Field>> {
        let lexer_token = expect_lexer!(
            self,
            LexerTokenKind::CardField(..)
                | LexerTokenKind::FieldSeparator { .. }
                | LexerTokenKind::BeginCommand
                | LexerTokenKind::Notetype(..)
                | LexerTokenKind::LineComment
                | LexerTokenKind::BlockComment
                | LexerTokenKind::Eof
        );

        match lexer_token {
            LexerToken {
                kind:
                    LexerTokenKind::BeginCommand
                    | LexerTokenKind::Notetype(..)
                    | LexerTokenKind::LineComment
                    | LexerTokenKind::BlockComment
                    | LexerTokenKind::Eof,
                span,
            } => {
                if acc.is_empty() {
                    Err(ParseError::EmptyNote(span))
                } else {
                    match acc.last().unwrap() {
                        Field {
                            separated: Some(span),
                            ..
                        } => {
                            acc.push(Field {
                                field: String::new(),
                                span: *span,
                                separated: None,
                            });
                        }
                        _ => (),
                    }
                    self.backup_token = Some(lexer_token);
                    Ok(acc)
                }
            }
            LexerToken {
                kind: LexerTokenKind::CardField(field),
                span,
            } => {
                let field = Field {
                    field,
                    separated: None,
                    span,
                };
                acc.push(field);
                self.parse_field_aux(acc)
            }
            LexerToken {
                kind: LexerTokenKind::FieldSeparator { .. },
                span,
            } => {
                let last = acc.pop();

                match last {
                    Some(
                        field @ Field {
                            separated: Some(..),
                            ..
                        },
                    ) => {
                        acc.push(field);
                        acc.push(Field {
                            field: String::new(),
                            separated: Some(span),
                            span,
                        })
                    }
                    Some(v) => acc.push(Field {
                        field: v.field,
                        separated: Some(span),
                        span: v.span.join(span),
                    }),
                    None => acc.push(Field {
                        field: String::new(),
                        separated: Some(span),
                        span,
                    }),
                }
                self.parse_field_aux(acc)
            }
            _ => unreachable!(),
        }
    }

    fn parse_let(&mut self) -> Result<Token> {
        let start_span = self.current_token.span;
        let lhs = match self.bump_lexer_no_white() {
            LexerToken {
                kind: LexerTokenKind::Ident(string),
                ..
            } => Ident(string),
            LexerToken { span, .. } => return Err(ParseError::Unexpected(span)),
        };
        expect_lexer!(self, LexerTokenKind::Assignment);
        let rhs = match self.bump_lexer_no_white() {
            LexerToken {
                kind: LexerTokenKind::Ident(string),
                ..
            } => Rhs::Ident(Ident(string)),
            LexerToken {
                kind: LexerTokenKind::Literal(literal),
                ..
            } => Rhs::Literal(literal),
            LexerToken { span, .. } => return Err(ParseError::Unexpected(span)),
        };

        let end_command = expect_lexer!(self, LexerTokenKind::EndCommand);

        let span = Span {
            start_row: start_span.start_row,
            start_col: start_span.start_col,
            end_row: end_command.span.end_row,
            end_col: end_command.span.end_col,
        };

        let kind = TokenKind::Command(Command::Let(Let { lhs, rhs, span }));

        Ok(Token { kind, span })
    }
}
