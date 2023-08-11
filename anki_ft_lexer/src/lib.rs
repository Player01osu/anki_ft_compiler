#![allow(dead_code)]
use std::fmt::Display;
use std::str::{Chars, FromStr};
use strum::Display;

pub fn tokenize(src: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(src);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::EOF {
            Some(token)
        } else {
            None
        }
    })
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub len: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Display)]
pub enum KW {
    Let,
}

impl FromStr for KW {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "let" => Ok(Self::Let),
            // FIXME this error sucks LOL
            _ => Err("Not a keyword"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbol {
    KW(KW),
    Other(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    String { item: String, terminated: bool },
    // TODO
    Num(Num),
    Bool(bool),
}

impl Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Num::Int(n) => n.fmt(f),
            Num::Float(n) => n.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Num {
    Int(i64),
    Float(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(Symbol),
    Literal(LiteralKind),
    LineComment,
    BlockComment { terminated: bool },

    Comma,
    Semi,
    Colon,
    Hyphen,
    Pound,
    Dollar,

    Eq,

    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    Whitespace,
    Unknown,
    EOF,
    DummyToken,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::KW(kw) => kw.fmt(f),
            Symbol::Other(s) => s.fmt(f),
        }
    }
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::String { item, .. } => write!(f, "{item}"),
            LiteralKind::Num(n) => n.fmt(f),
            LiteralKind::Bool(b) => b.fmt(f),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ident(i) => i.fmt(f),
            TokenKind::Literal(l) => l.fmt(f),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Hyphen => write!(f, "-"),
            TokenKind::Pound => write!(f, "#"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::OpenDelim(d) => match d {
                Delimiter::Parenthesis => write!(f, "("),
                Delimiter::Brace => write!(f, "["),
                Delimiter::Bracket => write!(f, "{{"),
                Delimiter::AngleBracket => write!(f, "<"),
                Delimiter::NoteType => write!(f, "#["),
            },
            TokenKind::CloseDelim(d) => match d {
                Delimiter::Parenthesis => write!(f, ")"),
                Delimiter::Brace => write!(f, "]"),
                Delimiter::Bracket => write!(f, "}}"),
                Delimiter::AngleBracket => write!(f, ">"),
                Delimiter::NoteType => write!(f, "]"),
            }
            TokenKind::LineComment => write!(f, "LineComment"),
            TokenKind::BlockComment { .. } => write!(f, "BlockComment"),
            TokenKind::Whitespace => write!(f, "Whitespace"),
            TokenKind::Unknown => write!(f, "Unknown"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::DummyToken => write!(f, "DummyToken"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
    /// `< ... >`
    AngleBracket,
    /// `#['
    NoteType,
}

impl Span {
    pub fn new(start_row: usize, start_col: usize, end_row: usize, end_col: usize) -> Self {
        let span = Self {
            start_row,
            start_col,
            end_row,
            end_col,
        };
        assert!(span.valid_span(), "{span} Invalid span");
        span
    }

    pub fn join(self, other: Span) -> Self {
        assert!(
            (other.end_row >= self.start_row && other.end_col >= self.start_col)
                || other.end_row > self.start_row,
            "{self}\n{other}\nInvalid span join"
        );

        Self::new(self.start_row, self.start_col, other.end_row, other.end_col)
    }

    fn valid_span(self) -> bool {
        (self.end_row >= self.start_row && self.end_col >= self.start_col)
            || self.end_row > self.start_row
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} => {}:{}",
            self.start_row, self.start_col, self.end_row, self.end_col
        )
    }
}

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    src: &'a str,
    buf: String,
    len_remaining: usize,
    row: usize,
    col: usize,
    chars: Chars<'a>,
}

impl Token {
    fn new(kind: TokenKind, len: usize, span: Span) -> Self {
        Self { kind, len, span }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            len: 0,
            kind: TokenKind::DummyToken,
            span: Span::default(),
        }
    }
}

const _DELIM: TokenKind = TokenKind::Semi;

fn is_keep_white(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '{' ) || is_string_literal(c)
}

fn is_begins_kw(c: char) -> bool {
    matches!(c, 'l')
}

fn is_newline(c: char) -> bool {
    matches!(c, '\r' | '\n')
}

fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t') || is_newline(c)
}

fn is_string_literal(c: char) -> bool {
    matches!(c, '"')
}

fn is_kw(str: &str) -> bool {
    matches!(str, "let")
}

fn is_end_kw(c: char) -> bool {
    matches!(c, c if is_whitespace(c) || is_end_of_ident(c))
}

fn is_line_comment(c: char) -> bool {
    matches!(c, '-')
}

fn is_block_comment(str: String) -> bool {
    matches!(str.as_str(), "-[[")
}

fn is_end_of_ident(c: char) -> bool {
    matches!(
        c,
        '#' | ';' | '\0' | '='
    ) || is_string_literal(c)
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            len_remaining: src.len(),
            chars: src.chars(),
            buf: String::new(),
            row: 1,
            col: 1,
        }
    }

    pub fn advance_token(&mut self) -> Token {
        let start_chars = self.chars.clone();
        let first_char = match self.chars.next() {
            Some(c) => c,
            None => {
                return Token::new(TokenKind::EOF, 0, Span::default());
            }
        };

        let token_kind = match first_char {
            '-' if is_block_comment(self.peak_n_collect(3)) => self.consume_block_comment(),
            '-' if is_line_comment(self.peak_first()) => self.consume_line_comment(),

            c if is_whitespace(c) => self.consume_whitespace(),

            c if is_string_literal(c) => self.consume_string_literal(c),

            // TODO
            //c if is_literal(c) => self.consume_literal(c),
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semi,
            ':' => TokenKind::Colon,
            '-' => TokenKind::Hyphen,
            '#' if self.peak_first() == '[' => {
                self.chars.next();
                TokenKind::OpenDelim(Delimiter::NoteType)
            }
            '#' => TokenKind::Pound,
            '$' => TokenKind::Dollar,
            '=' => TokenKind::Eq,

            ']' => TokenKind::CloseDelim(Delimiter::Bracket),

            '>' => TokenKind::CloseDelim(Delimiter::AngleBracket),

            '{' => TokenKind::OpenDelim(Delimiter::Brace),
            '}' => TokenKind::CloseDelim(Delimiter::Brace),

            c if is_begins_kw(c) => {
                self.buf.clear();
                self.buf.push(c);
                self.consume_begin_kw_ident()
            }
            c => {
                self.buf.clear();
                self.buf.push(c);
                self.consume_ident()
            } //_ => TokenKind::Unknown,
        };

        let (span, start_next) = self.get_span(start_chars, self.pos_within_token());
        self.reset_span(start_next);

        let token = Token::new(token_kind, self.pos_within_token(), span);
        self.reset_pos_within_token();
        token
    }

    fn reset_span(&mut self, (row, col): (usize, usize)) {
        self.row = row;
        self.col = col;
    }

    fn get_span(&self, chars: Chars<'a>, len: usize) -> (Span, (usize, usize)) {
        let newline_count = chars.clone().take(len).filter(|c| is_newline(*c)).count();
        let last_char = chars.clone().nth(len - 1).expect("Last char should exist.");

        let (end_row, end_col) = match newline_count {
            0 => (self.row, self.col + len - 1),
            1 if is_newline(last_char) => (self.row, self.col + len - 1),
            _ => (
                self.row + newline_count - (is_newline(last_char) as usize),
                chars.as_str()[..len]
                    .chars()
                    .rev()
                    .take_while(|c| !is_newline(*c))
                    .count()
                    + 1
                    - (!is_newline(last_char) as usize),
            ),
        };
        if is_newline(last_char) {
            (
                Span::new(self.row, self.col, end_row, end_col),
                (end_row + 1, 1),
            )
        } else {
            (
                Span::new(self.row, self.col, end_row, end_col),
                (end_row, end_col + 1),
            )
        }
    }

    fn peak_first(&self) -> char {
        match self.chars.clone().next() {
            Some(c) => c,
            // EOF
            None => '\0',
        }
    }

    fn peak_non_white(&self) -> char {
        self.chars
            .clone()
            .skip_while(|c| is_whitespace(*c))
            .next()
            .unwrap_or('\0')
    }

    fn peak_n(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or('\0')
    }

    // TODO could compare buf directly
    fn peak_n_collect(&mut self, mut n: usize) -> String {
        self.buf.clear();
        loop {
            self.buf.push(self.peak_n(n - 1));
            n -= 1;
            if n == 0 {
                return self.buf.chars().rev().collect();
            }
        }
    }

    pub fn pos_within_token(&self) -> usize {
        self.len_remaining - self.chars.as_str().len()
    }

    fn reset_pos_within_token(&mut self) {
        self.len_remaining = self.chars.as_str().len()
    }

    fn consume_while(&mut self, f: impl Fn(&char) -> bool) {
        // Peakable
        let mut chars = self.chars.clone();

        while let Some(c) = chars.next() {
            if !f(&c) {
                break;
            }
            self.chars.next();
        }
    }

    fn consume_block_comment(&mut self) -> TokenKind {
        fn transition_state(c: char, state: &mut String) {
            match state.as_str() {
                "-" if c == '-' => state.push('-'),
                "--" if c == ']' => state.push(']'),
                "--]" if c == ']' => state.push(']'),
                _ if c == '-' => state.push('-'),
                _ => state.clear(),
            }
        }

        self.buf.clear();
        while self.buf.as_str() != "--]]" {
            let c = match self.chars.next() {
                Some(c) => c,
                None => return TokenKind::BlockComment { terminated: false },
            };
            transition_state(c, &mut self.buf);
        }
        self.buf.clear();

        TokenKind::BlockComment { terminated: true }
    }

    fn consume_line_comment(&mut self) -> TokenKind {
        self.consume_while(|c| *c != '\n' && *c != '\r');
        TokenKind::LineComment
    }

    fn consume_begin_kw_ident(&mut self) -> TokenKind {
        loop {
            let c = match self.chars.next() {
                Some(c) => c,
                None => return TokenKind::Ident(Symbol::Other(self.buf.clone())),
            };
            self.buf.push(c);
            let peak_first = self.peak_first();
            if is_end_of_ident(peak_first) || is_end_kw(peak_first) {
                break;
            }
        }

        if !is_kw(&self.buf) {
            return self.consume_ident();
        }

        TokenKind::Ident(Symbol::KW(KW::from_str(&self.buf).unwrap()))
    }

    fn consume_ident(&mut self) -> TokenKind {
        //if self.buf.chars().next().unwrap() == '\\' {
        //    if self.peak_first() == '"' {
        //        self.buf.push('"');
        //        self.buf.push('"');
        //    }
        //    self.chars.next();
        //}

        loop {
            // FIXME ??????
            let peak_first = self.peak_first();

            if is_whitespace(peak_first) || is_end_of_ident(peak_first) {
                let peak_non_white = self.peak_non_white();
                if is_keep_white(peak_non_white) {
                    loop {
                        let c = self.peak_first();
                        if !is_whitespace(c) {
                            break;
                        }
                        self.buf.push(c);
                        self.chars.next();
                    }
                }
                if is_end_of_ident(peak_non_white) {
                    break;
                }
            }
            let c = match self.chars.next() {
                Some(c) if is_end_of_ident(c) => break,
                Some(c) => c,
                None => return TokenKind::Ident(Symbol::Other(self.buf.clone())),
            };
            self.buf.push(c);
        }

        TokenKind::Ident(Symbol::Other(self.buf.clone()))
    }

    fn consume_string_literal(&mut self, literal_char: char) -> TokenKind {
        self.buf.clear();
        let kind = loop {
            let c = match self.chars.next() {
                Some(c) => c,
                None => {
                    return TokenKind::Literal(LiteralKind::String {
                        item: self.buf.clone(),
                        terminated: false,
                    })
                }
            };

            match c {
                // Escaped
                '\\' if self.peak_first() == literal_char => {
                    let c = match self.chars.next() {
                        Some(c) => c,
                        None => {
                            return TokenKind::Literal(LiteralKind::String {
                                item: self.buf.clone(),
                                terminated: false,
                            })
                        }
                    };

                    // Escape quotation with two quotes (ie: "")
                    if literal_char == '"' {
                        self.buf.push(c);
                    }
                    self.buf.push(c);
                }
                c if c == literal_char => {
                    break LiteralKind::String {
                        item: self.buf.clone(),
                        terminated: true,
                    };
                }
                _ => self.buf.push(c),
            }
        };
        TokenKind::Literal(kind)
    }

    fn consume_whitespace(&mut self) -> TokenKind {
        self.consume_while(|c| is_whitespace(*c));
        TokenKind::Whitespace
    }
}

impl Token {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn kind(&self) -> TokenKind {
        self.kind.clone()
    }
}
