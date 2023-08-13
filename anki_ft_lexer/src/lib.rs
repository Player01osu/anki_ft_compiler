#![allow(dead_code)]
use std::{str::Chars, fmt::Display};
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Let,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
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

impl Span {
    pub fn join(self, other: Span) -> Self {
        assert!(
            (other.end_row >= self.start_row && other.end_col >= self.start_col)
                || other.end_row > self.start_row,
            "{self}\n{other}\nInvalid span join"
        );

        Self {
            start_row: self.start_row,
            start_col: self.start_col,
            end_row: other.end_row,
            end_col: other.end_col,
        }
    }

    fn valid_span(self) -> bool {
        (self.end_row >= self.start_row && self.end_col >= self.start_col)
            || self.end_row > self.start_row
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    LineComment,
    BlockComment,

    Ident(String),
    StringLiteral(String),
    Keyword(Keyword),
    Whitespace,

    Notetype(Option<String>),
    CardField(String),
    FieldSeparator,

    BeginCommand,
    EndCommand,
    Assignment,

    Eof,
    Illegal,
    Dummy,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    remaining: usize,

    prev_newline: bool,
    command_mode: bool,
    current_char: char,

    field_separator: char,

    row: usize,
    col: usize,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start_row: 0,
            start_col: 0,
            end_row: 0,
            end_col: 0,
        }
    }
}

fn eof_token() -> Token {
    Token {
        kind: TokenKind::Eof,
        span: Span::default(),
    }
}

fn is_end_ident(c: char) -> bool {
    matches!(c, ';' | '=') || c.is_whitespace()
}

fn is_keyword(s: &str) -> bool {
    matches!(s, "let")
}

impl Keyword {
    pub fn new(s: String) -> Self {
        match s.as_str() {
            "let" => Keyword::Let,
            s => panic!("Invalid keyword: {s}"),
        }
    }
}

fn is_end_cardfield(current_char: char, next_char: char, field_separator: char) -> bool {
    let card_type = current_char.is_whitespace() && next_char == '#';
    let end_field = next_char == field_separator;
    let begin_command = next_char == '>' && current_char == '\n';
    card_type || end_field || begin_command
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, field_separator: char) -> Self {
        Self {
            src,
            chars: src.chars(),
            field_separator,
            remaining: src.len(),
            prev_newline: true,
            command_mode: false,
            current_char: '\0',
            row: 1,
            col: 1,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let start_chars = self.chars.clone();
        let c = match dbg!(self.bump()) {
            Some(c) => c,
            None => return eof_token(),
        };

        let kind = match c {
            c if c.is_whitespace() => self.consume_whitespace(),
            '>' if self.prev_newline => self.begin_command(),
            '#' if self.is_notetype() => self.consume_notetype(),

            ';' if self.command_mode => self.end_command(),
            '=' if self.command_mode => TokenKind::Assignment,

            '"' => self.consume_string_literal(),
            c if self.field_separator == c => TokenKind::FieldSeparator,
            _ if self.command_mode => self.consume_ident(),
            _ => self.consume_cardfield(),
        };

        if self.current_char == '\n' {
            self.prev_newline = true;
        }

        let span = self.get_span(start_chars);
        self.reset_pos(span);

        Token { kind, span }
    }

    fn reset_pos(&mut self, span: Span) {
        let ends_newline = self.current_char == '\n';

        self.remaining = self.chars.as_str().len();
            //.graphemes(true).count();
        self.row = span.end_row + ends_newline as usize;
        self.col = if ends_newline { 1 } else { span.end_col + 1 };
    }

    fn get_span(&self, start_chars: Chars) -> Span {
        // Figure out unicode span shit.
        let len = self.remaining - self.chars.as_str()
            //.graphemes(true).count();
            .len();

        let newlines = start_chars
            .clone()
            //.as_str()
            //.graphemes(true)
            .take(len)
            .filter(|c| *c == '\n')
            .count();
        let ends_newline = self.current_char == '\n';

        let start_row = self.row;
        let start_col = self.col;
        let end_row = match newlines {
            0 => start_row,
            _ => start_row + newlines - ends_newline as usize,
        };
        let end_col = match newlines {
            0 => start_col + len,
            1 if ends_newline => start_col + len,
            _ => start_chars
                .clone()
                //.as_str()
                //.graphemes(true)
                .take(len)
                .fold(start_col, |acc, c| if c == '\n' { 1 } else { acc + 1 }),
        } - 1;

        Span {
            start_row,
            start_col,
            end_row,
            end_col,
        }
    }

    fn consume_whitespace(&mut self) -> TokenKind {
        loop {
            if !self.peak().is_whitespace() {
                break;
            }
            self.bump();
        }
        TokenKind::Whitespace
    }

    fn consume_string_literal(&mut self) -> TokenKind {
        let mut buf = String::new();
        let mut c = self.bump().unwrap_or('\0');

        loop {
            if c == '\0' || c == '"' {
                break;
            }
            buf.push(c);
            c = self.bump().unwrap_or('\0');
        }

        TokenKind::StringLiteral(buf)
    }

    fn consume_cardfield(&mut self) -> TokenKind {
        let mut buf = String::new();
        let mut c = self.current_char;

        loop {
            buf.push(c);
            if is_end_cardfield(c, self.peak(), self.field_separator) {
                break;
            }
            c = self.bump().unwrap_or('\0');
        }

        TokenKind::CardField(buf)
    }

    fn peak(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    fn consume_ident(&mut self) -> TokenKind {
        let mut buf = String::new();
        let mut c = self.current_char;
        loop {
            buf.push(c);
            let peak = self.peak();
            if is_end_ident(peak) {
                break;
            }
            c = self.bump().unwrap_or('\0');
        }

        if is_keyword(&buf) {
            let keyword = Keyword::new(buf);
            TokenKind::Keyword(keyword)
        } else {
            TokenKind::Ident(buf)
        }
    }

    fn end_command(&mut self) -> TokenKind {
        self.command_mode = false;
        TokenKind::EndCommand
    }

    fn is_notetype(&self) -> bool {
        self.prev_newline && self.peak() == '['
    }

    fn consume_notetype(&mut self) -> TokenKind {
        // Next char is '['
        self.bump();

        let mut buf = String::new();

        // TODO Fail when encountering newline or '\0'
        loop {
            let c = self.bump().unwrap_or('\0');
            if c == ']' || c == '\0' || c == '\n' {
                break;
            }
            buf.push(c);
        }
        let notetype = if buf.is_empty() { None } else { Some(buf) };

        TokenKind::Notetype(notetype)
    }

    fn begin_command(&mut self) -> TokenKind {
        self.command_mode = true;
        TokenKind::BeginCommand
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next();
        self.current_char = c.unwrap_or('\0');
        c
    }
}

impl Token {
    pub fn dummy() -> Token {
        Token {
            span: Span::default(),
            kind: TokenKind::Dummy,
        }
    }
}
