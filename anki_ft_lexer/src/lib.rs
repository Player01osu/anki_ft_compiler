#![allow(dead_code)]
pub mod span;

use std::{fmt::Display, str::Chars};
use span::Span;
#[allow(unused_imports)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(i32),
    Float(f32),
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    LineComment,
    BlockComment,

    Ident(String),
    Literal(Literal),
    Keyword(Keyword),
    Whitespace,

    Notetype(Option<String>),
    CardField(String),
    FieldSeparator { overwrite: bool },

    BeginCommand,
    EndCommand,
    Assignment,

    Eof,
    Illegal,
    Dummy,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => s.fmt(f),
            Literal::Number(n) => n.fmt(f),
            Literal::Float(n) => n.fmt(f),
        }
    }
}

// TODO: Use a global string buffer to avoid realloaction.
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

fn is_end_cardfield(current_char: char, chars: &[char], field_separator: char) -> bool {
    let (fst, snd, trd) = match chars {
        [fst, snd, trd] => (*fst, *snd, *trd),
        _ => unreachable!(),
    };
    let card_type = current_char == '\n' && fst == '#';
    let end_field = fst == field_separator;
    let begin_command = fst == '>' && current_char == '\n';
    let is_eof = fst == '\0';
    let is_overwrite_field = fst == '<' && is_overwrite(field_separator, (snd, trd));
    card_type || end_field || begin_command || is_eof || is_overwrite_field
}

fn is_overwrite(field_separator: char, (fst, snd): (char, char)) -> bool {
    field_separator == fst && snd == '>'
}

fn is_end_number_literal(c: char) -> bool {
    !(matches!(c, '0'..='9' | '.'))
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
        let c = match self.bump() {
            Some(c) => c,
            None => return eof_token(),
        };

        let kind =
        match c {

            c if c.is_whitespace() => self.consume_whitespace(),
            '>' if self.prev_newline => self.begin_command(),
            '#' if self.is_notetype() => self.consume_notetype(),

            ';' if self.command_mode => self.end_command(),
            '=' if self.command_mode => TokenKind::Assignment,

            '"' if self.command_mode => self.consume_string_literal(),
            '<' if is_overwrite(self.field_separator, self.peak_two()) => {
                self.consume_field_overwrite()
            }
            '0'..='9' if self.command_mode => self.consume_number_literal(),
            c if self.field_separator == c => TokenKind::FieldSeparator { overwrite: false },
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

    fn consume_number_literal(&mut self) -> TokenKind {
        let mut buf = String::new();
        let mut c = self.current_char;
        let mut is_float = false;
        loop {
            if c == '.' {
                is_float = true;
            }
            buf.push(c);
            if is_end_number_literal(self.peak()) {
                break;
            }
            c = self.bump().unwrap_or('\0');
        }

        if is_float {
            match buf.parse() {
                Ok(v) => TokenKind::Literal(Literal::Float(v)),
                Err(_) => TokenKind::Illegal,
            }
        } else {
            match buf.parse() {
                Ok(v) => TokenKind::Literal(Literal::Number(v)),
                Err(_) => TokenKind::Illegal,
            }
        }
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
        let len = self.remaining
            - self
                .chars
                .as_str()
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

    fn consume_field_overwrite(&mut self) -> TokenKind {
        // Separator
        self.bump();
        // '>'
        self.bump();
        TokenKind::FieldSeparator { overwrite: true }
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
            if c == '\\' {
                c = self.bump().unwrap_or('\0');
                buf.push(c);
                self.bump();
                continue;
            }
            if c == '\0' || c == '"' {
                break;
            }
            buf.push(c);
            c = self.bump().unwrap_or('\0');
        }

        TokenKind::Literal(Literal::String(buf))
    }

    fn consume_cardfield(&mut self) -> TokenKind {
        let mut buf = String::new();
        let mut c = self.current_char;

        loop {
            buf.push(c);
            if is_end_cardfield(c, &self.peak_n(3), self.field_separator) {
                break;
            }
            c = self.bump().unwrap_or('\0');
        }

        TokenKind::CardField(buf.trim().to_owned())
    }

    fn peak(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    fn peak_two(&self) -> (char, char) {
        let mut chars = self.chars.clone();
        (chars.next().unwrap_or('\0'), chars.next().unwrap_or('\0'))
    }

    fn peak_n(&self, n: usize) -> Vec<char> {
        let mut v = self.chars.clone().take(n).collect::<Vec<char>>();
        while v.len() < n {
            v.push('\0');
        }
        v
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
