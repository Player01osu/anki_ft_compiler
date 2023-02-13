use std::str::{Chars, FromStr};

pub fn tokenize(src: &str) -> impl Iterator<Item = Token> + '_ {
    let mut string_reader = StringReader::new(src);
    std::iter::from_fn(move || {
        let token = string_reader.next_token();
        if token.kind != TokenKind::EOF {
            Some(token)
        } else {
            None
        }
    })
}

#[derive(Clone, Debug)]
pub struct Token {
    kind: TokenKind,
    len: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol {
    KW(KW),
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    String { terminated: bool },
    // TODO
    Num,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
    /// `< ... >`
    AngleBracket,
}

#[derive(Debug)]
pub struct Cursor<'a> {
    buf: String,
    len_remaining: usize,
    chars: Chars<'a>,
}

#[derive(Debug)]
pub struct StringReader<'a> {
    start_pos: usize,
    pos: usize,
    cursor: Cursor<'a>,
}

impl Token {
    fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

const DELIM: TokenKind = TokenKind::Semi;

fn is_begins_kw(c: char) -> bool {
    match c {
        'l' => true,
        _ => false,
    }
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\r' | '\n' => true,
        _ => false,
    }
}

fn is_string_literal(c: char) -> bool {
    match c {
        '\'' | '"' => true,
        _ => false,
    }
}

fn is_kw(str: &str) -> bool {
    match str {
        "let" => true,
        _ => false,
    }
}

fn is_end_kw(c: char) -> bool {
    match c {
        c if is_whitespace(c) => true,
        c if is_end_of_ident(c) => true,
        _ => false,
    }
}

fn is_line_comment(c: char) -> bool {
    match c {
        '-' => true,
        _ => false,
    }
}

fn is_block_comment(str: String) -> bool {
    match str.as_str() {
        "-[[" => true,
        _ => false,
    }
}

fn is_end_of_ident(c: char) -> bool {
    match c {
        '$' | '#' | ';' | '=' | '[' | ']' | '{' | '}' => true,
        _ => false,
    }
}

impl<'a> Cursor<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            len_remaining: src.len(),
            chars: src.chars(),
            buf: String::new(),
        }
    }

    fn advance_token(&mut self) -> Token {
        let first_char = match self.chars.next() {
            Some(c) => c,
            None => {
                return Token::new(TokenKind::EOF, 0);
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
            '#' => TokenKind::Pound,
            '$' => TokenKind::Dollar,
            '=' => TokenKind::Eq,

            '[' => TokenKind::OpenDelim(Delimiter::Bracket),
            ']' => TokenKind::CloseDelim(Delimiter::Bracket),

            '<' => TokenKind::OpenDelim(Delimiter::AngleBracket),
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
        let token = Token::new(token_kind, self.pos_within_token());
        self.reset_pos_within_token();
        token
    }

    fn peak_first(&self) -> char {
        match self.chars.clone().next() {
            Some(c) => c,
            // EOF
            None => '\0',
        }
    }

    fn peak_n(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or('\0')
    }

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

    fn pos_within_token(&self) -> usize {
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
                None => return TokenKind::Ident(Symbol::Other),
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
        loop {
            // FIXME ??????
            if is_whitespace(self.peak_first()) || is_end_of_ident(self.peak_first()) {
                let next_non_white = self
                    .chars
                    .clone()
                    .skip_while(|c| is_whitespace(*c))
                    .next()
                    .unwrap_or('\0');
                if is_end_of_ident(next_non_white) {
                    break;
                }
            }
            let c = match self.chars.next() {
                Some(c) if is_end_of_ident(c) => {
                    break;
                }
                Some(c) => c,
                None => return TokenKind::Ident(Symbol::Other),
            };
            self.buf.push(c);
        }

        TokenKind::Ident(Symbol::Other)
    }

    fn consume_string_literal(&mut self, literal_char: char) -> TokenKind {
        let kind = loop {
            let c = match self.chars.next() {
                Some(c) => c,
                None => return TokenKind::Literal(LiteralKind::String { terminated: false }),
            };

            match c {
                // Escaped
                '\\' => {
                    if self.peak_first() == literal_char {
                        self.chars.next();
                    }
                }
                c if c == literal_char => {
                    break LiteralKind::String { terminated: true };
                }
                _ => (),
            }
        };
        TokenKind::Literal(kind)
    }

    fn consume_whitespace(&mut self) -> TokenKind {
        self.consume_while(|c| is_whitespace(*c));
        TokenKind::Whitespace
    }
}

impl<'a> StringReader<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            start_pos: 0,
            pos: 0,
            cursor: Cursor::new(src),
        }
    }

    pub fn next_token(&mut self) -> Token {
        let token = self.cursor.advance_token();
        self.offset(token.len);

        token
    }

    fn offset(&mut self, offset: usize) {
        self.pos += offset;
    }
}
