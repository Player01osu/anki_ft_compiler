#![allow(dead_code, unused_variables, unused_mut)]

use std::fs;
use std::fs::File;
use std::io::Cursor;
use std::io::Write;
use std::io::SeekFrom;
use std::io::Seek;
use std::time::SystemTime;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::btree_map::Entry;
use std::env;
use std::process::exit;
use std::str::Chars;
use lexopt::Arg;
use sha2::{Sha256, Digest};

#[derive(Clone, Copy, Default, Debug)]
struct Span {
    row_start: usize,
    col_start: usize,
    row_end: usize,
    col_end: usize,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}:{} => {}:{}",
            self.row_start + 1,
            self.col_start + 1,
            self.row_end + 1,
            self.col_end + 1
        )
    }
}

struct Lexer<'a> {
    src: &'a str,
    cursor: Chars<'a>,
    file_write: Option<Cursor<Vec<u8>>>,

    separator: char,

    prev_span: Span,
    row_prev: usize,
    col_prev: usize,
    row_cur: usize,
    col_cur: usize,
}

struct LexerBacktrack<'a> {
    cursor: Chars<'a>,
    row_prev: usize,
    col_prev: usize,
    row_cur: usize,
    col_cur: usize,
    position: Option<u64>,
}

#[derive(Default, Clone, Copy)]
enum ParseMode {
    #[default]
    Note,
    Statement,
}

#[derive(Debug)]
struct Token {
    span: Span,
    kind: TokenKind,
    len: usize,
}

#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
    BeginStmt,                  // '>'
    EndStmt,                    // ';'
    Let,                        // 'let'
    Deck,                       // 'deck'
    NoteTypeKW,                 // 'notetype'
    SeparatorKW,                // 'separator'
    IgnoreNewlinesKW,           // 'ignore_newlines'
    Assignment,                 // '='
    Ident(Box<str>),
    SLiteral(Box<str>),
    Boolean(bool),

    NoteField(Box<str>),
    NoteType(Option<Box<str>>),
    NoteTypeOpen,               // '#['
    NoteTypeClose,              // ']'
    BeginNoteTag,               // '@!'
    NoteTagSeparator,           // ','
    NoteTag(Box<str>),
    NoteGUID(Option<Box<str>>),
    Separator,

    Illegal,
}

#[derive(Debug)]
struct Node {
    span: Span,
    kind: NodeKind,
}

#[derive(Debug)]
enum NodeKind {
    Note(Note),
    Stmt(Stmt),
}

#[derive(Debug)]
struct Note {
    notetype: Option<Box<str>>,
    guid: Option<Box<str>>,
    fields: Box<[Box<str>]>,
    tags: Box<[Box<str>]>,
}

#[derive(Debug)]
enum Value {
    Ident(Box<str>),
    SLiteral(Box<str>),
    Boolean(bool),
}

#[derive(Debug)]
struct Let {
    lhs: Box<str>,
    rhs: Value,
}

#[derive(Debug)]
enum Stmt {
    Let(Let),

    Deck(Box<str>),
    Notetype(Box<str>),
    Separator(char),
    IgnoreNewlines(bool),

    // Attempt to assign unknown metadata
    BadMetaAssign(Box<str>),
}

struct AST<'a> {
    parser: Parser<'a>,
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    prev_token: Option<Token>,
    mode: ParseMode,
    nodes: Vec<Node>,
    errors: Vec<(Span, Box<str>)>,
    warnings: Vec<(Span, Box<str>)>,
    seed: u128,
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t'
}

fn is_start_ident(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_ident(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

impl Span {
    fn join(self, other: Self) -> Self {
        Self {
            row_start: self.row_start,
            col_start: self.col_start,
            row_end: other.row_end,
            col_end: other.col_end,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, enable_writer: bool) -> Self {
        let file_write = if enable_writer {
            Some(Cursor::new(vec![]))
        } else {
            None
        };
        Lexer {
            src,
            cursor: src.chars(),
            prev_span: Span::default(),
            file_write,
            separator: ';',
            row_prev: 0,
            col_prev: 0,
            row_cur: 0,
            col_cur: 0,
        }
    }

    fn bump(&mut self) -> Option<char> {
        match self.cursor.next() {
            Some(c@'\n') => {
                self.row_prev = self.row_cur;
                self.col_prev = self.col_cur;
                self.row_cur += 1;
                self.col_cur = 0;
                if let Some(ref mut f) = self.file_write {
                    match write!(f, "{c}") {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Could not write to buffer:{e}");
                            exit(1);
                        }
                    }
                }
                Some(c)
            }
            Some(c) => {
                self.row_prev = self.row_cur;
                self.col_prev = self.col_cur;
                self.col_cur += 1;
                if let Some(ref mut f) = self.file_write {
                    match write!(f, "{c}") {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Could not write to buffer:{e}");
                            exit(1);
                        }
                    }
                }
                Some(c)
            }
            None => None
        }
    }

    fn peak(&self) -> char {
        match self.cursor.clone().next() {
            Some(c) => c,
            None => '\0',
        }
    }

    pub fn set_separator(&mut self, separator: char) {
        self.separator = separator;
    }

    fn ccmp(&mut self, s: &'static str) -> bool {
        if self.cmp(s) {
            for _ in 0..s.len() { self.bump(); }
            true
        } else {
            false
        }
    }

    fn cmp(&self, s: &'static str) -> bool {
        self.cursor.as_str().starts_with(s)
    }

    fn consume_if_begin_statement(&mut self) -> bool {
        self.ccmp(">")
    }

    fn is_begin_statement(&self) -> bool {
        self.cmp(">")
    }

    fn consume_if_begin_tags(&mut self) -> bool {
        self.ccmp("@!")
    }

    fn is_begin_tags(&self) -> bool {
        self.cmp("@!")
    }

    fn consume_if_begin_notetype(&mut self) -> bool {
        self.ccmp("#[")
    }

    fn is_begin_notetype(&self) -> bool {
        self.cmp("#[")
    }

    fn backtrack_now(&mut self) -> LexerBacktrack<'a> {
        let position = match &mut self.file_write {
            Some(f) => {
                match f.stream_position() {
                    Ok(pos) => {
                        Some(pos)
                    }
                    Err(e) => {
                        eprintln!("Could not get file stream pos:{e}");
                        None
                    }
                }
            }
            None => {
                None
            }
        };

        LexerBacktrack {
            cursor: self.cursor.clone(),
            row_prev: self.row_prev,
            col_prev: self.col_prev,
            row_cur: self.row_cur,
            col_cur: self.col_cur,
            position,
        }
    }

    fn backtrack(&mut self, bt: LexerBacktrack<'a>) {
        self.cursor = bt.cursor.clone();
        self.row_prev = bt.row_prev;
        self.col_prev = bt.col_prev;
        self.row_cur = bt.row_cur;
        self.col_cur = bt.col_cur;
        if let Some(ref mut f) = self.file_write {
            if let Some(pos) = bt.position {
                match f.seek(SeekFrom::Start(pos)) {
                    Err(e) => {
                        eprintln!("Could not seek to position:{e}");
                        exit(1);
                    }
                    _ => {
                    }
                }
            }
        }
    }

    fn consume_notefield(&mut self) -> TokenKind {
        let mut s = String::new();
        loop {
            let peak = self.peak();
            if peak == self.separator { break; }

            match self.bump() {
                Some('\\') if self.peak() == self.separator => {
                    self.bump();
                    s.push(self.separator);
                }
                Some(c) => {
                    s.push(c);
                }
                None => { break; }
            };

            if self.col_cur == 0 {
                let bt = self.backtrack_now();
                let mut should_break;
                should_break = self.is_begin_statement();
                should_break |= self.is_begin_tags();
                should_break |= self.is_begin_notetype();

                self.backtrack(bt);
                if should_break { break; }
            }
        }

        TokenKind::NoteField(s.trim().into())
    }

    fn start_token(&self) -> (usize, usize, usize) {
        (self.row_cur, self.col_cur, self.cursor.as_str().len())
    }

    pub fn next_notefield(&mut self) -> Option<Token> {
        if self.cursor.as_str().len() == 0 { return None; }
        while matches!(self.peak(), ' ' | '\t' | '\n') {
            self.bump();
        }
        let (row_start, col_start, start_len) = self.start_token();

        let is_startline = self.col_cur == 0;

        let kind = if self.peak() == self.separator {
            self.bump();
            TokenKind::Separator
        } else if self.peak() == '\0' {
            return None;
        } else if is_startline && self.consume_if_begin_statement() {
            TokenKind::BeginStmt
        } else if is_startline && self.consume_if_begin_tags() {
            TokenKind::BeginNoteTag
        } else if is_startline && self.consume_if_begin_notetype() {
            TokenKind::NoteTypeOpen
        } else {
            self.consume_notefield()
        };

        let (span, len) = self.end_token(row_start, col_start, start_len);
        self.prev_span = span;
        Some(Token {
            span,
            kind,
            len,
        })
    }

    fn end_token(&self, row_start: usize, col_start: usize, start_len: usize) -> (Span, usize) {
        let row_end = self.row_prev;
        let col_end = self.col_prev;
        let span = Span {
            row_start: row_start + 1,
            col_start: col_start + 1,
            row_end: row_end + 1,
            col_end: col_end + 1
        };
        let len = start_len - self.cursor.as_str().len();
        return (span, len);
    }

    pub fn next_note_guid(&mut self) -> Option<Token> {
        let (row_start, col_start, start_len) = self.start_token();
        let mut s = String::new();
        loop {
            if self.peak() == '\n' { break; }
            match self.bump() {
                Some(c) => {
                    s.push(c);
                }
                None => {
                    break;
                }
            }
        }

        let guid = if s.trim().is_empty() {
            None
        } else {
            Some(s.trim().into())
        };

        let (span, len) = self.end_token(row_start, col_start, start_len);
        Some(Token {
            span,
            kind: TokenKind::NoteGUID(guid),
            len,
        })
    }

    fn consume_whitespace(&mut self) {
        while matches!(self.peak(), '\n' | '\r' | '\t' | ' ') {
            self.bump();
        }
    }

    pub fn next_note_tags(&mut self) -> Option<Token> {
        let (row_start, col_start, start_len) = self.start_token();

        while matches!(self.peak(), '\t' | ' ') {
            self.bump();
        }

        let kind = match self.peak() {
            ',' => {
                self.bump();
                TokenKind::NoteTagSeparator
            }
            '\n' => {
                return None;
            }
            _ => {
                let mut tag = String::new();
                while !matches!(self.peak(), ',' | ' ' | '\t' | '\n' | '\0') {
                    tag.push(self.bump().unwrap());
                }
                TokenKind::NoteTag(tag.into())
            }
        };

        let (span, len) = self.end_token(row_start, col_start, start_len);
        Some(Token {
            span,
            kind,
            len,
        })
    }

    fn consume_notetype(&mut self, first_char: char) -> TokenKind {
        let mut s = String::new();
        s.push(first_char);

        loop {
            if self.peak() == ']' {
                break;
            }
            match self.bump() {
                Some(c) => {
                    s.push(c);
                }
                None => {
                    break;
                }
            }
        }

        let notetype = if s.is_empty() {
            None
        } else {
            Some(s.into())
        };

        TokenKind::NoteType(notetype)
    }

    pub fn next_notetype(&mut self) -> Option<Token> {
        let (row_start, col_start, start_len) = self.start_token();
        let c = self.bump()?;

        let kind = match c {
            ']' => {
                TokenKind::NoteTypeClose
            }
            _ => {
                self.consume_notetype(c)
            }
        };

        let (span, len) = self.end_token(row_start, col_start, start_len);
        Some(Token {
            span,
            kind,
            len,
        })
    }

    fn consume_ident(&mut self) -> TokenKind {
        let mut s = String::new();
        loop {
            match self.bump() {
                Some(c) => {
                    s.push(c);
                    if !is_ident(self.peak()) {
                        break;
                    }
                }
                None => {
                    break;
                }
            }
        }

        TokenKind::Ident(s.into())
    }

    fn consume_sliteral(&mut self) -> TokenKind {
        let mut s = String::new();
        loop {
            match self.bump() {
                Some('\\') => {
                    match self.peak() {
                        '"' => {
                            self.bump();
                            s.push('"');
                        }
                        '\\' => {
                            self.bump();
                            s.push('\\');
                        }
                        't' => {
                            self.bump();
                            s.push('\t');
                        }
                        'n' => {
                            self.bump();
                            s.push('\n');
                        }
                        'r' => {
                            self.bump();
                            s.push('\r');
                        }
                        _ => {
                            s.push('\\');
                        }
                    }
                }
                Some(c) => {
                    s.push(c);
                    if self.peak() == '"' {
                        self.bump();
                        break;
                    }
                }
                None => {
                    break;
                }
            }
        }

        TokenKind::SLiteral(s.into())
    }

    pub fn next_stmt(&mut self) -> Option<Token> {
        let (row_start, col_start, start_len) = self.start_token();

        if is_whitespace(self.peak()) {
            self.bump();
            return self.next_stmt();
        }

        let mut kind;

        if self.ccmp("let") {
            kind = TokenKind::Let;
        } else if self.ccmp("deck") {
            kind = TokenKind::Deck;
        } else if self.ccmp("notetype") {
            kind = TokenKind::NoteTypeKW;
        } else if self.ccmp("separator") {
            kind = TokenKind::SeparatorKW;
        } else if self.ccmp("ignore_newlines") {
            kind = TokenKind::IgnoreNewlinesKW;
        } else if self.ccmp("true") {
            kind = TokenKind::Boolean(true);
        } else if self.ccmp("false") {
            kind = TokenKind::Boolean(false);
        } else if self.peak() == '"' {
            self.bump();
            kind = self.consume_sliteral();
        } else if self.peak() == '=' {
            self.bump();
            kind = TokenKind::Assignment;
        } else if self.peak() == ';' {
            self.bump();
            kind = TokenKind::EndStmt;
        } else if is_start_ident(self.peak()) {
            kind = self.consume_ident();
        } else {
            kind = TokenKind::Illegal;
        }

        let (span, len) = self.end_token(row_start, col_start, start_len);
        Some(Token {
            span,
            kind,
            len,
        })
    }
}

impl<'a> Parser<'a> {
    pub fn push_err(&mut self, span: Span, msg: String) {
        self.errors.push((span, msg.into()))
    }

    pub fn push_warning(&mut self, span: Span, msg: String) {
        self.warnings.push((span, msg.into()))
    }

    pub fn new(src: &'a str, enable_writer: bool) -> Self {
        let seed = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(t) => {
                t.as_nanos()
            }
            Err(e) => {
                eprintln!("Could not get system time, using default seed:{e}");
                0x6f393831cbaa607c9b0d5537f
            }
        };

        Parser {
            lexer: Lexer::new(&src, enable_writer),
            prev_token: None,
            mode: ParseMode::default(),
            nodes: vec![],
            errors: vec![],
            warnings: vec![],
            seed,
        }
    }

    fn parse_notetype(&mut self) -> Option<(Span, Option<Box<str>>)> {
        let tok = self.next_token(Lexer::next_notetype, "notetype")?;
        let notetype = match tok.kind {
            TokenKind::NoteType(s) => s,
            TokenKind::NoteTypeClose => {
                return Some((tok.span, None));
            }
            k => {
                let msg = format!("Expected notetype or notetype close (']'), but got {k:?}");
                self.push_err(tok.span, msg);
                return None;
            }
        };
        if !self.next_expect_kind(
            Lexer::next_notetype,
            &[TokenKind::NoteTypeClose],
            "notetype close (']')"
        ) {
            return None;
        }

        Some((tok.span, notetype))
    }

    fn parse_guid(&mut self) -> Option<Option<Box<str>>> {
        let tok = self.next_token(
            Lexer::next_note_guid,
            "guid or notefield"
        )?;
        let guid = match tok.kind {
            TokenKind::NoteGUID(guid) => {
                guid
            }
            k => {
                let msg = format!("Expected guid or notefield, but got {k:?}");
                self.push_err(tok.span, msg);
                return None;
            }
        };
        Some(guid)
    }

    fn parse_fields(&mut self) -> Option<Box<[Box<str>]>> {
        let mut fields = vec![];

        // Expect at least one notefield
        let tok = self.next_token(
            Lexer::next_notefield,
            "notefield"
        )?;

        let field = match tok.kind {
            TokenKind::NoteField(field) => {
                field
            }
            TokenKind::Separator => {
                self.prev_token = Some(tok);
                String::new().into()
            }
            k => {
                let msg = format!("Expected at least one notefield, but got {k:?}");
                self.push_err(tok.span, msg);
                return None;
            }
        };
        fields.push(field);

        loop {
            let tok = match self.next_token_no_check(Lexer::next_notefield) {
                Some(v) => v,
                _ => break,
            };
            if tok.kind != TokenKind::Separator {
                self.prev_token = Some(tok);
                break;
            }
            let tok = match self.next_token_no_check(Lexer::next_notefield) {
                Some(v) => v,
                None => {
                    fields.push(String::new().into());
                    break;
                }
            };
            match tok.kind {
                TokenKind::NoteField(field) => {
                    fields.push(field);
                }
                _ => {
                    fields.push(String::new().into());
                    let should_break = tok.kind != TokenKind::Separator;
                    self.prev_token = Some(tok);
                    if should_break { break; }
                }
            };
        }

        Some(fields.into())
    }

    fn parse_tags(&mut self) -> Option<(Span, Box<[Box<str>]>)> {
        let tok = match self.next_token_no_check(Lexer::next_notefield) {
            Some(v) => {
                v
            }
            None => {
                return Some((self.lexer.prev_span, vec![].into()));
            }
        };

        match tok.kind {
            TokenKind::BeginNoteTag => {
                let mut tags = vec![];
                let tok = match self.next_token_no_check(Lexer::next_note_tags) { Some(v) => {
                        v
                    }
                    None => {
                        return Some((self.lexer.prev_span, vec![].into()));
                    }
                };

                match tok.kind {
                    TokenKind::NoteTag(tag) => {
                        tags.push(tag);
                    }
                    k => {
                        let msg = format!("Expected a notetag, but got {k:?}");
                        self.push_err(tok.span, msg);
                        return None;
                    }
                }

                loop {
                    let tok = match self.next_token_no_check(Lexer::next_note_tags) {
                        Some(v) => v,
                        None => break,
                    };

                    if tok.kind != TokenKind::NoteTagSeparator  {
                        let msg = format!("Expected a notetag separator (','), but got {:?}", tok.kind);
                        self.push_err(tok.span, msg);
                        return None;
                    }

                    let tok = match self.next_token_no_check(Lexer::next_note_tags) {
                        Some(v) => v,
                        None => break,
                    };

                    match tok.kind {
                        TokenKind::NoteTag(tag) => {
                            tags.push(tag);
                        }
                        k => {
                            let msg = format!("Expected a notetag, but got {k:?}");
                            self.push_err(tok.span, msg);
                            return None;
                        }
                    }
                }

                Some((self.lexer.prev_span, tags.into()))
            }
            _ => {
                self.prev_token = Some(tok);
                Some((self.lexer.prev_span, vec![].into()))
            }
        }
    }

    fn parse_note(&mut self) -> Option<(Span, Note)> {
        let (start_span, notetype) = self.parse_notetype()?;
        let guid = match self.parse_guid()? {
            Some(guid) => {
                Some(guid)
            }
            None => {
                match &mut self.lexer.file_write {
                    Some(f) => {
                        let mut hasher = Sha256::new();
                        hasher.update(self.seed.to_le_bytes());
                        hasher.update(start_span.row_start.to_le_bytes());
                        hasher.update(start_span.col_start.to_le_bytes());
                        hasher.update(start_span.row_end.to_le_bytes());
                        hasher.update(start_span.col_end.to_le_bytes());
                        let mut guid = String::new();
                        for b in hasher.finalize()[..16].iter() {
                            guid.push_str(format!("{b:x}").as_str());
                        }
                        match write!(f, "{}", guid) {
                            Ok(_) => {
                                Some(guid.into())
                            }
                            Err(e) => {
                                eprintln!("Could not write to buffer:{e}");
                                None
                            }
                        }
                    }
                    None => {
                        None
                    }
                }
            }
        };

        let fields = self.parse_fields()?;
        let (end_span, tags) = self.parse_tags()?;

        Some((start_span.join(end_span), Note {
            notetype,
            guid,
            fields,
            tags,
        }))
    }

    fn next_token(
        &mut self,
        mut f: impl FnMut(&mut Lexer<'a>) -> Option<Token>,
        expect_kind: &'static str
    ) -> Option<Token> {
        match self.next_token_no_check(f) {
            Some(v) => {
                Some(v)
            }
            None => {
                let err = format!("Expected {expect_kind} but got EOF");
                let span = self.nodes.last().map(|n| n.span).unwrap_or_default();
                self.push_err(span, err);
                None
            }
        }
    }

    fn next_expect_kind(
        &mut self,
        mut f: impl FnMut(&mut Lexer<'a>) -> Option<Token>,
        kinds: &'static [TokenKind],
        expect_kind: &'static str
    ) -> bool
    {
        match self.next_token_no_check(f) {
            Some(v) if kinds.iter().any(|k| v.kind.eq(k)) => {
                true
            }
            Some(v) => {
                let err = format!("Expected {expect_kind} but got {:?}", v.kind);
                self.push_err(v.span, err);
                false
            }
            None => {
                let err = format!("Expected {expect_kind} but got EOF");
                let span = self.nodes.last().map(|n| n.span).unwrap_or_default();
                self.push_err(span, err);
                false
            }
        }
    }

    fn next_token_no_check(
        &mut self,
        mut f: impl FnMut(&mut Lexer<'a>) -> Option<Token>
    ) -> Option<Token> {
        match self.prev_token.take() {
            Some(v) => {
                Some(v)
            }
            None => {
                f(&mut self.lexer)
            }
        }
    }

    fn parse_builtin_any(&mut self) -> Option<Box<str>> {
        if !self.next_expect_kind(
            Lexer::next_stmt,
            &[TokenKind::Assignment],
            "'=' (assignment token)"
        ) {
            return None;
        }

        let tok = self.next_token(Lexer::next_stmt, "value")?;

        let rhs = match tok.kind {
            TokenKind::SLiteral(s) => {
                s
            }
            TokenKind::Boolean(b) => {
                b.to_string().into()
            }
            k => {
                let msg = format!("Expected value but got {:?}", k);
                self.push_err(tok.span, msg);
                return None;
            }
        };

        if !self.next_expect_kind(Lexer::next_stmt, &[TokenKind::EndStmt], "';' (end stmt)") {
            return None;
        }

        Some(rhs)
    }

    fn parse_builtin_slit(&mut self) -> Option<Box<str>> {
        if !self.next_expect_kind(
            Lexer::next_stmt,
            &[TokenKind::Assignment],
            "'=' (assignment token)"
        ) {
            return None;
        }

        let tok = self.next_token(Lexer::next_stmt, "ident")?;

        let rhs = match tok.kind {
            TokenKind::SLiteral(s) => {
                s
            }
            k => {
                let msg = format!("Expected value but got {:?}", k);
                self.push_err(tok.span, msg);
                return None;
            }
        };

        if !self.next_expect_kind(Lexer::next_stmt, &[TokenKind::EndStmt], "';' (end stmt)") {
            return None;
        }

        Some(rhs)
    }

    fn parse_builtin_boolean(&mut self) -> Option<bool> {
        if !self.next_expect_kind(
            Lexer::next_stmt,
            &[TokenKind::Assignment],
            "'=' (assignment token)"
        ) {
            return None;
        }

        let tok = self.next_token(Lexer::next_stmt, "ident")?;

        let rhs = match tok.kind {
            TokenKind::Boolean(b) => {
                b
            }
            k => {
                let msg = format!("Expected boolean but got {:?}", k);
                self.push_err(tok.span, msg);
                return None;
            }
        };

        if !self.next_expect_kind(Lexer::next_stmt, &[TokenKind::EndStmt], "';' (end stmt)") {
            return None;
        }

        Some(rhs)
    }

    fn parse_let(&mut self) -> Option<Let> {
        let mut tok = self.next_token(Lexer::next_stmt, "ident")?;

        let lhs = match tok.kind {
            TokenKind::Ident(s) => {
                s
            }
            k => {
                let msg = format!("Expected ident but got {:?}", k);
                self.push_err(tok.span, msg);
                return None;
            }
        };

        if !self.next_expect_kind(
            Lexer::next_stmt,
            &[TokenKind::Assignment],
            "'=' (assignment token)"
        ) {
            return None;
        }

        let mut tok = self.next_token(Lexer::next_stmt, "ident")?;

        let rhs = match tok.kind {
            TokenKind::Ident(s) => {
                Value::Ident(s)
            }
            TokenKind::SLiteral(s) => {
                Value::SLiteral(s)
            }
            TokenKind::Boolean(b) => {
                Value::Boolean(b)
            }
            k => {
                let msg = format!("Expected value but got {:?}", k);
                self.push_err(tok.span, msg);
                return None;
            }
        };

        if !self.next_expect_kind(Lexer::next_stmt, &[TokenKind::EndStmt], "';' (end stmt)") {
            return None;
        }

        Some(Let { lhs, rhs })
    }
}

impl AST<'_> {
    pub fn next_node(&mut self) -> Result<Option<Node>, ()> {
        match self.parser.mode {
            ParseMode::Note => {
                let token = match self.parser.next_token_no_check(Lexer::next_notefield) {
                    Some(tok) => tok,
                    None => return Ok(None),
                };
                match token.kind {
                    TokenKind::NoteTypeOpen => {
                        let (span, note) = match self.parser.parse_note() {
                            Some(v) => {
                                v
                            }
                            None => {
                                return Err(());
                            }
                        };
                        return Ok(Some(Node {
                            span,
                            kind: NodeKind::Note(note),
                        }))
                    }
                    TokenKind::BeginNoteTag => {
                        let msg = format!("Expected a notetype, but got notetag");
                        self.parser.push_err(token.span, msg);
                        self.parser.parse_tags();
                        return Err(());
                    }
                    TokenKind::BeginStmt => {
                        self.parser.mode = ParseMode::Statement;
                        return self.next_node();
                    }
                    TokenKind::NoteField(field) => {
                        let msg = format!("Expected a notetype, but got a notefield");
                        self.parser.push_err(token.span, msg);
                        self.parser.parse_fields();
                        return Err(());
                    }
                    k => {
                        let msg = format!("Unexpected token: {k:?}");
                        self.parser.push_err(token.span, msg);
                        return Err(());
                    }
                }
            }
            ParseMode::Statement => {
                let token = match self.parser.lexer.next_stmt() {
                    Some(v) => {
                        v
                    }
                    None => {
                        let msg = format!("Expected stmt, got EOF");
                        let span = self.parser.nodes.last().map(|n| n.span).unwrap_or_default();
                        self.parser.push_err(span, msg);
                        return Err(());
                    }
                };
                let stmt = match token.kind {
                    TokenKind::Let => {
                        Stmt::Let(self.parser.parse_let().ok_or(())?)
                    }
                    TokenKind::Deck => {
                        Stmt::Deck(self.parser.parse_builtin_slit().ok_or(())?)
                    }
                    TokenKind::NoteTypeKW => {
                        Stmt::Notetype(self.parser.parse_builtin_slit().ok_or(())?)
                    }
                    TokenKind::IgnoreNewlinesKW => {
                        Stmt::IgnoreNewlines(self.parser.parse_builtin_boolean().ok_or(())?)
                    }
                    TokenKind::Ident(s) => {
                        self.parser.parse_builtin_any().ok_or(())?;
                        Stmt::BadMetaAssign(s)
                    }
                    k => {
                        let msg = format!("Unexpected token: {k:?}");
                        self.parser.push_err(token.span, msg);
                        return Err(());
                    }
                };

                self.parser.mode = ParseMode::Note;
                // TODO: Return statement
                let span = token.span.join(self.parser.lexer.prev_span);
                let kind = NodeKind::Stmt(stmt);
                Ok(Some(Node {
                    span,
                    kind,
                }))
            }
        }
    }
}

fn parse_src(src: &str, enable_writer: bool) -> AST {
    let mut parser = Parser::new(src, enable_writer);

    AST {
        parser,
    }
}

#[derive(Debug, Default)]
enum StableGUIDOpt {
    #[default]
    None,
    Soft, // Create new file with contents copied over
    Hard, // Overwrite inputs with guid
}

#[derive(Debug)]
struct CompilerOptions {
    stable_guid: StableGUIDOpt,
}

fn sanitize_field(field: &str, ignore_newlines: bool, separator: char) -> Box<str> {
    let mut s = String::new();
    let mut s_vec = vec![];

    s.push('"');
    for c in field.chars() {
        match c {
            '\n' if !ignore_newlines => {
                s.push_str("<br>");
            }
            '\n' => {
                s_vec.push(s);
                s = String::new();
            }
            '"' => {
                s.push('"');
                s.push('"');
            }
            c => {
                s.push(c);
            }
        }
    }

    if s_vec.is_empty() {
        s.push('"');
        s.into()
    } else {
        let mut field = String::new();
        let mut first_part = true;
        for s_part in s_vec.iter().map(|s| s.trim()) {
            if first_part {
                first_part = false
            } else {
                field.push(' ');
            }
            field.push_str(s_part);
        }
        field.push_str(&s);
        field.push('"');
        field.into()
    }
}

fn generate_outputs(paths: &[String], output_name: &str, options: CompilerOptions) -> Result<(), ()> {
    let mut deck: Option<Box<str>> = None;
    let mut deck_set: BTreeSet<Box<str>> = BTreeSet::new();
    let mut notetype_map: BTreeMap<Box<str>, usize> = BTreeMap::new();
    let mut notetype_set: BTreeSet<Box<str>> = BTreeSet::new();
    let mut var_map: BTreeMap<Box<str>, Value> = BTreeMap::new();
    notetype_map.insert("Basic".into(), 2);
    notetype_map.insert("Basic (and reversed card)".into(), 2);
    notetype_map.insert("Cloze".into(), 2);
    let mut notetype: Box<str> = "Basic".into();
    let mut ignore_newlines = false;
    let mut num_notes = 0;

    let mut output = match File::create(output_name) {
        Ok(v) => {
            v
        }
        Err(e) => {
            eprintln!("Could not create file:\"{output_name}\":{e}");
            return Err(());
        }
    };

    match write!(&mut output, "#html:true\n") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Could not write to output file:{e}");
            return Err(());
        }
    }
    match write!(&mut output, "#separator:Semicolon\n") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Could not write to output file:{e}");
            return Err(());
        }
    }

    match write!(&mut output, "#guid column:1\n") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Could not write to output file:{e}");
            return Err(());
        }
    }

    match write!(&mut output, "#notetype column:2\n") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Could not write to output file:{e}");
            return Err(());
        }
    }

    match write!(&mut output, "#deck column:3\n") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Could not write to output file:{e}");
            return Err(());
        }
    }

    match write!(&mut output, "#tags column:4\n") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Could not write to output file:{e}");
            return Err(());
        }
    }

    for path in paths {
        let src = match fs::read_to_string(&path) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Could not read \"{path}\":{e}");
                return Err(());
            }
        };

        let (enable_writer, stable_path) = match options.stable_guid {
            StableGUIDOpt::None => {
                (false, String::new())
            }
            StableGUIDOpt::Soft => {
                let path = format!("{path}.stable");
                (true, path)
            }
            StableGUIDOpt::Hard => {
                (true, path.to_string())
            }
        };

        let mut ast = parse_src(&src, enable_writer);
        while let Ok(Some(node)) = ast.next_node() {
            match node.kind {
                NodeKind::Note(note) => {
                    num_notes += 1;
                    match note.guid {
                        Some(guid) => {
                            match write!(&mut output, "{guid};") {
                                Ok(_) => (),
                                Err(e) => {
                                    eprintln!("Could not write to output file:{e}");
                                    return Err(());
                                }
                            }
                        }
                        None => {
                            match write!(&mut output, ";") {
                                Ok(_) => (),
                                Err(e) => {
                                    eprintln!("Could not write to output file:{e}");
                                    return Err(());
                                }
                            }
                        }
                    }

                    let notetype = note.notetype.as_ref().unwrap_or(&notetype);
                    match write!(&mut output, "{notetype};") {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Could not write to output file:{e}");
                            return Err(());
                        }
                    }

                    match notetype_map.entry(notetype.clone()) {
                        Entry::Vacant(slot) => {
                            slot.insert(note.fields.len());
                        }
                        Entry::Occupied(v) => {
                            if *v.get() != note.fields.len() {
                                eprintln!("{path}:{}:{}:WARNING:Field lengths do not match for \"{notetype}\" notetype, first got {} fields, then got {} fields\n", node.span.row_start, node.span.row_end, *v.get(), note.fields.len());
                            }
                        }
                    }
                    notetype_set.insert(notetype.clone());

                    match &deck {
                        Some(deck) => {
                            match write!(&mut output, "{deck};") {
                                Ok(_) => (),
                                Err(e) => {
                                    eprintln!("Could not write to output file:{e}");
                                    return Err(());
                                }
                            }
                            deck_set.insert(deck.clone());
                        }
                        None => {
                            eprintln!("{path}:{}:{}:ERROR:\"deck\" metadata unset, set the deck before adding notes (ie: > deck = \"My Deck Here\")\n", node.span.row_start, node.span.col_start);
                            return Err(());
                        }
                    }

                    let mut first_tag = true;
                    for tag in note.tags.iter() {
                        if first_tag {
                            first_tag = false;
                        } else {
                            match write!(&mut output, " ") {
                                Ok(_) => (),
                                Err(e) => {
                                    eprintln!("Could not write to output file:{e}");
                                    return Err(());
                                }
                            }
                        }

                        match write!(&mut output, "{tag}") {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("Could not write to output file:{e}");
                                return Err(());
                            }
                        }
                    }

                    match write!(&mut output, ";") {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Could not write to output file:{e}");
                            return Err(());
                        }
                    }

                    let mut first_field = true;
                    for field in note.fields.iter() {
                        if first_field {
                            first_field = false;
                        } else {
                            match write!(&mut output, ";") {
                                Ok(_) => (),
                                Err(e) => {
                                    eprintln!("Could not write to output file:{e}");
                                    return Err(());
                                }
                            }
                        }

                        let sanitized = sanitize_field(field, ignore_newlines, ast.parser.lexer.separator);
                        match write!(&mut output, "{sanitized}") {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("Could not write to output file:{e}");
                                return Err(());
                            }
                        }
                    }
                    match write!(&mut output, "\n") {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Could not write to output file:{e}");
                            return Err(());
                        }
                    }
                }
                NodeKind::Stmt(stmt) => {
                    match stmt {
                        Stmt::Let(var_assign) => {
                            var_map.insert(var_assign.lhs, var_assign.rhs);
                        }
                        Stmt::Deck(s) => {
                            deck = Some(s);
                        }
                        Stmt::Notetype(s) => {
                            notetype = s;
                        }
                        Stmt::Separator(c) => {
                            ast.parser.lexer.set_separator(c);
                        }
                        Stmt::IgnoreNewlines(b) => {
                            ignore_newlines = b;
                        }
                        Stmt::BadMetaAssign(s) => {
                            eprintln!("{path}:{}:{}:ERROR:Attempt to assign unknown metadata:\"{s}\"\n", node.span.row_start, node.span.col_start);
                            eprintln!("  Try these instead: deck, notetype, separator, ignore_newlines");
                            return Err(());
                        }
                    }
                }
            }
        }

        for (span, error) in &ast.parser.errors {
            eprintln!("{path}:{}:{}:ERROR:{error}\n", span.row_start, span.col_start);
        }

        if !ast.parser.errors.is_empty() {
            drop(output);
            match fs::remove_file(output_name) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Could not remove output file:{e}");
                    exit(1);
                }
            }
            return Err(());
        }

        if let Some(f) = &mut ast.parser.lexer.file_write {
            match f.flush() {
                Ok(_) => {
                    let mut stable_file = File::create(stable_path).unwrap();
                    match stable_file.write(f.get_ref()) {
                        Ok(_) => {
                        }
                        Err(e) => {
                            eprintln!("Could not write to stable file:{e}");
                            return Err(());
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Could not flush buffer into file:{e}");
                }
            }
        }
    }

    println!("Compilation information:");
    println!("  note count:{num_notes}");
    println!("  notetype count:{}", notetype_set.len());
    println!("  deck count:{}", deck_set.len());
    println!("  variable count:{}", var_map.len());

    Ok(())
}

fn main() {
    let mut args = env::args();
    let program_name = args.next().expect("First argument should be program name (unix)");
    let mut options = CompilerOptions {
        stable_guid: StableGUIDOpt::None,
    };

    let mut paths = vec![];
    let mut parser = lexopt::Parser::from_env();
    let mut output_name = None;
    let mut parse_output_name = false;

    loop {
        let arg = match parser.next() {
            Ok(Some(v)) => {
                v
            }
            Ok(None) => {
                break;
            }
            Err(e) => {
                eprintln!("Could not parse arg:{e}");
                exit(1);
            }
        };
        match arg {
            Arg::Short('s') | Arg::Long("stable-soft") => {
                options.stable_guid = StableGUIDOpt::Soft;
            }
            Arg::Short('S') | Arg::Long("stable-hard") => {
                options.stable_guid = StableGUIDOpt::Hard;
            }
            Arg::Short('o') | Arg::Long("output") => {
                parse_output_name = true;
            }
            Arg::Short('h') | Arg::Long("help") => {
                println!("Usage: aftc [options] file...");
                exit(0);
            }
            Arg::Value(v) if parse_output_name => {
                match v.to_str() {
                    Some(v) => {
                        output_name = Some(v.into());
                    }
                    None => {
                        eprintln!("Non-utf8 encoded path:{}", v.to_string_lossy());
                        exit(1);
                    }
                }
                parse_output_name = false;
            }
            Arg::Value(v) => {
                match v.to_str() {
                    Some(v) => {
                        paths.push(v.to_string());
                    }
                    None => {
                        eprintln!("Non-utf8 encoded path:{}", v.to_string_lossy());
                        exit(1);
                    }
                }
            }
            _ => {
                eprintln!("Unknown argument provided:{arg:?}");
                exit(1);
            }
        }
    }

    if paths.is_empty() {
        eprintln!("Missing file");
        exit(1);
    }

    let output_name = match output_name {
        Some(v) => {
            v
        }
        None => {
            let path = paths.first().unwrap();
            let path = std::path::Path::new(path);
            match path.file_stem() {
                Some(path) => {
                    let path = path.to_string_lossy();
                    format!("{path}.aft.txt")
                }
                None => {
                    let path = path.to_string_lossy();
                    format!("{path}.aft.txt")
                }
            }
        }
    };

    match generate_outputs(&paths, &output_name, options) {
        Ok(_) => {
            exit(0);
        }
        Err(_) => {
            exit(1);
        }
    }
}
