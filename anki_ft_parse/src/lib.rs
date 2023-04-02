// TODO
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::error::Error;
use std::fmt::Display;

use anki_ft_lexer::Cursor;
use anki_ft_lexer::Delimiter;
use anki_ft_lexer::Delimiter::*;
use anki_ft_lexer::Token as LexerToken;
use anki_ft_lexer::TokenKind as LexerTokenKind;
use anki_ft_lexer::KW;

/*
 * Enriched tokens
 */

// TODO: Implement span for better error reporting.
#[derive(Debug, Default)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

use strum::Display;
#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Display)]
pub enum TokenKind {
    KeyWord,
    Ident,
    Literal,
    LineComment,
    BlockComment,

    Comma,
    Semi,
    Colon,
    Hyphen,
    Pound,
    Dollar,
    Eq,

    /// `( ... )`
    OpenParen,
    CloseParen,
    /// `{ ... }`
    OpenBrace,
    CloseBrace,
    /// `[ ... ]`
    OpenBracket,
    CloseBracket,

    /// `< ... >`
    OpenAngleBracket,
    CloseAngleBracket,

    Whitespace,
    Unknown,
    EOF,
    #[default]
    DummyToken,
}

#[derive(Debug)]
pub struct TokenMeta {
    pub kind: TokenKind,
    pub span: Span,
}

fn token_kind_from_close_delim(delim: Delimiter) -> TokenKind {
    match delim {
        Delimiter::Brace => TokenKind::CloseBrace,
        Delimiter::Parenthesis => TokenKind::CloseParen,
        Delimiter::AngleBracket => TokenKind::CloseAngleBracket,
        Delimiter::Bracket => TokenKind::CloseBracket,
    }
}

fn token_kind_from_open_delim(delim: Delimiter) -> TokenKind {
    match delim {
        Delimiter::Brace => TokenKind::OpenBrace,
        Delimiter::Parenthesis => TokenKind::OpenParen,
        Delimiter::AngleBracket => TokenKind::OpenAngleBracket,
        Delimiter::Bracket => TokenKind::OpenBracket,
    }
}

impl TokenMeta {
    pub fn new(lexer_token: LexerToken) -> Self {
        use anki_ft_lexer::TokenKind::*;
        match lexer_token.kind() {
            Ident(symbol) => match symbol {
                anki_ft_lexer::Symbol::KW(_) => Self {
                    kind: TokenKind::KeyWord,
                    span: Span::default(),
                },
                _ => Self {
                    kind: TokenKind::Ident,
                    span: Span::default(),
                },
            },
            Literal(kind) => Self {
                kind: TokenKind::Literal,
                span: Span::default(),
            },
            LineComment => Self {
                kind: TokenKind::LineComment,
                span: Span::default(),
            },
            BlockComment { terminated } => Self {
                kind: TokenKind::BlockComment,
                span: Span::default(),
            },
            Comma => Self {
                kind: TokenKind::Comma,
                span: Span::default(),
            },
            Semi => Self {
                kind: TokenKind::Semi,
                span: Span::default(),
            },
            Colon => Self {
                kind: TokenKind::Colon,
                span: Span::default(),
            },
            Hyphen => Self {
                kind: TokenKind::Hyphen,
                span: Span::default(),
            },
            Pound => Self {
                kind: TokenKind::Pound,
                span: Span::default(),
            },
            Dollar => Self {
                kind: TokenKind::Dollar,
                span: Span::default(),
            },
            Eq => Self {
                kind: TokenKind::Eq,
                span: Span::default(),
            },
            Whitespace => Self {
                kind: TokenKind::Whitespace,
                span: Span::default(),
            },
            EOF => Self {
                kind: TokenKind::EOF,
                span: Span::default(),
            },

            OpenDelim(delim) => Self {
                kind: token_kind_from_open_delim(delim),
                span: Span::default(),
            },
            CloseDelim(delim) => Self {
                kind: token_kind_from_close_delim(delim),
                span: Span::default(),
            },

            Unknown => Self {
                kind: TokenKind::Unknown,
                span: Span::default(),
            },
            DummyToken => Self {
                kind: TokenKind::DummyToken,
                span: Span::default(),
            },
        }
    }
}

impl Default for TokenMeta {
    fn default() -> Self {
        Self {
            kind: TokenKind::DummyToken,
            span: Span::default(),
        }
    }
}

impl TokenMeta {
    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

#[derive(Debug)]
pub enum KeyWord {
    Let,
}

#[derive(Debug)]
pub enum Identifier {
    Type,
}

#[derive(Debug)]
pub enum Expression {
    CardField,
}

/*
 * Generate concrete syntax tree.
 */
#[derive(Debug)]
pub struct Comments(TokenMeta);

#[derive(Debug)]
pub struct CommentBlock(TokenMeta);

#[derive(Debug)]
pub struct Card {
    note_type: NoteType,
    card_block: CardBlock,
}

#[derive(Debug)]
pub struct CardBlock {
    // TODO How to better represent this?
    card_fields: Vec<CardField>,
}

#[derive(Debug)]
pub struct CardField {
    expr: Expression,
    /// Card fields can optionally end with a separator (ie: ';', '|', ect.)
    separator: Option<TokenMeta>,
}

#[derive(Debug)]
pub struct NoteType {
    pound_sign: TokenMeta,
    open_brace: TokenMeta,
    close_brace: TokenMeta,
    note_type: Option<Identifier>,
}

#[derive(Debug)]
pub struct LetStatement {
    r#let: KeyWord,
    lhs: Identifier,
    eq: TokenMeta,
    rhs: Identifier,
    semi_colon: TokenMeta,
}

#[derive(Debug)]
pub enum Command {
    LetStatement(LetStatement),
}

#[derive(Debug)]
pub struct ClozeDeletion {
    open_brace: TokenMeta,
    cloze_number: TokenMeta,
    colon: TokenMeta,
    deletion: Identifier,
    close_brace: TokenMeta,
    hint_colon: Option<TokenMeta>,
    hint: Option<Identifier>,
}

#[derive(Debug)]
pub struct StringReader<'a> {
    start_pos: usize,
    pos: usize,
    cursor: Cursor<'a>,
}

#[derive(Debug)]
pub struct Parser<'a> {
    string_reader: StringReader<'a>,
    peak_buf: Vec<TokenMeta>,
    expected_token: TokenKind,
}

// TODO
fn cook_lexer_token(lexer_token: LexerToken) -> TokenMeta {
    TokenMeta::new(lexer_token)
}

#[derive(Debug)]
pub enum ParseError {
    Expected { expected: TokenKind, got: TokenKind },
    Other(&'static str),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;
        match self {
            Expected { expected, got } => write!(f, "Parse Error: Expected {expected}; got {got}"),
            Other(s) => write!(f, "{s}"),
        }
    }
}

impl Error for ParseError {}

impl<'a> Parser<'a> {
    pub fn new(string_reader: StringReader<'a>) -> Self {
        Self {
            string_reader,
            peak_buf: Vec::with_capacity(2),
            expected_token: TokenKind::default(),
        }
    }

    fn next_token(&mut self) -> TokenMeta {
        cook_lexer_token(self.string_reader.next_token())
    }

    pub fn parse_tokens(&mut self) -> Result<(), ParseError> {
        use TokenKind::*;

        loop {
            let token = self.next_token();
            //dbg!(&token);

            match token.kind() {
                KeyWord => {
                    dbg!("KeyWord");
                }
                Ident => {
                    dbg!("Ident");
                }
                Literal => {
                    dbg!("Literal");
                }
                LineComment => {
                    dbg!("LineComment");
                }
                BlockComment => {
                    dbg!("BlockComment");
                }
                OpenBrace => {
                    dbg!("OpenBrace");
                }
                CloseAngleBracket => {
                    if self.peak_white().kind() == Whitespace && self.peak_second().kind() == KeyWord {
                        dbg!(self.parse_command(token)?);
                    }
                }
                Pound => {
                    // Could be start of NoteType
                    match self.peak().kind() {
                        OpenBracket => {
                            dbg!(self.parse_card(token)?);
                        }
                        kind => {
                            dbg!(kind);
                        }
                    };
                }
                EOF => break,
                Whitespace => (),
                t => todo!("{t:?}"),
            }
        }
        Ok(())
    }

    fn parse_note_type(&mut self, pound_sign: TokenMeta) -> Result<NoteType, ParseError> {
        let open_bracket = self.next_expect(TokenKind::OpenBracket)?;
        let note_type = match self.peak().kind() {
            TokenKind::Ident => Some(self.next_expect_ident_type()?),
            _=> None,
        };
        let close_brace = self.next_expect(TokenKind::CloseBracket)?;

        Ok(NoteType {
            pound_sign,
            open_brace: open_bracket,
            close_brace,
            note_type,
        })
    }

    fn parse_command(&mut self, close_angle_bracket: TokenMeta) -> Result<Command, ParseError> {
        let keyword = self.next_expect_keyword()?;
        match keyword {
            KeyWord::Let => Ok(Command::LetStatement(self.parse_let_statement(keyword)?)),
        }
    }

    fn next_non_whitespace(&mut self) -> TokenMeta {
        loop {
            let token = self.next_token();
            if token.kind() != TokenKind::Whitespace {
                return token;
            }
        }
    }

    fn next_expect_keyword(&mut self) -> Result<KeyWord, ParseError> {
        // TODO Compare str
        match self.next_expect_non_whitespace(TokenKind::KeyWord) {
            Ok(_) => Ok(KeyWord::Let),
            Err(e) => Err(e),
        }
    }

    fn next_expect_ident_type(&mut self) -> Result<Identifier, ParseError> {
        match self.next_expect_non_whitespace(TokenKind::Ident) {
            Ok(_) => Ok(Identifier::Type),
            Err(e) => Err(e)
        }
    }

    fn parse_card_field(&mut self) -> Result<CardField, ParseError> {
        let separator = loop {
            //println!("Next: parse_card_field: {:?}, second: {:?}", next, self.peak_second_non_white());
            match self.peak().kind() {
                TokenKind::EOF => break None,
                TokenKind::Pound => {
                    if self.peak_second_white().kind() == TokenKind::OpenBracket {
                        break None;
                    }
                }
                TokenKind::CloseAngleBracket => {
                    //println!("Should be command: {:#?}: {:#?}", self, self.peak_second());
                    if self.peak_second().kind() == TokenKind::KeyWord {
                        self.consume_white();
                        break None;
                    }
                }
                _ => (),
            }

            let token = self.next_token();
            match token.kind() {
                TokenKind::Semi => break Some(token),
                _ => continue,
            }
        };

        Ok(CardField {
            expr: Expression::CardField,
            separator,
        })
    }

    fn consume_white(&mut self) {
        loop {
            if self.peak().kind() == TokenKind::Whitespace {
                self.string_reader.next_token();
                continue
            }
            break;
        }
    }

    fn parse_card(&mut self, token: TokenMeta) -> Result<Card, ParseError> {
        let note_type = self.parse_note_type(token)?;
        let card_block = self.parse_card_block()?;
        Ok(Card {
            note_type,
            card_block,
        })
    }

    fn parse_card_block(&mut self) -> Result<CardBlock, ParseError> {
        let mut card_fields = Vec::new();
        loop {
            let card_field = self.parse_card_field()?;
            match card_field.separator {
                Some(_) => {
                    card_fields.push(card_field);
                }
                None => {
                    card_fields.push(card_field);
                    break;
                }
            }
        }
        Ok(CardBlock { card_fields })
    }

    fn parse_let_statement(&mut self, r#let: KeyWord) -> Result<LetStatement, ParseError> {
        let lhs = self.next_expect_ident_type()?;
        let eq = self.next_expect_assignment()?;
        let rhs = self.next_expect_ident_type()?;
        let semi_colon = self.next_expect_terminate_expression()?;

        Ok(LetStatement {
            r#let,
            lhs,
            eq,
            rhs,
            semi_colon,
        })
    }

    fn next_expect_assignment(&mut self) -> Result<TokenMeta, ParseError> {
        self.next_expect_non_whitespace(TokenKind::Eq)
    }

    fn next_expect_terminate_expression(&mut self) -> Result<TokenMeta, ParseError> {
        self.next_expect_non_whitespace(TokenKind::Semi)
    }

    fn parse_cloze_deletion(&self) -> Result<ClozeDeletion, ParseError> {
        todo!()
    }

    fn peak(&self) -> TokenMeta {
        let mut cursor = self.string_reader.cursor.clone();
        loop {
            let lexer_token = cursor.advance_token();
            if lexer_token.kind() == LexerTokenKind::Whitespace {
                continue;
            }
            return cook_lexer_token(lexer_token)
        }
    }

    fn peak_white(&self) -> TokenMeta {
        cook_lexer_token(self.string_reader.cursor.clone().advance_token())
    }

    fn peak_second_white(&self) -> TokenMeta {
        let mut cursor = self.string_reader.cursor.clone();
        cursor.advance_token();
        cook_lexer_token(cursor.advance_token())
    }

    fn peak_second(&self) -> TokenMeta {
        let mut cursor = self.string_reader.cursor.clone();
        cursor.advance_token();
        loop {
            let lexer_token = cursor.advance_token();
            if lexer_token.kind() == LexerTokenKind::Whitespace {
                continue;
            }
            return cook_lexer_token(lexer_token)
        }
    }

    fn next_expect(&mut self, kind: TokenKind) -> Result<TokenMeta, ParseError> {
        let token = self.next_token();
        match token.kind() {
            t if t == kind => Result::Ok(token),
            t => Result::Err(ParseError::Expected {
                expected: kind,
                got: t,
            }),
        }
    }

    fn next_expect_non_whitespace(&mut self, kind: TokenKind) -> Result<TokenMeta, ParseError> {
        let token = self.next_non_whitespace();
        match token.kind() {
            t if t == kind => Result::Ok(token),
            t => Result::Err(ParseError::Expected {
                expected: kind,
                got: t,
            }),
        }
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

    pub fn parse_tokens(mut self) {
        Parser::new(self);
        //loop {
        //    let token = self.next_token();
        //    if let anki_ft_lexer::TokenKind::EOF = token.kind() {
        //        break;
        //    }

        //    match token.kind() {
        //        anki_ft_lexer::TokenKind::EOF => break,
        //        _ => todo!(),
        //    }
        //}
    }

    pub fn next_token(&mut self) -> anki_ft_lexer::Token {
        let token = self.cursor.advance_token();
        self.offset(token.len());

        token
    }

    fn offset(&mut self, offset: usize) {
        self.pos += offset;
    }
}
