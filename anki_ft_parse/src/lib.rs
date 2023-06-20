// TODO
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::error::Error;
use std::fmt::Display;

use anki_ft_lexer::Cursor;
use anki_ft_lexer::Span;
use anki_ft_lexer::Delimiter;
use anki_ft_lexer::Delimiter::*;
use anki_ft_lexer::Token as LexerToken;
use anki_ft_lexer::TokenKind as LexerTokenKind;
use anki_ft_lexer::TokenText;
use anki_ft_lexer::KW;

/*
 * Enriched tokens
 */

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
pub struct Token {
    pub kind: TokenKind,
    pub text: TokenText,
    pub span: Span,
}

#[derive(Debug)]
pub struct TokenOwned {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

#[derive(Debug)]
pub enum ConcreteGrammar {
    LineComment(LineComment),
    BlockComment(BlockComment),
    Card(Card),
    Command(Command),
    EOF,
}

//impl ToOwned for TokenMeta {
//    type Owned = TokenOwned;
//    fn to_owned(&self) -> Self::Owned {
//        TokenOwned {
//            kind: self.kind,
//            span: self.span,
//            text:
//        }
//    }
//}

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

fn is_end_of_expr(token_kind: TokenKind) -> bool {
    matches!(
        token_kind,
        TokenKind::Pound | TokenKind::CloseAngleBracket | TokenKind::EOF
    )
}

fn is_expr(token_kind: TokenKind) -> bool {
    matches!(token_kind, TokenKind::Ident | TokenKind::OpenBrace | TokenKind::Colon | TokenKind::CloseBrace | TokenKind::Literal | TokenKind::Whitespace)
}

fn is_cloze(token_kind: TokenKind) -> bool {
    matches!(token_kind, TokenKind::OpenBrace)
}

impl Token {
    pub fn new(lexer_token: LexerToken) -> Self {
        use anki_ft_lexer::TokenKind::*;
        match lexer_token.kind() {
            Ident(symbol) => match symbol {
                anki_ft_lexer::Symbol::KW(_) => Self {
                    kind: TokenKind::KeyWord,
                    text: lexer_token.text,
                    span: lexer_token.span,
                },
                _ => Self {
                    kind: TokenKind::Ident,
                    text: lexer_token.text,
                    span: lexer_token.span,
                },
            },
            Literal(kind) => Self {
                kind: TokenKind::Literal,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            LineComment => Self {
                kind: TokenKind::LineComment,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            BlockComment { terminated } => Self {
                kind: TokenKind::BlockComment,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Comma => Self {
                kind: TokenKind::Comma,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Semi => Self {
                kind: TokenKind::Semi,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Colon => Self {
                kind: TokenKind::Colon,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Hyphen => Self {
                kind: TokenKind::Hyphen,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Pound => Self {
                kind: TokenKind::Pound,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Dollar => Self {
                kind: TokenKind::Dollar,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Eq => Self {
                kind: TokenKind::Eq,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            Whitespace => Self {
                kind: TokenKind::Whitespace,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            EOF => Self {
                kind: TokenKind::EOF,
                text: lexer_token.text,
                span: lexer_token.span,
            },

            OpenDelim(delim) => Self {
                kind: token_kind_from_open_delim(delim),
                text: lexer_token.text,
                span: lexer_token.span,
            },
            CloseDelim(delim) => Self {
                kind: token_kind_from_close_delim(delim),
                text: lexer_token.text,
                span: lexer_token.span,
            },

            Unknown => Self {
                kind: TokenKind::Unknown,
                text: lexer_token.text,
                span: lexer_token.span,
            },
            DummyToken => Self {
                kind: TokenKind::DummyToken,
                text: lexer_token.text,
                span: lexer_token.span,
            },
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::DummyToken,
            text: TokenText::Empty,
            span: Span::default(),
        }
    }
}

impl Token {
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
    Name(Token),
    Type(Token),
}

#[derive(Debug)]
pub enum ExprType {
    Normal(Token),
    Cloze(ClozeDeletion),
    Empty,
}

#[derive(Debug)]
pub struct Expr {
    token: ExprType,
    next_token: Option<Box<Expr>>,
}

/*
 * Generate concrete syntax tree.
 */
#[derive(Debug)]
pub struct LineComment(Token);

#[derive(Debug)]
pub struct BlockComment(Token);

#[derive(Debug)]
pub struct Card {
    pub note_type: NoteType,
    pub card_block: CardBlock,
}

#[derive(Debug)]
pub struct CardBlock {
    // TODO How to better represent this?
    pub card_fields: Vec<CardField>,
}

#[derive(Debug)]
pub struct CardField {
    pub expr: Expr,
    /// Card fields can optionally end with a separator (ie: ';', '|', ect.)
    pub separator: Separator,
}

#[derive(Debug)]
pub enum Separator {
    Normal(Token),
    Overwrite(OverwriteSeparator),
    None,
}

#[derive(Debug)]
pub struct OverwriteSeparator {
    open_angle_bracket: Token,
    semi_colon: Token,
    close_angle_bracket: Token,
}

#[derive(Debug)]
pub struct NoteType {
    pound_sign: Token,
    open_brace: Token,
    close_brace: Token,
    note_type: Option<Identifier>,
}

#[derive(Debug)]
pub struct LetStatement {
    r#let: KeyWord,
    lhs: Identifier,
    eq: Token,
    rhs: Identifier,
    semi_colon: Token,
}

#[derive(Debug)]
pub enum Command {
    LetStatement(LetStatement),
}

#[derive(Debug)]
pub struct ClozeDeletion {
    open_brace: Token,
    cloze_number: Token,
    colon: Token,
    deletion: Identifier,
    close_brace: Token,
    hint_colon: Option<Token>,
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
    peak_buf: Vec<Token>,
    expected_token: TokenKind,
}

// TODO
fn cook_lexer_token(lexer_token: LexerToken) -> Token {
    Token::new(lexer_token)
}

fn is_separator(token_kind: TokenKind) -> bool {
    matches!(token_kind, TokenKind::Semi)
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

    fn next_token(&mut self) -> Token {
        cook_lexer_token(self.string_reader.next_token())
    }

    pub fn parse_grammar(&mut self) -> Result<ConcreteGrammar, ParseError> {
        use TokenKind::*;

        loop {
            let token = self.next_token();

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
                    use self::LineComment as LineCommentStruct;
                    return Ok(ConcreteGrammar::LineComment(LineCommentStruct(token)));
                }
                BlockComment => {
                    use self::BlockComment as BlockCommentStruct;
                    dbg!("BlockComment");
                    return Ok(ConcreteGrammar::BlockComment(BlockCommentStruct(token)));
                }
                OpenBrace => {
                    dbg!("OpenBrace");
                }
                CloseAngleBracket => {
                    if self.peak_white().kind() == Whitespace {
                        return Ok(ConcreteGrammar::Command(self.parse_command(token)?));
                    }
                }
                Pound => {
                    // Could be start of NoteType
                    match self.peak().kind() {
                        OpenBracket => {
                            return Ok(ConcreteGrammar::Card(self.parse_card(token)?));
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
        Ok(ConcreteGrammar::EOF)
    }

    fn parse_note_type(&mut self, pound_sign: Token) -> Result<NoteType, ParseError> {
        let open_bracket = self.next_expect(TokenKind::OpenBracket)?;
        let note_type = match self.peak().kind() {
            TokenKind::Ident => Some(self.next_expect_ident_type()?),
            _ => None,
        };
        let close_brace = self.next_expect(TokenKind::CloseBracket)?;

        Ok(NoteType {
            pound_sign,
            open_brace: open_bracket,
            close_brace,
            note_type,
        })
    }

    fn parse_command(&mut self, close_angle_bracket: Token) -> Result<Command, ParseError> {
        let keyword = self.next_expect_keyword()?;
        match keyword {
            KeyWord::Let => Ok(Command::LetStatement(self.parse_let_statement(keyword)?)),
        }
    }

    fn next_non_whitespace(&mut self) -> Token {
        loop {
            let token = self.next_token();
            if token.kind() != TokenKind::Whitespace {
                return token;
            }
        }
    }

    fn next_expect_keyword(&mut self) -> Result<KeyWord, ParseError> {
        match self.next_expect_non_whitespace(TokenKind::KeyWord) {
            Ok(keyword) => {
                let text = keyword.text.to_str();
                match text {
                    "let" => Ok(KeyWord::Let),
                    kw => {
                        panic!("Unknown keyword: {kw}");
                    }
                }
            }
            Err(e) => Err(e),
        }
    }

    fn next_expect_ident_type(&mut self) -> Result<Identifier, ParseError> {
        match self.next_expect_non_whitespace(TokenKind::Ident) {
            Ok(t) => Ok(Identifier::Type(t)),
            Err(e) => Err(e),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let token = self.next_token();

        let token = match token.kind() {
            TokenKind::Semi => ExprType::Empty,
            kind if is_cloze(kind) => ExprType::Cloze(self.parse_cloze_deletion(token)?),
            kind if !is_expr(kind) => return Err(ParseError::Expected{expected: TokenKind::Ident, got: kind}),
            _ => ExprType::Normal(token),
        };

        let next_token = if is_expr(self.peak().kind()) {
            Some(Box::new(self.parse_expression()?))
        } else {
            self.consume_white();
            None
        };

        Ok(Expr { token, next_token })
    }

    fn parse_card_field(&mut self) -> Result<CardField, ParseError> {
        // TODO Expressions aren't always one token.
        // For example, the following should parse
        // this is 'one field' of a card;     and this is another
        // ^       ^          ^^        ^     ^
        // ident   literal    ident     semi  ident
        // ^                           ^      ^
        // expr                     end expr  expr
        self.consume_white();
        let expr = self.parse_expression()?;
        let separator = {
            match self.peak().kind() {
                TokenKind::Semi => Separator::Normal(self.next_token()),
                TokenKind::OpenAngleBracket => {
                    if is_separator(self.peak_second().kind()) {
                        self.parse_overwrite_separator()?
                    } else {
                        Separator::None
                    }
                }
                _ => Separator::None
            }
        };

        Ok(CardField {
            expr,
            separator,
        })
    }

    fn parse_overwrite_separator(&mut self) -> Result<Separator, ParseError> {
        let open_angle_bracket = self.next_expect(TokenKind::OpenAngleBracket)?;
        let semi_colon = self.next_expect(TokenKind::Semi)?;
        let close_angle_bracket = self.next_expect(TokenKind::CloseAngleBracket)?;

        Ok(Separator::Overwrite(OverwriteSeparator {
            open_angle_bracket,
            semi_colon,
            close_angle_bracket,
        }))
    }

    fn consume_white(&mut self) {
        loop {
            if self.peak_white().kind() == TokenKind::Whitespace {
                self.string_reader.next_token();
                continue;
            }
            break;
        }
    }

    fn parse_card(&mut self, token: Token) -> Result<Card, ParseError> {
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
                Separator::None => {
                    card_fields.push(card_field);
                    break;
                }
                Separator::Normal(_) | Separator::Overwrite(_) => {
                    card_fields.push(card_field);
                    if is_end_of_expr(self.peak().kind()) {
                        break;
                    }
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

    fn next_expect_assignment(&mut self) -> Result<Token, ParseError> {
        self.next_expect_non_whitespace(TokenKind::Eq)
    }

    fn next_expect_terminate_expression(&mut self) -> Result<Token, ParseError> {
        self.next_expect_non_whitespace(TokenKind::Semi)
    }

    fn parse_cloze_deletion(&mut self, open_brace: Token) -> Result<ClozeDeletion, ParseError> {
        let open_brace = open_brace;
        let cloze_number = self.next_expect(TokenKind::Ident)?;
        let colon = self.next_expect(TokenKind::Colon)?;
        let deletion = self.next_expect_ident_type()?;


        let token = self.next_token();
        match token.kind() {
            TokenKind::Colon => {
                Ok(ClozeDeletion {
                    open_brace,
                    cloze_number,
                    colon,
                    deletion,
                    hint_colon: Some(token),
                    hint: Some(self.next_expect_ident_type()?),
                    close_brace: self.next_expect(TokenKind::CloseBrace)?,
                })
            }
            TokenKind::CloseBrace => {
                Ok(ClozeDeletion {
                    open_brace,
                    cloze_number,
                    colon,
                    deletion,
                    close_brace: token,
                    hint_colon: None,
                    hint: None,
                })
            }
            got => Err(ParseError::Expected{ expected: TokenKind::CloseBrace, got }),

        }
    }

    fn peak(&self) -> Token {
        let mut cursor = self.string_reader.cursor.clone();
        loop {
            let lexer_token = cursor.advance_token();
            if lexer_token.kind() == LexerTokenKind::Whitespace {
                continue;
            }
            return cook_lexer_token(lexer_token);
        }
    }

    fn peak_white(&self) -> Token {
        cook_lexer_token(self.string_reader.cursor.clone().advance_token())
    }

    fn peak_second_white(&self) -> Token {
        let mut cursor = self.string_reader.cursor.clone();
        cursor.advance_token();
        cook_lexer_token(cursor.advance_token())
    }

    fn peak_second(&self) -> Token {
        let mut cursor = self.string_reader.cursor.clone();
        cursor.advance_token();
        loop {
            let lexer_token = cursor.advance_token();
            if lexer_token.kind() == LexerTokenKind::Whitespace {
                continue;
            }
            return cook_lexer_token(lexer_token);
        }
    }

    fn next_expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        let token = self.next_token();
        match token.kind() {
            t if t == kind => Result::Ok(token),
            t => Result::Err(ParseError::Expected {
                expected: kind,
                got: t,
            }),
        }
    }

    fn next_expect_non_whitespace(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
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
