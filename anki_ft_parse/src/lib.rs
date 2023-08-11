// TODO
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::error::Error;
use std::fmt::Display;

use anki_ft_lexer::Cursor;
use anki_ft_lexer::Delimiter;
use anki_ft_lexer::Delimiter::*;
use anki_ft_lexer::LiteralKind;
use anki_ft_lexer::Span;
use anki_ft_lexer::Symbol;
use anki_ft_lexer::Token as LexerToken;
use anki_ft_lexer::TokenKind as LexerTokenKind;
use anki_ft_lexer::KW;

use strum::Display;

#[derive(Debug)]
pub enum Token {
    LineComment(LineComment),
    BlockComment(BlockComment),
    Card(Card),
    Command(Command),
    EOF,
}

fn is_end_of_expr(token_kind: LexerTokenKind) -> bool {
    matches!(
        token_kind,
        LexerTokenKind::OpenDelim(Delimiter::NoteType)
            | LexerTokenKind::Pound
            | LexerTokenKind::CloseDelim(Delimiter::AngleBracket)
            | LexerTokenKind::EOF
    )
}

fn is_expr(token_kind: &LexerTokenKind) -> bool {
    matches!(
        token_kind,
        LexerTokenKind::Ident(..)
            | LexerTokenKind::OpenDelim(Delimiter::Brace)
            | LexerTokenKind::Colon
            | LexerTokenKind::CloseDelim(Delimiter::Brace)
            | LexerTokenKind::Literal(..)
            | LexerTokenKind::Whitespace
    )
}

fn is_cloze(token_kind: &LexerTokenKind) -> bool {
    matches!(token_kind, LexerTokenKind::OpenDelim(Delimiter::Brace))
}

#[derive(Debug)]
pub enum Identifier {
    Name(LexerToken),
    Type(LexerToken),
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(t) => write!(f, "{}", t.kind()),
            Self::Type(t) => write!(f, "{}", t.kind()),
        }
    }
}

impl Identifier {
    pub fn span(&self) -> Span {
        match self {
            Self::Name(t) => t.span,
            Self::Type(t) => t.span,
        }
    }
}

#[derive(Debug)]
pub enum ExprType {
    Normal(LexerToken),
    Cloze(ClozeDeletion),
    Empty(Option<Separator>, Span),
}

impl Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Normal(token) => write!(f, "{}", token.kind()),
            ExprType::Cloze(cloze) => write!(f, "{}", cloze),
            ExprType::Empty(..) => Ok(()),
        }
    }
}

impl ExprType {
    pub fn span(&self) -> Span {
        match self {
            Self::Normal(t) => t.span,
            Self::Cloze(ClozeDeletion { span, .. }) => *span,
            Self::Empty(_, span) => *span,
        }
    }
}

#[derive(Debug)]
pub struct Expr {
    token: ExprType,
    next_token: Option<Box<Expr>>,
    span: Span,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.token {
            ExprType::Normal(LexerToken {
                kind: LexerTokenKind::Literal(LiteralKind::String { ref item, .. }),
                ..
            }) => write!(f, "\"\"{item}\"\"")?,
            _ => write!(f, "{}", self.token)?,
        }
        if let Some(ref expr) = self.next_token {
            write!(f, "{}", expr)?;
        }
        Ok(())
    }
}

/*
 * Generate concrete syntax tree.
 */
#[derive(Debug)]
pub struct LineComment(LexerToken);

#[derive(Debug)]
pub struct BlockComment(LexerToken);

#[derive(Debug)]
pub struct Card {
    pub notetype: NoteType,
    pub blocks: CardBlock,
    pub span: Span,
}

impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.blocks
                .fields
                .iter()
                .map(|f| f.to_string())
                .collect::<String>()
        )
    }
}

#[derive(Debug)]
pub struct CardBlock {
    // TODO How to better represent this?
    pub fields: Vec<CardField>,
    pub span: Span,
}

impl Display for CardBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for field in self.fields.iter() {
            write!(f, "{}{}", field.expr, field.separator)?;
        }
        Ok(())
    }
}

impl Display for CardField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"{}", self.expr, self.separator)
    }
}

impl Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Separator::None => Ok(()),
            _ => write!(f, ";"),
        }
    }
}

#[derive(Debug)]
pub struct CardField {
    pub expr: Expr,
    /// Card fields can optionally end with a separator (ie: ';', '|', ect.)
    pub separator: Separator,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum Separator {
    Normal(Span),
    Overwrite(Span),
    None,
}

#[derive(Debug)]
pub struct NoteType {
    pub notetype: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug)]
pub struct LetStatement {
    pub lhs: Identifier,
    pub rhs: Rhs,
    pub span: Span,
}

#[derive(Debug)]
pub enum Rhs {
    Ident(Span, LexerToken),
    Literal(Span, LexerToken),
}

impl Display for Rhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rhs::Literal(
                _,
                LexerToken {
                    kind: LexerTokenKind::Literal(LiteralKind::String { item, .. }),
                    ..
                },
            ) => item.fmt(f),
            Rhs::Literal(_, t) => t.fmt(f),
            Rhs::Ident(_, t) => t.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum Command {
    LetStatement(LetStatement),
}

#[derive(Debug)]
pub struct ClozeDeletion {
    cloze_number: LexerToken,
    deletion: Identifier,
    hint: Option<Identifier>,
    span: Span,
}

impl Display for ClozeDeletion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.hint {
            Some(ref hint) => write!(
                f,
                "{{{{{}::{}::{}}}}}",
                self.cloze_number, self.deletion, hint
            ),
            None => write!(f, "{{{{{}::{}}}}}", self.cloze_number, self.deletion),
        }
    }
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
    peak_buf: Vec<LexerToken>,
    expected_token: LexerTokenKind,
}

fn is_separator(token_kind: LexerTokenKind) -> bool {
    matches!(token_kind, LexerTokenKind::Semi)
}

#[derive(Debug)]
pub enum ParseError {
    Expected {
        expected: LexerTokenKind,
        got: LexerTokenKind,
        span: Span,
    },
    Other(Span, &'static str),
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::Expected { span, .. } => *span,
            ParseError::Other(span, _) => *span,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;
        write!(
            f,
            "{}:{} Parse Error: ",
            self.span().start_row,
            self.span().start_col
        )?;
        match self {
            Expected {
                expected,
                got,
                span,
            } => write!(f, "Expected {expected}; got {got}",),
            Other(_, s) => write!(f, "{s}"),
        }
    }
}

impl Error for ParseError {}

macro_rules! expect_next {
    ($self: ident, $kind: pat) => {{
        let token = $self.bump_token();
        match token.kind() {
            $kind => Ok(token),
            t => Err(ParseError::Expected {
                expected: LexerTokenKind::DummyToken,
                //expected: $kind,
                span: token.span,
                got: t,
            }),
        }
    }};
}

macro_rules! expect_non_whitespace {
    ($self: ident, $kind: pat) => {{
        let token = $self.next_non_whitespace();
        match token.kind() {
            $kind => Ok(token),
            t => Err(ParseError::Expected {
                expected: LexerTokenKind::DummyToken,
                //expected: $kind,
                span: token.span,
                got: t,
            }),
        }
    }};
}

impl<'a> Parser<'a> {
    pub fn new(string_reader: StringReader<'a>) -> Self {
        Self {
            string_reader,
            peak_buf: Vec::with_capacity(2),
            expected_token: LexerTokenKind::DummyToken,
        }
    }

    fn bump_token(&mut self) -> LexerToken {
        self.string_reader.next_token()
    }

    pub fn next_token(&mut self) -> Result<Token, ParseError> {
        use LexerTokenKind::*;

        let token = dbg!(self.bump_token());

        match token.kind() {
            LineComment => {
                use self::LineComment as LineCommentStruct;
                Ok(Token::LineComment(LineCommentStruct(token)))
            }
            BlockComment { .. } => {
                use self::BlockComment as BlockCommentStruct;
                Ok(Token::BlockComment(BlockCommentStruct(token)))
            }
            CloseDelim(Delimiter::AngleBracket) if self.peak_white().kind() == Whitespace => {
                Ok(Token::Command(self.parse_command(token)?))
            }
            OpenDelim(Delimiter::NoteType) => Ok(Token::Card(self.parse_card(token)?)),
            EOF => Ok(Token::EOF),
            Whitespace => self.next_token(),
            t => {
                dbg!(t);
                Err(ParseError::Other(token.span, "Unexpected Token"))
            }
        }
    }

    fn parse_note_type(&mut self, open_note_type: LexerToken) -> Result<NoteType, ParseError> {
        let note_type = match self.peak().kind() {
            LexerTokenKind::Ident(..) => Some(self.next_expect_ident_type()?),
            _ => None,
        };
        let close_bracket = expect_next!(self, LexerTokenKind::CloseDelim(Delimiter::Bracket))?;

        let span = open_note_type.span.join(close_bracket.span);
        Ok(NoteType {
            notetype: note_type,
            span,
        })
    }

    fn parse_command(&mut self, close_angle_bracket: LexerToken) -> Result<Command, ParseError> {
        let token = self.next_expect_keyword()?;
        match token.kind() {
            LexerTokenKind::Ident(Symbol::KW(KW::Let)) => {
                Ok(Command::LetStatement(self.parse_let_statement(token)?))
            }
            _ => unimplemented!(),
        }
    }

    fn next_non_whitespace(&mut self) -> LexerToken {
        loop {
            let token = self.bump_token();
            if token.kind() != LexerTokenKind::Whitespace {
                return token;
            }
        }
    }

    fn next_expect_keyword(&mut self) -> Result<LexerToken, ParseError> {
        expect_non_whitespace!(self, LexerTokenKind::Ident(Symbol::KW(..)))
    }

    fn next_expect_ident_type(&mut self) -> Result<Identifier, ParseError> {
        match expect_non_whitespace!(self, LexerTokenKind::Ident(Symbol::Other(..))) {
            Ok(t) => Ok(Identifier::Type(t)),
            Err(e) => Err(e),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let token = self.bump_token();

        let token = match token.kind() {
            LexerTokenKind::Semi => {
                ExprType::Empty(Some(Separator::Normal(token.span)), token.span)
            }
            LexerTokenKind::OpenDelim(Delimiter::AngleBracket)
                if is_separator(self.peak().kind())
                    && matches!(
                        self.peak_second().kind(),
                        LexerTokenKind::CloseDelim(Delimiter::AngleBracket)
                    ) =>
            {
                // ';'
                self.bump_token();
                // '>'
                let close_angle = self.bump_token();
                let span = token.span.join(close_angle.span);
                ExprType::Empty(Some(Separator::Overwrite(span)), span)
            }
            kind if is_cloze(&kind) => ExprType::Cloze(self.parse_cloze_deletion(token)?),
            kind if !is_expr(&kind) => {
                return Err(ParseError::Expected {
                    expected: LexerTokenKind::Ident(Symbol::Other(String::new())),
                    span: token.span,
                    got: kind,
                })
            }
            _ => ExprType::Normal(token),
        };

        let (next_token, span) = if is_expr(&self.peak().kind()) && !matches!(token, ExprType::Empty(..)) {
            let expr = self.parse_expression()?;
            let span = token.span().join(expr.span);
            (Some(Box::new(expr)), span)
        } else {
            self.consume_white();
            (None, token.span())
        };

        Ok(Expr {
            token,
            next_token,
            span,
        })
    }

    //fn parse_card_fields(&mut self) -> Result<Vec<CardField>, ParseError> {
    //    self.consume_white();
    //    let expr = self.parse_expression()?;

    //}

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
            if let ExprType::Empty(Some(separator), span) = expr.token {
                separator
            } else {
                match self.peak().kind() {
                    LexerTokenKind::Semi => Separator::Normal(self.bump_token().span),
                    LexerTokenKind::OpenDelim(Delimiter::AngleBracket) => {
                        if is_separator(self.peak_second().kind()) {
                            self.parse_overwrite_separator()?
                        } else {
                            Separator::None
                        }
                    }
                    _ => Separator::None,
                }
            }
        };
        let span = match separator {
            Separator::None => expr.span,
            Separator::Normal(span) | Separator::Overwrite(span) => expr.span.join(span),
        };

        Ok(CardField {
            expr,
            separator,
            span,
        })
    }

    fn parse_overwrite_separator(&mut self) -> Result<Separator, ParseError> {
        let open_angle_bracket =
            expect_next!(self, LexerTokenKind::OpenDelim(Delimiter::AngleBracket))?;
        expect_next!(self, LexerTokenKind::Semi)?;
        let close_angle_bracket =
            expect_next!(self, LexerTokenKind::CloseDelim(Delimiter::AngleBracket))?;
        let span = open_angle_bracket.span.join(close_angle_bracket.span);

        Ok(Separator::Overwrite(span))
    }

    fn consume_white(&mut self) {
        loop {
            if self.peak_white().kind() == LexerTokenKind::Whitespace {
                self.string_reader.next_token();
                continue;
            }
            break;
        }
    }

    fn parse_card(&mut self, open_note_type: LexerToken) -> Result<Card, ParseError> {
        let note_type = self.parse_note_type(open_note_type)?;
        let card_block = self.parse_card_block()?;
        let span = note_type.span.join(card_block.span);
        Ok(Card {
            notetype: note_type,
            blocks: card_block,
            span,
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

        assert!(!card_fields.is_empty());
        let first_field = &card_fields[0];
        let last_field = card_fields.last().unwrap();

        let span = first_field.span.join(last_field.span);
        Ok(CardBlock {
            fields: card_fields,
            span,
        })
    }

    fn parse_let_statement(&mut self, r#let: LexerToken) -> Result<LetStatement, ParseError> {
        let lhs = self.next_expect_ident_type()?;
        self.next_expect_assignment()?;
        //let rhs = self.next_expect_ident_type()?;
        let rhs = self.next_expect_rhs()?;
        let semi_colon = self.next_expect_terminate_expression()?;

        let span = r#let.span.join(semi_colon.span);

        Ok(LetStatement { lhs, rhs, span })
    }

    fn next_expect_rhs(&mut self) -> Result<Rhs, ParseError> {
        let token = expect_non_whitespace!(
            self,
            LexerTokenKind::Ident(..) | LexerTokenKind::Literal(..)
        )?;
        match token.kind {
            LexerTokenKind::Ident(..) => Ok(Rhs::Ident(token.span, token)),
            LexerTokenKind::Literal(..) => Ok(Rhs::Literal(token.span, token)),
            _ => unreachable!(),
        }
    }

    fn next_expect_assignment(&mut self) -> Result<LexerToken, ParseError> {
        expect_non_whitespace!(self, LexerTokenKind::Eq)
    }

    fn next_expect_terminate_expression(&mut self) -> Result<LexerToken, ParseError> {
        expect_non_whitespace!(self, LexerTokenKind::Semi)
    }

    fn parse_cloze_deletion(
        &mut self,
        open_brace: LexerToken,
    ) -> Result<ClozeDeletion, ParseError> {
        let cloze_number = expect_next!(self, LexerTokenKind::Ident(..))?;
        expect_next!(self, LexerTokenKind::Colon)?;
        let deletion = self.next_expect_ident_type()?;

        let token = self.bump_token();
        match token.kind() {
            LexerTokenKind::Colon => {
                let hint = self.next_expect_ident_type()?;
                let span = open_brace.span.join(hint.span());
                Ok(ClozeDeletion {
                    cloze_number,
                    deletion,
                    hint: Some(hint),
                    span,
                })
            }
            LexerTokenKind::CloseDelim(Delimiter::Brace) => {
                let span = open_brace.span.join(token.span);
                Ok(ClozeDeletion {
                    cloze_number,
                    deletion,
                    hint: None,
                    span,
                })
            }
            got => Err(ParseError::Expected {
                expected: LexerTokenKind::CloseDelim(Delimiter::Brace),
                span: token.span,
                got,
            }),
        }
    }

    fn peak(&self) -> LexerToken {
        let mut cursor = self.string_reader.cursor.clone();
        loop {
            let lexer_token = cursor.advance_token();
            if matches!(lexer_token.kind(), LexerTokenKind::Whitespace) {
                continue;
            }
            return lexer_token;
        }
    }

    fn peak_white(&self) -> LexerToken {
        self.string_reader.cursor.clone().advance_token()
    }

    fn peak_second_white(&self) -> LexerToken {
        let mut cursor = self.string_reader.cursor.clone();
        cursor.advance_token();
        cursor.advance_token()
    }

    fn peak_second(&self) -> LexerToken {
        let mut cursor = self.string_reader.cursor.clone();
        cursor.advance_token();
        loop {
            let lexer_token = cursor.advance_token();
            if matches!(lexer_token.kind(), LexerTokenKind::Whitespace) {
                continue;
            }
            return lexer_token;
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

    pub fn parse_tokens(self) {
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

    pub fn next_token(&mut self) -> LexerToken {
        let token = self.cursor.advance_token();
        self.offset(token.len());

        token
    }

    fn offset(&mut self, offset: usize) {
        self.pos += offset;
    }
}
