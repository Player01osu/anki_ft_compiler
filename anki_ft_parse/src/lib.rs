use anki_ft_lexer::Cursor;

#[derive(Clone, Debug)]
pub enum TokenKind {

}

#[derive(Clone, Debug)]
pub struct Token {
    kind: TokenKind,
    pub len: usize,
}

#[derive(Debug)]
pub struct StringReader<'a> {
    start_pos: usize,
    pos: usize,
    cursor: Cursor<'a>,
}

impl<'a> StringReader<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            start_pos: 0,
            pos: 0,
            cursor: Cursor::new(src),
        }
    }

    pub fn next_token(&mut self) -> anki_ft_lexer::Token {
        let token = self.cursor.advance_token();
        self.offset(token.len);

        token
    }

    fn offset(&mut self, offset: usize) {
        self.pos += offset;
    }
}

