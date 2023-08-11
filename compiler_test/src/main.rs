use std::{fs::File, io::{Read, stdin}};

use anki_ft_lexer::{Lexer, TokenKind};
fn main() {
    let mut f = File::open("test3.ankift").unwrap();
    let mut test = String::new();
    f.read_to_string(&mut test).unwrap();
    let mut lexer = Lexer::new(&test, ';');
    let mut b = String::new();

    loop {
        stdin().read_line(&mut b).unwrap();
        if matches!(dbg!(lexer.next_token()).kind, TokenKind::Eof) {
            break;
        }
    }
}
