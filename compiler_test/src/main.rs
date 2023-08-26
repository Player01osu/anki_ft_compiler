use std::{env::args, fs::File, io::{Read, stdin}};

use anki_ft_codegen::Generator;
use anki_ft_lexer::{Lexer, TokenKind as LexerTokenKind};
use anki_ft_parse::{Parser, TokenKind};

fn main() {
    let mut args = args();
    args.next();
    let filename = args.next().unwrap_or("test3.ankift".to_string());
    let mut f = File::open(filename).unwrap();
    let mut test = String::new();
    f.read_to_string(&mut test).unwrap();

    let generator = Generator::new(&test, ';');
    generator.generate().unwrap();

    //let mut lexer = Lexer::new(&test, ';');
    //let mut parser = Parser::new(&test, ';');
    //let mut b = String::new();

    //loop {
    //    stdin().read_line(&mut b).unwrap();
    //    dbg!(parser.next_token().unwrap());
    //    //dbg!(lexer.next_token());
    //    //if matches!(dbg!(lexer.next_token()).kind, TokenKind::Eof) {
    //    //    break;
    //    //}
    //}
}
