#![allow(unused_variables)]
#![allow(unused_imports)]
use anki_ft_codegen::Generator;
use anki_ft_lexer::{tokenize};
use anki_ft_parse::{Parser, StringReader, Token};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::{Path, PathBuf},
    str::FromStr, process::exit,
};
const TEST_FILE_PATH: &'static str = "./test3.ankift";


fn main() {
    let test_file = File::open(TEST_FILE_PATH).expect(&format!(
        "{TEST_FILE_PATH} not found: Make sure the file exists/you are in the correct folder"
    ));

    let mut src = String::new();
    BufReader::new(test_file).read_to_string(&mut src).unwrap();

    let mut s = tokenize(&src);
    let string_reader = StringReader::new(&src);

    let mut parser = Parser::new(string_reader);
    let mut generator = Generator::new(&src);
    generator.begin();

    //for _ in stdin().lines() {
    //    //dbg!(&generator);
    //    println!("{}", generator.next_card());
    //    //dbg!(s.next());
    //    //let grammar = dbg!(parser.next_token());
    //    //match grammar {
    //    //    Ok(v) => {
    //    //        dbg!(v);
    //    //    },
    //    //    Err(e) => {
    //    //        println!("{e}");
    //    //        exit(1);
    //    //    }
    //    //}
    //    //match grammar {
    //    //    ConcreteGrammar::Card(c) => {
    //    //        //let fields = c.card_block.card_fields.into_iter().map(|c|
    //    //        //    match c.expr {
    //    //        //        anki_ft_parse::Expression::CardField(t) => format!("{};", t.into_iter().map(|token| token.text.to_str().to_owned()).collect::<String>())
    //    //        //    }
    //    //        //).collect::<String>();
    //    //        //dbg!(fields);
    //    //    }
    //    //    _ =>()
    //    //}
    //}

    let grammar = parser.next_token().unwrap();

    // Grammar should be 'dropped' here as well.
    //drop(src);

    use std::io::stdin;
}
