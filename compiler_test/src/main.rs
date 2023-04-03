#![allow(unused_variables)]
#![allow(unused_imports)]
use anki_ft_lexer::{tokenize};
use anki_ft_parse::{Parser, StringReader};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::{Path, PathBuf},
    str::FromStr,
};
const TEST_FILE_PATH: &'static str = "./test.ankift";

fn main() {
    let test_file = File::open(TEST_FILE_PATH).expect(&format!(
        "{TEST_FILE_PATH} not found: Make sure the file exists/you are in the correct folder"
    ));

    let mut src = String::new();
    BufReader::new(test_file).read_to_string(&mut src).unwrap();

    //let mut s = tokenize(&src);
    let string_reader = StringReader::new(&src);


    let mut parser = Parser::new(string_reader);

    for _ in stdin().lines() {
        dbg!(parser.parse_grammar().unwrap());
    }

    use std::io::stdin;
}
