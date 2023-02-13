use anki_ft_lexer::{StringReader, tokenize};
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
    let mut s = tokenize(&src);
    use std::io::stdin;
    for _ in stdin().lines() {
        dbg!(s.next());
    }
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
    //dbg!(s.next_token());
}
