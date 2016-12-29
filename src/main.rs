extern crate signpostc_syntax as syntax;

use std::io::prelude::*;
use std::io;

use syntax::scanner::Scanner;
use syntax::position;
use syntax::token::TokenKind;
use syntax::errors::ErrorList;

const PROMPT: &'static str = ">> ";

fn main() {
    let stdin = io::stdin();
    print!("{}", PROMPT);
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.expect("failed to read line from stdin");
        let file = position::File::new(None, line.len());
        let errors = ErrorList::new();
        let mut sc = Scanner::new(file, &line, &errors);
        loop {
            let token = sc.scan();
            println!("{:?}", token);
            if token.kind() == TokenKind::EOF {
                break;
            }
        }
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
    }
}
