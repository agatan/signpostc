extern crate signpostc_syntax as syntax;

use std::env;
use std::io::prelude::*;
use std::io;
use std::fs;
use std::rc::Rc;

use syntax::scanner::Scanner;
use syntax::position;
use syntax::token::TokenKind;

const PROMPT: &'static str = ">> ";

fn repl() {
    let stdin = io::stdin();
    print!("{}", PROMPT);
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.expect("failed to read line from stdin");
        let file = position::File::new(None, line.len());
        let errors = Rc::default();
        let mut sc = Scanner::new(file, &line, errors);
        loop {
            let token = sc.scan();
            println!("{:?}", token);
            if token.kind() == TokenKind::EOF {
                break;
            }
        }
        let parse_result = syntax::parse_file("stdin", &line);
        println!("{:?}", parse_result);
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
    }
}

fn main() {
    if env::args().len() == 1 {
        repl();
    } else {
        let mut fp = fs::File::open(env::args().nth(1).unwrap()).expect("failed to open file");
        let mut input = String::new();
        fp.read_to_string(&mut input).expect("failed to read");
        let parse_result = syntax::parse_file(&env::args().nth(1).unwrap(), &input);
        println!("{:?}", parse_result);
    }
}
