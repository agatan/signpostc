extern crate signpostc_syntax as syntax;

use std::env;
use std::io::prelude::*;
use std::io;
use std::fs;

use syntax::ast;

const PROMPT: &'static str = ">> ";

fn repl() {
    let stdin = io::stdin();
    print!("{}", PROMPT);
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.expect("failed to read line from stdin");
        let parse_result = syntax::parse_expr(&line);

        match parse_result {
            Ok(expr) => {
                ast::dump(io::stdout(), &expr).unwrap();
            }
            Err(e) => {
                println!("errors: {:?}", e);
            }
        }
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
        if let Ok(prog) = parse_result {
            ast::dump(io::stdout(), &prog).unwrap();
        }
    }
}
