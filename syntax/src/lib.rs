#![feature(box_syntax)]

pub mod position;
pub mod symbol;
pub mod token;
pub mod errors;
pub mod scanner;
pub mod ast;
pub mod parser;

pub fn parse_file(filename: &str, src: &str) -> Result<ast::Program, errors::ErrorList> {
    let file = position::File::new(Some(filename), src.len());
    let mut parser = parser::Parser::new(file, src);
    let program = parser.parse_program();
    let errors = parser.into_errors();
    if errors.is_empty() {
        Ok(program)
    } else {
        Err(errors)
    }
}
