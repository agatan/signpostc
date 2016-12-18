//! `scanner` module implements the lexer phase of the language.
use errors::ErrorList;

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    src: &'a str,
    errors: &'a ErrorList<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str, errors: &'a ErrorList<'a>) -> Self {
        Scanner{
            src: src,
            errors: errors,
        }
    }
}
