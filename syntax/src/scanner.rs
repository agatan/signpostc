//! `scanner` module implements the lexer phase of the language.
use errors::ErrorList;
use position::File;

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    file: &'a File<'a>,
    src: &'a str,
    errors: &'a ErrorList<'a>,

    // scanning states
    ch: char,
    offset: usize,
    read_offset: usize,

    pub error_count: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(file: &'a File<'a>, src: &'a str, errors: &'a ErrorList<'a>) -> Self {
        Scanner{
            file: file,
            src: src,
            errors: errors,

            ch: ' ',
            offset: 0,
            read_offset: 0,

            error_count: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use errors::*;
    use position::*;
    use token::TokenKind;

    #[test]
    fn test_next_token() {
        let input = r#"
            let
        "#;
        let tests = vec![
            // token type, literal
            (TokenKind::Let, "let"),
        ];
        let file = File::new(Some("test"), input.len());
        let errors = ErrorList::new();
        let sc = Scanner::new(&file, input, &errors);
    }
}
