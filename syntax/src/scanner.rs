//! `scanner` module implements the lexer phase of the language.
use errors::ErrorList;
use position::File;

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    file: File<'a>,
    src: &'a str,
    errors: &'a ErrorList<'a>,

    // scanning states
    ch: char,
    offset: usize,
    read_offset: usize,

    pub error_count: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(file: File<'a>, src: &'a str, errors: &'a ErrorList<'a>) -> Self {
        let mut sc = Scanner{
            file: file,
            src: src,
            errors: errors,

            ch: ' ',
            offset: 0,
            read_offset: 0,

            error_count: 0,
        };
        sc.next();
        sc
    }

    fn error(&mut self, offset: usize, msg: String) {
        self.errors.add(self.file.position(self.file.pos(offset)), msg);
        self.error_count += 1;
    }

    fn is_eof(&self) -> bool {
        self.read_offset >= self.src.len()
    }

    fn next(&mut self) {
        if self.is_eof() {
            self.offset = self.src.len();
            self.ch = 0 as char;
        } else {
            self.offset = self.read_offset;
            if self.ch == '\n' {
                self.file.add_line(self.offset);
            }
            self.ch = self.src[self.read_offset..].chars().next().unwrap();
            let w = self.ch.len_utf8();
            self.read_offset += w;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' || self.is_eof() {
            self.next();
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
        let sc = Scanner::new(file, input, &errors);
    }
}
