//! `scanner` module implements the lexer phase of the language.
use token::{Token, TokenKind};
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

    fn take_while<F: Fn(char) -> bool>(&mut self, f: F) {
        while !self.is_eof() && f(self.ch) {
            self.next();
        }
    }

    fn skip_whitespace(&mut self) {
        self.take_while(|c| c.is_whitespace());
    }

    pub fn scan(&mut self) -> Token {
        self.skip_whitespace();

        let offset = self.offset;

        if self.is_eof() {
            Token::new(TokenKind::EOF, "")
        } else if self.ch.is_alphabetic() {
            let is_lowercase = self.ch.is_lowercase();
            self.scan_identifier(is_lowercase)
        } else {
            Token::new(TokenKind::Error, &self.src[offset..self.offset])
        }
    }

    fn scan_identifier(&mut self, is_lowercase: bool) -> Token {
        let offset = self.offset;
        self.take_while(|c| c.is_alphabetic());
        let src = &self.src[offset..self.offset];

        let kind = if is_lowercase {
            TokenKind::lookup(src)
        } else {
            TokenKind::Uident
        };
        Token::new(kind, src)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use errors::*;
    use position::*;
    use token::{Token, TokenKind};

    #[test]
    fn test_next_token() {
        let input = r#"
            let
        "#;
        let tests = vec![
            // token type, literal
            (TokenKind::Let, "let"),
            (TokenKind::EOF, ""),
        ];
        let file = File::new(Some("test"), input.len());
        let errors = ErrorList::new();
        let mut sc = Scanner::new(file, input, &errors);

        for (expected_kind, expected_lit) in tests {
            assert_eq!(Token::new(expected_kind, expected_lit), sc.scan());
        }
    }
}
