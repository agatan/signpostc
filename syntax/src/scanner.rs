//! `scanner` module implements the lexer phase of the language.
use std::rc::Rc;
use std::cell::RefCell;

use token::{Token, TokenKind};
use errors::ErrorList;
use position::File;

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    file: File,
    src: &'a str,
    errors: Rc<RefCell<ErrorList>>,

    // scanning states
    ch: char,
    offset: usize,
    read_offset: usize,

    pub error_count: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(file: File, src: &'a str, errors: Rc<RefCell<ErrorList>>) -> Self {
        let mut sc = Scanner {
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

    pub fn file(&self) -> &File {
        &self.file
    }

    fn error(&mut self, offset: usize, msg: String) {
        self.errors.borrow_mut().add(self.file.position(self.file.pos(offset)), msg);
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
        while f(self.ch) {
            self.next();
        }
    }

    fn skip_whitespace(&mut self) {
        self.take_while(|c| c.is_whitespace());
    }

    fn substr_from(&self, offset: usize) -> &str {
        &self.src[offset..self.offset]
    }

    pub fn scan(&mut self) -> Token {
        self.skip_whitespace();

        let offset = self.offset;
        let pos = self.file.pos(self.offset);

        if self.ch == 0 as char {
            Token::new(pos, TokenKind::EOF, "")
        } else if is_operator(self.ch) {
            self.scan_operator()
        } else if self.ch.is_alphabetic() {
            self.scan_identifier()
        } else if self.ch.is_digit(10) {
            self.scan_number()
        } else if self.ch == '"' {
            self.scan_string()
        } else {
            let ch = self.ch;
            self.next();
            match ch {
                ',' => Token::new(pos, TokenKind::Comma, self.substr_from(offset)),
                ':' => Token::new(pos, TokenKind::Colon, self.substr_from(offset)),
                ';' => Token::new(pos, TokenKind::Semicolon, self.substr_from(offset)),
                '(' => Token::new(pos, TokenKind::Lparen, self.substr_from(offset)),
                ')' => Token::new(pos, TokenKind::Rparen, self.substr_from(offset)),
                '{' => Token::new(pos, TokenKind::Lbrace, self.substr_from(offset)),
                '}' => Token::new(pos, TokenKind::Rbrace, self.substr_from(offset)),
                '[' => Token::new(pos, TokenKind::Lbrack, self.substr_from(offset)),
                ']' => Token::new(pos, TokenKind::Rbrack, self.substr_from(offset)),
                _ => {
                    self.error(offset, format!("unknown character: {}", ch));
                    Token::new(pos, TokenKind::Error, self.substr_from(offset))
                }
            }
        }
    }

    fn scan_operator(&mut self) -> Token {
        let offset = self.offset;
        let pos = self.file.pos(offset);
        self.take_while(is_operator);
        let src = self.substr_from(offset);
        let kind = TokenKind::lookup_operator(src);
        Token::new(pos, kind, src)
    }

    fn scan_identifier(&mut self) -> Token {
        let offset = self.offset;
        let is_lowercase = self.ch.is_lowercase();
        let pos = self.file.pos(offset);
        self.take_while(|c| c.is_alphabetic());
        let src = self.substr_from(offset);

        let kind = if is_lowercase {
            TokenKind::lookup(src)
        } else {
            TokenKind::Uident
        };
        Token::new(pos, kind, src)
    }

    fn scan_number(&mut self) -> Token {
        let offset = self.offset;
        let pos = self.file.pos(offset);
        self.take_while(|c| c.is_digit(10));
        let src = self.substr_from(offset);
        Token::new(pos, TokenKind::Int, src)
    }

    fn scan_string(&mut self) -> Token {
        let offset = self.offset;
        let pos = self.file.pos(offset);
        self.next();
        loop {
            if self.ch == 0 as char {
                self.error(offset, "unclosed string literal".to_string());
                return Token::new(pos, TokenKind::Error, self.substr_from(offset));
            }
            match self.ch {
                '"' => {
                    self.next();
                    break;
                }
                '\\' => {
                    self.next();
                    match self.ch {
                        'n' | 't' | 'a' | 'b' | 'r' | '"' | '\\' => self.next(),
                        _ => {
                            let o = self.offset;
                            let ch = self.ch;
                            self.error(o, format!("unexpected character '{}' after '\\'", ch));
                            return Token::new(pos, TokenKind::Error, self.substr_from(offset));
                        }
                    }
                }
                _ => self.next(),
            }
        }
        Token::new(pos, TokenKind::String, self.substr_from(offset))
    }
}

fn is_operator(c: char) -> bool {
    "!@$%^&*-+=?<>/|~".chars().any(|x| x == c)
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;
    use position::*;
    use token::TokenKind;

    #[test]
    fn test_next_token() {
        let input = r#"
            let ten: Int = 10;
            let five: Int = 5;

            def identity<T>(x: T) -> T {
                return x;
            }
            if true {
                1
            } else {
                2
            }
            match true {
                true => "true",
                false => "\n\a\b\\\"",
            }
        "#;
        let tests = vec![// token type, literal
                         (TokenKind::Let, "let"),
                         (TokenKind::Ident, "ten"),
                         (TokenKind::Colon, ":"),
                         (TokenKind::Uident, "Int"),
                         (TokenKind::Eq, "="),
                         (TokenKind::Int, "10"),
                         (TokenKind::Semicolon, ";"),

                         (TokenKind::Let, "let"),
                         (TokenKind::Ident, "five"),
                         (TokenKind::Colon, ":"),
                         (TokenKind::Uident, "Int"),
                         (TokenKind::Eq, "="),
                         (TokenKind::Int, "5"),
                         (TokenKind::Semicolon, ";"),

                         (TokenKind::Def, "def"),
                         (TokenKind::Ident, "identity"),
                         (TokenKind::Langle, "<"),
                         (TokenKind::Uident, "T"),
                         (TokenKind::Rangle, ">"),
                         (TokenKind::Lparen, "("),
                         (TokenKind::Ident, "x"),
                         (TokenKind::Colon, ":"),
                         (TokenKind::Uident, "T"),
                         (TokenKind::Rparen, ")"),
                         (TokenKind::Arrow, "->"),
                         (TokenKind::Uident, "T"),
                         (TokenKind::Lbrace, "{"),
                         (TokenKind::Return, "return"),
                         (TokenKind::Ident, "x"),
                         (TokenKind::Semicolon, ";"),
                         (TokenKind::Rbrace, "}"),

                         (TokenKind::If, "if"),
                         (TokenKind::True, "true"),
                         (TokenKind::Lbrace, "{"),
                         (TokenKind::Int, "1"),
                         (TokenKind::Rbrace, "}"),
                         (TokenKind::Else, "else"),
                         (TokenKind::Lbrace, "{"),
                         (TokenKind::Int, "2"),
                         (TokenKind::Rbrace, "}"),

                         (TokenKind::Match, "match"),
                         (TokenKind::True, "true"),
                         (TokenKind::Lbrace, "{"),
                         (TokenKind::True, "true"),
                         (TokenKind::FatArrow, "=>"),
                         (TokenKind::String, r#""true""#),
                         (TokenKind::Comma, ","),
                         (TokenKind::False, "false"),
                         (TokenKind::FatArrow, "=>"),
                         (TokenKind::String, r#""\n\a\b\\\"""#),
                         (TokenKind::Comma, ","),
                         (TokenKind::Rbrace, "}"),

                         (TokenKind::EOF, "")];
        let file = File::new(Some("test"), input.len());
        let errors = Rc::default();
        let mut sc = Scanner::new(file, input, errors);

        for (i, (expected_kind, expected_lit)) in tests.into_iter().enumerate() {
            let tok = sc.scan();
            assert_eq!(expected_kind,
                       tok.kind(),
                       "test[#{}]: expected literal = {}",
                       i,
                       expected_lit);
            assert_eq!(expected_lit,
                       tok.symbol().as_str(),
                       "test[#{}]: expected literal= {}",
                       i,
                       expected_lit);
        }
    }

    #[test]
    fn test_short_input() {
        let input = "a";
        let file = File::new(Some("test"), input.len());
        let errors = Rc::default();
        let mut sc = Scanner::new(file, input, errors);
        let tok = sc.scan();
        assert_eq!(TokenKind::Ident, tok.kind());
        assert_eq!("a", tok.symbol().as_str());
        let tok = sc.scan();
        assert_eq!(TokenKind::EOF, tok.kind());
    }
}
