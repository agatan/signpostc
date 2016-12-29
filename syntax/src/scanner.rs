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
                _ => Token::new(pos, TokenKind::Error, self.substr_from(offset)),
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
}

fn is_prefix_operator(c: char) -> bool {
    "!-@".chars().any(|x| x == c)
}

fn is_operator(c: char) -> bool {
    "!@$%^&*-+=?<>/|~".chars().any(|x| x == c)
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
            let ten: Int = 10;
            let five: Int = 5;

            def identity(x: Int) -> Int {
                return x;
            }
            if true {
                1
            } else {
                2
            }
            match true {
                true => 1,
                false => 2,
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
                         (TokenKind::Lparen, "("),
                         (TokenKind::Ident, "x"),
                         (TokenKind::Colon, ":"),
                         (TokenKind::Uident, "Int"),
                         (TokenKind::Rparen, ")"),
                         (TokenKind::Arrow, "->"),
                         (TokenKind::Uident, "Int"),
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
                         (TokenKind::Int, "1"),
                         (TokenKind::Comma, ","),
                         (TokenKind::False, "false"),
                         (TokenKind::FatArrow, "=>"),
                         (TokenKind::Int, "2"),
                         (TokenKind::Comma, ","),
                         (TokenKind::Rbrace, "}"),

                         (TokenKind::EOF, "")];
        let file = File::new(Some("test"), input.len());
        let errors = ErrorList::new();
        let mut sc = Scanner::new(file, input, &errors);

        for (expected_kind, expected_lit) in tests {
            let tok = sc.scan();
            assert_eq!(expected_kind, tok.kind());
            assert_eq!(expected_lit, tok.literal());
        }
    }

    #[test]
    fn test_short_input() {
        let input = "a";
        let file = File::new(Some("test"), input.len());
        let errors = ErrorList::new();
        let mut sc = Scanner::new(file, input, &errors);
        {
            let tok = sc.scan();
            assert_eq!(TokenKind::Ident, tok.kind());
            assert_eq!("a", tok.literal());
        }
        {
            let tok = sc.scan();
            assert_eq!(TokenKind::EOF, tok.kind());
        }
    }
}
