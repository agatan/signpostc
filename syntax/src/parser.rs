use std::rc::Rc;
use std::cell::RefCell;

use token::{Token, TokenKind};
use position::{Pos, Position, File};
use scanner::Scanner;
use errors::{ErrorList, Error};
use ast::{Program, Decl};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    errors: Rc<RefCell<ErrorList>>,

    current_token: Token,
    next_token: Token,

    error_count: usize,
}

macro_rules! try_parse {
    ( $parse:expr, $ret:expr ) => {
        if let Err(e) = $parse {
            self.annotate_error(e);
            return $ret;
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(file: File, src: &'a str) -> Self {
        let errors = Rc::new(RefCell::new(ErrorList::new()));
        let mut sc = Scanner::new(file, src, errors.clone());
        let cur_token = Token::dummy();
        let next_token = sc.scan();
        Parser {
            scanner: sc,
            errors: errors,

            current_token: cur_token,
            next_token: next_token,

            error_count: 0,
        }
    }

    pub fn into_errors(self) -> ErrorList {
        let errors = self.errors.clone();
        ::std::mem::drop(self);
        Rc::try_unwrap(errors).expect("errors ref count should be 1 here").into_inner()
    }

    fn annotate_error(&mut self, err: Error) {
        self.errors.borrow_mut().add_error(err);
        self.error_count += 1;
    }

    fn make_error(&self, pos: Pos, msg: String) -> Error {
        let position = self.scanner.file().position(pos);
        Error {
            position: position,
            message: msg,
        }
    }

    fn current_error(&mut self, msg: String) {
        let pos = self.current_token.pos();
        let err = self.make_error(pos, msg);
        self.annotate_error(err)
    }

    fn next_error(&mut self, msg: String) {
        let pos = self.next_token.pos();
        let err = self.make_error(pos, msg);
        self.annotate_error(err)
    }

    fn position(&self, pos: Pos) -> Position {
        self.scanner.file().position(pos)
    }

    pub fn succ_token(&mut self) {
        self.current_token = self.next_token;
        self.next_token = self.scanner.scan();
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        self.next_token.kind() == kind
    }

    fn expect_next(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.next_is(kind) {
            self.succ_token();
            Ok(())
        } else {
            let msg = format!("expected {}, got {}", kind, self.next_token.kind());
            let pos = self.next_token.pos();
            Err(Error {
                position: self.position(pos),
                message: msg,
            })
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut decls = Vec::new();
        while !self.next_is(TokenKind::EOF) {
            decls.push(self.parse_decl());
        }
        Program { decls: decls }
    }

    /// succeed tokens and sync next declaration (for error reporting).
    fn sync_decl(&mut self) {
        loop {
            if self.next_is(TokenKind::Def) || self.next_is(TokenKind::Let) ||
               self.next_is(TokenKind::EOF) {
                break;
            }
            self.succ_token();
        }
    }

    pub fn parse_decl(&mut self) -> Decl {
        let result = match self.next_token.kind() {
            TokenKind::Def => self.parse_def(),
            _ => {
                let position = self.position(self.next_token.pos());
                let msg = format!("unexcepted token: {}", self.current_token.kind());
                Err(Error { position: position, message: msg })
            }
        };
        match result {
            Ok(d) => d,
            Err(e) => {
                self.annotate_error(e);
                self.sync_decl();
                Decl::Error
            }
        }
    }

    fn parse_def(&mut self) -> Result<Decl, Error> {
        self.expect_next(TokenKind::Def)?;
        let pos = self.current_token.pos();
        self.expect_next(TokenKind::Ident)?;
        let name = self.current_token.symbol();
        // TODO(agatan): generics parameters
        self.expect_next(TokenKind::Lparen)?;
        // TODO(gataan): parameters
        while !self.next_is(TokenKind::Rparen) {
            self.succ_token();
            if self.next_is(TokenKind::EOF) {
                let pos = self.next_token.pos();
                let position = self.position(pos);
                return Err(Error {
                    position: position,
                    message: "unexpected EOF".to_string(),
                });
            }
        }
        self.succ_token();
        // TODO(agatan): return type spec
        self.expect_next(TokenKind::Lbrace)?;
        // TODO(gataan): body
        while !self.next_is(TokenKind::Rbrace) {
            self.succ_token();
            if self.next_is(TokenKind::EOF) {
                let pos = self.next_token.pos();
                let position = self.scanner.file().position(pos);
                return Err(Error {
                    position: position,
                    message: "unexpected EOF".to_string(),
                });
            }
        }
        self.succ_token();

        Ok(Decl::Def(pos, name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use position::File;
    use ast::*;

    #[test]
    fn test_parse_def() {
        let input = r#"
            def f() {
            }
        "#;
        let file = File::new(None, input.len());
        let mut parser = Parser::new(file, input);
        let program = parser.parse_program();
        assert_eq!(program.decls.len(),
                   1,
                   "decls size is not 1: {:?}",
                   program.decls);
        let ref decl = program.decls[0];
        match *decl {
            Decl::Def(..) => (),
            _ => {
                panic!(format!("expected Decl::Def, got {:?}: error: {:?}",
                               decl,
                               parser.into_errors()))
            }
        }
    }
}