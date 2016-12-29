use std::rc::Rc;
use std::cell::RefCell;

use token::{Token, TokenKind};
use position::{Pos, File};
use scanner::Scanner;
use errors::ErrorList;
use ast::{Program, Decl};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    errors: Rc<RefCell<ErrorList>>,

    current_token: Token,
    next_token: Token,

    error_count: usize,
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

    fn error(&mut self, pos: Pos, msg: String) {
        let position = self.scanner.file().position(pos);
        self.errors.borrow_mut().add(position, msg);
        self.error_count += 1;
    }

    fn current_error(&mut self, msg: String) {
        let pos = self.current_token.pos();
        self.error(pos, msg)
    }

    fn next_error(&mut self, msg: String) {
        let pos = self.next_token.pos();
        self.error(pos, msg)
    }

    pub fn succ_token(&mut self) {
        self.current_token = self.next_token;
        self.next_token = self.scanner.scan();
    }

    fn current_kind(&self) -> TokenKind {
        self.current_token.kind()
    }

    fn current_pos(&self) -> Pos {
        self.current_token.pos()
    }

    fn current_is(&self, kind: TokenKind) -> bool {
        self.current_token.kind() == kind
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        self.next_token.kind() == kind
    }

    fn expect_next(&mut self, kind: TokenKind, report_error: bool) -> bool {
        if self.next_is(kind) {
            self.succ_token();
            true
        } else {
            if report_error {
                let msg = format!("expected {:?}, got {:?}", kind, self.next_token.kind());
                let pos = self.next_token.pos();
                self.error(pos, msg);
            }
            false
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut decls = Vec::new();
        while self.next_token.kind() != TokenKind::EOF {
            decls.push(self.parse_decl());
        }
        Program { decls: decls }
    }

    /// succeed tokens and sync next declaration (for error reporting).
    fn sync_decl(&mut self) {
        loop {
            let kind = self.next_token.kind();
            if kind == TokenKind::Def || kind == TokenKind::Let || kind == TokenKind::EOF {
                break;
            }
            self.succ_token();
        }
    }

    pub fn parse_decl(&mut self) -> Decl {
        let result = match self.next_token.kind() {
            TokenKind::Def => self.parse_def(),
            _ => {
                let msg = format!("unexcepted token: {:?}", self.current_token.kind());
                self.current_error(msg);
                Decl::Error
            }
        };
        if Decl::Error == result {
            self.sync_decl();
        }
        result
    }

    fn parse_def(&mut self) -> Decl {
        if !self.expect_next(TokenKind::Def, true) {
            return Decl::Error;
        }
        let pos = self.current_token.pos();
        if !self.expect_next(TokenKind::Ident, true) {
            return Decl::Error;
        }
        let name = self.current_token.symbol();
        // TODO(agatan): generics parameters
        if !self.expect_next(TokenKind::Lparen, true) {
            return Decl::Error;
        }
        // TODO(gataan): parameters
        while !self.next_is(TokenKind::Rparen) {
            self.succ_token();
            if self.next_is(TokenKind::EOF) {
                self.next_error("unexpected EOF".to_string());
                return Decl::Error;
            }
        }
        self.succ_token();
        // TODO(agatan): return type spec
        if !self.expect_next(TokenKind::Lbrace, true) {
            return Decl::Error;
        }
        // TODO(gataan): body
        while !self.next_is(TokenKind::Rbrace) {
            self.succ_token();
            if self.next_is(TokenKind::EOF) {
                self.next_error("unexpected EOF".to_string());
                return Decl::Error;
            }
        }
        self.succ_token();

        Decl::Def(pos, name)
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
        assert_eq!(program.decls.len(), 1, "decls size is not 1: {:?}", program.decls);
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
