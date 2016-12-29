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
}

impl<'a> Parser<'a> {
    pub fn new(file: File, src: &'a str) -> Self {
        let errors = Rc::new(RefCell::new(ErrorList::new()));
        let mut sc = Scanner::new(file, src, errors.clone());
        let cur_token = sc.scan();
        let next_token = sc.scan();
        Parser {
            scanner: sc,
            errors: errors,

            current_token: cur_token,
            next_token: next_token,
        }
    }

    pub fn into_errors(self) -> ErrorList {
        let errors = self.errors.clone();
        ::std::mem::drop(self);
        Rc::try_unwrap(errors).expect("errors ref count should be 1 here").into_inner()
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

    fn expect_next(&mut self, kind: TokenKind) -> bool {
        if self.next_is(kind) {
            self.succ_token();
            true
        } else {
            false
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut decls = Vec::new();
        while self.current_token.kind() != TokenKind::EOF {
            decls.push(self.parse_decl());
        }
        Program { decls: decls }
    }

    pub fn parse_decl(&mut self) -> Decl {
        match self.current_token.kind() {
            TokenKind::Def => self.parse_def(),
            _ => {
                self.succ_token();
                Decl::Error
            }
        }
    }

    fn parse_def(&mut self) -> Decl {
        let pos = self.current_token.pos();
        self.succ_token();
        Decl::Def(pos)
    }
}
