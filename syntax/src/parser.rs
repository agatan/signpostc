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
        let cur_token = sc.scan();
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

    /// succeed tokens and sync next declaration (for error reporting).
    fn sync_decl(&mut self) {
        loop {
            let kind = self.current_token.kind();
            if kind == TokenKind::Def || kind == TokenKind::Let {
                break;
            }
            self.succ_token();
        }
    }

    pub fn parse_decl(&mut self) -> Decl {
        let result = match self.current_token.kind() {
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
        let pos = self.current_token.pos();
        self.succ_token();
        Decl::Def(pos)
    }
}
