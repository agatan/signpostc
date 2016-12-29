use std::rc::Rc;
use std::cell::RefCell;

use token::{Token, TokenKind};
use position::File;
use scanner::Scanner;
use errors::ErrorList;
use ast::{Program, Decl};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    errors: Rc<RefCell<ErrorList>>,

    cur: Token,
}

impl<'a> Parser<'a> {
    pub fn new(file: File, src: &'a str) -> Self {
        let errors = Rc::new(RefCell::new(ErrorList::new()));
        let mut sc = Scanner::new(file, src, errors.clone());
        let token = sc.scan();
        Parser {
            scanner: sc,
            errors: errors,

            cur: token,
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut decls = Vec::new();
        while self.cur.kind() != TokenKind::EOF {
            decls.push(self.parse_decl());
        }
        Program { decls: decls }
    }

    pub fn into_errors(self) -> ErrorList {
        let errors = self.errors.clone();
        ::std::mem::drop(self);
        Rc::try_unwrap(errors).expect("errors ref count should be 1 here").into_inner()
    }

    pub fn parse_decl(&mut self) -> Decl {
        Decl::Dummy
    }
}
