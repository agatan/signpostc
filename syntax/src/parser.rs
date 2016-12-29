use std::rc::Rc;
use std::cell::RefCell;

use position::File;
use scanner::Scanner;
use errors::ErrorList;
use ast;

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    errors: Rc<RefCell<ErrorList>>,
}

impl<'a> Parser<'a> {
    pub fn new(file: File, src: &'a str) -> Self {
        let errors = Rc::new(RefCell::new(ErrorList::new()));
        let sc = Scanner::new(file, src, errors.clone());
        Parser {
            scanner: sc,
            errors: errors,
        }
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let _ = self.scanner.scan();
        ()
    }

    pub fn into_errors(self) -> ErrorList {
        let errors = self.errors.clone();
        ::std::mem::drop(self);
        Rc::try_unwrap(errors).expect("errors ref count should be 1 here").into_inner()
    }
}
