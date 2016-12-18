//! This module implements `ErrorList` that holds errors in the syntax checking phase.
use std::cell::RefCell;
use std::error::Error as StdError;
use std::fmt;
use position::Position;

#[derive(Debug, Clone)]
pub struct Error<'a> {
    pub position: Position<'a>,
    pub message: String,
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.message)
    }
}

impl<'a> StdError for Error<'a> {
    fn description(&self) -> &str {
        "syntax error"
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct ErrorList<'a> {
    errors: RefCell<Vec<Error<'a>>>,
}

impl<'a> ErrorList<'a> {
    pub fn new() -> Self {
        ErrorList {
            errors: RefCell::default(),
        }
    }

    pub fn add(&self, position: Position<'a>, msg: String) {
        self.errors.borrow_mut().push(Error { position: position, message: msg })
    }
}

impl<'a> fmt::Display for ErrorList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let errs = self.errors.borrow();
        match errs.split_first() {
            None => "no errors".fmt(f),
            Some((first, rest)) => {
                write!(f, "{} (and {} more errors)", first, rest.len())
            }
        }
    }
}

impl<'a> StdError for ErrorList<'a> {
    fn description(&self) -> &str {
        "syntax error"
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}
