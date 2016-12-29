//! This module implements `ErrorList` that holds errors in the syntax checking phase.
use std::error::Error as StdError;
use std::fmt;
use position::Position;

#[derive(Debug, Clone)]
pub struct Error {
    pub position: Position,
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.message)
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        "syntax error"
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}

#[derive(Debug, Clone, Default)]
pub struct ErrorList {
    errors: Vec<Error>,
}

impl ErrorList {
    pub fn new() -> Self {
        ErrorList::default()
    }

    pub fn add(&mut self, position: Position, msg: String) {
        self.errors.push(Error { position: position, message: msg })
    }
}

impl fmt::Display for ErrorList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.errors.split_first() {
            None => "no errors".fmt(f),
            Some((first, rest)) => {
                write!(f, "{} (and {} more errors)", first, rest.len())
            }
        }
    }
}

impl StdError for ErrorList {
    fn description(&self) -> &str {
        "syntax error"
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}
