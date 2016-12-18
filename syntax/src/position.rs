use std::fmt;

/// `Position` represents a source position including file name, line number, column number and
/// byte offset.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Position<'a> {
    pub filename: Option<&'a str>, // None if the source is stdin
    pub line: isize, // starting at 1. if 0, the position is dummy.
    pub column: isize, // byte offset from the head of the line, starting at 1
    pub offset: isize, // byte offset from the head of the file, starting at 0
}

impl<'a> Position<'a> {
    pub fn dummy() -> Self {
        Position {
            filename: None,
            line: 0,
            column: 0,
            offset: 0,
        }
    }

    pub fn is_valid(&self) -> bool {
        self.line > 0 && self.column > 0
    }
}

impl<'a> fmt::Display for Position<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_valid() {
            match self.filename {
                None => write!(f, "{}:{}", self.line, self.column),
                Some(name) => write!(f, "{}:{}:{}", name, self.line, self.column),
            }
        } else {
            "-".fmt(f)
        }
    }
}
